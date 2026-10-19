# =============================================================================
# HPP 0-6 anos | CNES + SIA — UMA TABELA POR ESTABELECIMENTO
#
# Saida unica: estabelecimentos.csv (e .xlsx, se openxlsx estiver instalado).
# Uma linha por CNES, com tudo: tipo de unidade, leitos, habilitacoes,
# servicos, numeros do SIH, do linkage, dos obitos sem par e do SIA.
# Tudo o mais (cobertura por tipo, mapeamento dos hospitais) e recorte ou
# soma dessa tabela, impresso no console — nao gera arquivo novo.
#
#   PARTE 0 · baixa CNES (ST/HB/SR/LT) + nomes    ~30-60 min   [rede]
#   PARTE 1 · agrega o SIH por estabelecimento    ~20-30 min   [local]
#   PARTE 2 · MONTA A TABELA e imprime as leituras  segundos   [local]
#   PARTE 3 · baixa o SIA e acrescenta 3 colunas    horas      [rede]
#   PARTE 4 · TESTE: o BPA-I registra o obito?    ~10-40 min   [rede]
#             Baixa poucos UF-meses do SIA-PA, abre por tipo de documento
#             (BPA-C / BPA-I / APAC / RAAS) e confere, CNES a CNES e
#             competencia a competencia, se os obitos da DO aparecem como
#             "motivo de saida = obito" na producao ambulatorial. Grava
#             sia_bpai_teste.rds + relatorio_bpai_teste.txt. Independe das
#             partes 2 e 3.
#
# ORDEM SUGERIDA:  RODAR <- c(0, 1)  ->  RODAR <- 2  ->  (depois) RODAR <- 3
#                  RODAR <- 4 pode ir a qualquer momento (so precisa do SIM e
#                  do cnes_obitos_nao_capturados.csv).
#
# PRE-REQUISITO: 00_link_sih_sim_v3.R rodado na versao 3.3.
# Sessao LIMPA do R. Todas as partes sao retomaveis.
#
# -----------------------------------------------------------------------------
# NOTAS QUE IMPORTAM
#
# NOME DO ESTABELECIMENTO. Nenhuma das 4 bases do CNES traz o nome. Ele vem da
#   tabela auxiliar CADGER (microdatasus::fetch_cadger), baixada na PARTE 0.
#
# INTERNACAO POR CNES. O agregado da Etapa 5 do linkage nao guarda CNES. A
#   PARTE 1 rele os lotes do SIH direto. Nao mexe no linkage nem invalida o
#   cache dele.
#
# SIA. Nao identifica paciente e so registra obito em APAC/RAAS — no BPA, que
#   e o grosso da urgencia, o campo vem vazio. O SIA NAO devolve o obito: ele
#   mostra que a unidade atendia crianca de 0 a 6 naquela competencia, e em
#   que volume. Corroboracao de universo, nao pareamento.
#
# CODIGOS DE LEITO / SERVICO / HABILITACAO. As tabelas de-para completas do
#   DATASUS nao estao embutidas. O que e seguro esta mapeado; o resto sai como
#   "codigo NN (conferir)" e a PARTE 2 imprime os mais frequentes. Nada e
#   chutado.
#
# TIPO DE UNIDADE AO LONGO DO TEMPO. Vem do cadastro mais recente do periodo.
#   Unidade que mudou de tipo entre 2015 e 2024 aparece com o tipo final.
# =============================================================================


# #############################################################################
# CONTROLE
# #############################################################################

RODAR <- 4

MODO_SIA <- "piloto"   # so afeta a PARTE 3: "piloto" | "alvo" | "completo"

# PARTE 4 — quais UF-meses testar. "auto" escolhe os N UF-meses com mais
# obito sem par em LOCOCOR 2 (outro estabelecimento de saude), fora das UFs
# grandes (um mes de SIA-PA de SP passa de 1 GB). Para escolher na mao:
#   TESTE_BPAI$ufmes <- data.frame(uf = c("PE","PA"), ano = c(2022, 2023),
#                                  mes = c(5, 9))
TESTE_BPAI <- list(
  modo       = "auto",
  n          = 4,
  excluir_uf = c("SP", "MG", "RJ", "BA"),
  ufmes      = NULL,
  # SIM com todas as variaveis (o mesmo do 00_link_sih_sim_v3.R)
  arq_sim    = "G:/My Drive/INSPER/Trabalho/SIM/Dados/sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds")

# Hospitais do escopo. Busca por padrao de nome, restrita ao municipio (IBGE
# 6 digitos). Marca a coluna `hospital_escopo` na tabela — nao filtra nada.
# ATENCAO AOS PADROES. Na rodada de 26/08 "ICR" casou dentro de MICROecologia,
# MICROdent e MIKROS, e "SABARA" pegou laboratorio do bairro Sabara. Padrao
# curto agora vai com \b (limite de palavra) e os genericos ganharam contexto.
# Mesmo assim busca por nome erra: confira `escopo_relevante` antes de usar.
HOSPITAIS <- data.frame(
  rotulo    = c("HPP (Pequeno Principe)", "Sabara", "IMIP (Recife)",
                "Santo Antonio (POA)", "Instituto da Crianca (USP)"),
  padrao    = c("PEQUENO PRINCIPE",
                "INFANTIL SABARA|SABARA.*INFANTIL",
                "\\bIMIP\\b|MEDICINA INTEGRAL|FERNANDO FIGUEIRA",
                "CRIANCA SANTO ANTONIO|SANTA CASA|\\bSANTO ANTONIO\\b",
                "INSTITUTO DA CRIANCA|\\bICR\\b|HOSPITAL DAS CLINICAS.*FMUSP"),
  municipio = c("410690", "355030", "261160", "431490", "355030"),
  stringsAsFactors = FALSE)


# #############################################################################
# CONFIGURACAO
# #############################################################################

pacotes <- c("microdatasus", "dplyr", "data.table")
for (p in pacotes)
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(microdatasus); library(dplyr); library(data.table)
})

# DATASUS: HTTP nao fecha conexao a partir da rede do INSPER. O microdatasus
# vai por FTP porta 21. Nao trocar.
options(timeout = max(3600, getOption("timeout", 60)))

CFG <- list(
  dir_dash = "G:/My Drive/INSPER/Trabalho/Dash",
  dir_sih  = "G:/My Drive/INSPER/Trabalho/SIH/Dados/Temporarios_UF_Ano",
  ufs  = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE",
           "AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"),
  anos = 2015:2024,
  mes_ref = 12L,
  idade_max_anos = 6L,
  piloto_uf = "AC", piloto_ano = 2019)

CFG$dir_cnes  <- file.path(CFG$dir_dash, "dados", "cnes_bruto")
CFG$dir_sia   <- file.path(CFG$dir_dash, "dados", "sia_chunks")
CFG$arq_cadger<- file.path(CFG$dir_cnes, "_cadger.rds")
CFG$arq_sih   <- file.path(CFG$dir_dash, "dados", "sih_por_cnes.rds")
CFG$arq_link  <- file.path(CFG$dir_dash, "sih_sim_linkado.rds")
CFG$arq_nc    <- file.path(CFG$dir_dash, "cnes_obitos_nao_capturados.csv")
CFG$SAIDA     <- file.path(CFG$dir_dash, "estabelecimentos.csv")
CFG$SAIDA_XL  <- file.path(CFG$dir_dash, "estabelecimentos.xlsx")
CFG$SAIDA_DASH<- file.path(CFG$dir_dash, "estabelecimentos_dash.csv")
CFG$SAIDA_BPAI<- file.path(CFG$dir_dash, "sia_bpai_teste.rds")
CFG$REL_BPAI  <- file.path(CFG$dir_dash, "relatorio_bpai_teste.txt")
CFG$dir_bpai  <- file.path(CFG$dir_dash, "dados", "sia_bpai")
# No Google Drive o dir.create as vezes falha calado (pasta pai sincronizando).
# Como showWarnings = FALSE esconde isso, o erro so aparecia la na frente, no
# saveRDS, como "cannot open the connection". Agora cria, confere e reclama.
garante_pasta <- function(caminho) {
  if (dir.exists(caminho)) return(invisible(TRUE))
  dir.create(caminho, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(caminho)) {           # segunda tentativa: Drive sincronizando
    Sys.sleep(2)
    dir.create(caminho, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(caminho))
    stop("Nao consegui criar a pasta:\n  ", caminho,
         "\nCrie ela na mao no Explorer e rode de novo.", call. = FALSE)
  invisible(TRUE)
}
# Cria apenas as pastas necessarias para as partes selecionadas. A PARTE 2
# apenas le os insumos existentes e ja informa claramente quando eles faltam.
if (0 %in% RODAR) garante_pasta(CFG$dir_cnes)
if (3 %in% RODAR) garante_pasta(CFG$dir_sia)
if (4 %in% RODAR) garante_pasta(CFG$dir_bpai)

titulo <- function(t) cat("\n", strrep("=", 70), "\n  ", t, "\n",
                          strrep("=", 70), "\n", sep = "")
fmt  <- function(x) format(x, big.mark = ".", scientific = FALSE)
pad2 <- function(x) formatC(as.character(x), width = 2, flag = "0")
cnes7 <- function(x) { x <- gsub("[^0-9]", "", as.character(x))
x[x == "" | is.na(x)] <- NA_character_
ifelse(is.na(x), NA_character_, formatC(x, width = 7, flag = "0")) }
mun6 <- function(x) substr(gsub("[^0-9]", "", as.character(x)), 1, 6)
cid3 <- function(x) { x <- toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
x <- substr(x, 1, 3); x[!grepl("^[A-Z][0-9]{2}$", x)] <- NA_character_; x }
sem_acento <- function(x) { x <- toupper(as.character(x))
x <- iconv(x, to = "ASCII//TRANSLIT")
gsub("[^A-Z0-9 ]", " ", ifelse(is.na(x), "", x)) }
# Para PADRAO de regex: tira acento mas PRESERVA "|" e companhia. Usar
# sem_acento() aqui mataria a alternancia de "IMIP|MEDICINA INTEGRAL".
#
# NAO usar toupper aqui: \b (limite de palavra) vira \B (o OPOSTO de limite
# de palavra) e o padrao passa a casar exatamente o que deveria excluir — foi
# assim que "ICR" casou dentro de MICROecologia e "IMIP" parou de casar.
# Os padroes em HOSPITAIS ja vem em maiuscula.
sem_acento_pat <- function(x) iconv(as.character(x), to = "ASCII//TRANSLIT")

# Layout de HB/SR/LT varia entre competencias. Em vez de fixar um nome e
# quebrar em silencio, procura entre candidatos e registra o que faltou.
FALTANTES <- character(0)
pega_col <- function(d, cands, rotulo) {
  ok <- intersect(cands, names(d))
  if (!length(ok)) { FALTANTES <<- unique(c(FALTANTES, rotulo))
  return(rep(NA_character_, nrow(d))) }
  as.character(d[[ok[1]]])
}

# ---- de-para de tipo de unidade -------------------------------------------
# NOME diferente da coluna TP_UNID do ST de proposito: o data.table resolve
# nome de coluna antes do ambiente global e a de-para seria ignorada.
TP_UNID_NOME <- c(
  "01"="Posto de saude", "02"="Centro de saude / unidade basica",
  "04"="Policlinica", "05"="Hospital geral", "07"="Hospital especializado",
  "09"="Pronto socorro de hospital geral (antigo)",
  "12"="Pronto socorro traumato-ortopedico (antigo)", "15"="Unidade mista",
  "20"="Pronto socorro geral", "21"="Pronto socorro especializado",
  "22"="Consultorio", "32"="Unidade movel fluvial",
  "36"="Clinica / ambulatorio especializado",
  "39"="Unidade de apoio de diagnose e terapia", "40"="Unidade movel terrestre",
  "42"="Unidade movel pre-hospitalar (urgencia/emergencia)", "43"="Farmacia",
  "45"="Unidade de saude da familia", "50"="Unidade de vigilancia em saude",
  "60"="Cooperativa", "61"="Centro de parto normal", "62"="Hospital dia",
  "63"="Unidade autorizadora", "64"="Central de regulacao de servicos",
  "65"="Vigilancia epidemiologica (antigo)", "66"="Vigilancia sanitaria (antigo)",
  "67"="LACEN", "68"="Secretaria de saude",
  "69"="Centro de atencao hemoterapica/hematologica",
  "70"="Centro de atencao psicossocial (CAPS)",
  "71"="Centro de apoio a saude da familia (CASF)",
  "72"="Unidade de atencao a saude indigena", "73"="Pronto atendimento (UPA)",
  "74"="Academia da saude", "75"="Telessaude",
  "76"="Central de regulacao medica das urgencias",
  "77"="Atencao domiciliar isolado (home care)",
  "78"="Atencao em regime residencial", "79"="Oficina ortopedica",
  "80"="Laboratorio de saude publica", "81"="Central de regulacao")

TP_URGENCIA <- c("73","20","21","09","12","42","15")   # atende sem internar
TP_BASICA   <- c("01","02","45","04","36")
TP_SIA      <- c(TP_URGENCIA, TP_BASICA)               # recorte do SIA
MOTSAI_OBITO <- c("41","42","43")

natureza_de <- function(cod) { cod <- pad2(cod)
dplyr::case_when(cod %in% TP_URGENCIA       ~ "Urgencia sem internacao",
                 cod %in% c("05","07","62") ~ "Com internacao",
                 cod %in% TP_BASICA         ~ "Atencao basica / ambulatorial",
                 is.na(cod)                 ~ "Nao informado",
                 TRUE                       ~ "Outro") }

# ---- leitos: o codigo SO e unico dentro do TP_LEITO -------------------------
# Erro corrigido em 26/08: a primeira versao usava so CODLEITO e tratava como
# se fosse chave unica. Nao e. O mesmo numero aparece em grupos diferentes —
# 45 e "pediatria clinica" no grupo PEDIATRICO e outra coisa no CIRURGICO.
# A chave certa e o par (TP_LEITO, CODLEITO), e e assim que o script le agora.
#
# TP_LEITO: 1 cirurgico · 2 clinico · 3 complementar · 4 obstetrico
#           5 pediatrico · 6 outras especialidades · 7 hospital dia
#
# Abaixo so o que interessa a coorte 0-6 e foi conferido na tabela de dominio
# do CNES. O resto sai como "tipo T codigo NN (conferir)" — nada e chutado.
LEITO_UTI_NEO  <- c("63","80","81","82")          # UTI neonatal (e tipos I-III)
LEITO_UTI_PED  <- c("62","77","78","79")          # UTI pediatrica / infantil
LEITO_UCI_NEO  <- c("65","92","93")               # intermediaria / UCI neonatal
LEITO_UCI_PED  <- c("94")                         # UCI pediatrico
LEITO_PEDIATRICO <- c("45","68")                  # pediatria clinica/cirurgica

LEITO_NOME <- c(
  "3-62"="UTI infantil", "3-63"="UTI neonatal",
  "3-77"="UTI pediatrica tipo I", "3-78"="UTI pediatrica tipo II",
  "3-79"="UTI pediatrica tipo III", "3-80"="UTI neonatal tipo I",
  "3-81"="UTI neonatal tipo II", "3-82"="UTI neonatal tipo III",
  "3-64"="Unidade intermediaria", "3-65"="Unidade intermediaria neonatal",
  "3-92"="UCI neonatal convencional", "3-93"="UCI neonatal canguru",
  "3-94"="UCI pediatrico", "3-95"="UCI adulto",
  "5-45"="Pediatria clinica", "5-68"="Pediatria cirurgica",
  "2-33"="Clinica geral", "4-10"="Obstetricia cirurgica",
  "4-43"="Obstetricia clinica")

nome_leito <- function(tp, cod) {
  k <- paste0(as.character(tp), "-", pad2(cod))
  v <- unname(LEITO_NOME[k])
  ifelse(is.na(v), paste0("tipo ", tp, " codigo ", pad2(cod), " (conferir)"), v)
}


# #############################################################################
# PARTE 0 — CNES (ST, HB, SR, LT) + nomes
# #############################################################################

parte_0 <- function() {
  titulo("PARTE 0 · CNES (ST, HB, SR, LT) + nomes")
  
  if (!file.exists(CFG$arq_cadger)) {
    cat("Baixando CADGER (nomes)...\n")
    cad <- tryCatch(microdatasus::fetch_cadger(),
                    error = function(e) { cat("  [erro] ", conditionMessage(e),
                                              "\n"); NULL })
    if (!is.null(cad)) {
      cad <- as.data.table(cad); cad[, codestab := cnes7(CNES)]
      saveRDS(cad, CFG$arq_cadger)
      cat("  ", fmt(nrow(cad)), " estabelecimentos nomeados.\n", sep = "")
    } else cat("  Sem CADGER: a coluna de nome e a marcacao dos hospitais do\n",
               "  escopo ficam vazias. Rode a PARTE 0 de novo mais tarde.\n", sep = "")
  } else cat("CADGER ja baixado.\n")
  
  falhas <- character(0)
  for (base in c("CNES-ST","CNES-HB","CNES-SR","CNES-LT")) {
    sig <- sub("CNES-", "", base); cat("\n--- ", base, " ---\n", sep = "")
    for (ano in CFG$anos) for (uf in CFG$ufs) {
      arq <- file.path(CFG$dir_cnes, sprintf("%s_%s_%d.rds", sig, uf, ano))
      if (file.exists(arq)) next
      d <- tryCatch(fetch_datasus(year_start = ano, month_start = CFG$mes_ref,
                                  year_end = ano, month_end = CFG$mes_ref,
                                  uf = uf, information_system = base,
                                  timeout = 1800),
                    error = function(e) { cat("   [erro] ", uf, ano, ": ",
                                              conditionMessage(e), "\n"); NULL })
      if (is.null(d) || nrow(d) == 0) {
        falhas <- c(falhas, sprintf("%s %s/%d", sig, uf, ano))
        rm(d); gc(verbose = FALSE); next }
      d <- as.data.table(d); d[, `:=`(uf_arq = uf, ano_arq = ano)]
      saveRDS(d, arq)
      cat(sprintf("  %s %s %d: %s linhas\n", sig, uf, ano, fmt(nrow(d))))
      rm(d); gc(verbose = FALSE)
    }
  }
  if (length(falhas)) { cat("\nSem dados / erro (rode de novo):\n")
    print(utils::head(falhas, 30))
    if (length(falhas) > 30) cat("  ... e mais ", length(falhas) - 30, "\n")
  } else cat("\nSem falhas.\n")
  invisible(TRUE)
}


# #############################################################################
# PARTE 1 — SIH agregado por estabelecimento
# #############################################################################

parte_1 <- function() {
  titulo("PARTE 1 · SIH agregado por estabelecimento")
  if (!dir.exists(CFG$dir_sih))
    stop("Pasta do SIH nao encontrada: ", CFG$dir_sih, call. = FALSE)
  
  data_sih <- function(x) as.Date(as.character(x), format = "%Y%m%d")
  faixa_de <- function(d) dplyr::case_when(
    is.na(d) ~ NA_character_, d <= 27 ~ "Neonatal (0-27 dias)",
    d < 365 ~ "Pos-neonatal (28 d-<1 ano)", d <= 365*7 ~ "1 a 6 anos",
    TRUE ~ NA_character_)
  
  partes <- list()
  for (uf in CFG$ufs) {
    ini <- Sys.time(); n_uf <- 0L
    for (ano in CFG$anos) {
      f <- file.path(CFG$dir_sih, sprintf("sih_%s_%d.rds", uf, ano))
      if (!file.exists(f)) next
      df <- tryCatch(readRDS(f), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) next
      g <- function(n) if (n %in% names(df)) df[[n]] else NA
      d <- data.table(cnes = cnes7(g("CNES")), dt_nasc = data_sih(g("NASC")),
                      dt_saida = data_sih(g("DT_SAIDA")),
                      morte = suppressWarnings(as.integer(g("MORTE"))),
                      cobranca = as.character(g("COBRANCA")))
      rm(df)
      d[, idade_dias := as.integer(dt_saida - dt_nasc)]
      d <- d[!is.na(idade_dias) & idade_dias >= 0 &
               idade_dias <= 365*(CFG$idade_max_anos+1) & !is.na(cnes)]
      if (!nrow(d)) { rm(d); next }
      cob2 <- suppressWarnings(as.integer(substr(d$cobranca, 1, 2)))
      d[, desfecho := dplyr::case_when(
        !is.na(morte) & morte == 1 ~ "Obito", cob2 %in% 41:43 ~ "Obito",
        cob2 %in% 31:39 ~ "Transferencia", cob2 %in% 11:19 ~ "Alta",
        TRUE ~ "Outro")]
      d[, `:=`(faixa = faixa_de(idade_dias), ano = ano, uf = uf)]
      partes[[length(partes)+1]] <- d[, .(aih = .N),
                                      by = .(cnes, uf, ano, faixa, desfecho)]
      n_uf <- n_uf + nrow(d); rm(d); gc(verbose = FALSE)
    }
    cat(sprintf("  %s: %s AIHs 0-6 · %.1f min\n", uf, fmt(n_uf),
                as.numeric(difftime(Sys.time(), ini, units = "mins"))))
  }
  if (!length(partes)) stop("Nenhum lote do SIH lido.", call. = FALSE)
  ag <- rbindlist(partes)[, .(aih = sum(aih)),
                          by = .(cnes, uf, ano, faixa, desfecho)]
  saveRDS(ag, CFG$arq_sih)
  cat("\nGravado (insumo, nao e a tabela final): ", basename(CFG$arq_sih), " · ",
      fmt(nrow(ag)), " linhas · ", fmt(uniqueN(ag$cnes)), " CNES\n", sep = "")
  invisible(ag)
}


# #############################################################################
# PARTE 2 — MONTA A TABELA
# #############################################################################

le_cnes <- function(sig) {
  a <- list.files(CFG$dir_cnes,
                  pattern = sprintf("^%s_[A-Z]{2}_[0-9]{4}\\.rds$", sig),
                  full.names = TRUE)
  if (!length(a)) return(NULL)
  d <- rbindlist(lapply(a, function(f) tryCatch(readRDS(f), error = function(e) {
    cat("  [aviso] corrompido: ", basename(f), "\n"); NULL })),
    use.names = TRUE, fill = TRUE)
  if (!nrow(d)) return(NULL)
  d[, codestab := cnes7(CNES)]; d[!is.na(codestab)]
}

parte_2 <- function() {
  titulo("PARTE 2 · montando estabelecimentos.csv")
  
  st <- le_cnes("ST")
  if (is.null(st)) stop("Sem arquivos ST. Rode a PARTE 0.", call. = FALSE)
  
  # ---- cadastro: registro mais recente de cada CNES ------------------------
  setorder(st, codestab, -ano_arq)
  cad <- st[, .SD[1L], by = codestab]
  cad[, tipo_cod := pad2(pega_col(cad, "TP_UNID", "ST/TP_UNID"))]
  cad[, tipo_unidade := unname(TP_UNID_NOME[tipo_cod])]
  cad[is.na(tipo_unidade), tipo_unidade := paste0("codigo ", tipo_cod, " (conferir)")]
  T <- cad[, .(codestab, tipo_cod, tipo_unidade,
               natureza = natureza_de(tipo_cod),
               uf = uf_arq, cod_municipio = mun6(pega_col(cad, "CODUFMUN", "ST/CODUFMUN")),
               ano_cadastro = ano_arq,
               esfera = pega_col(cad, "ESFERA_A", "ST/ESFERA_A"),
               gestao = pega_col(cad, "TPGESTAO", "ST/TPGESTAO"),
               nivel_hierarquia = pega_col(cad, "NIV_HIER", "ST/NIV_HIER"),
               atende_urgencia = pega_col(cad, "URGEMERG", "ST/URGEMERG"),
               atende_ambulatorial = pega_col(cad, c("ATENDAMB","NIVATE_A"), "ST/ATENDAMB"),
               atende_hospitalar = pega_col(cad, c("ATENDHOS","NIVATE_H"), "ST/ATENDHOS"))]
  cat("Estabelecimentos no ST: ", fmt(nrow(T)), "\n", sep = "")
  
  # ---- nome ----------------------------------------------------------------
  if (file.exists(CFG$arq_cadger)) {
    nm <- unique(as.data.table(readRDS(CFG$arq_cadger))[, .(codestab, nome = FANTASIA)],
                 by = "codestab")
    T <- merge(T, nm, by = "codestab", all.x = TRUE)
  } else T[, nome := NA_character_]
  
  # ---- leitos (so a competencia mais recente de cada CNES) -----------------
  lt <- le_cnes("LT")
  if (!is.null(lt)) {
    # TP_LEITO e CODLEITO sao DUAS colunas distintas — o par e que identifica
    # a especialidade. Ler so uma delas foi o erro da primeira versao.
    lt[, leito_tp  := as.character(pega_col(lt, c("TP_LEITO","CODTPLEITO","TPLEITO"), "LT/TP_LEITO"))]
    lt[, leito_cod := pad2(pega_col(lt, c("CODLEITO","COD_LEITO"), "LT/CODLEITO"))]
    lt[, qt := suppressWarnings(as.numeric(
      pega_col(lt, c("QT_EXIST","QTDE_EXIST","QT_EXIS"), "LT/QT_EXIST")))]
    lt[, qs := suppressWarnings(as.numeric(
      pega_col(lt, c("QT_SUS","QTDE_SUS"), "LT/QT_SUS")))]
    lt[is.na(qt), qt := 0]; lt[is.na(qs), qs := 0]
    u <- lt[, .(ano_arq = max(ano_arq)), by = codestab]
    ltu <- merge(lt, u, by = c("codestab","ano_arq"))
    
    # Sem TP_LEITO nao da para separar UTI neonatal de qualquer outro "80":
    # nesse caso as colunas pediatricas ficam VAZIAS, nao aproximadas.
    tem_tp <- !all(is.na(ltu$leito_tp))
    if (!tem_tp)
      cat("\n[atencao] LT sem a coluna de tipo de leito. As colunas de UTI\n",
          "          neonatal/pediatrica ficam vazias — sem o par (tipo,codigo)\n",
          "          nao da para identificar a especialidade sem chutar.\n", sep = "")
    
    ag_lt <- if (tem_tp)
      ltu[, .(leitos_total = sum(qt), leitos_sus = sum(qs),
              leitos_uti_neo = sum(qt[leito_tp == "3" & leito_cod %in% LEITO_UTI_NEO]),
              leitos_uti_ped = sum(qt[leito_tp == "3" & leito_cod %in% LEITO_UTI_PED]),
              leitos_uci_neo = sum(qt[leito_tp == "3" & leito_cod %in% LEITO_UCI_NEO]),
              leitos_uci_ped = sum(qt[leito_tp == "3" & leito_cod %in% LEITO_UCI_PED]),
              leitos_pediatria = sum(qt[leito_tp == "5" & leito_cod %in% LEITO_PEDIATRICO])),
          by = codestab]
    else
      ltu[, .(leitos_total = sum(qt), leitos_sus = sum(qs)), by = codestab]
    
    if (tem_tp)
      ag_lt[, leitos_ped_neo := leitos_uti_neo + leitos_uti_ped +
              leitos_uci_neo + leitos_uci_ped + leitos_pediatria]
    T <- merge(T, ag_lt, by = "codestab", all.x = TRUE)
    
    cat("\nLeitos observados (top 20) — o que sair como (conferir) me manda:\n")
    print(head(ltu[, .(leitos = sum(qt)),
                   by = .(leito_tp, leito_cod,
                          nome = nome_leito(leito_tp, leito_cod))][order(-leitos)], 20))
  } else cat("\n[aviso] sem LT — colunas de leito vazias.\n")
  
  # ---- habilitacoes e servicos ---------------------------------------------
  hb <- le_cnes("HB")
  if (!is.null(hb)) {
    hb[, hab := pega_col(hb, c("SGRUPHAB","CO_HABILITACAO","COD_HAB"), "HB/SGRUPHAB")]
    hb <- hb[!is.na(hab) & nzchar(trimws(hab))]
    T <- merge(T, hb[, .(n_habilitacoes = uniqueN(hab),
                         habilitacoes = paste(sort(unique(hab)), collapse = ";")),
                     by = codestab], by = "codestab", all.x = TRUE)
    cat("\nHabilitacoes mais frequentes (top 10) — codigos brutos:\n")
    print(head(hb[, .(estab = uniqueN(codestab)), by = hab][order(-estab)], 10))
  } else cat("\n[aviso] sem HB — habilitacoes vazias.\n")
  
  sr <- le_cnes("SR")
  if (!is.null(sr)) {
    sr[, srv := pega_col(sr, c("SERV_ESP","CODSERVICO","COD_SERV"), "SR/SERV_ESP")]
    sr <- sr[!is.na(srv) & nzchar(trimws(srv))]
    T <- merge(T, sr[, .(n_servicos = uniqueN(srv),
                         servicos = paste(sort(unique(srv)), collapse = ";")),
                     by = codestab], by = "codestab", all.x = TRUE)
    cat("\nServicos especializados mais frequentes (top 10) — codigos brutos:\n")
    print(head(sr[, .(estab = uniqueN(codestab)), by = srv][order(-estab)], 10))
  } else cat("\n[aviso] sem SR — servicos vazios.\n")
  
  # ---- SIH ------------------------------------------------------------------
  if (file.exists(CFG$arq_sih)) {
    ag <- as.data.table(readRDS(CFG$arq_sih))
    w <- dcast(ag[, .(aih = sum(aih)), by = .(cnes, desfecho)],
               cnes ~ desfecho, value.var = "aih", fill = 0)
    setnames(w, "cnes", "codestab")
    nn <- setdiff(names(w), "codestab")
    setnames(w, nn, paste0("sih_", tolower(gsub("[^A-Za-z]", "", nn))))
    w[, sih_total := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("^sih_")]
    T <- merge(T, w, by = "codestab", all.x = TRUE)
  } else cat("\n[aviso] sem sih_por_cnes.rds — colunas do SIH vazias (PARTE 1).\n")
  
  # ---- linkage --------------------------------------------------------------
  if (file.exists(CFG$arq_link)) {
    lk <- as.data.table(readRDS(CFG$arq_link))
    lk[, codestab := cnes7(cnes)]
    T <- merge(T, lk[, .(link_episodios = .N,
                         link_obitos = sum(desfecho == "\u00d3bito", na.rm = TRUE),
                         link_transferencias = sum(desfecho == "Transfer\u00eancia", na.rm = TRUE),
                         link_pareados = sum(linkado, na.rm = TRUE)),
                     by = codestab], by = "codestab", all.x = TRUE)
    T[, link_pct_pareado := round(100 * link_pareados / pmax(1, link_obitos), 1)]
  } else cat("\n[aviso] sem sih_sim_linkado.rds — colunas do linkage vazias.\n")
  
  # ---- obitos sem par (lado SIM) --------------------------------------------
  if (file.exists(CFG$arq_nc)) {
    nc <- fread(CFG$arq_nc, colClasses = list(character = "codestab"))
    T <- merge(T, nc[, .(sim_obitos_sem_par = sum(obitos_nao_capturados),
                         sim_competencias_sem_par = .N), by = codestab],
               by = "codestab", all.x = TRUE)
  } else cat("\n[aviso] sem cnes_obitos_nao_capturados.csv (linkage v3.3).\n")
  
  # ---- SIA (se a PARTE 3 ja rodou) ------------------------------------------
  T <- junta_sia(T)
  
  # ---- perfil, porte e hospitais do escopo ----------------------------------
  if ("leitos_uti_neo" %in% names(T)) {
    T[, perfil_servico := fifelse(
      leitos_uti_neo > 0 & leitos_uti_ped > 0, "UTI neonatal + UTI pediatrica",
      fifelse(leitos_uti_neo > 0, "So UTI neonatal",
              fifelse(leitos_uti_ped > 0, "So UTI pediatrica",
                      fifelse(leitos_ped_neo > 0, "Leito pediatrico sem UTI",
                              "Sem leito pediatrico"))))]
    T[, porte := as.character(cut(leitos_total, c(-Inf,0,50,150,300,Inf),
                                  labels = c("Sem leito","Ate 50","51-150",
                                             "151-300","301+")))]
  }
  T[, hospital_escopo := NA_character_]
  if (!all(is.na(T$nome))) {
    T[, .nb := sem_acento(nome)]
    for (i in seq_len(nrow(HOSPITAIS))) {
      h <- HOSPITAIS[i, ]
      idx <- grepl(sem_acento_pat(h$padrao), T$.nb, perl = TRUE) &
        (is.na(h$municipio) | T$cod_municipio == h$municipio)
      T[idx, hospital_escopo := h$rotulo]
    }
    T[, .nb := NULL]
  }
  
  # Estabelecimento com algum volume. Consultorio, laboratorio e clinica de
  # diagnostico entram no CNES mas nao internam ninguem — mante-los na mesma
  # lista que hospital de mil leitos so atrapalha a leitura.
  vol <- function(n) if (n %in% names(T)) as.numeric(T[[n]]) else 0
  T[, escopo_relevante := (vol("leitos_total") > 0 | vol("sih_total") > 0 |
                             vol("link_episodios") > 0 |
                             vol("sim_obitos_sem_par") > 0)]
  
  # zeros em vez de NA nas colunas de contagem
  for (cc in names(T))
    if (is.numeric(T[[cc]])) T[is.na(get(cc)), (cc) := 0]
  
  ordem <- c("codestab","nome","hospital_escopo","escopo_relevante",
             "tipo_cod","tipo_unidade",
             "natureza","perfil_servico","porte","uf","cod_municipio",
             "leitos_total","leitos_sus","leitos_ped_neo","leitos_pediatria",
             "leitos_uti_neo","leitos_uti_ped","leitos_uci_neo","leitos_uci_ped",
             "n_habilitacoes","n_servicos",
             "sih_total","sih_alta","sih_obito","sih_transferencia","sih_outro",
             "link_episodios","link_obitos","link_transferencias",
             "link_pareados","link_pct_pareado",
             "sim_obitos_sem_par","sim_competencias_sem_par",
             "sia_registros_0a6","sia_registros_urgencia","sia_competencias",
             "esfera","gestao","nivel_hierarquia","atende_urgencia",
             "atende_ambulatorial","atende_hospitalar","ano_cadastro",
             "habilitacoes","servicos")
  setcolorder(T, intersect(ordem, names(T)))
  if ("link_obitos" %in% names(T)) setorder(T, -link_obitos, -sih_total)
  
  fwrite(T, CFG$SAIDA, bom = TRUE)
  cat("\n>>> TABELA: ", CFG$SAIDA, " — ", fmt(nrow(T)), " estabelecimentos, ",
      ncol(T), " colunas\n", sep = "")

  # Recorte leve para o app: so quem tem leito ou movimento, e so as colunas
  # que o painel usa. E ESTE arquivo que sobe para o shinyapps — o CSV
  # completo tem 535 mil linhas / 87 MB e nao deve ser publicado.
  cols_dash <- intersect(c("codestab","nome","hospital_escopo","tipo_cod",
                           "tipo_unidade","natureza","perfil_servico","porte",
                           "uf","cod_municipio","leitos_total","leitos_sus",
                           "leitos_uti_neo","leitos_uti_ped","leitos_uci_neo",
                           "leitos_uci_ped","leitos_pediatria","sih_total",
                           "sih_obito","link_episodios","link_obitos",
                           "link_pareados","link_pct_pareado",
                           "sim_obitos_sem_par","gestao","atende_urgencia",
                           "atende_hospitalar"), names(T))
  dash <- T[escopo_relevante == TRUE | !is.na(hospital_escopo), ..cols_dash]
  fwrite(dash, CFG$SAIDA_DASH, bom = FALSE)
  cat(">>> TABELA: ", CFG$SAIDA_DASH, " — ", fmt(nrow(dash)),
      " linhas (recorte para o app; e este que vai no deploy)\n", sep = "")
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    # O CSV leva o CNES inteiro; o xlsx leva so quem tem leito ou movimento.
    # 535 mil linhas num xlsx nao e planilha, e castigo.
    xl <- T[escopo_relevante == TRUE | !is.na(hospital_escopo)]
    openxlsx::write.xlsx(as.data.frame(xl), CFG$SAIDA_XL, asTable = TRUE,
                         overwrite = TRUE)
    cat(">>> TABELA: ", CFG$SAIDA_XL, " — ", fmt(nrow(xl)),
        " com leito ou movimento (o CSV tem todos)\n", sep = "")
  } else cat("[nota] openxlsx nao instalado — so o CSV. install.packages(\"openxlsx\")\n")
  
  leituras(T)
  if (length(FALTANTES)) {
    cat("\n[atencao] colunas nao encontradas (ficaram vazias): ",
        paste(FALTANTES, collapse = ", "), "\n", sep = "")
    cat("Me mande essa lista que eu ajusto os nomes candidatos.\n")
  }
  invisible(T)
}

# As leituras que a revisao pediu — recortes da MESMA tabela, no console.
leituras <- function(T) {
  if ("link_obitos" %in% names(T)) {
    cat("\n--- 1. ONDE O LINKAGE ENCONTRA A INTERNACAO (lado SIH) ---\n")
    cat("Dos episodios com desfecho obito no SIH, quantos acharam a DO.\n\n")
    a <- T[link_obitos > 0, .(estab = .N, obitos = sum(link_obitos),
                              pareados = sum(link_pareados)),
           by = .(tipo_unidade, natureza)]
    a[, pct := round(100*pareados/pmax(1,obitos), 1)][order(-obitos)]
    print(head(a[order(-obitos)], 15))
    if ("perfil_servico" %in% names(T)) {
      cat("\n--- por perfil de servico e porte ---\n\n")
      b <- T[link_obitos > 0, .(estab = .N, obitos = sum(link_obitos),
                                pareados = sum(link_pareados)),
             by = .(perfil_servico, porte)]
      b[, pct := round(100*pareados/pmax(1,obitos), 1)]
      print(head(b[order(-obitos)], 15))
    }
  }
  if ("sim_obitos_sem_par" %in% names(T)) {
    cat("\n--- 2. OBITOS SEM PAR NO SIH, POR TIPO DE UNIDADE (lado SIM) ---\n\n")
    c2 <- T[sim_obitos_sem_par > 0, .(estab = .N, obitos = sum(sim_obitos_sem_par)),
            by = .(tipo_unidade, natureza)]
    print(head(c2[order(-obitos)], 15))
    cat("\nCobertura baixa em unidade COM internacao e candidata a falha de\n")
    cat("pareamento ou internacao nao-SUS. Em unidade SEM internacao nao e\n")
    cat("falha: nao existe AIH para parear — esse grupo se confere no SIA.\n")
  }
  h_todos <- T[!is.na(hospital_escopo)]
  h <- h_todos[escopo_relevante == TRUE]
  cat("\n--- 3. HOSPITAIS DO ESCOPO ---\n")
  if (nrow(h_todos) > nrow(h))
    cat("(", nrow(h_todos) - nrow(h), " candidatos sem leito e sem movimento ",
        "omitidos aqui; estao no CSV)\n", sep = "")
  if (!nrow(h)) {
    cat("Nenhum candidato. Sem CADGER, ou ajuste `padrao`/`municipio` em\n")
    cat("HOSPITAIS, no topo do script.\n")
  } else {
    print(h[, intersect(c("hospital_escopo","codestab","nome","tipo_unidade",
                          "leitos_total","leitos_uti_neo","leitos_pediatria",
                          "sih_total","link_obitos","link_pct_pareado",
                          "sim_obitos_sem_par"),
                        names(h)), with = FALSE])
    faltou <- setdiff(HOSPITAIS$rotulo, h$hospital_escopo)
    if (length(faltou))
      cat("\n[atencao] sem candidato para: ", paste(faltou, collapse = ", "),
          "\nAjuste `padrao` ou `municipio` em HOSPITAIS.\n", sep = "")
    cat("\nComplexos hospitalares aparecem com varios CNES de proposito.\n")
    cat("A escolha de qual entra na analise e da equipe, nao do script.\n")
  }
}


# #############################################################################
# PARTE 3 — SIA (acrescenta 3 colunas a tabela)
#
# Nunca guarda microdado: baixa, filtra 0-6 e o recorte de unidade, agrega e
# salva um pedaco pequeno por UF-mes. Retomavel.
# #############################################################################

VARS_SIA <- c("PA_CODUNI","PA_TPUPS","PA_UFMUN","PA_CMP","PA_PROC_ID",
              "PA_CIDPRI","PA_IDADE","PA_SEXO","PA_MUNPCN","PA_CATEND",
              "PA_DOCORIG","PA_MOTSAI","PA_QTDAPR","PA_NIVCPL")

sia_mes <- function(uf_ref, ano_ref, mes_ref) {
  arq <- file.path(CFG$dir_sia, sprintf("sia_%s_%d_%02d.rds", uf_ref, ano_ref, mes_ref))
  if (file.exists(arq)) return("pulado")
  t0 <- Sys.time()
  bruto <- tryCatch(fetch_datasus(year_start = ano_ref, month_start = mes_ref,
                                  year_end = ano_ref, month_end = mes_ref,
                                  uf = uf_ref, information_system = "SIA-PA",
                                  vars = VARS_SIA, timeout = 3600),
                    error = function(e) { message("   [erro] ",
                                                  conditionMessage(e)); NULL })
  if (is.null(bruto) || nrow(bruto) == 0) return("falha")
  d <- as.data.table(bruto); rm(bruto)
  if (!"PA_IDADE" %in% names(d)) return("sem_idade")
  # No SIA a idade ja vem em anos. BPA-C (producao consolidada) nao tem idade
  # e cai fora aqui — nao da para contar producao agregada como atendimento
  # infantil individual.
  d[, idade := suppressWarnings(as.integer(PA_IDADE))]
  d <- d[!is.na(idade) & idade >= 0 & idade <= CFG$idade_max_anos]
  if (!nrow(d)) { rm(d); gc(verbose = FALSE); return("vazio") }
  d[, tp := pad2(PA_TPUPS)][, motsai := pad2(PA_MOTSAI)]
  d <- d[tp %in% TP_SIA | motsai %in% MOTSAI_OBITO]
  if (!nrow(d)) { rm(d); gc(verbose = FALSE); return("vazio") }
  d[, `:=`(codestab = cnes7(PA_CODUNI),
           ano = suppressWarnings(as.integer(substr(as.character(PA_CMP),1,4))),
           mes = suppressWarnings(as.integer(substr(as.character(PA_CMP),5,6))),
           urg = pad2(PA_CATEND) == "02", ob = motsai %in% MOTSAI_OBITO)]
  d[is.na(ano), ano := ano_ref]; d[is.na(mes), mes := mes_ref]
  ag <- d[, .(registros = .N, urgencia = sum(urg, na.rm = TRUE),
              obito = sum(ob, na.rm = TRUE)), by = .(codestab, ano, mes, tp)]
  # Gravar e o unico ponto do laco que toca o disco: se falhar, o mes inteiro
  # foi baixado a toa. Melhor dizer o caminho exato do que devolver
  # "cannot open the connection".
  garante_pasta(CFG$dir_sia)
  ok <- tryCatch({ saveRDS(ag, arq); TRUE },
                 error = function(e) { message("   [erro ao gravar] ", arq,
                                               "\n   ", conditionMessage(e))
                   FALSE })
  if (!ok) return("falha")
  s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  rm(d, ag); gc(verbose = FALSE); paste0("ok (", s, "s)")
}

junta_sia <- function(T) {
  a <- list.files(CFG$dir_sia, pattern = "^sia_.*\\.rds$", full.names = TRUE)
  if (!length(a)) return(T)
  s <- rbindlist(lapply(a, function(f) tryCatch(readRDS(f), error = function(e) NULL)),
                 use.names = TRUE, fill = TRUE)
  if (!nrow(s)) return(T)
  cat("\nSIA: ", fmt(length(a)), " pedacos, ", fmt(uniqueN(s$codestab)),
      " estabelecimentos.\n", sep = "")
  merge(T, s[, .(sia_registros_0a6 = sum(registros),
                 sia_registros_urgencia = sum(urgencia),
                 sia_competencias = uniqueN(paste(ano, mes))), by = codestab],
        by = "codestab", all.x = TRUE)
}

parte_3 <- function() {
  titulo(paste0("PARTE 3 · SIA-PA — modo ", MODO_SIA))
  if (MODO_SIA == "piloto") {
    g <- data.table(uf = CFG$piloto_uf, ano = CFG$piloto_ano, mes = 1:12)
    cat("PILOTO — ", CFG$piloto_uf, " ", CFG$piloto_ano,
        ". Meca o tempo antes do modo completo.\n\n", sep = "")
  } else if (MODO_SIA == "alvo") {
    if (!file.exists(CFG$arq_nc))
      stop("Modo 'alvo' precisa de cnes_obitos_nao_capturados.csv.", call. = FALSE)
    nc <- fread(CFG$arq_nc)
    g <- unique(nc[!is.na(uf_ocor), .(uf = uf_ocor, ano, mes)])
    cat("ALVO — ", nrow(g), " UF-meses com obito sem par.\n\n", sep = "")
  } else {
    g <- CJ(uf = CFG$ufs, ano = CFG$anos, mes = 1:12)
    cat("COMPLETO — ", nrow(g), " UF-meses. Isto leva horas.\n\n", sep = "")
  }
  setorder(g, uf, ano, mes)
  feitos <- 0L; falhas <- character(0); t0 <- Sys.time()
  for (i in seq_len(nrow(g))) {
    st <- tryCatch(sia_mes(g$uf[i], g$ano[i], g$mes[i]),
                   error = function(e) { message("   [erro fatal] ",
                                                 conditionMessage(e)); "falha" })
    if (st == "pulado") next
    feitos <- feitos + 1L
    if (st == "falha") falhas <- c(falhas, sprintf("%s %d/%02d", g$uf[i], g$ano[i], g$mes[i]))
    cat(sprintf("[%4d/%4d] %s %d/%02d ... %s\n", i, nrow(g), g$uf[i], g$ano[i],
                g$mes[i], st))
    if (feitos %% 10 == 0) {
      dec <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("        media %.1f min/mes · restam ~%.1f h\n",
                  dec/feitos, (nrow(g)-i)*(dec/feitos)/60))
    }
  }
  if (length(falhas)) { cat("\nSem dados / erro:\n"); print(utils::head(falhas, 30)) }
  if (MODO_SIA == "piloto" && feitos > 0) {
    dec <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat(sprintf("\nMedia: %.1f min/mes. Projecao 27 UF x 120 meses: %.0f h.\n",
                dec/feitos, (dec/feitos)*3240/60))
    cat("AC e UF pequena — SP, MG, RJ e BA levam varias vezes isso.\n")
    cat("Se a projecao for alta demais, use MODO_SIA <- \"alvo\".\n")
  }
  cat("\nAgora rode a PARTE 2 de novo: ela acrescenta as colunas sia_* a tabela.\n")
  invisible(feitos)
}




# #############################################################################
# PARTE 4 — TESTE: O BPA-I REGISTRA O OBITO?
#
# Pergunta: nos atendimentos de urgencia de 0-6 anos, o campo "motivo de
# saida" do SIA-PA e preenchido com obito (41/42/43) fora de APAC/RAAS — ou
# seja, no BPA individualizado? Se sim, o obito em LOCOCOR 2 (UPA, pronto
# atendimento, UBS) tem como ser conferido no SIA CNES a CNES; se nao, o SIA
# continua sendo so corroboracao de universo.
#
# Duas leituras, sempre por UF-mes:
#   A. lado SIA: registros de 0-6 anos por tipo de documento (PA_DOCORIG) e
#      quantos trazem obito (PA_MOTSAI 41-43 ou o flag PA_OBITO).
#   B. lado DO: obitos do SIM na mesma UF-mes, em LOCOCOR 1 e 2, e quantos
#      encontram, no MESMO CNES e MESMA competencia, (i) qualquer registro
#      de 0-6, (ii) registro BPA-I, (iii) registro com obito, e (iv) um
#      registro com obito de MESMA idade (anos), MESMO sexo e MESMO municipio
#      de residencia — o mais perto de pareamento individual que o SIA permite.
#
# O que NAO faz: nao guarda microdado do SIA (agrega antes de gravar) e nao
# chama de "pareado" o que e coincidencia de atributos. A leitura (iv) e um
# limite superior.
# #############################################################################

VARS_BPAI <- c("PA_CODUNI","PA_TPUPS","PA_CMP","PA_DOCORIG","PA_MOTSAI",
               "PA_OBITO","PA_ALTA","PA_TRANSF","PA_ENCERR","PA_PERMAN",
               "PA_IDADE","PA_SEXO","PA_MUNPCN","PA_CIDPRI","PA_CATEND",
               "PA_PROC_ID","PA_NIVCPL","PA_UFMUN")

DOCORIG_NOME <- c("C" = "BPA consolidado (BPA-C)",
                  "I" = "BPA individualizado (BPA-I)",
                  "P" = "APAC — procedimento principal",
                  "S" = "APAC — procedimento secundario",
                  "A" = "RAAS — atencao domiciliar",
                  "R" = "RAAS — psicossocial")
docorig_nome <- function(x) { x <- toupper(trimws(as.character(x)))
  v <- unname(DOCORIG_NOME[x])
  ifelse(is.na(v), paste0("codigo ", ifelse(is.na(x) | x == "", "vazio", x),
                          " (conferir)"), v) }

# SIM: DDMMAAAA; sexo 1 = M, 2 = F (no SIA vem "M"/"F")
data_sim <- function(x) { x <- gsub("[^0-9]", "", as.character(x))
  x[nchar(x) != 8] <- NA_character_
  as.Date(x, format = "%d%m%Y") }
sexo_sim_mf <- function(x) { x <- as.character(x)
  ifelse(x %in% c("1","M"), "M", ifelse(x %in% c("2","F"), "F", NA_character_)) }

escolhe_ufmes <- function() {
  if (!is.null(TESTE_BPAI$ufmes)) {
    g <- as.data.table(TESTE_BPAI$ufmes)
    stopifnot(all(c("uf","ano","mes") %in% names(g)))
    return(g[, .(uf = as.character(uf), ano = as.integer(ano), mes = as.integer(mes))])
  }
  if (!file.exists(CFG$arq_nc))
    stop("Modo auto precisa de cnes_obitos_nao_capturados.csv (linkage v3.3).",
         call. = FALSE)
  nc <- fread(CFG$arq_nc, colClasses = list(character = "codestab"))
  g <- nc[local_ocor == 2 & !is.na(uf_ocor) & !uf_ocor %in% TESTE_BPAI$excluir_uf,
          .(n_l2 = sum(obitos_nao_capturados)), by = .(uf = uf_ocor, ano, mes)]
  setorder(g, -n_l2)
  cat("UF-meses escolhidos (mais obito sem par em LOCOCOR 2, fora de ",
      paste(TESTE_BPAI$excluir_uf, collapse = "/"), "):\n", sep = "")
  print(head(g, TESTE_BPAI$n))
  head(g, TESTE_BPAI$n)[, .(uf, ano, mes)]
}

# SIM recortado para as UF-meses do teste, ja no formato do SIA.
sim_para_teste <- function(g) {
  if (!file.exists(TESTE_BPAI$arq_sim))
    stop("SIM nao encontrado: ", TESTE_BPAI$arq_sim, call. = FALSE)
  s <- as.data.table(readRDS(TESTE_BPAI$arq_sim))
  col <- function(n) if (n %in% names(s)) s[[n]] else rep(NA_character_, nrow(s))
  d <- data.table(codestab  = cnes7(col("CODESTAB")),
                  dt_obito  = data_sim(col("DTOBITO")),
                  dt_nasc   = data_sim(col("DTNASC")),
                  sexo      = sexo_sim_mf(col("SEXO")),
                  munres    = mun6(col("CODMUNRES")),
                  munocor   = mun6(col("CODMUNOCOR")),
                  lococor   = as.character(col("LOCOCOR")),
                  cid       = cid3(col("CAUSABAS")))
  rm(s); gc(verbose = FALSE)
  d <- d[!is.na(dt_obito) & !is.na(dt_nasc)]
  d[, `:=`(ano = as.integer(format(dt_obito, "%Y")),
           mes = as.integer(format(dt_obito, "%m")),
           idade = as.integer(floor(as.numeric(dt_obito - dt_nasc) / 365.25)))]
  UF_COD <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
              "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
              "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR",
              "42"="SC","43"="RS","50"="MS","51"="MT","52"="GO","53"="DF")
  d[, uf := unname(UF_COD[substr(munocor, 1, 2)])]
  d <- d[idade >= 0 & idade <= CFG$idade_max_anos & lococor %in% c("1","2")]
  merge(d, g, by = c("uf","ano","mes"))
}

# Um UF-mes do SIA-PA, 0-6 anos, todos os documentos. Devolve o microdado
# reduzido SO para uso em memoria nesta funcao chamadora; em disco vai
# apenas o agregado.
bpai_baixa <- function(uf_ref, ano_ref, mes_ref) {
  t0 <- Sys.time()
  bruto <- tryCatch(fetch_datasus(year_start = ano_ref, month_start = mes_ref,
                                  year_end = ano_ref, month_end = mes_ref,
                                  uf = uf_ref, information_system = "SIA-PA",
                                  vars = VARS_BPAI, timeout = 3600),
                    error = function(e) { message("   [erro] ",
                                                  conditionMessage(e)); NULL })
  if (is.null(bruto) || !nrow(bruto)) return(NULL)
  d <- as.data.table(bruto); rm(bruto)
  for (v in setdiff(VARS_BPAI, names(d))) d[, (v) := NA_character_]
  d[, idade := suppressWarnings(as.integer(PA_IDADE))]
  d <- d[!is.na(idade) & idade >= 0 & idade <= CFG$idade_max_anos]
  if (!nrow(d)) return(d)
  d[, `:=`(codestab = cnes7(PA_CODUNI),
           doc      = toupper(trimws(as.character(PA_DOCORIG))),
           tp       = pad2(PA_TPUPS),
           motsai   = pad2(PA_MOTSAI),
           sexo     = toupper(trimws(as.character(PA_SEXO))),
           munres   = mun6(PA_MUNPCN),
           cid      = cid3(PA_CIDPRI),
           urg      = pad2(PA_CATEND) == "02",
           flag_ob  = trimws(as.character(PA_OBITO)) %in% c("1","S"),
           ano      = suppressWarnings(as.integer(substr(as.character(PA_CMP),1,4))),
           mes      = suppressWarnings(as.integer(substr(as.character(PA_CMP),5,6))))]
  d[is.na(ano), ano := ano_ref]; d[is.na(mes), mes := mes_ref]
  d[, ob := motsai %in% MOTSAI_OBITO | flag_ob]
  cat(sprintf("   %s %d/%02d: %s registros de 0-6 anos em %ds\n", uf_ref, ano_ref,
              mes_ref, fmt(nrow(d)),
              round(as.numeric(difftime(Sys.time(), t0, units = "secs")))))
  d[, .(codestab, doc, tp, motsai, flag_ob, ob, idade, sexo, munres, cid, urg, ano, mes)]
}

parte_4 <- function() {
  titulo("PARTE 4 · teste: o BPA-I registra o obito?")
  g <- escolhe_ufmes()
  sim <- sim_para_teste(g)
  cat("\nObitos do SIM (LOCOCOR 1 e 2) nas UF-meses do teste: ", fmt(nrow(sim)),
      "\n", sep = "")
  print(sim[, .(obitos = .N), by = .(uf, ano, mes, lococor)][order(uf, ano, mes, lococor)])

  A <- list(); B <- list(); C <- list(); D <- list()
  for (i in seq_len(nrow(g))) {
    # nomes com sufixo _i de proposito: dentro do data.table, `ano == ano`
    # compararia a coluna com ela mesma e selecionaria tudo.
    uf_i <- g$uf[i]; ano_i <- g$ano[i]; mes_i <- g$mes[i]
    arq <- file.path(CFG$dir_bpai, sprintf("bpai_%s_%d_%02d.rds", uf_i, ano_i, mes_i))
    if (file.exists(arq)) {
      cat(sprintf("[%d/%d] %s %d/%02d — ja feito, pulando\n", i, nrow(g), uf_i, ano_i, mes_i))
      r <- readRDS(arq); A[[i]] <- r$A; B[[i]] <- r$B; C[[i]] <- r$C; D[[i]] <- r$D
      next
    }
    cat(sprintf("[%d/%d] %s %d/%02d baixando ...\n", i, nrow(g), uf_i, ano_i, mes_i))
    d <- bpai_baixa(uf_i, ano_i, mes_i)
    if (is.null(d)) { cat("   sem dados / erro — pulado\n"); next }
    d <- d[ano == ano_i & mes == mes_i]   # competencia exata

    # A. lado SIA: por documento
    a <- d[, .(registros = .N, urgencia = sum(urg),
               com_obito = sum(ob), motsai_obito = sum(motsai %in% MOTSAI_OBITO),
               flag_obito = sum(flag_ob), cnes = uniqueN(codestab)),
           by = .(doc)][, `:=`(uf = uf_i, ano = ano_i, mes = mes_i,
                               documento = docorig_nome(doc))]
    # A2. motivos de saida observados no BPA-I (para ver o que vem preenchido)
    a2 <- d[doc == "I", .(registros = .N), by = .(motsai)][order(-registros)][
      , `:=`(uf = uf_i, ano = ano_i, mes = mes_i)]
    # A3. obitos do SIA por tipo de unidade
    a3 <- d[ob == TRUE, .(registros_obito = .N, cnes = uniqueN(codestab)),
            by = .(tp, doc)][, `:=`(uf = uf_i, ano = ano_i, mes = mes_i,
                                    tipo_unidade = TP_UNID_NOME[tp])]

    # B. lado DO: CNES a CNES, na competencia
    s <- sim[uf == uf_i & ano == ano_i & mes == mes_i]
    cn_any  <- unique(d$codestab)
    cn_bpai <- unique(d[doc == "I", codestab])
    cn_ob   <- unique(d[ob == TRUE, codestab])
    s[, `:=`(sia_qualquer = codestab %in% cn_any,
             sia_bpai     = codestab %in% cn_bpai,
             sia_obito    = codestab %in% cn_ob)]
    # (iv) coincidencia de atributos com um registro de obito
    ob_attr <- unique(d[ob == TRUE & !is.na(sexo) & !is.na(munres),
                        .(codestab, idade, sexo, munres, cid)])
    ob_attr[, attr := TRUE]
    s <- merge(s, unique(ob_attr[, .(codestab, idade, sexo, munres, attr)]),
               by = c("codestab","idade","sexo","munres"), all.x = TRUE)
    s[is.na(attr), attr := FALSE]
    ob_cid <- unique(ob_attr[, .(codestab, idade, sexo, munres, cid, attr_cid = TRUE)])
    s <- merge(s, ob_cid, by = c("codestab","idade","sexo","munres","cid"), all.x = TRUE)
    s[is.na(attr_cid), attr_cid := FALSE]
    b <- s[, .(obitos_sim = .N,
               com_cnes = sum(!is.na(codestab)),
               cnes_com_registro_sia = sum(sia_qualquer),
               cnes_com_bpai = sum(sia_bpai),
               cnes_com_obito_sia = sum(sia_obito),
               obito_mesma_idade_sexo_mun = sum(attr),
               obito_mesma_idade_sexo_mun_cid = sum(attr_cid)),
           by = .(lococor)][, `:=`(uf = uf_i, ano = ano_i, mes = mes_i)]
    # C. os CNES da DO em LOCOCOR 2 e o que o SIA tem deles
    cc <- s[lococor == "2" & !is.na(codestab), .(obitos_sim = .N), by = codestab]
    cc <- merge(cc, d[, .(sia_registros = .N, sia_bpai = sum(doc == "I"),
                          sia_obitos = sum(ob), tp = tp[1]), by = codestab],
                by = "codestab", all.x = TRUE)
    cc[, `:=`(uf = uf_i, ano = ano_i, mes = mes_i, tipo_unidade = TP_UNID_NOME[tp])]
    # D. reverso: registros com obito no SIA que NAO tem obito na DO no CNES
    dd <- d[ob == TRUE, .(sia_obitos = .N), by = .(codestab, doc)]
    dd <- merge(dd, s[!is.na(codestab), .(obitos_sim = .N), by = codestab],
                by = "codestab", all.x = TRUE)
    dd[is.na(obitos_sim), obitos_sim := 0L]
    dd[, `:=`(uf = uf_i, ano = ano_i, mes = mes_i)]

    r <- list(A = list(doc = a, motsai_bpai = a2, obito_tipo = a3), B = b, C = cc, D = dd)
    saveRDS(r, arq)
    A[[i]] <- r$A; B[[i]] <- r$B; C[[i]] <- r$C; D[[i]] <- r$D
    rm(d, s); gc(verbose = FALSE)
  }
  feitos <- which(!vapply(B, is.null, logical(1)))
  if (!length(feitos)) stop("Nenhum UF-mes baixado.", call. = FALSE)

  A_doc  <- rbindlist(lapply(A[feitos], `[[`, "doc"), fill = TRUE)
  A_mot  <- rbindlist(lapply(A[feitos], `[[`, "motsai_bpai"), fill = TRUE)
  A_tipo <- rbindlist(lapply(A[feitos], `[[`, "obito_tipo"), fill = TRUE)
  Bt <- rbindlist(B[feitos], fill = TRUE); Ct <- rbindlist(C[feitos], fill = TRUE)
  Dt <- rbindlist(D[feitos], fill = TRUE)

  # ---- resumo ---------------------------------------------------------------
  res_doc <- A_doc[, .(registros = sum(registros), com_obito = sum(com_obito),
                       motsai_obito = sum(motsai_obito), flag_obito = sum(flag_obito),
                       cnes = sum(cnes)), by = documento][order(-registros)]
  res_doc[, pct_com_obito := round(100 * com_obito / pmax(1, registros), 3)]
  res_mot <- A_mot[, .(registros = sum(registros)), by = motsai][order(-registros)]
  res_B <- Bt[, .(obitos_sim = sum(obitos_sim), com_cnes = sum(com_cnes),
                  cnes_com_registro_sia = sum(cnes_com_registro_sia),
                  cnes_com_bpai = sum(cnes_com_bpai),
                  cnes_com_obito_sia = sum(cnes_com_obito_sia),
                  obito_mesma_idade_sexo_mun = sum(obito_mesma_idade_sexo_mun),
                  obito_mesma_idade_sexo_mun_cid = sum(obito_mesma_idade_sexo_mun_cid)),
              by = lococor][order(lococor)]
  res_B[, local := ifelse(lococor == "1", "Hospital", "Outro estab. de saude")]
  pc <- function(a, b) sprintf("%5.1f%%", 100 * a / pmax(1, b))

  L <- c(
    "TESTE: O BPA-I REGISTRA O OBITO?  (SIA-PA, 0-6 anos)",
    paste0("Gerado em: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste0("UF-meses: ", paste(sprintf("%s %d/%02d", g$uf[feitos], g$ano[feitos],
                                        g$mes[feitos]), collapse = " · ")),
    "",
    "A. LADO SIA — registros de 0-6 anos por tipo de documento",
    "   (com_obito = PA_MOTSAI 41/42/43 ou PA_OBITO = 1)",
    capture.output(print(res_doc[, .(documento, registros, com_obito, pct_com_obito,
                                     motsai_obito, flag_obito)], row.names = FALSE)),
    "",
    "   Motivo de saida (PA_MOTSAI) observado no BPA-I:",
    capture.output(print(head(res_mot, 12), row.names = FALSE)),
    "",
    "   Registros com obito no SIA, por tipo de unidade e documento:",
    capture.output(print(A_tipo[, .(registros_obito = sum(registros_obito)),
                                by = .(tipo_unidade, documento = docorig_nome(doc))][
                                  order(-registros_obito)][1:min(.N, 12)], row.names = FALSE)),
    "",
    "B. LADO DO — obitos do SIM nas mesmas UF-meses e o que o SIA tem",
    "   do MESMO CNES na MESMA competencia",
    unlist(lapply(seq_len(nrow(res_B)), function(i) { r <- res_B[i]; c(
      sprintf("   %-22s obitos na DO ........................ %s", r$local, fmt(r$obitos_sim)),
      sprintf("   %-22s   com CNES informado ................ %s (%s)", "", fmt(r$com_cnes), pc(r$com_cnes, r$obitos_sim)),
      sprintf("   %-22s   CNES com algum registro 0-6 no SIA  %s (%s)", "", fmt(r$cnes_com_registro_sia), pc(r$cnes_com_registro_sia, r$obitos_sim)),
      sprintf("   %-22s   CNES com registro BPA-I ........... %s (%s)", "", fmt(r$cnes_com_bpai), pc(r$cnes_com_bpai, r$obitos_sim)),
      sprintf("   %-22s   CNES com registro de OBITO no SIA . %s (%s)", "", fmt(r$cnes_com_obito_sia), pc(r$cnes_com_obito_sia, r$obitos_sim)),
      sprintf("   %-22s   obito SIA c/ mesma idade+sexo+mun . %s (%s)  <- limite superior", "", fmt(r$obito_mesma_idade_sexo_mun), pc(r$obito_mesma_idade_sexo_mun, r$obitos_sim)),
      sprintf("   %-22s   ... e mesmo CID (3 chars) ......... %s (%s)", "", fmt(r$obito_mesma_idade_sexo_mun_cid), pc(r$obito_mesma_idade_sexo_mun_cid, r$obitos_sim)),
      "") })),
    "C. REVERSO — registros com obito no SIA cujo CNES nao tem obito na DO",
    "   naquela competencia (obito ambulatorial que o SIM nao situa ali, ou",
    "   LOCOCOR/CNES errado na DO):",
    sprintf("   registros com obito no SIA: %s · em CNES sem obito na DO: %s (%s)",
            fmt(sum(Dt$sia_obitos)), fmt(sum(Dt$sia_obitos[Dt$obitos_sim == 0])),
            pc(sum(Dt$sia_obitos[Dt$obitos_sim == 0]), sum(Dt$sia_obitos))),
    "",
    "COMO LER",
    "  - Se 'com_obito' no BPA-I for ~0, o motivo de saida nao e preenchido na",
    "    urgencia: o SIA nao devolve o obito e fica como corroboracao de universo.",
    "  - Se for relevante, a linha 'CNES com registro de OBITO no SIA' em",
    "    LOCOCOR 2 diz quantos obitos da DO tem um obito ambulatorial no mesmo",
    "    lugar e mes; a linha de idade+sexo+municipio e o mais perto de",
    "    pareamento que o SIA permite — e continua sendo limite superior, nao par.",
    "  - Nada aqui entra no painel como 'pareado'.")
  writeLines(L, CFG$REL_BPAI)
  cat(paste(L, collapse = "\n"), "\n")

  saveRDS(list(gerado_em = Sys.time(), ufmes = g[feitos],
               sia_por_documento = as.data.frame(res_doc),
               sia_por_documento_ufmes = as.data.frame(A_doc),
               motsai_bpai = as.data.frame(res_mot),
               sia_obito_por_tipo = as.data.frame(A_tipo),
               do_vs_sia = as.data.frame(res_B),
               do_vs_sia_ufmes = as.data.frame(Bt),
               cnes_lococor2 = as.data.frame(Ct),
               reverso = as.data.frame(Dt)), CFG$SAIDA_BPAI)
  cat("\n>>> ", CFG$SAIDA_BPAI, "\n>>> ", CFG$REL_BPAI, "\n", sep = "")
  invisible(NULL)
}

# #############################################################################
# EXECUCAO
# #############################################################################

PARTES <- list("0" = parte_0, "1" = parte_1, "2" = parte_2, "3" = parte_3,
               "4" = parte_4)
inval <- setdiff(as.character(RODAR), names(PARTES))
if (length(inval))
  stop("Parte invalida em RODAR: ", paste(inval, collapse = ", "),
       ". Use 0, 1, 2, 3 ou 4.", call. = FALSE)

tt <- Sys.time()
for (p in as.character(sort(unique(RODAR)))) PARTES[[p]]()
titulo(paste0("FIM — partes ", paste(sort(unique(RODAR)), collapse = ", "), " em ",
              round(as.numeric(difftime(Sys.time(), tt, units = "mins")), 1), " min"))
if (!2 %in% RODAR) cat("Proxima: RODAR <- 2 (monta a tabela)\n")
