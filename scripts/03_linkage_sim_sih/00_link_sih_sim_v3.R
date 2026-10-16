# ============================================================================
# 00_link_sih_sim_v3.R  — LINKAGE SIM ↔ SIH EM 5 ETAPAS (fluxo da proposta)
#
# v3.1 (07/08): base analítica restrita a óbito/transferência (o agregado da
#   Etapa 5 cobre as altas); qualidade de preenchimento entre os pareados.
#
# v3.3 (26/08): responde aos comentários da proposta de linkage.
#   (a) CASCATA DE DESFECHOS AIH A AIH. O painel mostrava T₀ → T₁ → T₂ como
#       barras independentes sobre a mesma base, o que sugeria um funil onde
#       não há um. Agora cada episódio é aberto na sequência real das suas
#       AIHs: a AIH nº 1 é o estágio 1 e tem um desfecho (alta, óbito,
#       transferência); a AIH nº 2 é o estágio 2 e só existe para quem
#       continuou; e assim por diante. O agregado vai em `cascata_estagios.rds`
#       e cobre TODOS os episódios (inclusive os de alta, que não entram na
#       base analítica).
#   (b) CAPTURA DO ALVO POR LOCAL DE OCORRÊNCIA. O alvo da Etapa 1 mistura
#       LOCOCOR 1 (hospital) e LOCOCOR 2 (outro estabelecimento de saúde —
#       UPA, pronto atendimento, UBS). Óbito em LOCOCOR 2 não gera AIH e
#       portanto NÃO PODE ser pareado: contá-lo no denominador do linkage
#       rebaixa a cobertura sem que exista falha de pareamento. A v3.3 marca
#       cada óbito do alvo como pareado ou não e abre a conta por LOCOCOR,
#       por UF e por estabelecimento (CODESTAB) — esta última é a lista que
#       alimenta a conferência no SIA.
#
# v3.2 (08/08): CORRIGE a classificação do desfecho do episódio. Antes, o
#   desfecho vinha da última AIH por data (last), o que perdia o óbito quando
#   a última AIH tinha motivo de cobrança fora das faixas mapeadas ou quando
#   duas AIHs empatavam na data — 48.602 AIHs de óbito caíam no grupo "sem
#   óbito" da Etapa 5 e ficavam fora do denominador do linkage. Agora o óbito
#   tem prioridade sobre a ordem, e a AIH de óbito é a referência do episódio
#   (fornece CNES e data de saída para o pareamento). Também: motivos de
#   cobrança não mapeados deixam de virar NA silencioso, a reconciliação passa
#   a ser gravada em duas trilhas (SIM × SIH) e o script checa a consistência
#   dos totais antes de terminar.
# Observatório de Saúde Infantil | INSPER · Hospital Pequeno Príncipe
# Coorte 0 a 6 anos · 2015–2024 · sem download (lê as bases locais)
#
# O QUE MUDA EM RELAÇÃO À v2
#   A v2 partia do SIH (episódios com óbito/transferência) e fazia left_join
#   com o SIM. A v3 inverte a direção, seguindo a proposta em 5 etapas:
#
#   ETAPA 1 · Identificar no SIM os óbitos HOSPITALARES (LOCOCOR) — o alvo.
#             Não há como saber se a internação foi paga pelo SUS, então todo
#             óbito hospitalar é candidato a estar na AIH.
#   ETAPA 2 · Linkage do alvo com as AIHs cujo desfecho foi óbito.
#             Chaves: data de nascimento, sexo, data da alta/óbito, CNES
#             (CODESTAB no SIM), município de residência. Raça/cor entra como
#             verificação. Dois níveis:
#               EXATO ........... todas as chaves batem, data idêntica
#               PROBABILÍSTICO .. bloco nasc+sexo, escore com buffer de até
#                                 ±3 dias na data e concordância parcial de
#                                 CNES/município/raça
#   ETAPA 3 · Para o subgrupo linkado, retroagir e buscar internações
#             ANTERIORES (desfecho alta ou transferência) em janela de 30
#             dias antes da internação-índice (45 e 60 como sensibilidade).
#   ETAPA 4 · Base analítica de trajetória e transição de CID — mesma
#             estrutura que o app já consome, agora com o histórico.
#   ETAPA 5 · As AIHs que NÃO são óbito nem internação anterior de óbito
#             formam o grupo de "baixa mortalidade" — agregado por ano, UF,
#             faixa, sexo e sistema do CID (causas e tempo de internação).
#
# ENTRADAS (já existem na sua máquina)
#   SIH · SIH/Dados/Temporarios_UF_Ano/sih_{UF}_{ano}.rds   (270 lotes)
#   SIM · SIM/Dados/sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds
#
# SAÍDAS (todas na pasta do Dash)
#   sih_sim_linkado.rds           — base analítica (compatível com o app v4/v5,
#                                   com as colunas novas do fluxo v3)
#   linkage_qualidade.rds         — métricas de qualidade (funil, por ano, por
#                                   UF, exato × probabilístico, sensibilidade)
#   sih_nao_obito_agregado.rds    — Etapa 5 agregada (sem microdado)
#   relatorio_linkage_v3.txt      — relatório em texto
#
# COMO RODAR
#   Rscript 00_link_sih_sim_v3.R      (sem internet; ~20–40 min Brasil inteiro)
# ============================================================================

CONFIG <- list(
  dir_sih   = "G:/Meu Drive/INSPER/Trabalho/SIH/Dados/Temporarios_UF_Ano",
  arq_sim   = "G:/Meu Drive/INSPER/Trabalho/SIM/Dados/sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds",
  dir_dash  = "G:/Meu Drive/INSPER/Trabalho/Dash",
  ufs       = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE",
                "AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"),
  anos      = 2015:2024,
  idade_max_anos = 6,
  
  # --- Etapa 1: o que conta como óbito hospitalar no SIM (LOCOCOR) ---------
  #   1 = hospital · 2 = outros estabelecimentos de saúde
  #   O alvo ampliado usa os dois; o relatório abre a composição.
  lococor_alvo = c("1", "2"),
  
  # --- Etapa 2: pareamento -------------------------------------------------
  buffer_dias   = 3,     # tolerância |data óbito SIM − data saída SIH| no probabilístico
  escore_minimo = 3,     # escore mínimo para aceitar um par probabilístico
  
  # --- Etapa 3: retroação --------------------------------------------------
  janela_principal = 30,          # dias antes da internação-índice
  janelas_sens     = c(30, 45, 60),
  gap_episodio     = 2            # dias entre alta e reinternação no MESMO episódio
)
CONFIG$saida_link  <- file.path(CONFIG$dir_dash, "sih_sim_linkado.rds")
CONFIG$saida_qual  <- file.path(CONFIG$dir_dash, "linkage_qualidade.rds")
CONFIG$saida_baixa <- file.path(CONFIG$dir_dash, "sih_nao_obito_agregado.rds")
CONFIG$saida_casc  <- file.path(CONFIG$dir_dash, "cascata_estagios.rds")
CONFIG$saida_cnes  <- file.path(CONFIG$dir_dash, "cnes_obitos_nao_capturados.csv")
CONFIG$relatorio   <- file.path(CONFIG$dir_dash, "relatorio_linkage_v3.txt")
CONFIG$app_r       <- file.path(CONFIG$dir_dash, "app.R")
CONFIG$parciais    <- file.path(CONFIG$dir_dash, "dados", "parciais_v3")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

dir.create(CONFIG$parciais, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------------
# CACHE VERSIONADO — evita reaproveitar resultado gerado por versao anterior
#
# O script salva o resultado de cada UF em dados/parciais_v3 para poder ser
# interrompido e retomado. O risco e obvio: depois de uma correcao na logica,
# esses arquivos continuam la e a UF e PULADA, de modo que a correcao nao
# chega ao resultado final. Em vez de exigir limpeza manual, a versao fica
# gravada junto com o cache: se nao bater, o cache e descartado sozinho.
# ----------------------------------------------------------------------------
VERSAO_LOGICA <- "v3.3"
arq_versao <- file.path(CONFIG$parciais, "_versao.txt")
versao_cache <- if (file.exists(arq_versao))
  tryCatch(readLines(arq_versao, warn = FALSE)[1], error = function(e) NA_character_) else NA_character_

if (!identical(versao_cache, VERSAO_LOGICA)) {
  antigos <- list.files(CONFIG$parciais, pattern = "\\.rds$", full.names = TRUE)
  if (length(antigos)) {
    message("Cache de outra versao (", versao_cache %||% "sem marcador",
            ") — descartando ", length(antigos), " arquivos parciais.")
    file.remove(antigos)
  }
  writeLines(VERSAO_LOGICA, arq_versao)
} else {
  n_ok <- length(list.files(CONFIG$parciais, pattern = "\\.rds$"))
  if (n_ok) message("Cache ", VERSAO_LOGICA, " encontrado: ", n_ok,
                    " UF ja processadas serao reaproveitadas.")
}

precisa <- function(p) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  if (!requireNamespace(p, quietly = TRUE)) stop("Nao consegui instalar ", p)
}
precisa("dplyr"); precisa("data.table")
suppressPackageStartupMessages({library(dplyr); library(data.table)})

# ----------------------------------------------------------------------------
# 0. Conferência das entradas
# ----------------------------------------------------------------------------
if (!dir.exists(CONFIG$dir_sih))
  stop("Pasta do SIH nao encontrada:\n  ", CONFIG$dir_sih, call. = FALSE)
if (!file.exists(CONFIG$arq_sim))
  stop("Base do SIM nao encontrada:\n  ", CONFIG$arq_sim, call. = FALSE)
message("Lotes do SIH: ",
        length(list.files(CONFIG$dir_sih, pattern = "^sih_[A-Z]{2}_[0-9]{4}\\.rds$")),
        " | SIM: ", round(file.size(CONFIG$arq_sim) / 1e6, 1), " MB\n")

# ----------------------------------------------------------------------------
# 1. Motor de classificação CID reaproveitado do app (bloco 1f)
# ----------------------------------------------------------------------------
carrega_motor_cid <- function(path = CONFIG$app_r) {
  if (!file.exists(path)) stop("app.R nao encontrado em ", path)
  linhas <- readLines(path, encoding = "UTF-8", warn = FALSE)
  i0 <- grep("^# 1f\\. MOTOR DE CLASSIFICA", linhas)
  i1 <- grep("^pal_sistema <- c\\(", linhas)
  if (length(i0) == 0 || length(i1) == 0)
    stop("Bloco 1f nao localizado em app.R.")
  tmp <- tempfile(fileext = ".R")
  writeLines(c("`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a",
               linhas[i0[1]:(i1[1] - 1)]), tmp, useBytes = TRUE)
  source(tmp, encoding = "UTF-8", local = globalenv())
  message("Motor CID carregado: ", nrow(cid_dic), " codigos.\n")
}
carrega_motor_cid()

# ----------------------------------------------------------------------------
# 2. Padronização — cada sistema com a sua própria regra
# ----------------------------------------------------------------------------
data_sih <- function(x) as.Date(as.character(x), format = "%Y%m%d")   # AAAAMMDD
data_sim <- function(x) as.Date(as.character(x), format = "%d%m%Y")   # DDMMAAAA

sexo_sih <- function(x) dplyr::case_when(   # 1 masculino · 3 feminino
  as.character(x) %in% c("1", "M") ~ "Masculino",
  as.character(x) %in% c("3", "F") ~ "Feminino",
  TRUE ~ NA_character_)
sexo_sim <- function(x) dplyr::case_when(   # 1 masculino · 2 feminino
  as.character(x) == "1" ~ "Masculino",
  as.character(x) == "2" ~ "Feminino",
  TRUE ~ NA_character_)

raca_sih_f <- function(x) dplyr::case_when(   # 03 parda · 04 amarela
  as.character(x) == "01" ~ "Branca",   as.character(x) == "02" ~ "Preta",
  as.character(x) == "03" ~ "Parda",    as.character(x) == "04" ~ "Amarela",
  as.character(x) == "05" ~ "Indigena", TRUE ~ NA_character_)
raca_sim_f <- function(x) dplyr::case_when(   # 3 amarela · 4 parda
  as.character(x) == "1" ~ "Branca",   as.character(x) == "2" ~ "Preta",
  as.character(x) == "3" ~ "Amarela",  as.character(x) == "4" ~ "Parda",
  as.character(x) == "5" ~ "Indigena", TRUE ~ NA_character_)

cid3 <- function(x) {
  x <- toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
  x <- substr(x, 1, 3)
  x[!grepl("^[A-Z][0-9]{2}$", x)] <- NA_character_
  x
}
cnes7 <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  ifelse(is.na(x), NA_character_, formatC(x, width = 7, flag = "0"))
}
mun6 <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  substr(x, 1, 6)
}
col_ou_na <- function(df, nome) if (nome %in% names(df)) df[[nome]] else NA

# Acumulador da cobranca fora das faixas mapeadas (diagnostico, nao decisao).
COB_NAO_MAPEADA <- list()

UF_REG <- c(RO="Norte",AC="Norte",AM="Norte",RR="Norte",PA="Norte",AP="Norte",TO="Norte",
            MA="Nordeste",PI="Nordeste",CE="Nordeste",RN="Nordeste",PB="Nordeste",
            PE="Nordeste",AL="Nordeste",SE="Nordeste",BA="Nordeste",
            MG="Sudeste",ES="Sudeste",RJ="Sudeste",SP="Sudeste",
            PR="Sul",SC="Sul",RS="Sul",
            MS="Centro-Oeste",MT="Centro-Oeste",GO="Centro-Oeste",DF="Centro-Oeste")
UF_COD <- c("11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
            "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL",
            "28"="SE","29"="BA","31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR",
            "42"="SC","43"="RS","50"="MS","51"="MT","52"="GO","53"="DF")

faixa_etaria <- function(dias) dplyr::case_when(
  is.na(dias)     ~ NA_character_,
  dias <= 27      ~ "Neonatal (0\u201327 dias)",
  dias <  365     ~ "P\u00f3s-neonatal (28 d\u2013<1 ano)",
  dias <= 365 * 7 ~ "1 a 6 anos",
  TRUE            ~ NA_character_)

# ============================================================================
# ETAPA 1 — SIM: identificar os óbitos e separar os hospitalares (o alvo)
# ============================================================================
message("ETAPA 1 · Lendo o SIM e identificando os obitos hospitalares ...")
sim_raw <- readRDS(CONFIG$arq_sim)
sim <- data.frame(
  dt_nasc    = data_sim(col_ou_na(sim_raw, "DTNASC")),
  dt_obito   = data_sim(col_ou_na(sim_raw, "DTOBITO")),
  sexo       = sexo_sim(col_ou_na(sim_raw, "SEXO")),
  raca_sim   = raca_sim_f(col_ou_na(sim_raw, "RACACOR")),
  munic_res  = mun6(col_ou_na(sim_raw, "CODMUNRES")),
  munic_ocor = mun6(col_ou_na(sim_raw, "CODMUNOCOR")),
  codestab   = cnes7(col_ou_na(sim_raw, "CODESTAB")),
  local_ocor = as.character(col_ou_na(sim_raw, "LOCOCOR")),
  cid_obito  = cid3(col_ou_na(sim_raw, "CAUSABAS")),
  cid_obito_original = cid3(col_ou_na(sim_raw, "CAUSABAS_O")),
  linha_a    = cid3(col_ou_na(sim_raw, "LINHAA")),
  linha_ii   = cid3(col_ou_na(sim_raw, "LINHAII")),
  stringsAsFactors = FALSE)
rm(sim_raw); gc(verbose = FALSE)

sim$ano_obito <- as.integer(format(sim$dt_obito, "%Y"))
sim <- sim[!is.na(sim$dt_nasc) & !is.na(sim$dt_obito) & sim$ano_obito %in% CONFIG$anos, ]
sim$uf_res <- unname(UF_COD[substr(sim$munic_res, 1, 2)])

N_SIM_TOTAL <- nrow(sim)
comp_lococor <- as.data.frame(table(local_ocor = ifelse(is.na(sim$local_ocor), "NA",
                                                        sim$local_ocor)))
sim$alvo <- !is.na(sim$local_ocor) & sim$local_ocor %in% CONFIG$lococor_alvo
N_SIM_HOSP <- sum(sim$alvo)

# Funil por ano (denominadores da qualidade do linkage)
sim_por_ano <- sim %>% group_by(ano_obito) %>%
  summarise(obitos_sim = dplyr::n(), obitos_hosp = sum(alvo), .groups = "drop")
sim_por_uf <- sim %>% filter(!is.na(uf_res)) %>% group_by(uf_res) %>%
  summarise(obitos_sim = dplyr::n(), obitos_hosp = sum(alvo), .groups = "drop")

alvo <- as.data.table(sim[sim$alvo, ])
alvo[, id_sim := .I]
alvo[, bloco := paste(dt_nasc, sexo, sep = "|")]
setkey(alvo, bloco)
message("  obitos 0-6 no SIM: ", format(N_SIM_TOTAL, big.mark = "."),
        " · hospitalares (alvo): ", format(N_SIM_HOSP, big.mark = "."),
        sprintf(" (%.1f%%)\n", 100 * N_SIM_HOSP / N_SIM_TOTAL))

# ============================================================================
# Leitura do SIH por UF — todas as AIHs da coorte (óbitos, altas, transferências)
# ============================================================================
le_sih_uf <- function(uf) {
  partes <- list()
  for (ano in CONFIG$anos) {
    f <- file.path(CONFIG$dir_sih, sprintf("sih_%s_%d.rds", uf, ano))
    if (!file.exists(f)) { message("    lote ausente: ", basename(f)); next }
    df <- tryCatch(readRDS(f), error = function(e) {
      message("    lote corrompido: ", basename(f)); NULL })
    if (is.null(df) || nrow(df) == 0) next
    d <- data.frame(
      cnes      = cnes7(col_ou_na(df, "CNES")),
      munic_res = mun6(col_ou_na(df, "MUNIC_RES")),
      dt_nasc   = data_sih(col_ou_na(df, "NASC")),
      dt_inter  = data_sih(col_ou_na(df, "DT_INTER")),
      dt_saida  = data_sih(col_ou_na(df, "DT_SAIDA")),
      sexo      = sexo_sih(col_ou_na(df, "SEXO")),
      raca_sih  = raca_sih_f(col_ou_na(df, "RACA_COR")),
      cid_entrada    = cid3(col_ou_na(df, "DIAG_PRINC")),
      cid_secundario = cid3(col_ou_na(df, "DIAG_SECUN")),
      morte     = suppressWarnings(as.integer(col_ou_na(df, "MORTE"))),
      cobranca  = as.character(col_ou_na(df, "COBRANCA")),
      stringsAsFactors = FALSE)
    rm(df)
    d$idade_dias <- as.integer(d$dt_saida - d$dt_nasc)
    d <- d[!is.na(d$dt_nasc) & !is.na(d$dt_saida) & !is.na(d$idade_dias) &
             d$idade_dias >= 0 & d$idade_dias <= 365 * (CONFIG$idade_max_anos + 1), ]
    if (nrow(d)) partes[[length(partes) + 1]] <- d
  }
  if (!length(partes)) return(NULL)
  d <- as.data.frame(data.table::rbindlist(partes)); rm(partes)
  # Motivo de saida/permanencia (COBRANCA). MORTE == 1 tem precedencia: pega
  # o obito mesmo quando o motivo de cobranca esta fora das faixas abaixo.
  # Os codigos que nao caem em nenhuma faixa NAO viram NA silencioso — vao
  # para "Outro/nao classificado" e sao contabilizados em COB_NAO_MAPEADA,
  # gravado no arquivo de qualidade para inspecao (nada e adivinhado aqui).
  cob2 <- suppressWarnings(as.integer(substr(d$cobranca, 1, 2)))
  d$desfecho <- dplyr::case_when(
    !is.na(d$morte) & d$morte == 1 ~ "\u00d3bito",
    cob2 %in% 41:43                ~ "\u00d3bito",
    cob2 %in% 31:39                ~ "Transfer\u00eancia",
    cob2 %in% 11:19                ~ "Alta",
    cob2 %in% 21:29                ~ "Perman\u00eancia",
    cob2 == 51                     ~ "Encerramento administrativo",
    TRUE                           ~ "Outro/n\u00e3o classificado")
  nm <- is.na(cob2) | !(cob2 %in% c(11:19, 21:29, 31:39, 41:43, 51))
  if (any(nm)) {
    tb <- as.data.frame(table(cob = cob2[nm]), stringsAsFactors = FALSE)
    if (nrow(tb)) {
      tb$uf <- uf
      COB_NAO_MAPEADA[[length(COB_NAO_MAPEADA) + 1]] <<- tb
    }
  }
  d$faixa  <- faixa_etaria(d$idade_dias)
  d$uf     <- uf
  d$regiao <- unname(UF_REG[uf])
  d$ano    <- as.integer(format(d$dt_saida, "%Y"))
  d
}

# ============================================================================
# ETAPA 2 — Linkage do alvo com as AIHs de óbito (exato + probabilístico)
# ============================================================================
# Escore do pareamento probabilístico (aplicado dentro do bloco nasc+sexo):
#   data |óbito − saída| = 0 ......... +3      1 dia +2      2–3 dias +1
#   CNES igual (quando ambos têm) .... +2
#   município de residência igual .... +2
#   raça/cor concordante ............. +1
#   Par aceito com escore >= CONFIG$escore_minimo (3). O EXATO exige data
#   idêntica + município igual + CNES igual (quando os dois têm CNES).
# Cada AIH pareia com no máximo um óbito do SIM e vice-versa (dedupe pelo
# maior escore; empate decidido pela menor distância de data).
# ============================================================================
pareia_uf <- function(sih_ob) {
  if (is.null(sih_ob) || nrow(sih_ob) == 0) return(NULL)
  so <- as.data.table(sih_ob)
  so[, id_sih := .I]
  so[, bloco := paste(dt_nasc, sexo, sep = "|")]
  
  cand <- merge(so, alvo[, .(bloco, id_sim, dt_obito, raca_sim, munic_res_sim = munic_res,
                             munic_ocor, codestab, local_ocor, cid_obito,
                             cid_obito_original, linha_a, linha_ii)],
                by = "bloco", allow.cartesian = TRUE)
  if (nrow(cand) == 0) return(NULL)
  
  cand[, dist := abs(as.integer(dt_obito - dt_saida))]
  cand <- cand[dist <= CONFIG$buffer_dias]
  if (nrow(cand) == 0) return(NULL)
  
  cand[, pt_data := fifelse(dist == 0, 3L, fifelse(dist == 1, 2L, 1L))]
  cand[, cnes_ok  := !is.na(cnes) & !is.na(codestab) & cnes == codestab]
  cand[, mun_ok   := !is.na(munic_res) & !is.na(munic_res_sim) & munic_res == munic_res_sim]
  cand[, raca_ok  := !is.na(raca_sih) & !is.na(raca_sim) & raca_sih == raca_sim]
  cand[, escore := pt_data + 2L * cnes_ok + 2L * mun_ok + 1L * raca_ok]
  cand[, exato := dist == 0 & mun_ok & (cnes_ok | is.na(cnes) | is.na(codestab))]
  cand <- cand[escore >= CONFIG$escore_minimo | exato]
  if (nrow(cand) == 0) return(NULL)
  
  # dedupe: melhor par por AIH, depois por óbito do SIM (greedy pelo escore)
  setorder(cand, id_sih, -exato, -escore, dist)
  cand <- cand[, .SD[1L], by = id_sih]
  setorder(cand, id_sim, -exato, -escore, dist)
  cand <- cand[, .SD[1L], by = id_sim]
  
  cand[, metodo := fifelse(exato, "Exato", "Probabil\u00edstico")]
  cand
}

# ============================================================================
# ETAPA 3 — Retroagir: internações anteriores à internação-índice
# ============================================================================
historico_uf <- function(sih_todas, indice) {
  # sih_todas: todas as AIHs da UF · indice: AIHs de óbito linkadas (Etapa 2)
  if (is.null(indice) || nrow(indice) == 0) return(NULL)
  prev <- as.data.table(sih_todas)[desfecho %in% c("Alta", "Transfer\u00eancia")]
  if (nrow(prev) == 0) return(NULL)
  prev[, bloco := paste(dt_nasc, sexo, munic_res, sep = "|")]
  idx <- as.data.table(indice)
  idx[, bloco := paste(dt_nasc, sexo, munic_res, sep = "|")]
  
  pj <- merge(idx[, .(id_sih, bloco, dt_inter_idx = dt_inter)],
              prev[, .(bloco, dt_saida_prev = dt_saida, dt_inter_prev = dt_inter,
                       desfecho_prev = desfecho, cid_prev = cid_entrada)],
              by = "bloco", allow.cartesian = TRUE)
  if (nrow(pj) == 0) return(NULL)
  pj[, gap := as.integer(dt_inter_idx - dt_saida_prev)]
  jmax <- max(CONFIG$janelas_sens)
  pj <- pj[gap > 0 & gap <= jmax]           # gap 0/negativo = mesmo episódio
  if (nrow(pj) == 0) return(NULL)
  
  setorder(pj, id_sih, gap)
  res <- pj[, .(
    n_prev_60        = .N,
    n_prev_45        = sum(gap <= 45),
    n_prev_30        = sum(gap <= 30),
    gap_prev         = gap[1L],
    desfecho_prev    = desfecho_prev[1L],
    cid_prev         = cid_prev[1L],
    dt_saida_prev    = dt_saida_prev[1L]
  ), by = id_sih]
  # pares completos: usados na Etapa 5 para excluir TODAS as internações
  # anteriores identificadas (não só a mais próxima)
  list(res = res,
       pares = unique(pj[, .(pac = bloco, dt_saida = dt_saida_prev)]))
}

# ============================================================================
# Processamento por UF
# ============================================================================
processa_uf <- function(uf) {
  arq <- file.path(CONFIG$parciais, paste0(uf, ".rds"))
  if (file.exists(arq)) { message("UF ", uf, " ja processada — pulando."); return(readRDS(arq)) }
  ini <- Sys.time()
  message("===== ", uf, " =====")
  
  sih <- le_sih_uf(uf)
  if (is.null(sih)) { message("  sem SIH para ", uf); return(NULL) }
  message("  AIHs 0-6 anos: ", format(nrow(sih), big.mark = "."))
  
  # -- consolida episódios (encadeia transferências, como na v2) -------------
  sih$pac <- paste(sih$dt_nasc, sih$sexo, sih$munic_res, sep = "|")
  sih <- sih %>% arrange(pac, dt_inter, dt_saida) %>%
    group_by(pac) %>%
    mutate(gap = as.integer(dt_inter - dplyr::lag(dt_saida)),
           episodio = cumsum(is.na(gap) | gap > CONFIG$gap_episodio)) %>%
    ungroup()
  
  # DESFECHO DO EPISODIO — o obito tem prioridade sobre a ordem das AIHs.
  #
  # A versao anterior usava last(desfecho), ou seja, o desfecho da ULTIMA AIH
  # por data de internacao. Isso perdia o obito em duas situacoes: quando a
  # ultima AIH do episodio tinha motivo de cobranca fora das faixas mapeadas
  # (desfecho NA) e quando duas AIHs empatavam na data. O efeito era duplo —
  # o episodio saia do denominador do linkage E entrava no grupo "de baixa
  # mortalidade" da Etapa 5. Na execucao de 07/08 isso somava 48.602 AIHs de
  # obito classificadas como internacao sem obito associado.
  #
  # Agora: se QUALQUER AIH do episodio registra obito, o episodio e obito, e a
  # AIH de referencia (a que fornece CNES e data de saida para o pareamento
  # com o SIM) passa a ser a propria AIH de obito.
  sih <- sih %>%
    group_by(pac, episodio) %>%
    mutate(
      tem_obito = any(desfecho == "\u00d3bito", na.rm = TRUE),
      .ref = {
        i <- which(desfecho == "\u00d3bito")
        j <- which(!is.na(desfecho) & desfecho != "Outro/n\u00e3o classificado")
        if (length(i)) max(i) else if (length(j)) max(j) else dplyr::n()
      },
      t0 = min(dt_inter, na.rm = TRUE),
      t2 = max(dt_saida, na.rm = TRUE),
      n_transf   = sum(desfecho == "Transfer\u00eancia", na.rm = TRUE),
      n_aih      = dplyr::n(),
      cid_ent_t0 = dplyr::first(cid_entrada),
      desf_final = dplyr::if_else(tem_obito, "\u00d3bito", desfecho[.ref[1]])) %>%
    ungroup()
  
  ep <- sih %>%
    group_by(pac, episodio) %>%
    dplyr::slice(dplyr::first(.ref)) %>%   # AIH de referencia (a de obito, quando existe)
    ungroup() %>%
    transmute(dt_nasc, sexo, raca_sih, munic_res, cnes, uf, regiao, faixa, idade_dias,
              ano = as.integer(format(dt_saida, "%Y")),
              dt_inter = t0, dt_saida,
              cid_entrada = cid_ent_t0, cid_secundario,
              desfecho = desf_final, n_transferencias = n_transf, n_aih,
              dias_internacao = pmax(1L, as.integer(t2 - t0)))
  ep <- ep[ep$ano %in% CONFIG$anos & !is.na(ep$desfecho), ]
  
  # -- CASCATA DE ESTAGIOS (v3.3) -------------------------------------------
  # Abre o episodio na sequencia real das AIHs. O estagio 1 e a primeira AIH
  # do episodio; o estagio 2 so existe para quem teve uma segunda AIH; e assim
  # por diante. O desfecho gravado aqui e o da PROPRIA AIH daquele estagio —
  # nao o desfecho final do episodio. E isso que permite ler "no estagio 1,
  # tantos foram para alta, tantos morreram, tantos foram transferidos; dos
  # transferidos, no estagio 2 ..." sem inventar nenhuma etapa.
  #
  # Cobre TODOS os episodios, inclusive os que terminam em alta — que ficam
  # de fora da base analitica do app e por isso nao poderiam ser recuperados
  # depois. O resultado e um agregado pequeno, nao um microdado.
  cs <- as.data.table(sih)[, .(pac, episodio, uf, regiao, sexo, faixa,
                               desfecho, dt_inter, dt_saida,
                               ano = as.integer(format(dt_saida, "%Y")))]
  setorder(cs, pac, episodio, dt_inter, dt_saida)
  cs[, estagio := seq_len(.N), by = .(pac, episodio)]
  # Estrato do EPISODIO (primeira AIH), para o agregado nao trocar de faixa
  # etaria no meio da trajetoria.
  cs[, `:=`(ano_ep   = ano[1L],
            faixa_ep = faixa[1L],
            sexo_ep  = sexo[1L],
            n_aih_ep = .N), by = .(pac, episodio)]
  cs <- cs[ano_ep %in% CONFIG$anos & !is.na(faixa_ep)]
  cs[is.na(desfecho), desfecho := "Outro/não classificado"]
  # Desfecho da AIH ANTERIOR. E o que permite ler a subpopulacao: "dos que
  # foram transferidos no estagio 1, no estagio 2 tantos tiveram alta, tantos
  # morreram, tantos foram transferidos de novo". Sem esta coluna a leitura
  # seria "quem teve uma segunda AIH", que nao e a mesma pergunta — a segunda
  # AIH tambem aparece depois de alta com reinternacao rapida.
  cs[, desfecho_ant := shift(desfecho, 1L, type = "lag"), by = .(pac, episodio)]
  cs[estagio == 1L, desfecho_ant := "— (entrada)"]
  cascata <- cs[, .(n = .N),
                by = .(ano = ano_ep, uf, regiao, faixa = faixa_ep, sexo = sexo_ep,
                       estagio, desfecho_ant, desfecho)]
  rm(cs); gc(verbose = FALSE)
  
  # -- ETAPA 2: pareia episódios com desfecho óbito contra o alvo do SIM -----
  ep_ob <- ep[ep$desfecho == "\u00d3bito", ]
  message("  episodios: ", format(nrow(ep), big.mark = "."),
          " · com obito: ", format(nrow(ep_ob), big.mark = "."))
  par2 <- pareia_uf(ep_ob)
  n_lk <- if (is.null(par2)) 0L else nrow(par2)
  message("  pareados com o SIM: ", format(n_lk, big.mark = "."),
          sprintf(" (%.1f%% dos episodios-obito)", 100 * n_lk / max(1, nrow(ep_ob))))
  
  # -- ETAPA 3: histórico das internações-índice linkadas --------------------
  h3 <- if (!is.null(par2))
    historico_uf(sih, merge(as.data.table(ep_ob)[, id_sih := .I],
                            par2[, .(id_sih)], by = "id_sih")) else NULL
  # (id_sih de ep_ob = número da linha, o mesmo usado em pareia_uf)
  hist3      <- if (is.null(h3)) NULL else h3$res
  pares_hist <- if (is.null(h3)) NULL else h3$pares
  
  # -- monta a saída da UF ---------------------------------------------------
  ep <- as.data.table(ep)
  ep[, id_sih := NA_integer_]                            # id só vale nos óbitos
  ep_ob_idx <- which(ep$desfecho == "\u00d3bito")
  ep$id_sih[ep_ob_idx] <- seq_along(ep_ob_idx)
  
  if (!is.null(par2)) {
    par2_cols <- par2[, .(id_sih, metodo, escore, dist_data = dist,
                          dt_obito, raca_sim, munic_ocor, local_ocor, codestab,
                          cid_obito, cid_obito_original, linha_a, linha_ii)]
    ep <- merge(ep, par2_cols, by = "id_sih", all.x = TRUE)
  } else {
    ep[, `:=`(metodo = NA_character_, escore = NA_integer_, dist_data = NA_integer_,
              dt_obito = as.Date(NA), raca_sim = NA_character_,
              munic_ocor = NA_character_, local_ocor = NA_character_,
              codestab = NA_character_, cid_obito = NA_character_,
              cid_obito_original = NA_character_, linha_a = NA_character_,
              linha_ii = NA_character_)]
  }
  if (!is.null(hist3)) {
    ep <- merge(ep, hist3, by = "id_sih", all.x = TRUE)
  } else {
    ep[, `:=`(n_prev_60 = NA_integer_, n_prev_45 = NA_integer_, n_prev_30 = NA_integer_,
              gap_prev = NA_integer_, desfecho_prev = NA_character_,
              cid_prev = NA_character_, dt_saida_prev = as.Date(NA))]
  }
  ep[, linkado := !is.na(metodo)]
  ep[, raca_concorda := !is.na(raca_sih) & !is.na(raca_sim) & raca_sih == raca_sim]
  ep[, raca_cor := dplyr::coalesce(raca_sih, fifelse(linkado, raca_sim, NA_character_))]
  
  # -- ETAPA 5 (parcial da UF): AIHs fora do circuito de óbito ---------------
  # Exclui (a) TODAS as AIHs de episódios com óbito — agora pelo desfecho
  # robusto do episódio, mais uma trava por AIH, de modo que nenhuma AIH de
  # óbito possa cair no grupo "de baixa mortalidade" — e (b) todas as
  # internações anteriores identificadas na Etapa 3 (qualquer janela).
  bx <- as.data.table(sih)[
    (!is.na(desf_final) & desf_final != "\u00d3bito") &
      (is.na(desfecho) | desfecho != "\u00d3bito")]
  bx[, pac := paste(dt_nasc, sexo, munic_res, sep = "|")]
  if (!is.null(pares_hist) && nrow(pares_hist)) {
    bx <- merge(bx, pares_hist[, .(pac, dt_saida, eh_hist = TRUE)],
                by = c("pac", "dt_saida"), all.x = TRUE)
    bx <- bx[is.na(eh_hist)]
  }
  cl <- cid_classify(bx$cid_entrada)
  bx[, sistema := cl$sistema]
  baixa <- bx[, .(n = .N,
                  dias_medio   = mean(pmax(1L, as.integer(dt_saida - dt_inter)), na.rm = TRUE),
                  dias_mediano = as.numeric(stats::median(pmax(1L, as.integer(dt_saida - dt_inter)), na.rm = TRUE))),
              by = .(ano, uf, regiao, faixa, sexo, sistema, desfecho)]
  rm(sih, bx); gc(verbose = FALSE)
  
  out <- list(ep = as.data.frame(ep), baixa = as.data.frame(baixa),
              cascata = as.data.frame(cascata),
              # ids dos obitos do SIM efetivamente pareados nesta UF — sao
              # eles que permitem separar, no alvo, quem foi capturado de quem
              # nao foi (v3.3). Vai no cache da UF para sobreviver a retomada.
              id_sim = if (is.null(par2)) integer(0) else as.integer(par2$id_sim))
  message("  concluida em ",
          round(as.numeric(difftime(Sys.time(), ini, units = "mins")), 1), " min")
  saveRDS(out, arq)
  out
}

# ----------------------------------------------------------------------------
# Execução
# ----------------------------------------------------------------------------
message("UFs: ", paste(CONFIG$ufs, collapse = " "), " | anos: ",
        min(CONFIG$anos), "-", max(CONFIG$anos), "\n")

todas <- lapply(CONFIG$ufs, function(u)
  tryCatch(processa_uf(u), error = function(e) { message("ERRO em ", u, ": ", e$message); NULL }))
todas <- todas[!vapply(todas, is.null, logical(1))]
if (!length(todas)) stop("Nenhuma UF processada.")

base  <- as.data.frame(rbindlist(lapply(todas, `[[`, "ep"),    use.names = TRUE, fill = TRUE))
baixa <- as.data.frame(rbindlist(lapply(todas, `[[`, "baixa"), use.names = TRUE, fill = TRUE))

# ---- v3.3: cascata de estagios e ids do SIM pareados ------------------------
casc_l <- lapply(todas, function(x) x$cascata)
casc_l <- casc_l[!vapply(casc_l, is.null, logical(1))]
cascata <- if (length(casc_l))
  as.data.frame(rbindlist(casc_l, use.names = TRUE, fill = TRUE)) else
    data.frame(ano = integer(0), uf = character(0), regiao = character(0),
               faixa = character(0), sexo = character(0), estagio = integer(0),
               desfecho_ant = character(0), desfecho = character(0),
               n = integer(0), stringsAsFactors = FALSE)
rm(casc_l)

ID_SIM_PAREADOS <- unique(unlist(lapply(todas, function(x) x$id_sim), use.names = FALSE))
ID_SIM_PAREADOS <- ID_SIM_PAREADOS[!is.na(ID_SIM_PAREADOS)]

rm(todas); gc(verbose = FALSE)

# Um mesmo obito do SIM nao pode ter sido capturado por duas UFs: o dedupe
# por id_sim acontece dentro de pareia_uf, mas o alvo e nacional e a UF do
# SIH e a do estabelecimento. Se aparecer duplicata entre UFs, o unique acima
# ja resolveu — a checagem abaixo so garante que nao ha id fora do alvo.
stopifnot(all(ID_SIM_PAREADOS %in% alvo$id_sim))
alvo[, pareado := id_sim %in% ID_SIM_PAREADOS]
alvo[, ano_ob := as.integer(format(dt_obito, "%Y"))]
alvo[, uf_ocor := unname(UF_COD[substr(munic_ocor, 1, 2)])]

LOCOCOR_NOME <- c("1" = "Hospital",
                  "2" = "Outro estabelecimento de saúde",
                  "3" = "Domicílio", "4" = "Via pública",
                  "5" = "Outros", "6" = "Aldeia indígena",
                  "9" = "Ignorado")

CAPT_LOCOCOR <- as.data.frame(
  alvo[, .(alvo = .N, pareados = sum(pareado), nao_pareados = sum(!pareado),
           pct_capturado = 100 * mean(pareado)), by = .(local_ocor)])
CAPT_LOCOCOR <- CAPT_LOCOCOR[order(CAPT_LOCOCOR$local_ocor), ]
CAPT_LOCOCOR$local <- unname(LOCOCOR_NOME[CAPT_LOCOCOR$local_ocor])
CAPT_LOCOCOR$local[is.na(CAPT_LOCOCOR$local)] <- "Não informado"
CAPT_LOCOCOR <- CAPT_LOCOCOR[, c("local_ocor", "local", "alvo", "pareados",
                                 "nao_pareados", "pct_capturado")]

message("\nv3.3 · captura do alvo por local de ocorrencia:")
print(CAPT_LOCOCOR)

# Lista completa de estabelecimentos com obito nao capturado, por competencia.
# E a entrada do 10_sia_*: com CNES e ano/mes da a competencia exata a
# conferir na producao ambulatorial.
cnes_nc <- alvo[pareado == FALSE & !is.na(codestab),
                .(obitos_nao_capturados = .N),
                by = .(codestab, local_ocor, uf_ocor,
                       ano = ano_ob, mes = as.integer(format(dt_obito, "%m")))]
setorder(cnes_nc, -obitos_nao_capturados)
data.table::fwrite(cnes_nc, CONFIG$saida_cnes)
message("Gravado: ", CONFIG$saida_cnes, " (",
        format(nrow(cnes_nc), big.mark = "."), " linhas · ",
        format(length(unique(cnes_nc$codestab)), big.mark = "."), " CNES)")

# ============================================================================
# ETAPA 4 — Base analítica: classificação CID e transições (com histórico)
# ============================================================================
# ---- Contagens do lado SIH, ANTES de restringir a base analítica --------
# São os denominadores da trilha do SIH na reconciliação. Precisam ser lidos
# aqui porque `final` é filtrado logo abaixo para óbito + transferência.
N_EP_TOTAL  <- nrow(base)
N_EP_ALTA   <- sum(base$desfecho == "Alta", na.rm = TRUE)
N_EP_TRANSF <- sum(base$desfecho == "Transfer\u00eancia", na.rm = TRUE)
N_EP_OUTRO  <- N_EP_TOTAL - N_EP_ALTA - N_EP_TRANSF -
  sum(base$desfecho == "\u00d3bito", na.rm = TRUE)
N_AIH_TOTAL <- sum(base$n_aih, na.rm = TRUE)
COB_NM <- if (length(COB_NAO_MAPEADA))
  as.data.frame(rbindlist(COB_NAO_MAPEADA, use.names = TRUE, fill = TRUE)) %>%
  group_by(cob) %>% summarise(n = sum(Freq), .groups = "drop") %>%
  arrange(desc(n)) %>% as.data.frame() else
    data.frame(cob = character(0), n = integer(0))

ce <- cid_classify(base$cid_entrada)
co <- cid_classify(base$cid_obito)
cp <- cid_classify(base$cid_prev)

final <- base %>%
  transmute(ano, regiao, uf, faixa, sexo, raca_cor,
            cid_entrada, cid_obito, cid_obito_original, cid_secundario,
            desfecho, n_transferencias, n_aih, dias_internacao,
            data_t0 = dt_inter, data_t2 = dt_saida, dt_obito,
            cnes, munic_res, munic_ocor, local_ocor,
            linkado, metodo, escore, dist_data, raca_concorda, idade_dias,
            raca_sih_orig = raca_sih, raca_sim_orig = raca_sim,
            # Etapa 3 — histórico
            n_prev_30, n_prev_45, n_prev_60, gap_prev, desfecho_prev,
            cid_prev, nm_prev = cp$nome, sis_prev = cp$sistema,
            nm_entrada  = ce$nome,     nm_obito  = co$nome,
            sis_entrada = ce$sistema,  sis_obito = co$sistema,
            grp_entrada = ce$grupo,    grp_obito = co$grupo,
            gen_entrada = ce$generico, gen_obito = co$generico) %>%
  mutate(teve_prev_30 = !is.na(n_prev_30) & n_prev_30 > 0,
         teve_prev_45 = !is.na(n_prev_45) & n_prev_45 > 0,
         teve_prev_60 = !is.na(n_prev_60) & n_prev_60 > 0,
         mudou_cid = !is.na(cid_obito) & cid_entrada != cid_obito,
         mudou_sis = !is.na(sis_obito) & sis_entrada != sis_obito,
         transicao = dplyr::case_when(
           is.na(cid_obito)                  ~ "Sem \u00f3bito registrado",
           !mudou_cid & gen_obito == 1       ~ "Mesmo CID, gen\u00e9rico (causa nunca esclarecida)",
           !mudou_cid                        ~ "Mesmo CID, espec\u00edfico",
           gen_entrada == 1 & gen_obito == 1 ~ "Gen\u00e9rico \u2192 outro gen\u00e9rico (causa nunca esclarecida)",
           gen_entrada == 1 & gen_obito == 0 ~ "Gen\u00e9rico \u2192 Espec\u00edfico (ganho diagn\u00f3stico)",
           gen_entrada == 0 & gen_obito == 1 ~ "Espec\u00edfico \u2192 Gen\u00e9rico (perda diagn\u00f3stica)",
           mudou_sis                         ~ "Espec\u00edfico \u2192 Espec\u00edfico, outro sistema",
           TRUE                              ~ "Espec\u00edfico \u2192 Espec\u00edfico, mesmo sistema"))

# Base analítica do app: restrita aos episódios com óbito ou transferência.
# As internações com alta já estão representadas no agregado da Etapa 5 —
# mantê-las aqui infla o rds para >180 MB e derruba o deploy no shinyapps.
final <- final %>% filter(desfecho %in% c("\u00d3bito", "Transfer\u00eancia"))

saveRDS(final, CONFIG$saida_link)
message("\nGravado: ", CONFIG$saida_link, " (", format(nrow(final), big.mark = "."), " linhas)")

saveRDS(baixa, CONFIG$saida_baixa)
message("Gravado: ", CONFIG$saida_baixa, " (", format(nrow(baixa), big.mark = "."),
        " linhas agregadas — Etapa 5)")

saveRDS(cascata, CONFIG$saida_casc)
message("Gravado: ", CONFIG$saida_casc, " (", format(nrow(cascata), big.mark = "."),
        " linhas agregadas — cascata de estagios, ",
        format(sum(cascata$n[cascata$estagio == 1]), big.mark = "."),
        " episodios no estagio 1)")

# ============================================================================
# Qualidade do linkage — métricas para o app e para o relatório
# ============================================================================
ob  <- final %>% filter(desfecho == "\u00d3bito")
lk  <- ob %>% filter(linkado)
n_exato <- sum(lk$metodo == "Exato", na.rm = TRUE)
n_prob  <- sum(lk$metodo == "Probabil\u00edstico", na.rm = TRUE)

# Obitos distintos do SIM pareados (unidade: obito). nrow(lk) e maior:
# conta episodios do SIH, e um obito pode ter mais de um.
N_OB_PAREADOS <- if (exists("CAPT_LOCOCOR") && nrow(CAPT_LOCOCOR))
  sum(CAPT_LOCOCOR$pareados) else nrow(lk)

qual <- list(
  gerado_em = format(Sys.time(), "%Y-%m-%d %H:%M"),
  config = CONFIG[c("lococor_alvo", "buffer_dias", "escore_minimo",
                    "janela_principal", "janelas_sens")],
  
  # ---- RECONCILIACAO EM DUAS TRILHAS -----------------------------------
  # As duas bases NAO sao subconjunto uma da outra: o SIM conta OBITOS e o
  # SIH conta EPISODIOS de internacao. Os dois universos so se encontram no
  # numero de pareados. Guardar as trilhas separadas evita a leitura errada
  # de que 374.604 "cai" para 323.802 — sao eixos diferentes.
  # v3.3: o alvo aparece aberto por local de ocorrencia. Sem essa quebra, a
  # cobertura de 50,6% e lida como falha de pareamento \u2014 quando boa parte do
  # denominador e obito que nunca poderia ter gerado AIH.
  trilha_sim = data.frame(
    etapa = c("\u00d3bitos de 0 a 6 anos no SIM",
              "\u00d3bitos hospitalares (alvo \u2014 Etapa 1)",
              "\u2014 em hospital (LOCOCOR 1)",
              "\u2014 em outro estabelecimento de sa\u00fade (LOCOCOR 2)",
              "Alvo capturado na AIH (pareados)",
              "Alvo n\u00e3o capturado"),
    # Unidade desta trilha e OBITO: o capturado e o numero de obitos
    # DISTINTOS do SIM com pelo menos uma AIH pareada (N_OB_PAREADOS), nao
    # nrow(lk), que conta EPISODIOS do SIH — um obito pareado a dois
    # episodios (reinternacao) entraria duas vezes. Ate a v3.3 a linha usava
    # nrow(lk) e o "nao capturado" saia 184.995 quando a captura por LOCOCOR
    # dava 188.987.
    n = c(N_SIM_TOTAL, N_SIM_HOSP,
          sum(alvo$local_ocor == "1", na.rm = TRUE),
          sum(alvo$local_ocor == "2", na.rm = TRUE),
          N_OB_PAREADOS, N_SIM_HOSP - N_OB_PAREADOS),
    unidade = "\u00f3bitos (SIM)", stringsAsFactors = FALSE),
  
  trilha_sih = data.frame(
    etapa = c("Epis\u00f3dios de interna\u00e7\u00e3o de 0 a 6 anos",
              "\u2014 com desfecho alta",
              "\u2014 com desfecho transfer\u00eancia",
              "\u2014 com desfecho \u00f3bito",
              "Base anal\u00edtica (\u00f3bito + transfer\u00eancia)",
              "\u00d3bitos pareados com o SIM (Etapa 2)",
              "\u00d3bitos sem par no SIM"),
    n = c(N_EP_TOTAL, N_EP_ALTA, N_EP_TRANSF, nrow(ob),
          nrow(ob) + N_EP_TRANSF, nrow(lk), nrow(ob) - nrow(lk)),
    unidade = "epis\u00f3dios (SIH)", stringsAsFactors = FALSE),
  
  # Funil das etapas 1–3 (mantido para o gráfico do painel)
  funil = data.frame(
    etapa = c("\u00d3bitos 0\u20136 anos no SIM",
              "\u00d3bitos hospitalares no SIM (alvo \u2014 Etapa 1)",
              "Epis\u00f3dios SIH com desfecho \u00f3bito",
              "Pareados SIM\u2194SIH (Etapa 2)",
              "\u2014 dos quais, match exato",
              "\u2014 dos quais, match probabil\u00edstico",
              "Com interna\u00e7\u00e3o anterior em 30 dias (Etapa 3)"),
    n = c(N_SIM_TOTAL, N_SIM_HOSP, nrow(ob), nrow(lk), n_exato, n_prob,
          sum(lk$teve_prev_30))),
  
  # Unidades de contagem — exibidas no painel para nao confundir os eixos
  unidades = data.frame(
    conceito = c("\u00d3bito (SIM)", "AIH (SIH)", "Epis\u00f3dio (SIH)"),
    definicao = c("um registro de Declara\u00e7\u00e3o de \u00d3bito",
                  "uma Autoriza\u00e7\u00e3o de Interna\u00e7\u00e3o Hospitalar",
                  paste0("AIHs do mesmo paciente encadeadas com at\u00e9 ",
                         CONFIG$gap_episodio, " dias entre alta e nova entrada")),
    total = c(N_SIM_TOTAL, N_AIH_TOTAL, N_EP_TOTAL), stringsAsFactors = FALSE),
  
  cobranca_nao_mapeada = COB_NM,
  
  # Composição do LOCOCOR no SIM (transparência da Etapa 1)
  lococor = comp_lococor,
  
  # ---- v3.3 · POR QUE O ALVO NAO E TODO CAPTURAVEL ----------------------
  # O alvo da Etapa 1 e "obito hospitalar", mas LOCOCOR 1 e LOCOCOR 2 tem
  # naturezas diferentes: o obito em hospital pode ter gerado AIH; o obito em
  # "outro estabelecimento de saude" (UPA, pronto atendimento, UBS) nao gera
  # AIH nenhuma — o atendimento e ambulatorial e vai para o SIA. Manter os
  # dois no mesmo denominador faz a cobertura do linkage parecer baixa por um
  # motivo que nao e falha de pareamento.
  captura_lococor = CAPT_LOCOCOR,
  
  captura_lococor_uf = as.data.frame(
    alvo[!is.na(uf_res), .(alvo = .N, pareados = sum(pareado),
                           pct_capturado = 100 * mean(pareado)),
         by = .(uf = uf_res, local_ocor)][order(uf, local_ocor)]),
  
  captura_lococor_ano = as.data.frame(
    alvo[!is.na(ano_ob), .(alvo = .N, pareados = sum(pareado),
                           pct_capturado = 100 * mean(pareado)),
         by = .(ano = ano_ob, local_ocor)][order(ano, local_ocor)]),
  
  # Os 40 estabelecimentos que mais concentram obito nao capturado. E a
  # amostra que o painel exibe; a lista completa vai para o CSV que alimenta
  # a conferencia no SIA.
  top_cnes_nao_capturado = as.data.frame(
    alvo[pareado == FALSE & !is.na(codestab),
         .(obitos_nao_capturados = .N),
         by = .(codestab, local_ocor, uf = uf_ocor)][
           order(-obitos_nao_capturados)][seq_len(min(40, .N))]),
  
  # Quanto do nao capturado sequer tem CNES informado na DO — sem CNES nao ha
  # como conferir no SIA nem em lugar nenhum.
  nao_capturado_sem_cnes = as.data.frame(
    alvo[pareado == FALSE, .(n = .N),
         by = .(local_ocor, tem_cnes = !is.na(codestab))]),
  
  # Por ano: cobertura e composição exato × probabilístico
  por_ano = ob %>% group_by(ano) %>%
    summarise(episodios_obito = dplyr::n(),
              pareados  = sum(linkado),
              exato     = sum(metodo == "Exato", na.rm = TRUE),
              prob      = sum(metodo == "Probabil\u00edstico", na.rm = TRUE),
              com_prev_30 = sum(teve_prev_30),
              .groups = "drop") %>%
    left_join(sim_por_ano, by = c("ano" = "ano_obito")) %>%
    mutate(cobertura_alvo = 100 * pareados / obitos_hosp),
  
  # Por UF
  por_uf = ob %>% group_by(uf, regiao) %>%
    summarise(episodios_obito = dplyr::n(),
              pareados  = sum(linkado),
              exato     = sum(metodo == "Exato", na.rm = TRUE),
              prob      = sum(metodo == "Probabil\u00edstico", na.rm = TRUE),
              com_prev_30 = sum(teve_prev_30),
              .groups = "drop") %>%
    left_join(sim_por_uf, by = c("uf" = "uf_res")) %>%
    mutate(cobertura_alvo = 100 * pareados / obitos_hosp),
  
  # Sensibilidade da janela de retroação (Etapa 3)
  sensibilidade = data.frame(
    janela = CONFIG$janelas_sens,
    com_prev = c(sum(lk$teve_prev_30), sum(lk$teve_prev_45), sum(lk$teve_prev_60)),
    pct = 100 * c(mean(lk$teve_prev_30), mean(lk$teve_prev_45), mean(lk$teve_prev_60))),
  
  # Distribuição do escore e da distância de data nos probabilísticos
  escore_dist = lk %>% filter(metodo == "Probabil\u00edstico") %>%
    count(escore, dist_data, name = "n"),
  
  # Desfecho da internação anterior (Etapa 3): alta × transferência
  prev_desfecho = lk %>% filter(teve_prev_30) %>%
    count(desfecho_prev, name = "n") %>%
    mutate(pct = 100 * n / sum(n))
)
saveRDS(qual, CONFIG$saida_qual)
message("Gravado: ", CONFIG$saida_qual)

# ----------------------------------------------------------------------------
# Checagens de consistencia — falham alto em vez de gerar numero divergente
# ----------------------------------------------------------------------------
checa <- function(cond, msg) if (!isTRUE(cond)) stop("INCONSISTENCIA: ", msg, call. = FALSE)

checa(nrow(final) == nrow(ob) + N_EP_TRANSF, "base analitica != obitos + transferencias")
checa(nrow(lk) <= nrow(ob),                  "pareados > obitos do SIH")
checa(nrow(lk) <= N_SIM_HOSP,                "pareados > alvo do SIM")
checa(n_exato + n_prob == nrow(lk),          "exato + probabilistico != pareados")
checa(sum(lk$teve_prev_30) <= sum(lk$teve_prev_45) &&
        sum(lk$teve_prev_45) <= sum(lk$teve_prev_60),
      "janelas de retroacao nao sao monotonicas")
checa(!any(baixa$desfecho == "\u00d3bito", na.rm = TRUE),
      "AIH com desfecho obito dentro do grupo de baixa mortalidade (Etapa 5)")
checa(N_EP_TOTAL >= nrow(final), "episodios totais < base analitica")
# ---- v3.3 -------------------------------------------------------------------
# CAPT_LOCOCOR conta OBITOS DO SIM distintos; nrow(lk) conta EPISODIOS do SIH
# marcados como linkados. Os dois nao sao iguais por construcao: o dedupe por
# id_sim acontece DENTRO de pareia_uf, ou seja, por UF, enquanto o alvo e
# nacional. Uma crianca residente numa UF e atendida em outra pode ter o mesmo
# obito pareado em duas UFs — dois episodios do SIH apontando para um obito do
# SIM. Nesse caso lk e maior. Nunca o contrario: se CAPT ficar MAIOR que lk,
# ai sim ha id_sim fora do alvo e o script para.
dup_par <- nrow(lk) - sum(CAPT_LOCOCOR$pareados)
checa(dup_par >= 0, "captura por LOCOCOR maior que o total de pareados")
if (dup_par > 0)
  message("Nota: ", format(dup_par, big.mark = "."), " episodios do SIH (",
          sprintf("%.2f%%", 100 * dup_par / nrow(lk)),
          ") pareiam com um obito do SIM ja pareado em outra UF. ",
          "Contagem por LOCOCOR usa obitos distintos do SIM: ",
          format(sum(CAPT_LOCOCOR$pareados), big.mark = "."), ".")
checa(sum(CAPT_LOCOCOR$alvo) == N_SIM_HOSP,
      "alvo por LOCOCOR nao fecha com o alvo da Etapa 1")
# O obito em LOCOCOR 2 (outro estabelecimento de saude) nao gera AIH: se
# aparecer pareamento em volume nesse grupo, ou o LOCOCOR esta errado na DO
# ou o pareamento esta frouxo. Nao e erro fatal, mas tem de aparecer no log.
if (nrow(CAPT_LOCOCOR)) {
  l2 <- CAPT_LOCOCOR[CAPT_LOCOCOR$local_ocor == "2", ]
  if (nrow(l2) && isTRUE(l2$pct_capturado > 20))
    message("AVISO: ", sprintf("%.1f%%", l2$pct_capturado),
            " dos obitos em 'outro estabelecimento de saude' foram pareados ",
            "com AIH. Conferir LOCOCOR na DO e o escore do probabilistico.")
}
checa(nrow(cascata) > 0, "cascata de estagios vazia")
checa(min(cascata$estagio) == 1, "cascata sem estagio 1")
# Todo episodio passa pelo estagio 1, entao o estagio 1 deve ficar proximo do
# total de episodios da trilha do SIH. Nao sao identicos por construcao: a
# trilha exclui episodios com desfecho final NA e usa o ano da AIH de
# referencia, enquanto a cascata usa o ano da primeira AIH. Divergencia
# pequena e esperada; grande e sinal de bug — por isso avisa em vez de parar.
d_casc <- sum(cascata$n[cascata$estagio == 1]) - N_EP_TOTAL
if (abs(d_casc) > N_EP_TOTAL * 0.03)
  message("AVISO: estagio 1 da cascata difere de N_EP_TOTAL em ",
          format(d_casc, big.mark = "."), " (",
          sprintf("%.1f%%", 100 * d_casc / N_EP_TOTAL), ").")
# A cascata so pode encolher: ninguem chega ao estagio k+1 sem passar por k.
casc_est <- tapply(cascata$n, cascata$estagio, sum)
checa(all(diff(as.numeric(casc_est)) <= 0), "cascata de estagios nao e monotonica")
message("Checagens de consistencia: OK")

# ============================================================================
# Relatório em texto
# ============================================================================
pct <- function(a, b) sprintf("%.1f%%", 100 * a / max(1, b))
amb      <- lk %>% filter(!is.na(raca_sih_orig), !is.na(raca_sim_orig))
conc_amb <- if (nrow(amb)) mean(amb$raca_sih_orig == amb$raca_sim_orig) else NA_real_

rel <- c(
  "RELATORIO DE LINKAGE SIM x SIH v3 — fluxo em 5 etapas — coorte 0 a 6 anos",
  paste0("Gerado em: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  paste0("Periodo: ", min(final$ano, na.rm = TRUE), "-", max(final$ano, na.rm = TRUE),
         " | UFs: ", length(unique(final$uf))),
  paste0("Buffer de data: +/- ", CONFIG$buffer_dias,
         " dias | escore minimo: ", CONFIG$escore_minimo,
         " | janela de retroacao: ", CONFIG$janela_principal, " dias"),
  "",
  "RECONCILIACAO — DUAS TRILHAS, DOIS UNIVERSOS",
  "  O SIM conta OBITOS; o SIH conta EPISODIOS de internacao. Nenhum e",
  "  subconjunto do outro: os dois so se encontram no numero de pareados.",
  "",
  "  TRILHA SIM (unidade: obito)",
  paste0("    Obitos de 0 a 6 anos ................ ", format(N_SIM_TOTAL, big.mark = ".")),
  paste0("    Obitos hospitalares (alvo) .......... ", format(N_SIM_HOSP, big.mark = "."),
         "  (", pct(N_SIM_HOSP, N_SIM_TOTAL), " do SIM)"),
  paste0("    Capturados na AIH (obitos distintos)  ", format(N_OB_PAREADOS, big.mark = "."),
         "  (", pct(N_OB_PAREADOS, N_SIM_HOSP), " do alvo)"),
  paste0("      [episodios do SIH pareados: ", format(nrow(lk), big.mark = "."),
         " — ", format(nrow(lk) - N_OB_PAREADOS, big.mark = "."),
         " obitos com mais de um episodio]"),
  paste0("    Nao capturados ...................... ", format(N_SIM_HOSP - N_OB_PAREADOS, big.mark = "."),
         "  (", pct(N_SIM_HOSP - N_OB_PAREADOS, N_SIM_HOSP), " do alvo)"),
  "",
  "  TRILHA SIH (unidade: episodio de internacao)",
  paste0("    Episodios de 0 a 6 anos ............. ", format(N_EP_TOTAL, big.mark = "."),
         "   [", format(N_AIH_TOTAL, big.mark = "."), " AIHs]"),
  paste0("      alta .............................. ", format(N_EP_ALTA, big.mark = ".")),
  paste0("      transferencia ..................... ", format(N_EP_TRANSF, big.mark = ".")),
  paste0("      obito ............................. ", format(nrow(ob), big.mark = ".")),
  paste0("      outro/nao classificado ............ ", format(N_EP_OUTRO, big.mark = ".")),
  paste0("    Base analitica (obito+transferencia)  ", format(nrow(final), big.mark = "."),
         "   <- e este o numero exibido no painel"),
  paste0("    Obitos pareados ..................... ", format(nrow(lk), big.mark = "."),
         "  (", pct(nrow(lk), nrow(ob)), " dos obitos do SIH)"),
  "",
  "ETAPA 1 — ALVO",
  paste0("  Obitos 0-6 no SIM ................... ", format(N_SIM_TOTAL, big.mark = ".")),
  paste0("  Obitos hospitalares (alvo) .......... ", format(N_SIM_HOSP, big.mark = "."),
         "  (", pct(N_SIM_HOSP, N_SIM_TOTAL), ")"),
  "",
  "  CAPTURA POR LOCAL DE OCORRENCIA (v3.3)",
  "  O obito em LOCOCOR 2 (outro estabelecimento de saude: UPA, pronto",
  "  atendimento, UBS) e atendimento ambulatorial e nao gera AIH — logo nao",
  "  pode ser pareado com o SIH. Mante-lo no denominador rebaixa a cobertura",
  "  sem que exista falha de pareamento. A conferencia desse grupo e no SIA.",
  paste0("    ", sprintf("%-34s %10s %10s %8s", "local de ocorrencia", "alvo",
                         "pareados", "captura")),
  if (nrow(CAPT_LOCOCOR))
    unlist(lapply(seq_len(nrow(CAPT_LOCOCOR)), function(i)
      paste0("    ", sprintf("%-34s %10s %10s %7.1f%%",
                             substr(CAPT_LOCOCOR$local[i], 1, 34),
                             format(CAPT_LOCOCOR$alvo[i], big.mark = "."),
                             format(CAPT_LOCOCOR$pareados[i], big.mark = "."),
                             CAPT_LOCOCOR$pct_capturado[i]))))
  else "    (sem dados)",
  "",
  "  CASCATA DE ESTAGIOS (v3.3) — desfecho da AIH em cada estagio do episodio",
  "  Estagio 1 = primeira AIH do episodio. Estagio 2 so existe para quem teve",
  "  uma segunda AIH. O desfecho e o da propria AIH daquele estagio.",
  local({
    cc <- cascata %>% group_by(estagio, desfecho) %>%
      summarise(n = sum(n), .groups = "drop_last") %>%
      mutate(p = 100 * n / sum(n)) %>% ungroup() %>%
      filter(estagio <= 4) %>% arrange(estagio, desc(n))
    c(paste0("    ", sprintf("%-8s %-30s %12s %8s",
                             "estagio", "desfecho da AIH", "n", "%")),
      if (nrow(cc)) unlist(lapply(seq_len(nrow(cc)), function(i)
        paste0("    ", sprintf("%-8s %-30s %12s %7.1f%%",
                               cc$estagio[i], substr(cc$desfecho[i], 1, 30),
                               format(cc$n[i], big.mark = "."),
                               cc$p[i])))) else "    (sem registros)")
  }),
  "",
  "  SUBPOPULACAO DOS TRANSFERIDOS — o que acontece no estagio seguinte",
  local({
    cc <- cascata %>% filter(desfecho_ant == "Transferência", estagio <= 4) %>%
      group_by(estagio, desfecho) %>%
      summarise(n = sum(n), .groups = "drop_last") %>%
      mutate(p = 100 * n / sum(n)) %>% ungroup() %>%
      filter(estagio <= 4) %>% arrange(estagio, desc(n))
    c(paste0("    ", sprintf("%-8s %-30s %12s %8s",
                             "estagio", "desfecho da AIH", "n", "%")),
      if (nrow(cc)) unlist(lapply(seq_len(nrow(cc)), function(i)
        paste0("    ", sprintf("%-8s %-30s %12s %7.1f%%",
                               cc$estagio[i], substr(cc$desfecho[i], 1, 30),
                               format(cc$n[i], big.mark = "."),
                               cc$p[i])))) else "    (sem registros)")
  }),
  "",
  "ETAPA 2 — LINKAGE OBITO x AIH",
  paste0("  Episodios SIH com desfecho obito .... ", format(nrow(ob), big.mark = ".")),
  paste0("  Pareados ............................ ", format(nrow(lk), big.mark = "."),
         "  (", pct(nrow(lk), nrow(ob)), " dos episodios-obito; ",
         pct(nrow(lk), N_SIM_HOSP), " do alvo)"),
  paste0("    match exato ....................... ", format(n_exato, big.mark = "."),
         "  (", pct(n_exato, nrow(lk)), ")"),
  paste0("    match probabilistico .............. ", format(n_prob, big.mark = "."),
         "  (", pct(n_prob, nrow(lk)), ")"),
  paste0("  Raca/cor preenchida nas duas bases .. ", pct(nrow(amb), nrow(lk)),
         " | concordancia: ", sprintf("%.1f%%", 100 * conc_amb)),
  "",
  "ETAPA 3 — HISTORICO (internacao anterior ao episodio-indice)",
  paste0("  Janela 30 dias ...................... ", format(sum(lk$teve_prev_30), big.mark = "."),
         "  (", pct(sum(lk$teve_prev_30), nrow(lk)), ")"),
  paste0("  Janela 45 dias ...................... ", format(sum(lk$teve_prev_45), big.mark = "."),
         "  (", pct(sum(lk$teve_prev_45), nrow(lk)), ")"),
  paste0("  Janela 60 dias ...................... ", format(sum(lk$teve_prev_60), big.mark = "."),
         "  (", pct(sum(lk$teve_prev_60), nrow(lk)), ")"),
  "",
  "ETAPA 4 — TRANSICOES DE CID (entre os pareados)",
  capture.output(print(as.data.frame(
    lk %>% count(transicao) %>%
      mutate(pct = sprintf("%.1f%%", 100 * n / sum(n))) %>% arrange(desc(n))))),
  "",
  "ETAPA 5 — INTERNACOES SEM OBITO ASSOCIADO (agregado)",
  paste0("  Linhas agregadas .................... ", format(nrow(baixa), big.mark = ".")),
  paste0("  Internacoes representadas ........... ", format(sum(baixa$n), big.mark = ".")),
  "",
  "QUALIDADE DE PREENCHIMENTO (entre os obitos pareados)",
  paste0("  Raca/cor ausente .................... ", sprintf("%.1f%%", 100 * mean(is.na(lk$raca_cor)))),
  paste0("  CID de entrada generico ............. ", sprintf("%.1f%%", 100 * mean(lk$gen_entrada == 1, na.rm = TRUE))),
  paste0("  CID de obito generico ............... ", sprintf("%.1f%%", 100 * mean(lk$gen_obito == 1, na.rm = TRUE))))

writeLines(rel, CONFIG$relatorio)
cat("\n", paste(rel, collapse = "\n"), "\n")
message("\nPronto. Recarregue o Shiny — o app v5 le os tres arquivos novos.")