############################################################
# SIH-RD | INTERNAÇÕES INFANTO-JUVENIS BRASIL (2015-2024)
# POPULAÇÃO: 0 a 6 Anos
# DENOMINADOR: Nascidos Vivos (SINASC - Nível Estadual)
# SCRIPT OTIMIZADO PARA MEMÓRIA
############################################################

# ==============================================================================
# 1) PACOTES
# ==============================================================================

pacotes <- c(
  "data.table", "ggplot2", "readr", "writexl", "tidyr", "geobr", "sf",
  "RColorBrewer", "stringr", "scales", "forcats", "dplyr", "utils"
)

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(data.table)
library(ggplot2)
library(readr)
library(writexl)
library(tidyr)
library(geobr)
library(sf)
library(RColorBrewer)
library(stringr)
library(scales)
library(forcats)
library(dplyr)

options(scipen = 999)

# ==============================================================================
# 2) PARÂMETROS E DIRETÓRIOS
# ==============================================================================

dir_dados    <- "G:/Meu Drive/INSPER/Trabalho/SIH/Dados"
dir_analises <- "G:/Meu Drive/INSPER/Trabalho/SIH/Análises/Subpop_0_6_Anos"

if (!dir.exists(dir_analises)) dir.create(dir_analises, recursive = TRUE)

arquivo_sih <- file.path(dir_dados, "sih_brasil_0_a_6_anos_todas_vars_2015_2024.csv")
arquivo_sinasc <- file.path(dir_dados, "sinasc_cnv_nvuf113232189_120_73_168.csv")

if (!file.exists(arquivo_sih)) stop("Arquivo do SIH não encontrado em: ", arquivo_sih)
if (!file.exists(arquivo_sinasc)) stop("Arquivo do SINASC não encontrado em: ", arquivo_sinasc)

MULT <- 1000
LABEL_TAXA <- "por 1.000 nascidos vivos"

# ==============================================================================
# 3) TEMA E DICIONÁRIOS
# ==============================================================================

tema_executivo <- function(...) {
  theme_classic(base_size = 14) +
    theme(
      plot.title    = element_text(face = "bold", size = 16, color = "#1a252f"),
      plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 15)),
      plot.caption  = element_text(size = 9, color = "#95a5a6", hjust = 0),
      axis.title    = element_text(face = "bold", color = "#2c3e50"),
      axis.text     = element_text(color = "#34495e"),
      axis.line     = element_line(color = "#bdc3c7", linewidth = 0.5),
      panel.grid.major.y = element_line(color = "#ecf0f1", linetype = "dashed"),
      legend.position = "bottom",
      legend.title    = element_blank(),
      plot.background  = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      ...
    )
}

cores_faixa <- c(
  "Neonatal Precoce (0-6 dias)"      = "#c0392b",
  "Neonatal Tardia (7-27 dias)"      = "#d35400",
  "Pós-Neonatal (28 dias a <1 ano)"  = "#f39c12",
  "1 a 6 Anos"                       = "#2980b9"
)

mapa_regioes <- c(
  "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste",
  "4" = "Sul",   "5" = "Centro-Oeste"
)

mapa_estados <- c(
  "11" = "Rondônia",     "12" = "Acre",           "13" = "Amazonas",
  "14" = "Roraima",      "15" = "Pará",           "16" = "Amapá",
  "17" = "Tocantins",    "21" = "Maranhão",       "22" = "Piauí",
  "23" = "Ceará",        "24" = "Rio Grande do Norte", "25" = "Paraíba",
  "26" = "Pernambuco",   "27" = "Alagoas",        "28" = "Sergipe",
  "29" = "Bahia",        "31" = "Minas Gerais",   "32" = "Espírito Santo",
  "33" = "Rio de Janeiro", "35" = "São Paulo",    "41" = "Paraná",
  "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
  "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
  "52" = "Goiás",        "53" = "Distrito Federal"
)

mapa_estados_inverso <- setNames(names(mapa_estados), mapa_estados)

mapa_raca_cor <- c(
  "1" = "Branca",
  "2" = "Preta",
  "3" = "Amarela",
  "4" = "Parda",
  "5" = "Indígena",
  "9" = "Ignorado"
)

mapa_sexo <- c(
  "1" = "Masculino",
  "2" = "Feminino",
  "3" = "Ignorado",
  "0" = "Ignorado"
)

mapa_morte <- c(
  "0" = "Não",
  "1" = "Sim"
)

mapa_complexidade <- c(
  "00" = "Não informado",
  "01" = "Atenção básica",
  "02" = "Média complexidade",
  "03" = "Alta complexidade",
  "0"  = "Não informado",
  "1"  = "Atenção básica",
  "2"  = "Média complexidade",
  "3"  = "Alta complexidade"
)

# ==============================================================================
# 4) FUNÇÕES AUXILIARES OTIMIZADAS
# ==============================================================================

pegar_primeira_coluna_existente <- function(nomes_df, possiveis_nomes) {
  nomes_existentes <- possiveis_nomes[possiveis_nomes %in% nomes_df]
  if (length(nomes_existentes) == 0) return(NA_character_)
  nomes_existentes[1]
}

copiar_coluna_segura <- function(dt, nova_coluna, candidatos) {
  col <- pegar_primeira_coluna_existente(names(dt), candidatos)
  if (is.na(col)) {
    dt[, (nova_coluna) := NA_character_]
  } else {
    dt[, (nova_coluna) := as.character(get(col))]
  }
  invisible(col)
}

modo_seguro <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

n_modo_seguro <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_integer_)
  as.integer(sort(table(x), decreasing = TRUE)[1])
}

salvar_png <- function(plot, nome, width, height, dpi = 300) {
  ggsave(
    filename = file.path(dir_analises, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

# ==============================================================================
# 5) CARREGAMENTO OTIMIZADO DA BASE SIH
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 1: LENDO BASE SIH COM COLUNAS NECESSÁRIAS\n")
cat("══════════════════════════════════════════════════════════\n\n")

header_sih <- names(data.table::fread(arquivo_sih, nrows = 0, showProgress = FALSE))

candidatas_sih <- unique(c(
  "ANO_CMPT", "ANO_INTER", "ANO_OBITO",
  "MUNIC_RES", "CODMUNRES", "MUNIC_MOV", "CODMUNOCOR",
  "CEP", "CEP_RES", "CEPRES", "CEP_PACIENTE", "CEP_RESID",
  "DIAG_PRINC", "DIAG_PRINCIPAL", "CAUSABAS", "CAPITULO_CID", "CID", "CID10",
  "DIAG_SECUN", "DIAG_SECUND", "DIAG_SECUNDARIO", "CID_MORTE",
  "COD_IDADE", "IDADE",
  "PROC_REA", "PROC_REALIZADO", "PROCREA", "PROC_SOLIC", "PROCEDIMENTO",
  "CNES", "CODESTAB", "COD_ESTAB", "CODIGO_CNES", "ESTAB", "ESTABELECI",
  "NOMEESTAB", "NOME_ESTAB", "NOME_HOSPITAL", "HOSPITAL", "ESTABELECIMENTO",
  "RACACOR", "RACA_COR", "RacaCor", "raça_cor", "cor_raca",
  "SEXO", "sexo",
  "MORTE", "OBITO", "óbito", "IND_OBITO",
  "CAR_INT", "CARATER", "CARATER_ATENDIMENTO", "CARATER_INTERNA",
  "COMPLEX", "COMPLEXIDADE",
  "VAL_TOT", "VALOR_TOTAL", "VAL_SH", "VAL_SP"
))

cols_ler <- intersect(candidatas_sih, header_sih)

if (length(cols_ler) == 0) {
  stop("Nenhuma coluna esperada foi encontrada no arquivo SIH. Verifique os nomes das variáveis.")
}

cat("  Colunas lidas do SIH:", length(cols_ler), "de", length(header_sih), "colunas totais.\n\n")

base_analise <- data.table::fread(
  arquivo_sih,
  sep = ",",
  select = cols_ler,
  showProgress = TRUE
)

data.table::setDT(base_analise)

gc()

# ==============================================================================
# 6) PADRONIZAÇÃO DAS VARIÁVEIS ESSENCIAIS SEM CÓPIAS GRANDES
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 2: PADRONIZANDO VARIÁVEIS DO SIH\n")
cat("══════════════════════════════════════════════════════════\n\n")

copiar_coluna_segura(base_analise, "ANO_EVENTO_RAW", c("ANO_CMPT", "ANO_INTER", "ANO_OBITO"))
copiar_coluna_segura(base_analise, "MUN_RES_SAFE", c("MUNIC_RES", "CODMUNRES"))
copiar_coluna_segura(base_analise, "MUN_MOV_SAFE", c("MUNIC_MOV", "CODMUNOCOR"))
copiar_coluna_segura(base_analise, "CID_SAFE", c("DIAG_PRINC", "DIAG_PRINCIPAL", "CAUSABAS", "CAPITULO_CID", "CID", "CID10"))

col_cid_princ <- pegar_primeira_coluna_existente(names(base_analise), c("DIAG_PRINC", "DIAG_PRINCIPAL", "CID_SAFE", "CID", "CID10"))
col_cid_secun <- pegar_primeira_coluna_existente(names(base_analise), c("DIAG_SECUN", "DIAG_SECUND", "DIAG_SECUNDARIO"))
col_cid_morte <- pegar_primeira_coluna_existente(names(base_analise), c("CID_MORTE"))
col_proc      <- pegar_primeira_coluna_existente(names(base_analise), c("PROC_REA", "PROC_REALIZADO", "PROCREA", "PROC_SOLIC", "PROCEDIMENTO"))
col_estab     <- pegar_primeira_coluna_existente(names(base_analise), c("CNES", "CODESTAB", "COD_ESTAB", "CODIGO_CNES", "ESTAB", "ESTABELECI"))
col_nome_estab <- pegar_primeira_coluna_existente(names(base_analise), c("NOMEESTAB", "NOME_ESTAB", "NOME_HOSPITAL", "HOSPITAL", "ESTABELECIMENTO"))
col_raca      <- pegar_primeira_coluna_existente(names(base_analise), c("RACACOR", "RACA_COR", "RacaCor", "raça_cor", "cor_raca"))
col_sexo      <- pegar_primeira_coluna_existente(names(base_analise), c("SEXO", "sexo"))
col_morte     <- pegar_primeira_coluna_existente(names(base_analise), c("MORTE", "OBITO", "óbito", "IND_OBITO"))
col_carater   <- pegar_primeira_coluna_existente(names(base_analise), c("CAR_INT", "CARATER", "CARATER_ATENDIMENTO", "CARATER_INTERNA"))
col_complex   <- pegar_primeira_coluna_existente(names(base_analise), c("COMPLEX", "COMPLEXIDADE"))
col_valor     <- pegar_primeira_coluna_existente(names(base_analise), c("VAL_TOT", "VALOR_TOTAL", "VAL_SH", "VAL_SP"))

base_analise[, ANO_EVENTO := suppressWarnings(as.numeric(ANO_EVENTO_RAW))]
base_analise[, MUN_RES_SAFE := as.character(MUN_RES_SAFE)]
base_analise[, MUN_MOV_SAFE := as.character(MUN_MOV_SAFE)]
base_analise[, CID_SAFE := as.character(CID_SAFE)]

# Idade otimizada
cat("  Classificando faixa etária...\n")

if ("COD_IDADE" %in% names(base_analise)) {
  base_analise[, COD_IDADE_CHR := as.character(COD_IDADE)]
  base_analise[, VAL_IDADE := suppressWarnings(as.numeric(IDADE))]
  
  base_analise[, GRUPO_ETARIO := fcase(
    COD_IDADE_CHR == "2" & VAL_IDADE <= 6,
    "Neonatal Precoce (0-6 dias)",
    COD_IDADE_CHR == "2" & VAL_IDADE >= 7 & VAL_IDADE <= 27,
    "Neonatal Tardia (7-27 dias)",
    COD_IDADE_CHR == "3",
    "Pós-Neonatal (28 dias a <1 ano)",
    COD_IDADE_CHR == "4" & VAL_IDADE >= 1 & VAL_IDADE <= 6,
    "1 a 6 Anos",
    default = "Sem informação precisa"
  )]
  
  base_analise[, COD_IDADE_CHR := NULL]
  
} else {
  base_analise[, IDADE_CHR := as.character(IDADE)]
  base_analise[, UNIDADE_IDADE := substr(IDADE_CHR, 1, 1)]
  base_analise[, VALOR_IDADE := suppressWarnings(as.numeric(substr(IDADE_CHR, 2, 3)))]
  
  base_analise[, GRUPO_ETARIO := fcase(
    UNIDADE_IDADE %in% c("0", "1") | (UNIDADE_IDADE == "2" & VALOR_IDADE <= 6),
    "Neonatal Precoce (0-6 dias)",
    UNIDADE_IDADE == "2" & VALOR_IDADE >= 7 & VALOR_IDADE <= 27,
    "Neonatal Tardia (7-27 dias)",
    (UNIDADE_IDADE == "2" & VALOR_IDADE > 27) | UNIDADE_IDADE == "3",
    "Pós-Neonatal (28 dias a <1 ano)",
    UNIDADE_IDADE == "4" & VALOR_IDADE >= 1 & VALOR_IDADE <= 6,
    "1 a 6 Anos",
    default = "Sem informação precisa"
  )]
  
  base_analise[, IDADE_CHR := NULL]
}

gc()

# Geografia e CID
cat("  Criando variáveis geográficas e capítulos CID...\n")

base_analise[, COD_ESTADO := substr(MUN_RES_SAFE, 1, 2)]
base_analise[, COD_MUN_RES := substr(MUN_RES_SAFE, 1, 6)]
base_analise[, COD_MUN_OCOR := substr(MUN_MOV_SAFE, 1, 6)]
base_analise[, MACRORREGIAO := mapa_regioes[substr(COD_ESTADO, 1, 1)]]
base_analise[, NOME_ESTADO := mapa_estados[COD_ESTADO]]
base_analise[, CAPITULO_CID := substr(CID_SAFE, 1, 1)]

base_analise[, CAPITULO_DESC := fcase(
  CAPITULO_CID %in% c("A", "B"), "Infecciosas e Parasitárias",
  CAPITULO_CID == "J", "Aparelho Respiratório",
  CAPITULO_CID == "P", "Afecções Perinatais",
  CAPITULO_CID == "Q", "Malformações Congênitas",
  CAPITULO_CID %in% c("V", "W", "X", "Y"), "Causas Externas",
  default = "Outras Causas"
)]

base_analise[, CAUSA_PRIORITARIA := fcase(
  CAPITULO_CID %in% c("A", "B", "J"), "Ação Prioritária (Prevenção/Tratamento)",
  CAPITULO_CID == "P", "Atenção à Gestação e Parto",
  CAPITULO_CID == "Q", "Malformações (Alta Complexidade)",
  default = "Outras Causas / Difícil Prevenção"
)]

# Variáveis adicionais para perfil detalhado e óbito hospitalar
base_analise[, CID_PRINCIPAL := if (!is.na(col_cid_princ)) as.character(get(col_cid_princ)) else as.character(CID_SAFE)]
base_analise[, CID_SECUNDARIO := if (!is.na(col_cid_secun)) as.character(get(col_cid_secun)) else NA_character_]
base_analise[, CID_MORTE_SIH := if (!is.na(col_cid_morte)) as.character(get(col_cid_morte)) else NA_character_]
base_analise[, PROCEDIMENTO := if (!is.na(col_proc)) as.character(get(col_proc)) else NA_character_]
base_analise[, COD_ESTABELECIMENTO := if (!is.na(col_estab)) as.character(get(col_estab)) else NA_character_]
base_analise[, NOME_ESTABELECIMENTO := if (!is.na(col_nome_estab)) as.character(get(col_nome_estab)) else NA_character_]
base_analise[, RACA_COR_RAW := if (!is.na(col_raca)) as.character(get(col_raca)) else NA_character_]
base_analise[, SEXO_RAW := if (!is.na(col_sexo)) as.character(get(col_sexo)) else NA_character_]
base_analise[, MORTE_RAW := if (!is.na(col_morte)) as.character(get(col_morte)) else NA_character_]
base_analise[, CARATER_INTERNACAO := if (!is.na(col_carater)) as.character(get(col_carater)) else NA_character_]
base_analise[, COMPLEXIDADE_RAW := if (!is.na(col_complex)) as.character(get(col_complex)) else NA_character_]
base_analise[, VALOR_TOTAL := if (!is.na(col_valor)) suppressWarnings(as.numeric(get(col_valor))) else NA_real_]

# Recodificações com default vetorial feitas por substituição.
# Isso evita erro do fcase(): "Length of 'default' must be 1".

base_analise[, RACA_COR := RACA_COR_RAW]
base_analise[is.na(RACA_COR) | RACA_COR == "", RACA_COR := "Sem informação"]
base_analise[RACA_COR %in% names(mapa_raca_cor), RACA_COR := mapa_raca_cor[RACA_COR]]

base_analise[, SEXO_DESC := SEXO_RAW]
base_analise[is.na(SEXO_DESC) | SEXO_DESC == "", SEXO_DESC := "Sem informação"]
base_analise[SEXO_DESC %in% names(mapa_sexo), SEXO_DESC := mapa_sexo[SEXO_DESC]]

base_analise[, OBITO_HOSPITALAR := MORTE_RAW]
base_analise[is.na(OBITO_HOSPITALAR) | OBITO_HOSPITALAR == "", OBITO_HOSPITALAR := "Sem informação"]
base_analise[OBITO_HOSPITALAR %in% names(mapa_morte), OBITO_HOSPITALAR := mapa_morte[OBITO_HOSPITALAR]]
base_analise[OBITO_HOSPITALAR %in% c("Sim", "SIM", "sim", "S", "s"), OBITO_HOSPITALAR := "Sim"]
base_analise[OBITO_HOSPITALAR %in% c("Não", "NAO", "Nao", "não", "N", "n"), OBITO_HOSPITALAR := "Não"]

base_analise[, COMPLEXIDADE_DESC := COMPLEXIDADE_RAW]
base_analise[is.na(COMPLEXIDADE_DESC) | COMPLEXIDADE_DESC == "", COMPLEXIDADE_DESC := "Sem informação"]
base_analise[COMPLEXIDADE_DESC %in% names(mapa_complexidade), COMPLEXIDADE_DESC := mapa_complexidade[COMPLEXIDADE_DESC]]

base_analise[, ESTAB_LABEL := fifelse(
  !is.na(NOME_ESTABELECIMENTO) & NOME_ESTABELECIMENTO != "",
  NOME_ESTABELECIMENTO,
  fifelse(!is.na(COD_ESTABELECIMENTO) & COD_ESTABELECIMENTO != "", COD_ESTABELECIMENTO, "Sem informação")
)]

gc()

# ==============================================================================
# 7) DENOMINADOR: SINASC POR UF
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 3: LENDO NASCIDOS VIVOS - SINASC POR UF\n")
cat("══════════════════════════════════════════════════════════\n\n")

sinasc_raw <- read_csv2(
  arquivo_sinasc,
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)

sinasc_uf <- sinasc_raw %>%
  rename(UF_NOME = 1) %>%
  mutate(
    NOME_ESTADO = str_trim(UF_NOME),
    cod_estado  = mapa_estados_inverso[NOME_ESTADO]
  ) %>%
  filter(!is.na(cod_estado)) %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "ano",
    values_to = "nascidos_vivos"
  ) %>%
  mutate(
    ano = as.numeric(ano),
    nascidos_vivos = as.numeric(nascidos_vivos)
  ) %>%
  select(ano, cod_estado, NOME_ESTADO, nascidos_vivos) %>%
  filter(!is.na(nascidos_vivos))

sinasc_br <- sinasc_uf %>%
  group_by(ano) %>%
  summarise(nascidos_br = sum(nascidos_vivos), .groups = "drop")

sinasc_macro <- sinasc_uf %>%
  mutate(MACRORREGIAO = mapa_regioes[substr(cod_estado, 1, 1)]) %>%
  group_by(ano, MACRORREGIAO) %>%
  summarise(nascidos_macro = sum(nascidos_vivos), .groups = "drop")

cat("  ✓ SINASC lido e formatado.\n\n")

# ==============================================================================
# 8) CALIBRAGEM DA TAXA
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 4: CALIBRAGEM DA TAXA DE INTERNAÇÃO\n")
cat("══════════════════════════════════════════════════════════\n\n")

n_internacoes_infantis <- base_analise[
  GRUPO_ETARIO %in% c(
    "Neonatal Precoce (0-6 dias)",
    "Neonatal Tardia (7-27 dias)",
    "Pós-Neonatal (28 dias a <1 ano)"
  ),
  .N
]

n_anos_sih <- uniqueN(base_analise[!is.na(ANO_EVENTO), ANO_EVENTO])
n_anos_sinasc <- length(unique(sinasc_br$ano))

tmi_media <- (n_internacoes_infantis / n_anos_sih) /
  (sum(sinasc_br$nascidos_br, na.rm = TRUE) / n_anos_sinasc) * MULT

cat("  Taxa de Internação Infantil (< 1 ano) média no período:\n")
cat("  →", round(tmi_media, 2), LABEL_TAXA, "\n\n")

# ==============================================================================
# 9) TABELAS-BASE PARA FIGURAS
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 5: GERANDO TABELAS-BASE E FIGURAS\n")
cat("══════════════════════════════════════════════════════════\n\n")

evolucao_etaria <- base_analise[
  GRUPO_ETARIO != "Sem informação precisa" & !is.na(ANO_EVENTO),
  .N,
  by = .(ANO_EVENTO, GRUPO_ETARIO)
] %>%
  as.data.frame()

causas_taxa <- base_analise[
  !is.na(ANO_EVENTO),
  .(internacoes = .N),
  by = .(ANO_EVENTO, CAPITULO_DESC)
] %>%
  as.data.frame() %>%
  left_join(sinasc_br, by = c("ANO_EVENTO" = "ano")) %>%
  mutate(taxa = internacoes / nascidos_br * MULT)

causas_prio_faixa <- base_analise[
  GRUPO_ETARIO != "Sem informação precisa",
  .N,
  by = .(GRUPO_ETARIO, CAUSA_PRIORITARIA)
] %>%
  as.data.frame() %>%
  group_by(GRUPO_ETARIO) %>%
  mutate(Perc = N / sum(N)) %>%
  ungroup() %>%
  rename(n = N)

hetero_taxa <- base_analise[
  !is.na(MACRORREGIAO) & GRUPO_ETARIO != "Sem informação precisa" & !is.na(ANO_EVENTO),
  .(internacoes = .N),
  by = .(MACRORREGIAO, GRUPO_ETARIO, ANO_EVENTO)
] %>%
  as.data.frame() %>%
  left_join(sinasc_macro, by = c("ANO_EVENTO" = "ano", "MACRORREGIAO")) %>%
  group_by(MACRORREGIAO, GRUPO_ETARIO) %>%
  summarise(
    internacoes_total = sum(internacoes, na.rm = TRUE),
    nv_total = sum(nascidos_macro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_media = internacoes_total / nv_total * MULT)

# ==============================================================================
# 10) FIGURAS GERAIS
# ==============================================================================

cat("  Fig01: Evolução por faixa etária...\n")

g1 <- ggplot(evolucao_etaria, aes(x = ANO_EVENTO, y = N, color = GRUPO_ETARIO, group = GRUPO_ETARIO)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = cores_faixa) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Evolução das Internações por Faixa Etária (0 a 6 Anos)",
    subtitle = "Brasil, 2015–2024 | Internações absolutas",
    x = "Ano", y = "Número de Internações",
    caption = "Fonte: SIH/DATASUS"
  ) +
  tema_executivo()

salvar_png(g1, "Fig01_Evolucao_Faixa_Etaria_Absoluto.png", 10, 6)

cat("  Fig02: Evolução das causas por taxa...\n")

g2 <- ggplot(causas_taxa, aes(x = ANO_EVENTO, y = taxa, fill = CAPITULO_DESC)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title    = "Evolução das Causas de Internação (0 a 6 Anos)",
    subtitle = paste0("Taxa ", LABEL_TAXA, " | 2015–2024"),
    x = "Ano", y = paste0("Taxa (", LABEL_TAXA, ")"),
    caption = "Fonte: SIH e SINASC/DATASUS"
  ) +
  tema_executivo() +
  theme(legend.position = "right")

salvar_png(g2, "Fig02_Evolucao_Causas_Taxa.png", 10, 6)

cat("  Fig03: Causas prioritárias por faixa etária...\n")

g3 <- ggplot(causas_prio_faixa, aes(x = reorder(CAUSA_PRIORITARIA, Perc), y = Perc)) +
  geom_col(fill = "#2c3e50", width = 0.65) +
  geom_text(aes(label = percent(Perc, accuracy = 0.1)), hjust = -0.1, color = "#2c3e50", fontface = "bold", size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.25))) +
  facet_wrap(~GRUPO_ETARIO, ncol = 2, scales = "free_x") +
  labs(
    title    = "Causas Prioritárias de Internação por Faixa Etária",
    subtitle = "Proporção de internações por grupo de ação | Brasil, 2015–2024",
    x = NULL, y = "Proporção do Total",
    caption = "Fonte: SIH/DATASUS"
  ) +
  tema_executivo() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"),
    strip.background = element_rect(fill = "#ecf0f1", color = NA)
  )

salvar_png(g3, "Fig03_Causas_Prioritarias_por_Faixa.png", 12, 8)

cat("  Fig04: Heterogeneidade regional...\n")

g4 <- ggplot(hetero_taxa, aes(x = reorder(MACRORREGIAO, -taxa_media), y = taxa_media, fill = GRUPO_ETARIO)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = cores_faixa) +
  labs(
    title    = "Heterogeneidade Regional — Taxa de Internação por Faixa",
    subtitle = paste0("Taxa média ", LABEL_TAXA, " | Acumulado 2015–2024"),
    x = "Macrorregião", y = paste0("Taxa (", LABEL_TAXA, ")"),
    caption = "Fonte: SIH e SINASC/DATASUS"
  ) +
  tema_executivo()

salvar_png(g4, "Fig04_Heterogeneidade_Macro_Taxa.png", 10, 6)

# ==============================================================================
# 11) MAPAS DE TAXA
# ==============================================================================

cat("  Fig05: Mapa por Estado...\n")

internacoes_uf <- base_analise[
  !is.na(COD_ESTADO) & !is.na(ANO_EVENTO),
  .(internacoes = .N),
  by = .(cod_estado = COD_ESTADO, ANO_EVENTO)
] %>%
  as.data.frame() %>%
  left_join(sinasc_uf, by = c("ANO_EVENTO" = "ano", "cod_estado")) %>%
  group_by(cod_estado) %>%
  summarise(
    internacoes_total = sum(internacoes, na.rm = TRUE),
    nv_total = sum(nascidos_vivos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    code_state = as.numeric(cod_estado),
    taxa_estado = internacoes_total / nv_total * MULT
  )

malha_br <- read_state(year = 2020, showProgress = FALSE) %>%
  mutate(code_state = as.numeric(code_state))

mapa_uf <- left_join(malha_br, internacoes_uf, by = "code_state")

g5_uf <- ggplot(mapa_uf) +
  geom_sf(aes(fill = taxa_estado), color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlGnBu")[2:9],
    name = "Taxa por\n1.000 N.V.",
    labels = function(x) round(x, 1)
  ) +
  labs(
    title    = "Taxa de Internação por Estado (0 a 6 Anos)",
    subtitle = paste0("Internações ", LABEL_TAXA, " | Acumulado 2015–2024"),
    caption  = "Fonte: SIH e SINASC/DATASUS"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#2c3e50", hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", hjust = 0.5, margin = margin(b = 15)),
    plot.caption  = element_text(size = 9, color = "#95a5a6", hjust = 0),
    legend.position = "right",
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

salvar_png(g5_uf, "Fig05_Mapa_Taxa_Estado.png", 8, 8)

cat("  Fig06: Mapa por Macrorregião...\n")

internacoes_macro <- base_analise[
  !is.na(MACRORREGIAO) & !is.na(ANO_EVENTO),
  .(internacoes = .N),
  by = .(MACRORREGIAO, ANO_EVENTO)
] %>%
  as.data.frame() %>%
  left_join(sinasc_macro, by = c("ANO_EVENTO" = "ano", "MACRORREGIAO")) %>%
  group_by(MACRORREGIAO) %>%
  summarise(
    internacoes_total = sum(internacoes, na.rm = TRUE),
    nv_total = sum(nascidos_macro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_macro = internacoes_total / nv_total * MULT)

macro_lookup <- tibble(
  code_state = as.numeric(names(mapa_estados)),
  MACRORREGIAO = mapa_regioes[substr(names(mapa_estados), 1, 1)]
)

mapa_macro <- malha_br %>%
  left_join(macro_lookup, by = "code_state") %>%
  left_join(internacoes_macro %>% select(MACRORREGIAO, taxa_macro), by = "MACRORREGIAO") %>%
  group_by(MACRORREGIAO) %>%
  summarise(taxa_macro = first(taxa_macro), .groups = "drop")

g6_macro <- ggplot(mapa_macro) +
  geom_sf(aes(fill = taxa_macro), color = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlGnBu")[2:9],
    name = "Taxa por\n1.000 N.V.",
    labels = function(x) round(x, 1)
  ) +
  labs(
    title    = "Taxa de Internação por Macrorregião (0 a 6 Anos)",
    subtitle = paste0("Internações ", LABEL_TAXA, " | Acumulado 2015–2024"),
    caption  = "Fonte: SIH e SINASC/DATASUS"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7f8c8d"),
    plot.caption = element_text(size = 9, color = "#95a5a6", hjust = 0),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

salvar_png(g6_macro, "Fig06_Mapa_Taxa_Macrorregiao.png", 8, 8)

gc()

# ==============================================================================
# 12) FLUXO RESIDÊNCIA → MUNICÍPIO DE INTERNAÇÃO
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 6: ANÁLISE DE FLUXO MUNICIPAL E ESTADUAL\n")
cat("══════════════════════════════════════════════════════════\n\n")

fluxo <- base_analise[
  !is.na(COD_MUN_RES) & !is.na(COD_MUN_OCOR) &
    nchar(COD_MUN_RES) >= 6 & nchar(COD_MUN_OCOR) >= 6,
  .(
    COD_MUN_RES,
    COD_MUN_OCOR,
    INTER_FORA = COD_MUN_RES != COD_MUN_OCOR,
    UF_RES = substr(COD_MUN_RES, 1, 2),
    UF_OCOR = substr(COD_MUN_OCOR, 1, 2)
  )
]

fluxo[, INTER_FORA_UF := UF_RES != UF_OCOR]

cat("  Fig07: Proporção de internações fora do município de residência...\n")

fluxo_uf <- fluxo[, .(
  total = .N,
  fora_mun = sum(INTER_FORA, na.rm = TRUE),
  fora_uf = sum(INTER_FORA_UF, na.rm = TRUE)
), by = UF_RES] %>%
  as.data.frame() %>%
  mutate(
    perc_fora_mun = fora_mun / total,
    perc_fora_uf = fora_uf / total,
    NOME_ESTADO = mapa_estados[UF_RES]
  ) %>%
  filter(!is.na(NOME_ESTADO))

g7 <- ggplot(fluxo_uf, aes(x = reorder(NOME_ESTADO, perc_fora_mun), y = perc_fora_mun)) +
  geom_col(fill = "#8e44ad", width = 0.6) +
  geom_text(aes(label = percent(perc_fora_mun, accuracy = 0.1)), hjust = -0.1, size = 3, color = "#2c3e50") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Proporção de Internações Fora do Município de Residência",
    subtitle = "Crianças de 0 a 6 anos | Por UF de residência | 2015–2024",
    x = NULL, y = "% de internações fora do município",
    caption = "Fonte: SIH/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

salvar_png(g7, "Fig07_Fluxo_Internacoes_Fora_Municipio.png", 11, 8)

cat("  Preparando nomes dos municípios...\n")

mun_nomes <- tryCatch({
  geobr::lookup_muni(code_muni = "all") %>%
    mutate(cod_mun_6 = substr(as.character(code_muni), 1, 6)) %>%
    select(cod_mun_6, name_muni, abbrev_state) %>%
    distinct() %>%
    as.data.table()
}, error = function(e) {
  warning("Não foi possível carregar lookup_muni do geobr. Os códigos municipais serão usados como labels.")
  NULL
})

cat("  Fig08: Top 30 municípios polo de atendimento infantil...\n")

polos <- fluxo[INTER_FORA == TRUE, .(internacoes_recebidas = .N), by = COD_MUN_OCOR][
  order(-internacoes_recebidas)
][1:min(.N, 30)]

if (!is.null(mun_nomes)) {
  polos <- merge(
    polos,
    mun_nomes,
    by.x = "COD_MUN_OCOR",
    by.y = "cod_mun_6",
    all.x = TRUE
  )
  polos[, label := fifelse(!is.na(name_muni), paste0(name_muni, " (", abbrev_state, ")"), COD_MUN_OCOR)]
} else {
  polos[, label := COD_MUN_OCOR]
}

polos_df <- as.data.frame(polos)

g8 <- ggplot(polos_df, aes(x = reorder(label, internacoes_recebidas), y = internacoes_recebidas)) +
  geom_col(fill = "#16a085", width = 0.6) +
  geom_text(aes(label = comma(internacoes_recebidas)), hjust = -0.1, size = 3, color = "#2c3e50", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 30 Municípios Polo de Atendimento Infantil",
    subtitle = "Internações de crianças NÃO residentes ocorridas no município | 2015–2024",
    x = NULL, y = "Internações recebidas",
    caption  = "Fonte: SIH/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

salvar_png(g8, "Fig08_Polos_Saude_Infantil_Top30.png", 11, 9)

cat("  Fig09: Heatmap de fluxo inter-UF...\n")

fluxo_interuf <- fluxo[INTER_FORA_UF == TRUE, .(internacoes = .N), by = .(UF_RES, UF_OCOR)][
  order(-internacoes)
][1:min(.N, 50)]

fluxo_interuf[, NM_RES := mapa_estados[UF_RES]]
fluxo_interuf[, NM_OCOR := mapa_estados[UF_OCOR]]
fluxo_interuf <- fluxo_interuf[!is.na(NM_RES) & !is.na(NM_OCOR)]

fluxo_interuf_df <- as.data.frame(fluxo_interuf)

g9 <- ggplot(fluxo_interuf_df, aes(x = NM_OCOR, y = NM_RES, fill = internacoes)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(colors = brewer.pal(9, "Greens")[2:9], name = "Internações", labels = comma) +
  geom_text(aes(label = comma(internacoes)), size = 2.5, color = "black") +
  labs(
    title    = "Fluxo de Internações Infantis entre UFs",
    subtitle = "Top 50 pares UF residência → UF de internação | 2015–2024",
    x = "UF de ocorrência", y = "UF de residência",
    caption = "Fonte: SIH/DATASUS"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d"),
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y   = element_text(size = 8),
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

salvar_png(g9, "Fig09_Heatmap_Fluxo_InterUF.png", 12, 10)

# ==============================================================================
# 13) TOP 10 MUNICÍPIOS RECEPTORES: ORIGEM, CID, CAUSA, HOSPITAL, PERFIL
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 7: TOP 10 MUNICÍPIOS RECEPTORES - ANÁLISE DETALHADA\n")
cat("══════════════════════════════════════════════════════════\n\n")

# Base resumida apenas com as colunas necessárias para o detalhamento.
# Não usa dplyr::mutate na base completa.

fluxo_detalhado_sih <- base_analise[
  !is.na(COD_MUN_RES) & !is.na(COD_MUN_OCOR) &
    nchar(COD_MUN_RES) >= 6 & nchar(COD_MUN_OCOR) >= 6 &
    COD_MUN_RES != COD_MUN_OCOR,
  .(
    COD_MUN_RES,
    COD_MUN_OCOR,
    UF_RES = substr(COD_MUN_RES, 1, 2),
    UF_OCOR = substr(COD_MUN_OCOR, 1, 2),
    CAPITULO_DESC,
    CAUSA_PRIORITARIA,
    CID_PRINCIPAL,
    CID_SECUNDARIO,
    CID_MORTE_SIH,
    PROCEDIMENTO,
    GRUPO_ETARIO,
    RACA_COR,
    SEXO_DESC,
    OBITO_HOSPITALAR,
    COMPLEXIDADE_DESC,
    CARATER_INTERNACAO,
    COD_ESTABELECIMENTO,
    NOME_ESTABELECIMENTO,
    ESTAB_LABEL,
    VALOR_TOTAL
  )
]

if (!is.null(mun_nomes)) {
  mun_res <- copy(mun_nomes)
  setnames(mun_res, c("cod_mun_6", "name_muni", "abbrev_state"), c("COD_MUN_RES", "MUNICIPIO_RES", "UF_RES_SIGLA"))
  mun_ocor <- copy(mun_nomes)
  setnames(mun_ocor, c("cod_mun_6", "name_muni", "abbrev_state"), c("COD_MUN_OCOR", "MUNICIPIO_OCOR", "UF_OCOR_SIGLA"))
  
  fluxo_detalhado_sih <- merge(fluxo_detalhado_sih, mun_res, by = "COD_MUN_RES", all.x = TRUE)
  fluxo_detalhado_sih <- merge(fluxo_detalhado_sih, mun_ocor, by = "COD_MUN_OCOR", all.x = TRUE)
  
  fluxo_detalhado_sih[, MUNICIPIO_RES_LABEL := fifelse(
    !is.na(MUNICIPIO_RES), paste0(MUNICIPIO_RES, " (", UF_RES_SIGLA, ")"), COD_MUN_RES
  )]
  fluxo_detalhado_sih[, MUNICIPIO_OCOR_LABEL := fifelse(
    !is.na(MUNICIPIO_OCOR), paste0(MUNICIPIO_OCOR, " (", UF_OCOR_SIGLA, ")"), COD_MUN_OCOR
  )]
  
} else {
  fluxo_detalhado_sih[, MUNICIPIO_RES_LABEL := COD_MUN_RES]
  fluxo_detalhado_sih[, MUNICIPIO_OCOR_LABEL := COD_MUN_OCOR]
}

top10_municipios_receptores_sih <- fluxo_detalhado_sih[
  , .(internacoes_recebidas = .N),
  by = .(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL)
][order(-internacoes_recebidas)][1:min(.N, 10)]

fluxo_top10 <- merge(
  fluxo_detalhado_sih,
  top10_municipios_receptores_sih[, .(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL)],
  by = c("COD_MUN_OCOR", "MUNICIPIO_OCOR_LABEL"),
  all = FALSE
)

cat("  Criando tabela resumo dos Top 10 receptores...\n")

tab_top10_receptores_sih_resumo <- fluxo_top10[, .(
  internacoes_recebidas = .N,
  municipios_origem_distintos = uniqueN(COD_MUN_RES),
  ufs_origem_distintas = uniqueN(UF_RES),
  principal_origem = modo_seguro(MUNICIPIO_RES_LABEL),
  internacoes_principal_origem = n_modo_seguro(MUNICIPIO_RES_LABEL),
  principal_causa = modo_seguro(CAPITULO_DESC),
  internacoes_principal_causa = n_modo_seguro(CAPITULO_DESC),
  principal_cid = modo_seguro(CID_PRINCIPAL),
  internacoes_principal_cid = n_modo_seguro(CID_PRINCIPAL),
  principal_cid_morte_entre_obitos = modo_seguro(CID_MORTE_SIH[OBITO_HOSPITALAR == "Sim"]),
  obitos_principal_cid_morte = n_modo_seguro(CID_MORTE_SIH[OBITO_HOSPITALAR == "Sim"]),
  principal_faixa_etaria = modo_seguro(GRUPO_ETARIO),
  internacoes_principal_faixa = n_modo_seguro(GRUPO_ETARIO),
  principal_cor_raca = modo_seguro(RACA_COR),
  internacoes_principal_cor_raca = n_modo_seguro(RACA_COR),
  principal_estabelecimento = modo_seguro(ESTAB_LABEL),
  internacoes_principal_estabelecimento = n_modo_seguro(ESTAB_LABEL),
  obitos_hospitalares = sum(OBITO_HOSPITALAR == "Sim", na.rm = TRUE),
  valor_total_internacoes = sum(VALOR_TOTAL, na.rm = TRUE),
  valor_medio_internacao = mean(VALOR_TOTAL, na.rm = TRUE)
), by = MUNICIPIO_OCOR_LABEL]

tab_top10_receptores_sih_resumo[, `:=`(
  perc_principal_origem = round(100 * internacoes_principal_origem / internacoes_recebidas, 1),
  perc_principal_causa = round(100 * internacoes_principal_causa / internacoes_recebidas, 1),
  perc_principal_cid = round(100 * internacoes_principal_cid / internacoes_recebidas, 1),
  perc_principal_faixa = round(100 * internacoes_principal_faixa / internacoes_recebidas, 1),
  perc_principal_cor_raca = round(100 * internacoes_principal_cor_raca / internacoes_recebidas, 1),
  perc_principal_estabelecimento = round(100 * internacoes_principal_estabelecimento / internacoes_recebidas, 1),
  letalidade_hospitalar_percentual = round(100 * obitos_hospitalares / internacoes_recebidas, 2),
  valor_total_internacoes = round(valor_total_internacoes, 2),
  valor_medio_internacao = round(valor_medio_internacao, 2)
)]

setorder(tab_top10_receptores_sih_resumo, -internacoes_recebidas)
setnames(tab_top10_receptores_sih_resumo, "MUNICIPIO_OCOR_LABEL", "Municipio_receptor")

cat("  Criando tabela detalhada dos Top 10 receptores...\n")

tab_top10_receptores_sih_detalhada <- fluxo_top10[, .(
  Internacoes = .N,
  Valor_total = sum(VALOR_TOTAL, na.rm = TRUE),
  Valor_medio = mean(VALOR_TOTAL, na.rm = TRUE)
), by = .(
  Municipio_receptor = MUNICIPIO_OCOR_LABEL,
  Municipio_residencia = MUNICIPIO_RES_LABEL,
  Capitulo_diagnostico = CAPITULO_DESC,
  Causa_prioritaria = CAUSA_PRIORITARIA,
  CID_principal = CID_PRINCIPAL,
  CID_secundario = CID_SECUNDARIO,
  CID_morte = CID_MORTE_SIH,
  Procedimento = PROCEDIMENTO,
  Faixa_etaria = GRUPO_ETARIO,
  Cor_raca = RACA_COR,
  Sexo = SEXO_DESC,
  Obito_hospitalar = OBITO_HOSPITALAR,
  Complexidade = COMPLEXIDADE_DESC,
  Carater_internacao = CARATER_INTERNACAO,
  Codigo_estabelecimento = COD_ESTABELECIMENTO,
  Nome_estabelecimento = NOME_ESTABELECIMENTO
)]

total_por_receptor <- tab_top10_receptores_sih_detalhada[, .(
  Total_recebido_municipio = sum(Internacoes)
), by = Municipio_receptor]

tab_top10_receptores_sih_detalhada <- merge(
  tab_top10_receptores_sih_detalhada,
  total_por_receptor,
  by = "Municipio_receptor",
  all.x = TRUE
)

tab_top10_receptores_sih_detalhada[, Percentual_no_municipio := round(100 * Internacoes / Total_recebido_municipio, 1)]
tab_top10_receptores_sih_detalhada[, Valor_total := round(Valor_total, 2)]
tab_top10_receptores_sih_detalhada[, Valor_medio := round(Valor_medio, 2)]

setorder(tab_top10_receptores_sih_detalhada, -Total_recebido_municipio, Municipio_receptor, -Internacoes)

tab_top10_receptores_sih_detalhada <- tab_top10_receptores_sih_detalhada[
  , head(.SD, 25),
  by = Municipio_receptor
]

cat("  Fig10: Heatmap dos principais fluxos para os Top 10 municípios receptores...\n")

fluxos_top10_sih_grafico <- fluxo_top10[
  , .(internacoes = .N),
  by = .(MUNICIPIO_RES_LABEL, MUNICIPIO_OCOR_LABEL)
][order(MUNICIPIO_OCOR_LABEL, -internacoes)]

fluxos_top10_sih_grafico <- fluxos_top10_sih_grafico[
  , head(.SD, 8),
  by = MUNICIPIO_OCOR_LABEL
]

fluxos_top10_sih_grafico_df <- as.data.frame(fluxos_top10_sih_grafico) %>%
  mutate(
    MUNICIPIO_OCOR_LABEL = fct_reorder(MUNICIPIO_OCOR_LABEL, internacoes, .fun = sum),
    MUNICIPIO_RES_LABEL = fct_reorder(MUNICIPIO_RES_LABEL, internacoes, .fun = sum)
  )

g10 <- ggplot(
  fluxos_top10_sih_grafico_df,
  aes(x = MUNICIPIO_OCOR_LABEL, y = MUNICIPIO_RES_LABEL, fill = internacoes)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(internacoes)), color = "black", fontface = "bold", size = 3) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Greens")[2:9],
    name = "Internações",
    labels = comma
  ) +
  labs(
    title = "Principais Fluxos para os Top 10 Municípios Receptores",
    subtitle = "Município de residência → município de internação | Crianças de 0 a 6 anos | 2015–2024",
    x = "Município receptor da internação",
    y = "Município de residência",
    caption = "Fonte: SIH/DATASUS"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10.5, color = "#7f8c8d", margin = margin(b = 12)),
    plot.caption = element_text(size = 9, color = "#95a5a6", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, color = "#34495e"),
    axis.text.y = element_text(size = 8, color = "#34495e"),
    axis.title = element_text(face = "bold", color = "#2c3e50"),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

salvar_png(g10, "Fig10_Fluxos_Top10_Municipios_Receptores_SIH.png", 12, 9)

write_xlsx(
  list(
    "Top10_Resumo_Executivo" = as.data.frame(tab_top10_receptores_sih_resumo),
    "Top10_Detalhado" = as.data.frame(tab_top10_receptores_sih_detalhada),
    "Dados_Grafico_Fluxos" = as.data.frame(fluxos_top10_sih_grafico)
  ),
  path = file.path(dir_analises, "Tabelas_Top10_Municipios_Receptores_SIH.xlsx")
)

gc()

# ==============================================================================
# 14) ÓBITOS HOSPITALARES NO SIH
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 8: ÓBITOS HOSPITALARES NO SIH\n")
cat("══════════════════════════════════════════════════════════\n\n")

if (is.na(col_morte)) {
  warning("Nenhuma coluna de óbito hospitalar encontrada. Procure por MORTE, OBITO ou IND_OBITO na base SIH.")
  
  tab_obitos_hospitalares_sih <- data.frame(
    ANO_EVENTO = numeric(),
    internacoes = numeric(),
    obitos_hospitalares = numeric(),
    letalidade_hospitalar_percentual = numeric()
  )
  
} else {
  
  obitos_hospitalares_sih <- base_analise[
    OBITO_HOSPITALAR %in% c("Sim", "Não") & !is.na(ANO_EVENTO),
    .(
      internacoes = .N,
      obitos_hospitalares = sum(OBITO_HOSPITALAR == "Sim", na.rm = TRUE)
    ),
    by = ANO_EVENTO
  ][order(ANO_EVENTO)]
  
  obitos_hospitalares_sih[, letalidade_hospitalar := obitos_hospitalares / internacoes]
  
  max_obitos <- max(obitos_hospitalares_sih$obitos_hospitalares, na.rm = TRUE)
  if (!is.finite(max_obitos) || max_obitos == 0) max_obitos <- 1
  
  obitos_hospitalares_sih_df <- as.data.frame(obitos_hospitalares_sih)
  
  g11 <- ggplot(obitos_hospitalares_sih_df, aes(x = ANO_EVENTO)) +
    geom_col(aes(y = obitos_hospitalares), fill = "#2c3e50", width = 0.65) +
    geom_line(
      aes(y = letalidade_hospitalar * max_obitos),
      color = "#c0392b",
      linewidth = 1.2,
      group = 1
    ) +
    geom_point(
      aes(y = letalidade_hospitalar * max_obitos),
      color = "#c0392b",
      size = 3
    ) +
    geom_text(
      aes(y = obitos_hospitalares, label = comma(obitos_hospitalares)),
      vjust = -0.4,
      size = 3.2,
      fontface = "bold",
      color = "#2c3e50"
    ) +
    geom_text(
      aes(y = letalidade_hospitalar * max_obitos, label = percent(letalidade_hospitalar, accuracy = 0.1)),
      vjust = 1.7,
      size = 3.1,
      fontface = "bold",
      color = "#c0392b"
    ) +
    scale_x_continuous(breaks = 2015:2024) +
    scale_y_continuous(
      labels = comma,
      sec.axis = sec_axis(
        trans = ~ . / max_obitos,
        labels = percent_format(accuracy = 0.1),
        name = "Letalidade hospitalar"
      ),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title = "Óbitos Hospitalares entre Internações de Crianças de 0 a 6 Anos",
      subtitle = "Número absoluto de óbitos hospitalares e letalidade hospitalar | Brasil, 2015–2024",
      x = "Ano",
      y = "Óbitos hospitalares",
      caption = "Fonte: SIH/DATASUS"
    ) +
    tema_executivo() +
    theme(
      axis.title.y.right = element_text(face = "bold", color = "#c0392b"),
      axis.text.y.right = element_text(color = "#c0392b")
    )
  
  salvar_png(g11, "Fig11_Obitos_Hospitalares_SIH.png", 10, 6)
  
  tab_obitos_hospitalares_sih <- obitos_hospitalares_sih[, .(
    ANO_EVENTO,
    internacoes,
    obitos_hospitalares,
    letalidade_hospitalar_percentual = round(100 * letalidade_hospitalar, 2)
  )] %>%
    as.data.frame()
  
  tab_cid_morte_sih <- base_analise[
    OBITO_HOSPITALAR == "Sim" & !is.na(CID_MORTE_SIH) & CID_MORTE_SIH != "",
    .(obitos_hospitalares = .N),
    by = .(CID_MORTE = CID_MORTE_SIH, CAPITULO_CID_MORTE = substr(CID_MORTE_SIH, 1, 1))
  ][order(-obitos_hospitalares)] %>%
    as.data.frame()
}

# ==============================================================================
# 13B) SEGMENTAÇÃO DE CID, POLOS POR ESPECIALIDADE E FLUXO POR CEP (SIH)
#      Espelha as análises do SIM, agora sobre INTERNAÇÕES. Dois pontos próprios
#      do SIH: (i) o CEP de residência da AIH existe de fato — o fluxo origem é
#      real, não fallback; (ii) afecções perinatais são tratadas SEPARADAMENTE
#      dos demais grupos de alta complexidade (no SIH, parte do volume perinatal
#      reflete o local de PARTO, não a busca por tratamento). O conceito de
#      "referência terapêutica" (= alta complexidade SEM perinatal) é o fluxo
#      "limpo" para a leitura de centros de referência e vazios assistenciais.
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 7B: CID SEGMENTADO, ESPECIALIDADE E FLUXO POR CEP\n")
cat("══════════════════════════════════════════════════════════\n\n")

# Parâmetros das novas análises ------------------------------------------------
N_TOP_POLOS    <- 12    # nº de municípios-polo detalhados nas análises por faixa/CEP
N_TOP_POLO_ESP <- 8     # nº de polos exibidos por especialidade (ranking)
N_TOP_ORIGENS  <- 5     # nº de origens por polo no heatmap de fluxo
N_DIGITOS_CEP  <- 5     # nível de agregação do CEP (5=sub-setor; 3=setor; 2=sub-região)
usar_cnes_cep  <- TRUE  # se TRUE, tenta obter o CEP do hospital a partir de uma base CNES
arquivo_cnes   <- file.path(dir_dados, "cnes_estabelecimentos.csv") # ajuste o nome se necessário

# Helpers para ordenar barras dentro de cada facet (sem depender de tidytext) --
reorder_within2 <- function(x, by, within, sep = "___") {
  stats::reorder(paste(x, within, sep = sep), by)
}
scale_x_reordered2 <- function(...) {
  ggplot2::scale_x_discrete(labels = function(x) gsub("___.+$", "", x), ...)
}

# Agrega o CEP a uma "área CEP" (prefixo), corrigindo o zero à esquerda --------
# (CEPs de São Paulo começam com 0 e o fread costuma ler como inteiro.)
area_cep <- function(cep, n = N_DIGITOS_CEP) {
  cep <- gsub("[^0-9]", "", as.character(cep))
  cep <- ifelse(nchar(cep) > 8, substr(cep, 1, 8), cep)
  cep <- ifelse(!is.na(cep) & cep != "", str_pad(cep, 8, side = "left", pad = "0"), NA_character_)
  ifelse(!is.na(cep) & nchar(cep) >= n, substr(cep, 1, n), NA_character_)
}

# Classificação dos grupos diagnósticos marcadores de polo (CID-10 básico) -----
classificar_grupo_cid_polo <- function(cid) {
  cid   <- toupper(trimws(as.character(cid)))
  letra <- substr(cid, 1, 1)
  num   <- suppressWarnings(as.numeric(substr(cid, 2, 3)))
  fcase(
    letra == "C" | (letra == "D" & !is.na(num) & num <= 48), "Neoplasias (Oncologia)",
    letra == "Q" & !is.na(num) & num >= 20 & num <= 28,      "Cardiopatias Congênitas",
    letra == "Q",                                            "Malformações Congênitas (outras)",
    letra == "P",                                            "Afecções Perinatais",
    letra == "G",                                            "Doenças do Sistema Nervoso",
    letra == "E" & !is.na(num) & num >= 70 & num <= 90,      "Doenças Metabólicas/Genéticas",
    letra %in% c("A", "B"),                                  "Doenças Infecciosas",
    letra == "J",                                            "Aparelho Respiratório",
    letra %in% c("V", "W", "X", "Y"),                        "Causas Externas",
    default = "Outras Causas"
  )
}

grupos_alta_complexidade <- c(
  "Neoplasias (Oncologia)", "Cardiopatias Congênitas",
  "Malformações Congênitas (outras)", "Afecções Perinatais",
  "Doenças do Sistema Nervoso", "Doenças Metabólicas/Genéticas"
)
grupos_referencia_terapeutica <- setdiff(grupos_alta_complexidade, "Afecções Perinatais")

# (a) Resolver coluna de CEP de residência -------------------------------------
col_cep_sih        <- pegar_primeira_coluna_existente(
  names(base_analise),
  c("CEP", "CEP_RES", "CEPRES", "CEP_PACIENTE", "CEP_RESID")
)
tem_cep_residencia <- !is.na(col_cep_sih)

# (b) Top N municípios-polo (receptores de internações de fora) ----------------
top_polos_sih <- fluxo_detalhado_sih[
  , .(internacoes_recebidas = .N),
  by = .(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL)
][order(-internacoes_recebidas)][1:min(.N, N_TOP_POLOS)]

# (c) Base de fluxo restrita aos polos, carregando o CEP da AIH ----------------
cols_keep <- c("COD_MUN_RES", "COD_MUN_OCOR", "CID_PRINCIPAL", "GRUPO_ETARIO",
               "COD_ESTABELECIMENTO", "OBITO_HOSPITALAR", "VALOR_TOTAL")
if (tem_cep_residencia) cols_keep <- c(cols_keep, col_cep_sih)

fluxo_polos_sih <- base_analise[
  !is.na(COD_MUN_RES) & !is.na(COD_MUN_OCOR) &
    nchar(COD_MUN_RES) >= 6 & nchar(COD_MUN_OCOR) >= 6 &
    COD_MUN_RES != COD_MUN_OCOR &
    COD_MUN_OCOR %in% top_polos_sih$COD_MUN_OCOR,
  ..cols_keep
]

if (tem_cep_residencia) {
  setnames(fluxo_polos_sih, col_cep_sih, "CEP_PACIENTE")
} else {
  fluxo_polos_sih[, CEP_PACIENTE := NA_character_]
}

# Rótulos de município (ocorrência via top_polos; residência via mun_nomes) ----
fluxo_polos_sih <- merge(
  fluxo_polos_sih,
  top_polos_sih[, .(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL)],
  by = "COD_MUN_OCOR", all.x = TRUE
)

if (exists("mun_nomes") && !is.null(mun_nomes)) {
  mr <- as.data.table(mun_nomes)[, .(COD_MUN_RES = cod_mun_6,
                                     MUNICIPIO_RES = name_muni,
                                     UF_RES_SIGLA = abbrev_state)]
  fluxo_polos_sih <- merge(fluxo_polos_sih, mr, by = "COD_MUN_RES", all.x = TRUE)
  fluxo_polos_sih[, MUNICIPIO_RES_LABEL := fifelse(
    !is.na(MUNICIPIO_RES), paste0(MUNICIPIO_RES, " (", UF_RES_SIGLA, ")"), COD_MUN_RES
  )]
} else {
  fluxo_polos_sih[, MUNICIPIO_RES_LABEL := COD_MUN_RES]
}

# Segmentação de CID + tipo de fluxo -------------------------------------------
fluxo_polos_sih[, GRUPO_CID_POLO := classificar_grupo_cid_polo(CID_PRINCIPAL)]
fluxo_polos_sih[, MARCADOR_REFERENCIA := GRUPO_CID_POLO %in% grupos_referencia_terapeutica]
fluxo_polos_sih[, TIPO_FLUXO := fcase(
  GRUPO_CID_POLO == "Afecções Perinatais",           "Perinatal (nascimento/parto)",
  GRUPO_CID_POLO %in% grupos_referencia_terapeutica, "Referência terapêutica",
  default = "Demais causas"
)]
fluxo_polos_sih[, AREA_CEP_ORIGEM := if (tem_cep_residencia) area_cep(CEP_PACIENTE) else NA_character_]
fluxo_polos_sih[, ORIGEM := if (tem_cep_residencia)
  fifelse(!is.na(AREA_CEP_ORIGEM), AREA_CEP_ORIGEM, MUNICIPIO_RES_LABEL) else MUNICIPIO_RES_LABEL]

# (d) CEP do estabelecimento (destino) via CNES, opcional ----------------------
cnes_cep <- NULL
if (isTRUE(usar_cnes_cep) && file.exists(arquivo_cnes)) {
  cnes_cep <- tryCatch({
    as.data.table(read_csv2(arquivo_cnes, locale = locale(encoding = "ISO-8859-1"),
                            show_col_types = FALSE))
  }, error = function(e) NULL)
  if (!is.null(cnes_cep)) {
    setnames(cnes_cep, toupper(names(cnes_cep)))
    col_cnes_id  <- pegar_primeira_coluna_existente(names(cnes_cep), c("CNES", "CODESTAB", "CO_CNES", "COD_CNES"))
    col_cnes_cep <- pegar_primeira_coluna_existente(names(cnes_cep), c("CEP", "CO_CEP", "NU_CEP", "CEP_ESTAB"))
    if (!is.na(col_cnes_id) && !is.na(col_cnes_cep)) {
      cnes_cep <- cnes_cep[, .(
        COD_ESTABELECIMENTO = str_pad(gsub("[^0-9]", "", as.character(get(col_cnes_id))), 7, pad = "0"),
        CEP_HOSPITAL = as.character(get(col_cnes_cep))
      )]
      cnes_cep <- unique(cnes_cep, by = "COD_ESTABELECIMENTO")
      fluxo_polos_sih[, COD_ESTABELECIMENTO := str_pad(gsub("[^0-9]", "", COD_ESTABELECIMENTO), 7, pad = "0")]
      fluxo_polos_sih <- merge(fluxo_polos_sih, cnes_cep, by = "COD_ESTABELECIMENTO", all.x = TRUE)
      fluxo_polos_sih[, AREA_CEP_DESTINO := area_cep(CEP_HOSPITAL)]
    } else {
      cnes_cep <- NULL
    }
  }
}
tem_cep_hospital <- !is.null(cnes_cep)

if (!tem_cep_residencia) {
  cat("  ⚠ Coluna de CEP de residência não encontrada na base SIH lida.\n")
  cat("    → Verifique se 'CEP' está em candidatas_sih; usando município como origem.\n\n")
} else {
  cat("  ✓ CEP de residência (AIH) carregado — fluxo de origem em nível de área CEP.\n\n")
}
if (!tem_cep_hospital) {
  cat("  ⚠ Base CNES de CEP do hospital não carregada (", arquivo_cnes, ").\n", sep = "")
  cat("    → Destino no nível município-polo.\n\n")
}

# ── Fig12: Heatmap grupo de CID × faixa etária nos polos ──────────────────────
cat("  Fig12: Heatmap de grupos de CID por faixa etária (polos)...\n")

heat_cid_faixa_sih <- fluxo_polos_sih[
  GRUPO_ETARIO != "Sem informação precisa",
  .(internacoes = .N), by = .(GRUPO_CID_POLO, GRUPO_ETARIO)
] %>%
  as.data.frame() %>%
  mutate(
    GRUPO_ETARIO   = factor(GRUPO_ETARIO, levels = names(cores_faixa)),
    GRUPO_CID_POLO = fct_reorder(GRUPO_CID_POLO, internacoes, .fun = sum)
  )

g_cidf <- ggplot(heat_cid_faixa_sih, aes(x = GRUPO_ETARIO, y = GRUPO_CID_POLO, fill = internacoes)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(internacoes)), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn")[2:9], name = "Internações", labels = comma) +
  labs(
    title    = "Segmentação Diagnóstica nos Municípios-Polo, por Faixa Etária",
    subtitle = paste0("Internações de crianças NÃO residentes recebidas nos ", N_TOP_POLOS,
                      " maiores polos | 0 a 6 anos | 2015–2024"),
    x = "Faixa etária", y = "Grupo diagnóstico (CID-10 básico)",
    caption = "Fonte: SIH/DATASUS | Grupos de alta complexidade = marcadores de centros de referência"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle    = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "#95a5a6", hjust = 0),
    axis.text.x      = element_text(angle = 20, hjust = 1, size = 9, color = "#34495e"),
    axis.text.y      = element_text(size = 9, color = "#34495e"),
    axis.title       = element_text(face = "bold", color = "#2c3e50"),
    panel.grid       = element_blank(),
    legend.position  = "right",
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

salvar_png(g_cidf, "Fig12_CID_Segmentado_por_Faixa_Polos_SIH.png", 11, 7)

tab_polo_cid_faixa_sih <- fluxo_polos_sih[
  GRUPO_ETARIO != "Sem informação precisa",
  .(internacoes = .N), by = .(MUNICIPIO_OCOR_LABEL, GRUPO_CID_POLO, TIPO_FLUXO, GRUPO_ETARIO)
]
tab_polo_cid_faixa_sih[, total_polo := sum(internacoes), by = MUNICIPIO_OCOR_LABEL]
tab_polo_cid_faixa_sih[, perc_no_polo := round(100 * internacoes / total_polo, 1)]
setorder(tab_polo_cid_faixa_sih, -total_polo, MUNICIPIO_OCOR_LABEL, -internacoes)
setnames(tab_polo_cid_faixa_sih,
         c("MUNICIPIO_OCOR_LABEL", "GRUPO_CID_POLO", "TIPO_FLUXO", "GRUPO_ETARIO",
           "internacoes", "total_polo", "perc_no_polo"),
         c("Municipio_polo", "Grupo_CID", "Tipo_fluxo", "Faixa_etaria",
           "Internacoes", "Total_polo", "Percentual_no_polo"))
tab_polo_cid_faixa_sih <- as.data.frame(tab_polo_cid_faixa_sih)

# ── Fig13: Ranking de polos por especialidade (referência terapêutica) ────────
cat("  Fig13: Ranking de polos por especialidade...\n")

esp_dt <- fluxo_detalhado_sih[, .(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL, CID_PRINCIPAL)]
esp_dt[, GRUPO_CID_POLO := classificar_grupo_cid_polo(CID_PRINCIPAL)]
esp_dt <- esp_dt[GRUPO_CID_POLO %in% grupos_referencia_terapeutica]

tab_polo_por_especialidade_sih <- esp_dt[
  , .(internacoes_recebidas = .N),
  by = .(GRUPO_CID_POLO, COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL)
]
tab_polo_por_especialidade_sih[, total_especialidade := sum(internacoes_recebidas), by = GRUPO_CID_POLO]
tab_polo_por_especialidade_sih[, perc_na_especialidade := round(100 * internacoes_recebidas / total_especialidade, 1)]
setorder(tab_polo_por_especialidade_sih, GRUPO_CID_POLO, -internacoes_recebidas)
tab_polo_por_especialidade_sih[, rank_polo := rowid(GRUPO_CID_POLO)]

fig_polo_esp_sih <- tab_polo_por_especialidade_sih[, head(.SD, N_TOP_POLO_ESP), by = GRUPO_CID_POLO] %>%
  as.data.frame()

g_esp <- ggplot(
  fig_polo_esp_sih,
  aes(x = reorder_within2(MUNICIPIO_OCOR_LABEL, internacoes_recebidas, GRUPO_CID_POLO),
      y = internacoes_recebidas, fill = GRUPO_CID_POLO)
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = comma(internacoes_recebidas)), hjust = -0.1, size = 2.8, color = "#2c3e50") +
  coord_flip() +
  scale_x_reordered2() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  facet_wrap(~GRUPO_CID_POLO, scales = "free", ncol = 2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "Polos por Especialidade — Fluxo de Referência Terapêutica",
    subtitle = paste0("Maiores receptores de internações de NÃO residentes, por grupo de alta complexidade\n",
                      "(exclui afecções perinatais = fluxo de parto) | 0 a 6 anos | 2015–2024"),
    x = NULL, y = "Internações recebidas de não residentes",
    caption = "Fonte: SIH/DATASUS | Identifica a vocação de cada polo (oncologia, cardiopatia, neuro, etc.)"
  ) +
  tema_executivo() +
  theme(
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.text       = element_text(face = "bold", size = 11, color = "#2c3e50"),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    legend.position  = "none"
  )

salvar_png(g_esp, "Fig13_Polos_por_Especialidade_SIH.png", 12, 11)

setnames(tab_polo_por_especialidade_sih,
         c("GRUPO_CID_POLO", "MUNICIPIO_OCOR_LABEL", "internacoes_recebidas",
           "total_especialidade", "perc_na_especialidade", "rank_polo"),
         c("Especialidade", "Municipio_polo", "Internacoes_recebidas",
           "Total_especialidade", "Percentual_na_especialidade", "Rank_polo"))
tab_polo_por_especialidade_sih <- as.data.frame(
  tab_polo_por_especialidade_sih[, .(Especialidade, Rank_polo, Municipio_polo, COD_MUN_OCOR,
                                     Internacoes_recebidas, Total_especialidade, Percentual_na_especialidade)]
)

# ── Fig14: Fluxo origem (CEP) → polo no fluxo de referência terapêutica ───────
cat("  Fig14: Fluxo origem (CEP/município) → polo, referência terapêutica...\n")

fluxo_cep_grafico_sih <- fluxo_polos_sih[
  MARCADOR_REFERENCIA == TRUE & !is.na(ORIGEM),
  .(internacoes = .N), by = .(ORIGEM, MUNICIPIO_OCOR_LABEL)
][order(MUNICIPIO_OCOR_LABEL, -internacoes)]
fluxo_cep_grafico_sih <- fluxo_cep_grafico_sih[, head(.SD, N_TOP_ORIGENS), by = MUNICIPIO_OCOR_LABEL] %>%
  as.data.frame() %>%
  mutate(
    MUNICIPIO_OCOR_LABEL = fct_reorder(MUNICIPIO_OCOR_LABEL, internacoes, .fun = sum),
    ORIGEM               = fct_reorder(ORIGEM, internacoes, .fun = sum)
  )

rotulo_origem  <- if (tem_cep_residencia) paste0("Área CEP de residência (", N_DIGITOS_CEP, " díg.)") else "Município de residência"
rotulo_destino <- if (tem_cep_hospital) "Polo receptor (CEP/hospital)" else "Município-polo receptor"
fonte_cep      <- if (tem_cep_residencia) "Fonte: SIH/DATASUS (CEP da AIH)" else "Fonte: SIH/DATASUS (nível município — CEP indisponível)"

g_cep <- ggplot(fluxo_cep_grafico_sih, aes(x = MUNICIPIO_OCOR_LABEL, y = ORIGEM, fill = internacoes)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(internacoes)), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu")[2:9], name = "Internações",
                       trans = "sqrt", labels = comma) +
  labs(
    title    = "Fluxo Origem → Polo no Fluxo de Referência Terapêutica",
    subtitle = paste0(rotulo_origem, " → ", rotulo_destino,
                      " | Top ", N_TOP_ORIGENS, " origens por polo | exclui perinatal | 0 a 6 anos | 2015–2024"),
    x = rotulo_destino, y = rotulo_origem,
    caption = paste0(fonte_cep, " | Alta complexidade (sem perinatal) = marcadores de polo")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle    = element_text(size = 9.5, color = "#7f8c8d", margin = margin(b = 12)),
    plot.caption     = element_text(size = 9, color = "#95a5a6", hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e"),
    axis.text.y      = element_text(size = 9, color = "#34495e"),
    axis.title       = element_text(face = "bold", color = "#2c3e50"),
    panel.grid       = element_blank(),
    legend.position  = "right",
    plot.background  = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

salvar_png(g_cep, "Fig14_Fluxo_CEP_Origem_Destino_Polos_SIH.png", 11, 9)

# Tabela: fluxo origem → polo por grupo de CID (mantém perinatal via Tipo_fluxo)
tab_fluxo_cep_polos_sih <- fluxo_polos_sih[
  GRUPO_CID_POLO %in% grupos_alta_complexidade & !is.na(ORIGEM),
  .(internacoes = .N), by = .(MUNICIPIO_OCOR_LABEL, ORIGEM, GRUPO_CID_POLO, TIPO_FLUXO, GRUPO_ETARIO)
]
tab_fluxo_cep_polos_sih[, perc_no_polo := round(100 * internacoes / sum(internacoes), 1), by = MUNICIPIO_OCOR_LABEL]
setorder(tab_fluxo_cep_polos_sih, MUNICIPIO_OCOR_LABEL, -internacoes)
setnames(tab_fluxo_cep_polos_sih,
         c("MUNICIPIO_OCOR_LABEL", "ORIGEM", "GRUPO_CID_POLO", "TIPO_FLUXO", "GRUPO_ETARIO",
           "internacoes", "perc_no_polo"),
         c("Municipio_polo", "Origem", "Grupo_CID", "Tipo_fluxo", "Faixa_etaria",
           "Internacoes", "Percentual_no_polo"))
tab_fluxo_cep_polos_sih <- as.data.frame(tab_fluxo_cep_polos_sih)

# (e) Distância de deslocamento (centroides municipais via geobr) --------------
dist_polos_sih <- tryCatch({
  centroides <- read_municipality(year = 2020, showProgress = FALSE) %>%
    sf::st_make_valid()
  centroides$cod_mun_6 <- substr(as.character(centroides$code_muni), 1, 6)
  cent_pt <- suppressWarnings(sf::st_centroid(centroides))
  coords  <- sf::st_coordinates(cent_pt)
  cent_dt <- data.table(cod_mun_6 = cent_pt$cod_mun_6, lon = coords[, 1], lat = coords[, 2])
  
  dd <- fluxo_polos_sih[, .(COD_MUN_RES, COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL)]
  dd <- merge(dd, cent_dt, by.x = "COD_MUN_RES",  by.y = "cod_mun_6", all.x = TRUE)
  setnames(dd, c("lon", "lat"), c("lon_res", "lat_res"))
  dd <- merge(dd, cent_dt, by.x = "COD_MUN_OCOR", by.y = "cod_mun_6", all.x = TRUE)
  setnames(dd, c("lon", "lat"), c("lon_ocor", "lat_ocor"))
  dd <- dd[!is.na(lon_res) & !is.na(lon_ocor)]
  dd[, dist_km := 6371 * 2 * asin(pmin(1, sqrt(
    sin((lat_ocor - lat_res) * pi / 180 / 2)^2 +
      cos(lat_res * pi / 180) * cos(lat_ocor * pi / 180) *
      sin((lon_ocor - lon_res) * pi / 180 / 2)^2
  )))]
  dd
}, error = function(e) { cat("  ⚠ Distâncias não calculadas:", conditionMessage(e), "\n"); NULL })

# Tabela: resumo por polo (indicador discriminante de referência terapêutica) --
tab_polos_resumo_cep_sih <- fluxo_polos_sih[, .(
  internacoes_recebidas         = .N,
  perc_perinatal                = round(100 * mean(GRUPO_CID_POLO == "Afecções Perinatais"), 1),
  perc_referencia_terapeutica   = round(100 * mean(MARCADOR_REFERENCIA), 1),
  grupo_referencia_predominante = modo_seguro(GRUPO_CID_POLO[MARCADOR_REFERENCIA == TRUE]),
  faixa_predominante            = modo_seguro(GRUPO_ETARIO[GRUPO_ETARIO != "Sem informação precisa"]),
  municipios_origem_distintos   = uniqueN(COD_MUN_RES),
  obitos_hospitalares           = sum(OBITO_HOSPITALAR == "Sim", na.rm = TRUE),
  valor_total                   = round(sum(VALOR_TOTAL, na.rm = TRUE), 2)
), by = MUNICIPIO_OCOR_LABEL][order(-internacoes_recebidas)]

if (tem_cep_residencia) {
  acp <- fluxo_polos_sih[!is.na(AREA_CEP_ORIGEM),
                         .(areas_cep_origem_distintas = uniqueN(AREA_CEP_ORIGEM)),
                         by = MUNICIPIO_OCOR_LABEL]
  tab_polos_resumo_cep_sih <- merge(tab_polos_resumo_cep_sih, acp, by = "MUNICIPIO_OCOR_LABEL", all.x = TRUE)
}

if (!is.null(dist_polos_sih)) {
  dist_resumo <- dist_polos_sih[, .(
    dist_mediana_km = round(as.numeric(median(dist_km, na.rm = TRUE)), 1),
    dist_p90_km     = round(as.numeric(quantile(dist_km, 0.9, na.rm = TRUE)), 1)
  ), by = MUNICIPIO_OCOR_LABEL]
  tab_polos_resumo_cep_sih <- merge(tab_polos_resumo_cep_sih, dist_resumo, by = "MUNICIPIO_OCOR_LABEL", all.x = TRUE)
}

setorder(tab_polos_resumo_cep_sih, -internacoes_recebidas)
setnames(tab_polos_resumo_cep_sih, "MUNICIPIO_OCOR_LABEL", "Municipio_polo")
tab_polos_resumo_cep_sih <- as.data.frame(tab_polos_resumo_cep_sih)

# Workbook dedicado das novas análises -----------------------------------------
write_xlsx(
  list(
    "Polo_CID_Faixa"         = tab_polo_cid_faixa_sih,
    "Polo_por_Especialidade" = tab_polo_por_especialidade_sih,
    "Fluxo_CEP_Polos"        = tab_fluxo_cep_polos_sih,
    "Polos_Resumo_CEP"       = tab_polos_resumo_cep_sih
  ),
  path = file.path(dir_analises, "Tabelas_CID_Segmentado_e_Fluxo_CEP_SIH.xlsx")
)

cat("  ✓ Novas análises (CID segmentado + especialidade + fluxo CEP + resumo) geradas!\n\n")

gc()

# ==============================================================================
# 15) TABELAS EXECUTIVAS FINAIS
# ==============================================================================

cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 9: TABELAS EXECUTIVAS FINAIS\n")
cat("══════════════════════════════════════════════════════════\n\n")

tab_evolucao <- evolucao_etaria %>%
  pivot_wider(names_from = GRUPO_ETARIO, values_from = N, values_fill = 0) %>%
  arrange(ANO_EVENTO)

tab_causas <- base_analise[
  !is.na(ANO_EVENTO),
  .N,
  by = .(ANO_EVENTO, CAPITULO_DESC)
] %>%
  as.data.frame() %>%
  rename(n = N) %>%
  pivot_wider(names_from = CAPITULO_DESC, values_from = n, values_fill = 0) %>%
  arrange(ANO_EVENTO)

tab_prio_faixa <- causas_prio_faixa %>%
  select(GRUPO_ETARIO, CAUSA_PRIORITARIA, n, Perc) %>%
  arrange(GRUPO_ETARIO, desc(Perc))

tab_fluxo <- fluxo_uf %>%
  select(NOME_ESTADO, total, fora_mun, perc_fora_mun, fora_uf, perc_fora_uf) %>%
  arrange(desc(perc_fora_mun))

tab_polos <- polos_df %>%
  select(any_of(c("label", "COD_MUN_OCOR", "internacoes_recebidas"))) %>%
  arrange(desc(internacoes_recebidas))

tab_fluxo_interuf <- fluxo_interuf_df %>%
  select(NM_RES, NM_OCOR, internacoes) %>%
  arrange(desc(internacoes))

tab_taxa_uf <- internacoes_uf %>%
  mutate(NOME_ESTADO = mapa_estados[cod_estado]) %>%
  select(NOME_ESTADO, internacoes_total, nascidos_vivos = nv_total, taxa_estado) %>%
  arrange(desc(taxa_estado))

tab_taxa_macro <- internacoes_macro %>%
  select(MACRORREGIAO, internacoes_total, nascidos_vivos = nv_total, taxa_macro) %>%
  arrange(desc(taxa_macro))

abas_excel <- list(
  "1_Evolucao_Idade"       = as.data.frame(tab_evolucao),
  "2_Evolucao_Causas"      = as.data.frame(tab_causas),
  "3_Prio_por_Faixa"       = as.data.frame(tab_prio_faixa),
  "4_Fluxo_por_UF"         = as.data.frame(tab_fluxo),
  "5_Polos_Saude_Infantil" = as.data.frame(tab_polos),
  "6_Fluxo_InterUF"        = as.data.frame(tab_fluxo_interuf),
  "7_Taxa_por_UF"          = as.data.frame(tab_taxa_uf),
  "8_Taxa_por_Macro"       = as.data.frame(tab_taxa_macro),
  "9_Top10_Receptores"     = as.data.frame(tab_top10_receptores_sih_resumo),
  "10_Top10_Detalhado"     = as.data.frame(tab_top10_receptores_sih_detalhada),
  "11_Obitos_Hospitalares" = as.data.frame(tab_obitos_hospitalares_sih),
  "12_CID_Morte_Hospitalar" = if (exists("tab_cid_morte_sih")) as.data.frame(tab_cid_morte_sih) else data.frame(),
  "13_Polo_CID_Faixa"       = tab_polo_cid_faixa_sih,
  "14_Polo_Especialidade"   = tab_polo_por_especialidade_sih,
  "15_Fluxo_CEP_Polos"      = tab_fluxo_cep_polos_sih,
  "16_Polos_Resumo_CEP"     = tab_polos_resumo_cep_sih
)

write_xlsx(
  abas_excel,
  path = file.path(dir_analises, "Tabelas_Executivas_Internacoes_0_6_Anos.xlsx")
)

cat("══════════════════════════════════════════════════════════\n")
cat("  PROCESSAMENTO CONCLUÍDO COM SUCESSO!\n")
cat("══════════════════════════════════════════════════════════\n\n")

cat("Arquivos principais gerados em:\n")
cat("  ", dir_analises, "\n\n")
cat("Figuras geradas:\n")
cat("  Fig01_Evolucao_Faixa_Etaria_Absoluto.png\n")
cat("  Fig02_Evolucao_Causas_Taxa.png\n")
cat("  Fig03_Causas_Prioritarias_por_Faixa.png\n")
cat("  Fig04_Heterogeneidade_Macro_Taxa.png\n")
cat("  Fig05_Mapa_Taxa_Estado.png\n")
cat("  Fig06_Mapa_Taxa_Macrorregiao.png\n")
cat("  Fig07_Fluxo_Internacoes_Fora_Municipio.png\n")
cat("  Fig08_Polos_Saude_Infantil_Top30.png\n")
cat("  Fig09_Heatmap_Fluxo_InterUF.png\n")
cat("  Fig10_Fluxos_Top10_Municipios_Receptores_SIH.png\n")
cat("  Fig11_Obitos_Hospitalares_SIH.png\n")
cat("  Fig12_CID_Segmentado_por_Faixa_Polos_SIH.png\n")
cat("  Fig13_Polos_por_Especialidade_SIH.png\n")
cat("  Fig14_Fluxo_CEP_Origem_Destino_Polos_SIH.png\n\n")
cat("Planilhas geradas:\n")
cat("  Tabelas_Executivas_Internacoes_0_6_Anos.xlsx\n")
cat("  Tabelas_Top10_Municipios_Receptores_SIH.xlsx\n")
cat("  Tabelas_CID_Segmentado_e_Fluxo_CEP_SIH.xlsx\n")