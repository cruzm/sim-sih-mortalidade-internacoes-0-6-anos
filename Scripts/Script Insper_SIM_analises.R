############################################################
# SIM-DO | MORTALIDADE INFANTO-JUVENIL BRASIL (2015-2024)
# DENOMINADOR: Nascidos Vivos (SINASC - Nível Estadual)
############################################################

# 1) Pacotes -------------------------------------------------------------------
pacotes <- c("dplyr", "ggplot2", "readr", "writexl", "tidyr", "geobr", "sf",
             "RColorBrewer", "stringr", "scales", "forcats", "utils")
for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(dplyr)
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

# 2) Parâmetros e Diretórios ---------------------------------------------------
dir_dados    <- "G:/Meu Drive/INSPER/Trabalho/SIM/Dados"
dir_analises <- "G:/Meu Drive/INSPER/Trabalho/SIM/Análises/Subpop_0_6_Anos"

if (!dir.exists(dir_analises)) dir.create(dir_analises, recursive = TRUE)

# Tema Executivo ---------------------------------------------------------------
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

# 3) Carregamento e Preparação dos Dados de Óbitos -----------------------------
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 1: PREPARANDO BASE DE ÓBITOS\n")
cat("══════════════════════════════════════════════════════════\n\n")

arquivo_obitos <- file.path(dir_dados, "sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds")
if(!file.exists(arquivo_obitos)) stop("Arquivo de óbitos não encontrado!")

base_0_a_6 <- readRDS(arquivo_obitos)

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
  "33" = "Rio de Janeiro","35" = "São Paulo",     "41" = "Paraná",
  "42" = "Santa Catarina","43" = "Rio Grande do Sul",
  "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
  "52" = "Goiás",        "53" = "Distrito Federal"
)

# Dicionário reverso para cruzar nome do estado com o código
mapa_estados_inverso <- setNames(names(mapa_estados), mapa_estados)

base_analise <- base_0_a_6 %>%
  mutate(
    UNIDADE_IDADE = substr(as.character(IDADE), 1, 1),
    VALOR_IDADE   = suppressWarnings(as.numeric(substr(as.character(IDADE), 2, 3))),
    
    GRUPO_ETARIO = case_when(
      UNIDADE_IDADE %in% c("0", "1") |
        (UNIDADE_IDADE == "2" & VALOR_IDADE <= 6)  ~ "Neonatal Precoce (0-6 dias)",
      UNIDADE_IDADE == "2" & VALOR_IDADE >= 7 &
        VALOR_IDADE <= 27                          ~ "Neonatal Tardia (7-27 dias)",
      (UNIDADE_IDADE == "2" & VALOR_IDADE > 27) |
        UNIDADE_IDADE == "3"                       ~ "Pós-Neonatal (28 dias a <1 ano)",
      UNIDADE_IDADE == "4" & VALOR_IDADE >= 1 &
        VALOR_IDADE <= 6                           ~ "1 a 6 Anos",
      TRUE ~ "Sem informação precisa"
    ),
    
    COD_ESTADO   = substr(CODMUNRES, 1, 2),
    COD_MUN_RES  = substr(CODMUNRES, 1, 6),
    COD_MUN_OCOR = substr(CODMUNOCOR, 1, 6),
    MACRORREGIAO = mapa_regioes[substr(COD_ESTADO, 1, 1)],
    NOME_ESTADO  = mapa_estados[COD_ESTADO],
    
    CAPITULO_DESC = case_when(
      CAPITULO_CID %in% c("A", "B")        ~ "Infecciosas e Parasitárias",
      CAPITULO_CID == "J"                  ~ "Aparelho Respiratório",
      CAPITULO_CID == "P"                  ~ "Afecções Perinatais",
      CAPITULO_CID == "Q"                  ~ "Malformações Congênitas",
      CAPITULO_CID %in% c("V","W","X","Y") ~ "Causas Externas",
      TRUE                                 ~ "Outras Causas"
    ),
    
    CAUSA_PRIORITARIA = case_when(
      CAPITULO_CID %in% c("A", "B", "J")  ~ "Ação Prioritária (Prevenção/Tratamento)",
      CAPITULO_CID == "P"                 ~ "Atenção à Gestação e Parto",
      CAPITULO_CID == "Q"                 ~ "Malformações (Alta Complexidade)",
      TRUE                                ~ "Outras Causas / Difícil Prevenção"
    )
  )

# ==============================================================================
# 4) DENOMINADOR: Lendo CSV Estadual do SINASC Formatado
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 2: LENDO NASCIDOS VIVOS (SINASC POR UF)\n")
cat("══════════════════════════════════════════════════════════\n\n")

arquivo_sinasc <- file.path(dir_dados, "sinasc_cnv_nvuf113232189_120_73_168.csv")

if (!file.exists(arquivo_sinasc)) {
  stop(paste("Erro: Arquivo do SINASC não encontrado em", arquivo_sinasc))
}

# Lendo o CSV com locale ajustado para pegar acentuação padrão de Excel no Brasil
sinasc_raw <- read_csv2(arquivo_sinasc, locale = locale(encoding = "ISO-8859-1"), show_col_types = FALSE)

sinasc_uf <- sinasc_raw %>%
  rename(UF_NOME = 1) %>% # Pega a primeira coluna (região/uf) independente do nome exato
  mutate(
    NOME_ESTADO = str_trim(UF_NOME),
    cod_estado  = mapa_estados_inverso[NOME_ESTADO]
  ) %>%
  # Filtra para manter apenas as linhas que bateram com o nome de algum Estado
  filter(!is.na(cod_estado)) %>%
  # Transforma os anos em colunas para formato longo
  pivot_longer(
    cols = matches("^[0-9]{4}$"), # Pega todas as colunas que são "2015", "2016", etc
    names_to = "ano",
    values_to = "nascidos_vivos"
  ) %>%
  mutate(
    ano = as.numeric(ano),
    nascidos_vivos = as.numeric(nascidos_vivos)
  ) %>%
  select(ano, cod_estado, NOME_ESTADO, nascidos_vivos) %>%
  filter(!is.na(nascidos_vivos))

# Agregações do Denominador
sinasc_br <- sinasc_uf %>%
  group_by(ano) %>%
  summarise(nascidos_br = sum(nascidos_vivos), .groups = "drop")

sinasc_macro <- sinasc_uf %>%
  mutate(MACRORREGIAO = mapa_regioes[substr(cod_estado, 1, 1)]) %>%
  group_by(ano, MACRORREGIAO) %>%
  summarise(nascidos_macro = sum(nascidos_vivos), .groups = "drop")

cat("  ✓ Arquivo Estadual do SINASC lido e formatado com sucesso!\n")
cat("    Total de nascidos vivos (2024):", 
    format(sinasc_br$nascidos_br[sinasc_br$ano == max(sinasc_br$ano)], big.mark = "."), "\n\n")


# ==============================================================================
# 5) CALIBRAGEM DA TAXA
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 3: CALIBRAGEM DA TAXA DE MORTALIDADE\n")
cat("══════════════════════════════════════════════════════════\n\n")

MULT <- 1000
LABEL_TAXA <- "por 1.000 nascidos vivos"

# Calculando a TMI (menores de 1 ano) para referência do console
obitos_infantis <- base_analise %>%
  filter(GRUPO_ETARIO %in% c("Neonatal Precoce (0-6 dias)", 
                             "Neonatal Tardia (7-27 dias)", 
                             "Pós-Neonatal (28 dias a <1 ano)"))

tmi_media <- (nrow(obitos_infantis) / length(unique(base_analise$ANO_OBITO))) / 
  (sum(sinasc_br$nascidos_br) / length(unique(sinasc_br$ano))) * MULT

cat("  Taxa de Mortalidade Infantil (< 1 ano) média no período:\n")
cat("  →", round(tmi_media, 2), LABEL_TAXA, "\n\n")


# ==============================================================================
# 6) FIGURAS
# ==============================================================================
cat("══════════════════════════════════════════════════════════\n")
cat("  ETAPA 4: GERANDO FIGURAS\n")
cat("══════════════════════════════════════════════════════════\n\n")

# ── Fig01: Evolução por Faixa Etária (ABSOLUTO) ────────
cat("  Fig01: Evolução por Faixa Etária (absoluto)...\n")

evolucao_etaria <- base_analise %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(ANO_OBITO, GRUPO_ETARIO)

g1 <- ggplot(evolucao_etaria, aes(x = ANO_OBITO, y = n, color = GRUPO_ETARIO, group = GRUPO_ETARIO)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(values = cores_faixa) +
  scale_x_continuous(breaks = 2015:2024) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Evolução da Mortalidade por Faixa Etária (0 a 6 Anos)",
    subtitle = "Brasil, 2015–2024 | Óbitos absolutos",
    x = "Ano", y = "Número de Óbitos",
    caption = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo()

ggsave(file.path(dir_analises, "Fig01_Evolucao_Faixa_Etaria_Absoluto.png"), g1, width = 10, height = 6, dpi = 300)

# ── Fig02: Evolução das Causas — TAXA ponderada pelo SINASC ────────────────
cat("  Fig02: Evolução das Causas (taxa)...\n")

causas_taxa <- base_analise %>%
  count(ANO_OBITO, CAPITULO_DESC, name = "obitos") %>%
  left_join(sinasc_br, by = c("ANO_OBITO" = "ano")) %>%
  mutate(taxa = obitos / nascidos_br * MULT)

g2 <- ggplot(causas_taxa, aes(x = ANO_OBITO, y = taxa, fill = CAPITULO_DESC)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title    = "Evolução das Causas de Mortalidade (0 a 6 Anos)",
    subtitle = paste0("Taxa ", LABEL_TAXA, " | 2015–2024"),
    x = "Ano", y = paste0("Taxa (", LABEL_TAXA, ")"),
    caption = "Fonte: SIM e SINASC/DATASUS"
  ) +
  tema_executivo() +
  theme(legend.position = "right")

ggsave(file.path(dir_analises, "Fig02_Evolucao_Causas_Taxa.png"), g2, width = 10, height = 6, dpi = 300)

# ── Fig03: Causas Prioritárias POR FAIXA ETÁRIA ──────────────────
cat("  Fig03: Causas Prioritárias por Faixa Etária...\n")

causas_prio_faixa <- base_analise %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(GRUPO_ETARIO, CAUSA_PRIORITARIA) %>%
  group_by(GRUPO_ETARIO) %>%
  mutate(Perc = n / sum(n)) %>%
  ungroup()

g3 <- ggplot(causas_prio_faixa, aes(x = reorder(CAUSA_PRIORITARIA, Perc), y = Perc)) +
  geom_col(fill = "#2c3e50", width = 0.65) +
  geom_text(aes(label = percent(Perc, accuracy = 0.1)), hjust = -0.1, color = "#2c3e50", fontface = "bold", size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.25))) +
  facet_wrap(~GRUPO_ETARIO, ncol = 2, scales = "free_x") +
  labs(
    title    = "Causas Prioritárias de Mortalidade por Faixa Etária",
    subtitle = "Proporção de óbitos por grupo de ação | Brasil, 2015–2024",
    x = NULL, y = "Proporção do Total de Óbitos",
    caption = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"),
        strip.background = element_rect(fill = "#ecf0f1", color = NA))

ggsave(file.path(dir_analises, "Fig03_Causas_Prioritarias_por_Faixa.png"), g3, width = 12, height = 8, dpi = 300)

# ── Fig04: Heterogeneidade por Macrorregião — TAXA ponderada ─────────────────
cat("  Fig04: Heterogeneidade Regional (taxa)...\n")

hetero_taxa <- base_analise %>%
  filter(!is.na(MACRORREGIAO), GRUPO_ETARIO != "Sem informação precisa") %>%
  count(MACRORREGIAO, GRUPO_ETARIO, ANO_OBITO, name = "obitos") %>%
  left_join(sinasc_macro, by = c("ANO_OBITO" = "ano", "MACRORREGIAO")) %>%
  group_by(MACRORREGIAO, GRUPO_ETARIO) %>%
  summarise(
    obitos_total = sum(obitos),
    nv_total     = sum(nascidos_macro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_media = obitos_total / nv_total * MULT)

g4 <- ggplot(hetero_taxa, aes(x = reorder(MACRORREGIAO, -taxa_media), y = taxa_media, fill = GRUPO_ETARIO)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = cores_faixa) +
  labs(
    title    = "Heterogeneidade Regional — Taxa de Mortalidade por Faixa Etária",
    subtitle = paste0("Taxa média ", LABEL_TAXA, " | Acumulado 2015–2024"),
    x = "Macrorregião", y = paste0("Taxa (", LABEL_TAXA, ")"),
    caption = "Fonte: SIM e SINASC/DATASUS"
  ) +
  tema_executivo()

ggsave(file.path(dir_analises, "Fig04_Heterogeneidade_Macro_Taxa.png"), g4, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7) MAPAS DE TAXA (Estado e Macrorregião)
# ==============================================================================

# ── Fig05: Mapa por Estado ───────────────────────────────────────────────────
cat("  Fig05: Mapa de Taxa por Estado...\n")

obitos_uf <- base_analise %>%
  count(cod_estado = COD_ESTADO, ANO_OBITO, name = "obitos") %>%
  left_join(sinasc_uf, by = c("ANO_OBITO" = "ano", "cod_estado")) %>%
  group_by(cod_estado) %>%
  summarise(
    obitos_total = sum(obitos),
    nv_total     = sum(nascidos_vivos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    code_state = as.numeric(cod_estado),
    taxa_estado = obitos_total / nv_total * MULT
  )

malha_br <- read_state(year = 2020, showProgress = FALSE) %>%
  mutate(code_state = as.numeric(code_state))

mapa_uf <- left_join(malha_br, obitos_uf, by = "code_state")

g5_uf <- ggplot(mapa_uf) +
  geom_sf(aes(fill = taxa_estado), color = "black", linewidth = 0.2) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd")[2:9],
    name = "Taxa por\n1.000 N.V.",
    labels = function(x) round(x, 1)
  ) +
  labs(
    title    = "Taxa de Mortalidade por Estado (0 a 6 Anos)",
    subtitle = paste0("Óbitos ", LABEL_TAXA, " | Acumulado 2015–2024"),
    caption  = "Fonte: SIM e SINASC/DATASUS"
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

ggsave(file.path(dir_analises, "Fig05_Mapa_Taxa_Estado.png"), g5_uf, width = 8, height = 8, dpi = 300)

# ── Fig06: Mapa por Macrorregião ──────────────────────────────────────────
cat("  Fig06: Mapa de Taxa por Macrorregião...\n")

obitos_macro <- base_analise %>%
  filter(!is.na(MACRORREGIAO)) %>%
  count(MACRORREGIAO, ANO_OBITO, name = "obitos") %>%
  left_join(sinasc_macro, by = c("ANO_OBITO" = "ano", "MACRORREGIAO")) %>%
  group_by(MACRORREGIAO) %>%
  summarise(
    obitos_total = sum(obitos),
    nv_total     = sum(nascidos_macro, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(taxa_macro = obitos_total / nv_total * MULT)

macro_lookup <- tibble(
  code_state   = as.numeric(names(mapa_estados)),
  MACRORREGIAO = mapa_regioes[substr(names(mapa_estados), 1, 1)]
)

mapa_macro <- malha_br %>%
  left_join(macro_lookup, by = "code_state") %>%
  left_join(obitos_macro %>% select(MACRORREGIAO, taxa_macro), by = "MACRORREGIAO") %>%
  group_by(MACRORREGIAO) %>%
  summarise(taxa_macro = first(taxa_macro), .groups = "drop")

g6_macro <- ggplot(mapa_macro) +
  geom_sf(aes(fill = taxa_macro), color = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "YlOrRd")[2:9],
    name = "Taxa por\n1.000 N.V.",
    labels = function(x) round(x, 1)
  ) +
  labs(
    title    = "Taxa de Mortalidade por Macrorregião (0 a 6 Anos)",
    subtitle = paste0("Óbitos ", LABEL_TAXA, " | Acumulado 2015–2024"),
    caption  = "Fonte: SIM e SINASC/DATASUS"
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

ggsave(file.path(dir_analises, "Fig06_Mapa_Taxa_Macrorregiao.png"), g6_macro, width = 8, height = 8, dpi = 300)

# ==============================================================================
# 8) FLUXO RESIDÊNCIA → MUNICÍPIO DE ÓBITO
# ==============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 5: ANÁLISE DE FLUXO MUNICIPAL E ESTADUAL\n")
cat("══════════════════════════════════════════════════════════\n\n")

fluxo <- base_analise %>%
  filter(!is.na(COD_MUN_RES), !is.na(COD_MUN_OCOR),
         nchar(COD_MUN_RES) >= 6, nchar(COD_MUN_OCOR) >= 6) %>%
  mutate(
    OBITO_FORA    = COD_MUN_RES != COD_MUN_OCOR,
    UF_RES        = substr(COD_MUN_RES, 1, 2),
    UF_OCOR       = substr(COD_MUN_OCOR, 1, 2),
    OBITO_FORA_UF = UF_RES != UF_OCOR
  )

# ── Fig07: Proporção fora da residência por UF ───────────────────────────────
cat("  Fig07: Proporção de óbitos fora do município...\n")

fluxo_uf <- fluxo %>%
  group_by(UF_RES) %>%
  summarise(
    total        = n(),
    fora_mun     = sum(OBITO_FORA),
    fora_uf      = sum(OBITO_FORA_UF),
    perc_fora_mun = fora_mun / total,
    perc_fora_uf  = fora_uf / total,
    .groups = "drop"
  ) %>%
  mutate(NOME_ESTADO = mapa_estados[UF_RES]) %>%
  filter(!is.na(NOME_ESTADO))

g7 <- ggplot(fluxo_uf, aes(x = reorder(NOME_ESTADO, perc_fora_mun), y = perc_fora_mun)) +
  geom_col(fill = "#8e44ad", width = 0.6) +
  geom_text(aes(label = percent(perc_fora_mun, accuracy = 0.1)), hjust = -0.1, size = 3, color = "#2c3e50") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Proporção de Óbitos Fora do Município de Residência",
    subtitle = "Crianças de 0 a 6 anos | Por UF de residência | 2015–2024",
    x = NULL, y = "% de óbitos fora do município",
    caption = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

ggsave(file.path(dir_analises, "Fig07_Fluxo_Obitos_Fora_Municipio.png"), g7, width = 11, height = 8, dpi = 300)

# ── Fig08: TOP 30 Municípios Polo ─────────────
cat("  Fig08: Top 30 Polos de Saúde Infantil...\n")

polos <- fluxo %>%
  filter(OBITO_FORA) %>%
  count(COD_MUN_OCOR, name = "obitos_recebidos") %>%
  arrange(desc(obitos_recebidos)) %>%
  slice_head(n = 30)

mun_nomes <- tryCatch({
  geobr::lookup_muni(code_muni = "all") %>%
    mutate(cod_mun_6 = substr(as.character(code_muni), 1, 6)) %>%
    select(cod_mun_6, name_muni, abbrev_state) %>%
    distinct()
}, error = function(e) { NULL })

if (!is.null(mun_nomes)) {
  polos <- polos %>%
    left_join(mun_nomes, by = c("COD_MUN_OCOR" = "cod_mun_6")) %>%
    mutate(label = ifelse(!is.na(name_muni), paste0(name_muni, " (", abbrev_state, ")"), COD_MUN_OCOR))
} else {
  polos$label <- polos$COD_MUN_OCOR
}

g8 <- ggplot(polos, aes(x = reorder(label, obitos_recebidos), y = obitos_recebidos)) +
  geom_col(fill = "#e74c3c", width = 0.6) +
  geom_text(aes(label = comma(obitos_recebidos)), hjust = -0.1, size = 3, color = "#2c3e50", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Top 30 Municípios Polo de Saúde Infantil",
    subtitle = "Óbitos de crianças NÃO residentes ocorridos no município | 2015–2024",
    x = NULL, y = "Óbitos recebidos",
    caption  = "Fonte: SIM/DATASUS"
  ) +
  tema_executivo() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

ggsave(file.path(dir_analises, "Fig08_Polos_Saude_Infantil_Top30.png"), g8, width = 11, height = 9, dpi = 300)

# ── Fig09: Heatmap de Fluxo Inter-UF ─────────────────────────────────────────
cat("  Fig09: Heatmap de Fluxo Inter-UF...\n")

fluxo_interuf <- fluxo %>%
  filter(OBITO_FORA_UF) %>%
  count(UF_RES, UF_OCOR, name = "obitos") %>%
  mutate(NM_RES = mapa_estados[UF_RES], NM_OCOR = mapa_estados[UF_OCOR]) %>%
  filter(!is.na(NM_RES), !is.na(NM_OCOR)) %>%
  arrange(desc(obitos)) %>%
  slice_head(n = 50) 

g9 <- ggplot(fluxo_interuf, aes(x = NM_OCOR, y = NM_RES, fill = obitos)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")[2:9], name = "Óbitos", labels = comma) +
  geom_text(aes(label = comma(obitos)), size = 2.5, color = "white", fontface = "bold") +
  labs(
    title    = "Fluxo de Mortalidade Infantil entre UFs",
    subtitle = "Top 50 pares UF residência → UF de óbito | 2015–2024",
    x = "UF de Ocorrência do Óbito", y = "UF de Residência",
    caption = "Fonte: SIM/DATASUS"
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

ggsave(file.path(dir_analises, "Fig09_Heatmap_Fluxo_InterUF.png"), g9, width = 12, height = 10, dpi = 300)

# ==============================================================================
# 8B) EXPLORAÇÃO DOS TOP 10 MUNICÍPIOS QUE RECEBEM PESSOAS DE FORA
#     Origem, causa, hospital, CID, faixa etária e cor/raça
# ==============================================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 5B: TOP 10 MUNICÍPIOS RECEPTORES - ANÁLISE DETALHADA\n")
cat("══════════════════════════════════════════════════════════\n\n")

# ------------------------------------------------------------------------------
# Funções auxiliares para pegar variáveis que podem mudar de nome no SIM
# ------------------------------------------------------------------------------

pegar_primeira_coluna_existente <- function(df, possiveis_nomes) {
  nomes_existentes <- possiveis_nomes[possiveis_nomes %in% names(df)]
  if (length(nomes_existentes) == 0) return(NA_character_)
  nomes_existentes[1]
}

col_cid <- pegar_primeira_coluna_existente(
  base_analise,
  c("CAUSABAS", "CAUSABAS_O", "CAUSABAS_original", "CID", "CID10")
)

col_causa_linha <- pegar_primeira_coluna_existente(
  base_analise,
  c("LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII")
)

col_raca <- pegar_primeira_coluna_existente(
  base_analise,
  c("RACACOR", "RACA_COR", "RacaCor", "raça_cor", "cor_raca")
)

col_estab <- pegar_primeira_coluna_existente(
  base_analise,
  c("CODESTAB", "COD_ESTAB", "CNES", "CODIGO_CNES", "ESTAB", "ESTABELECI")
)

col_nome_estab <- pegar_primeira_coluna_existente(
  base_analise,
  c("NOMEESTAB", "NOME_ESTAB", "NOME_HOSPITAL", "HOSPITAL", "ESTABELECIMENTO")
)

# ------------------------------------------------------------------------------
# Dicionários
# ------------------------------------------------------------------------------

mapa_raca_cor <- c(
  "1" = "Branca",
  "2" = "Preta",
  "3" = "Amarela",
  "4" = "Parda",
  "5" = "Indígena",
  "9" = "Ignorado"
)

# Garantir nomes dos municípios
if (!exists("mun_nomes") || is.null(mun_nomes)) {
  mun_nomes <- tryCatch({
    geobr::lookup_muni(code_muni = "all") %>%
      mutate(cod_mun_6 = substr(as.character(code_muni), 1, 6)) %>%
      select(cod_mun_6, name_muni, abbrev_state) %>%
      distinct()
  }, error = function(e) { NULL })
}

# ------------------------------------------------------------------------------
# Base enriquecida de fluxo
# ------------------------------------------------------------------------------

fluxo_detalhado <- fluxo %>%
  filter(OBITO_FORA) %>%
  mutate(
    CID_BASICO = if (!is.na(col_cid)) as.character(.data[[col_cid]]) else NA_character_,
    CAUSA_LINHA_SIM = if (!is.na(col_causa_linha)) as.character(.data[[col_causa_linha]]) else NA_character_,
    COD_ESTABELECIMENTO = if (!is.na(col_estab)) as.character(.data[[col_estab]]) else NA_character_,
    NOME_ESTABELECIMENTO = if (!is.na(col_nome_estab)) as.character(.data[[col_nome_estab]]) else NA_character_,
    RACA_COR_RAW = if (!is.na(col_raca)) as.character(.data[[col_raca]]) else NA_character_,
    RACA_COR = case_when(
      is.na(RACA_COR_RAW) | RACA_COR_RAW == "" ~ "Sem informação",
      RACA_COR_RAW %in% names(mapa_raca_cor) ~ mapa_raca_cor[RACA_COR_RAW],
      TRUE ~ RACA_COR_RAW
    )
  )

if (!is.null(mun_nomes)) {
  fluxo_detalhado <- fluxo_detalhado %>%
    left_join(
      mun_nomes %>%
        rename(
          COD_MUN_RES_LOOKUP = cod_mun_6,
          MUNICIPIO_RES = name_muni,
          UF_RES_SIGLA = abbrev_state
        ),
      by = c("COD_MUN_RES" = "COD_MUN_RES_LOOKUP")
    ) %>%
    left_join(
      mun_nomes %>%
        rename(
          COD_MUN_OCOR_LOOKUP = cod_mun_6,
          MUNICIPIO_OCOR = name_muni,
          UF_OCOR_SIGLA = abbrev_state
        ),
      by = c("COD_MUN_OCOR" = "COD_MUN_OCOR_LOOKUP")
    ) %>%
    mutate(
      MUNICIPIO_RES_LABEL = if_else(
        !is.na(MUNICIPIO_RES),
        paste0(MUNICIPIO_RES, " (", UF_RES_SIGLA, ")"),
        COD_MUN_RES
      ),
      MUNICIPIO_OCOR_LABEL = if_else(
        !is.na(MUNICIPIO_OCOR),
        paste0(MUNICIPIO_OCOR, " (", UF_OCOR_SIGLA, ")"),
        COD_MUN_OCOR
      )
    )
} else {
  fluxo_detalhado <- fluxo_detalhado %>%
    mutate(
      MUNICIPIO_RES_LABEL = COD_MUN_RES,
      MUNICIPIO_OCOR_LABEL = COD_MUN_OCOR
    )
}

# ------------------------------------------------------------------------------
# Identificar top 10 municípios receptores
# ------------------------------------------------------------------------------

top10_municipios_receptores <- fluxo_detalhado %>%
  count(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL, name = "obitos_recebidos") %>%
  arrange(desc(obitos_recebidos)) %>%
  slice_head(n = 10)

# ------------------------------------------------------------------------------
# TABELA PRINCIPAL: top 10 receptores, principais origens, causas, CID,
# hospital/estabelecimento, faixa etária e cor/raça
# ------------------------------------------------------------------------------

tab_top10_receptores_detalhada <- fluxo_detalhado %>%
  semi_join(top10_municipios_receptores, by = c("COD_MUN_OCOR", "MUNICIPIO_OCOR_LABEL")) %>%
  group_by(
    MUNICIPIO_OCOR_LABEL,
    MUNICIPIO_RES_LABEL,
    CAPITULO_DESC,
    CAUSA_PRIORITARIA,
    CID_BASICO,
    GRUPO_ETARIO,
    RACA_COR,
    COD_ESTABELECIMENTO,
    NOME_ESTABELECIMENTO
  ) %>%
  summarise(
    obitos = n(),
    .groups = "drop"
  ) %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  mutate(
    total_recebido_municipio = sum(obitos),
    percentual_no_municipio = obitos / total_recebido_municipio
  ) %>%
  ungroup() %>%
  arrange(desc(total_recebido_municipio), MUNICIPIO_OCOR_LABEL, desc(obitos)) %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  mutate(
    percentual_no_municipio = round(100 * percentual_no_municipio, 1)
  ) %>%
  rename(
    Municipio_receptor = MUNICIPIO_OCOR_LABEL,
    Municipio_residencia = MUNICIPIO_RES_LABEL,
    Capitulo_causa = CAPITULO_DESC,
    Causa_prioritaria = CAUSA_PRIORITARIA,
    CID_basico = CID_BASICO,
    Faixa_etaria = GRUPO_ETARIO,
    Cor_raca = RACA_COR,
    Codigo_estabelecimento = COD_ESTABELECIMENTO,
    Nome_estabelecimento = NOME_ESTABELECIMENTO,
    Obitos = obitos,
    Total_recebido_municipio = total_recebido_municipio,
    Percentual_no_municipio = percentual_no_municipio
  )

# ------------------------------------------------------------------------------
# Tabelas auxiliares para leitura executiva
# ------------------------------------------------------------------------------

tab_top10_receptores_resumo <- fluxo_detalhado %>%
  semi_join(top10_municipios_receptores, by = c("COD_MUN_OCOR", "MUNICIPIO_OCOR_LABEL")) %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  summarise(
    obitos_recebidos = n(),
    municipios_origem_distintos = n_distinct(COD_MUN_RES),
    ufs_origem_distintas = n_distinct(UF_RES),
    principal_origem = names(sort(table(MUNICIPIO_RES_LABEL), decreasing = TRUE))[1],
    obitos_principal_origem = as.numeric(sort(table(MUNICIPIO_RES_LABEL), decreasing = TRUE)[1]),
    principal_causa = names(sort(table(CAPITULO_DESC), decreasing = TRUE))[1],
    obitos_principal_causa = as.numeric(sort(table(CAPITULO_DESC), decreasing = TRUE)[1]),
    principal_faixa_etaria = names(sort(table(GRUPO_ETARIO), decreasing = TRUE))[1],
    obitos_principal_faixa = as.numeric(sort(table(GRUPO_ETARIO), decreasing = TRUE)[1]),
    principal_cor_raca = names(sort(table(RACA_COR), decreasing = TRUE))[1],
    obitos_principal_cor_raca = as.numeric(sort(table(RACA_COR), decreasing = TRUE)[1]),
    .groups = "drop"
  ) %>%
  arrange(desc(obitos_recebidos)) %>%
  mutate(
    perc_principal_origem = round(100 * obitos_principal_origem / obitos_recebidos, 1),
    perc_principal_causa = round(100 * obitos_principal_causa / obitos_recebidos, 1),
    perc_principal_faixa = round(100 * obitos_principal_faixa / obitos_recebidos, 1),
    perc_principal_cor_raca = round(100 * obitos_principal_cor_raca / obitos_recebidos, 1)
  ) %>%
  rename(
    Municipio_receptor = MUNICIPIO_OCOR_LABEL
  )

# ------------------------------------------------------------------------------
# GRÁFICO: heatmap dos principais fluxos residência → top 10 municípios receptores
# ------------------------------------------------------------------------------

cat("  Fig10: Heatmap dos fluxos para os Top 10 municípios receptores...\n")

fluxos_top10_grafico <- fluxo_detalhado %>%
  semi_join(top10_municipios_receptores, by = c("COD_MUN_OCOR", "MUNICIPIO_OCOR_LABEL")) %>%
  count(MUNICIPIO_RES_LABEL, MUNICIPIO_OCOR_LABEL, name = "obitos") %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  arrange(desc(obitos), .by_group = TRUE) %>%
  slice_head(n = 8) %>%
  ungroup() %>%
  mutate(
    MUNICIPIO_OCOR_LABEL = fct_reorder(MUNICIPIO_OCOR_LABEL, obitos, .fun = sum),
    MUNICIPIO_RES_LABEL = fct_reorder(MUNICIPIO_RES_LABEL, obitos, .fun = sum)
  )

g10 <- ggplot(
  fluxos_top10_grafico,
  aes(x = MUNICIPIO_OCOR_LABEL, y = MUNICIPIO_RES_LABEL, fill = obitos)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(obitos)), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradientn(
    colors = brewer.pal(9, "Reds")[2:9],
    name = "Óbitos",
    labels = comma
  ) +
  labs(
    title = "Principais Fluxos para os Top 10 Municípios Receptores",
    subtitle = "Município de residência → município de ocorrência do óbito | Crianças de 0 a 6 anos | 2015–2024",
    x = "Município receptor do óbito",
    y = "Município de residência",
    caption = "Fonte: SIM/DATASUS"
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

ggsave(
  file.path(dir_analises, "Fig10_Fluxos_Top10_Municipios_Receptores.png"),
  g10,
  width = 12,
  height = 9,
  dpi = 300
)

# ------------------------------------------------------------------------------
# Salvar tabelas específicas desta análise
# ------------------------------------------------------------------------------

write_xlsx(
  list(
    "Top10_Resumo_Executivo" = tab_top10_receptores_resumo,
    "Top10_Detalhado" = tab_top10_receptores_detalhada,
    "Dados_Grafico_Fluxos" = fluxos_top10_grafico
  ),
  path = file.path(dir_analises, "Tabelas_Top10_Municipios_Receptores.xlsx")
)

cat("  ✓ Tabela e gráfico dos Top 10 municípios receptores gerados com sucesso!\n\n")

# ==============================================================================
# 8C) SEGMENTAÇÃO DE CID (GRUPOS MARCADORES DE POLO) POR FAIXA ETÁRIA
#     Objetivo: identificar quais recortes diagnósticos — sobretudo os de alta
#     complexidade — concentram-se nos municípios-polo, por faixa etária. São
#     esses grupos que costumam "puxar" deslocamentos e revelar polos de
#     referência. (Reaproveitável para o SIH: basta trocar a base de entrada.)
#
#     IMPORTANTE (v3): as afecções perinatais são tratadas SEPARADAMENTE dos
#     demais grupos de alta complexidade. O óbito perinatal recebido de fora
#     reflete em grande parte o LOCAL DE PARTO (gestação de risco referenciada à
#     maternidade da capital), e não o deslocamento da criança doente em busca
#     de tratamento. Por isso criamos o conceito de "referência terapêutica"
#     (= alta complexidade SEM perinatal), que é o fluxo "limpo" para a leitura
#     de centros de referência e vazios assistenciais.
# ==============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 5C: SEGMENTAÇÃO DE CID POR FAIXA ETÁRIA (POLOS)\n")
cat("══════════════════════════════════════════════════════════\n\n")

# Parâmetros das novas análises ------------------------------------------------
N_TOP_POLOS    <- 12    # nº de municípios-polo detalhados nas análises por faixa/CEP
N_TOP_POLO_ESP <- 8     # nº de polos exibidos por especialidade (ranking)
N_TOP_ORIGENS  <- 5     # nº de origens por polo no heatmap de fluxo (Fig12)
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

# Classificação dos grupos diagnósticos marcadores de polo --------------------
# Baseado no CID-10 básico (CAUSABAS, 3 caracteres). Os grupos de alta
# complexidade exigem centros de referência e tendem a gerar deslocamento.
classificar_grupo_cid_polo <- function(cid) {
  cid   <- toupper(trimws(as.character(cid)))
  letra <- substr(cid, 1, 1)
  num   <- suppressWarnings(as.numeric(substr(cid, 2, 3)))
  case_when(
    letra == "C" | (letra == "D" & !is.na(num) & num <= 48) ~ "Neoplasias (Oncologia)",
    letra == "Q" & !is.na(num) & num >= 20 & num <= 28      ~ "Cardiopatias Congênitas",
    letra == "Q"                                            ~ "Malformações Congênitas (outras)",
    letra == "P"                                            ~ "Afecções Perinatais",
    letra == "G"                                            ~ "Doenças do Sistema Nervoso",
    letra == "E" & !is.na(num) & num >= 70 & num <= 90      ~ "Doenças Metabólicas/Genéticas",
    letra %in% c("A", "B")                                  ~ "Doenças Infecciosas",
    letra == "J"                                            ~ "Aparelho Respiratório",
    letra %in% c("V", "W", "X", "Y")                        ~ "Causas Externas",
    TRUE                                                    ~ "Outras Causas"
  )
}

grupos_alta_complexidade <- c(
  "Neoplasias (Oncologia)",
  "Cardiopatias Congênitas",
  "Malformações Congênitas (outras)",
  "Afecções Perinatais",
  "Doenças do Sistema Nervoso",
  "Doenças Metabólicas/Genéticas"
)

# Referência terapêutica = alta complexidade SEM perinatal (fluxo "limpo") -----
grupos_referencia_terapeutica <- setdiff(grupos_alta_complexidade, "Afecções Perinatais")

# Enriquecer o fluxo (óbitos de NÃO residentes) com o grupo de CID -------------
fluxo_detalhado <- fluxo_detalhado %>%
  mutate(
    GRUPO_CID_POLO      = classificar_grupo_cid_polo(CID_BASICO),
    ALTA_COMPLEXIDADE   = GRUPO_CID_POLO %in% grupos_alta_complexidade,
    MARCADOR_REFERENCIA = GRUPO_CID_POLO %in% grupos_referencia_terapeutica,
    TIPO_FLUXO = case_when(
      GRUPO_CID_POLO == "Afecções Perinatais"           ~ "Perinatal (nascimento/parto)",
      GRUPO_CID_POLO %in% grupos_referencia_terapeutica ~ "Referência terapêutica",
      TRUE                                              ~ "Demais causas"
    )
  )

# Top N municípios-polo (receptores de óbitos de fora) -------------------------
top_polos_receptores <- fluxo_detalhado %>%
  count(COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL, name = "obitos_recebidos") %>%
  arrange(desc(obitos_recebidos)) %>%
  slice_head(n = N_TOP_POLOS)

fluxo_polos <- fluxo_detalhado %>%
  semi_join(top_polos_receptores, by = c("COD_MUN_OCOR", "MUNICIPIO_OCOR_LABEL"))

# TABELA NOVA (parte 1): polo × grupo de CID × faixa etária --------------------
tab_polo_cid_faixa <- fluxo_polos %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  group_by(MUNICIPIO_OCOR_LABEL, GRUPO_CID_POLO, TIPO_FLUXO, GRUPO_ETARIO) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  mutate(
    total_polo   = sum(obitos),
    perc_no_polo = round(100 * obitos / total_polo, 1)
  ) %>%
  ungroup() %>%
  arrange(desc(total_polo), MUNICIPIO_OCOR_LABEL, desc(obitos)) %>%
  rename(
    Municipio_polo     = MUNICIPIO_OCOR_LABEL,
    Grupo_CID          = GRUPO_CID_POLO,
    Tipo_fluxo         = TIPO_FLUXO,
    Faixa_etaria       = GRUPO_ETARIO,
    Obitos             = obitos,
    Total_polo         = total_polo,
    Percentual_no_polo = perc_no_polo
  )

# ── Fig11: Heatmap grupo de CID × faixa etária nos polos ──────────────────────
cat("  Fig11: Heatmap de grupos de CID por faixa etária (polos)...\n")

heat_cid_faixa <- fluxo_polos %>%
  filter(GRUPO_ETARIO != "Sem informação precisa") %>%
  count(GRUPO_CID_POLO, GRUPO_ETARIO, name = "obitos") %>%
  mutate(
    GRUPO_ETARIO   = factor(GRUPO_ETARIO, levels = names(cores_faixa)),
    GRUPO_CID_POLO = fct_reorder(GRUPO_CID_POLO, obitos, .fun = sum)
  )

g11 <- ggplot(heat_cid_faixa, aes(x = GRUPO_ETARIO, y = GRUPO_CID_POLO, fill = obitos)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(obitos)), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradientn(colors = brewer.pal(9, "PuBuGn")[2:9], name = "Óbitos", labels = comma) +
  labs(
    title    = "Segmentação Diagnóstica nos Municípios-Polo, por Faixa Etária",
    subtitle = paste0("Óbitos de crianças NÃO residentes recebidos nos ", N_TOP_POLOS,
                      " maiores polos | 0 a 6 anos | 2015–2024"),
    x = "Faixa etária", y = "Grupo diagnóstico (CID-10 básico)",
    caption = "Fonte: SIM/DATASUS | Grupos de alta complexidade = marcadores de centros de referência"
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

ggsave(file.path(dir_analises, "Fig11_CID_Segmentado_por_Faixa_Polos.png"),
       g11, width = 11, height = 7, dpi = 300)

# TABELA NOVA (parte 2): RANKING DE POLOS POR ESPECIALIDADE --------------------
# Para CADA grupo de referência terapêutica, quem são os maiores receptores de
# óbitos de não residentes? Revela a VOCAÇÃO de cada polo (quem é polo de
# oncologia, de cardiopatia, etc.) — em vez do grupo apenas "predominante".
tab_polo_por_especialidade <- fluxo_detalhado %>%
  filter(MARCADOR_REFERENCIA) %>%
  count(GRUPO_CID_POLO, COD_MUN_OCOR, MUNICIPIO_OCOR_LABEL, name = "obitos_recebidos") %>%
  group_by(GRUPO_CID_POLO) %>%
  mutate(
    total_especialidade = sum(obitos_recebidos),
    perc_na_especialidade = round(100 * obitos_recebidos / total_especialidade, 1),
    rank_polo = row_number(desc(obitos_recebidos))
  ) %>%
  arrange(GRUPO_CID_POLO, desc(obitos_recebidos)) %>%
  ungroup() %>%
  rename(
    Especialidade              = GRUPO_CID_POLO,
    Municipio_polo             = MUNICIPIO_OCOR_LABEL,
    Obitos_recebidos           = obitos_recebidos,
    Total_especialidade        = total_especialidade,
    Percentual_na_especialidade = perc_na_especialidade,
    Rank_polo                  = rank_polo
  ) %>%
  select(Especialidade, Rank_polo, Municipio_polo, COD_MUN_OCOR,
         Obitos_recebidos, Total_especialidade, Percentual_na_especialidade)

# ── Fig13: Polos por especialidade (fluxo de referência terapêutica) ──────────
cat("  Fig13: Ranking de polos por especialidade...\n")

fig_polo_esp <- tab_polo_por_especialidade %>%
  group_by(Especialidade) %>%
  slice_head(n = N_TOP_POLO_ESP) %>%
  ungroup()

g13 <- ggplot(
  fig_polo_esp,
  aes(x = reorder_within2(Municipio_polo, Obitos_recebidos, Especialidade),
      y = Obitos_recebidos, fill = Especialidade)
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = comma(Obitos_recebidos)), hjust = -0.1, size = 2.8, color = "#2c3e50") +
  coord_flip() +
  scale_x_reordered2() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  facet_wrap(~Especialidade, scales = "free", ncol = 2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title    = "Polos por Especialidade — Fluxo de Referência Terapêutica",
    subtitle = paste0("Maiores receptores de óbitos de NÃO residentes, por grupo de alta complexidade\n",
                      "(exclui afecções perinatais = fluxo de parto) | 0 a 6 anos | 2015–2024"),
    x = NULL, y = "Óbitos recebidos de não residentes",
    caption = "Fonte: SIM/DATASUS | Identifica a vocação de cada polo (oncologia, cardiopatia, neuro, etc.)"
  ) +
  tema_executivo() +
  theme(
    axis.line.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    strip.text       = element_text(face = "bold", size = 11, color = "#2c3e50"),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    legend.position  = "none"
  )

ggsave(file.path(dir_analises, "Fig13_Polos_por_Especialidade.png"),
       g13, width = 12, height = 11, dpi = 300)

# ==============================================================================
# 8D) FLUXO ORIGEM → DESTINO POR CEP (PACIENTE × HOSPITAL)
#     Objetivo: detalhar deslocamentos abaixo do nível municipal usando o CEP de
#     residência (paciente) e o CEP do estabelecimento (hospital, via CNES),
#     focando no FLUXO DE REFERÊNCIA TERAPÊUTICA (exclui perinatal) e produzindo
#     insumos para identificar potenciais vazios assistenciais.
#
#     OBS.: o CEP de residência costuma estar disponível no SIH (campo CEP da
#     AIH); no SIM ele pode não constar do extrato público. O script DETECTA a
#     presença da coluna e, se ausente, usa o MUNICÍPIO de residência como
#     origem — a mesma lógica roda diretamente sobre a base do SIH.
# ==============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 5D: FLUXO ORIGEM → DESTINO POR CEP\n")
cat("══════════════════════════════════════════════════════════\n\n")

# Função para agregar o CEP a uma "área CEP" (prefixo) -------------------------
area_cep <- function(cep, n = N_DIGITOS_CEP) {
  cep <- gsub("[^0-9]", "", as.character(cep))
  cep <- ifelse(nchar(cep) > 8, substr(cep, 1, 8), cep)
  ifelse(!is.na(cep) & nchar(cep) >= n & cep != "", substr(cep, 1, n), NA_character_)
}

# (a) CEP de residência (origem) -----------------------------------------------
col_cep_res        <- pegar_primeira_coluna_existente(
  fluxo_detalhado,
  c("CEP", "CEP_RES", "CEPRES", "CODCEP", "CEPADR", "CEP_RESID")
)
tem_cep_residencia <- !is.na(col_cep_res)

# (b) CEP do estabelecimento (destino) via CNES --------------------------------
cnes_cep <- NULL
if (isTRUE(usar_cnes_cep) && file.exists(arquivo_cnes)) {
  cnes_cep <- tryCatch({
    read_csv2(arquivo_cnes, locale = locale(encoding = "ISO-8859-1"),
              show_col_types = FALSE) %>% rename_with(toupper)
  }, error = function(e) NULL)
  
  if (!is.null(cnes_cep)) {
    col_cnes_id  <- pegar_primeira_coluna_existente(cnes_cep, c("CNES", "CODESTAB", "CO_CNES", "COD_CNES"))
    col_cnes_cep <- pegar_primeira_coluna_existente(cnes_cep, c("CEP", "CO_CEP", "NU_CEP", "CEP_ESTAB"))
    if (!is.na(col_cnes_id) && !is.na(col_cnes_cep)) {
      cnes_cep <- cnes_cep %>%
        transmute(
          COD_ESTABELECIMENTO = str_pad(gsub("[^0-9]", "", as.character(.data[[col_cnes_id]])), 7, pad = "0"),
          CEP_HOSPITAL        = as.character(.data[[col_cnes_cep]])
        ) %>%
        distinct(COD_ESTABELECIMENTO, .keep_all = TRUE)
    } else {
      cnes_cep <- NULL
    }
  }
}
tem_cep_hospital <- !is.null(cnes_cep)

# (c) Construir base de fluxo CEP restrita aos polos ---------------------------
fluxo_cep_base <- fluxo_polos %>%
  mutate(
    AREA_CEP_ORIGEM     = if (tem_cep_residencia) area_cep(.data[[col_cep_res]]) else NA_character_,
    COD_ESTABELECIMENTO = str_pad(gsub("[^0-9]", "", COD_ESTABELECIMENTO), 7, pad = "0")
  )

if (tem_cep_hospital) {
  fluxo_cep_base <- fluxo_cep_base %>%
    left_join(cnes_cep, by = "COD_ESTABELECIMENTO") %>%
    mutate(AREA_CEP_DESTINO = area_cep(CEP_HOSPITAL))
} else {
  fluxo_cep_base <- fluxo_cep_base %>%
    mutate(CEP_HOSPITAL = NA_character_, AREA_CEP_DESTINO = NA_character_)
}

# Dimensões origem/destino com fallback hierárquico (CEP → município) ----------
fluxo_cep_base <- fluxo_cep_base %>%
  mutate(
    ORIGEM  = if (tem_cep_residencia) coalesce(AREA_CEP_ORIGEM, MUNICIPIO_RES_LABEL)  else MUNICIPIO_RES_LABEL,
    DESTINO = if (tem_cep_hospital)   coalesce(AREA_CEP_DESTINO, MUNICIPIO_OCOR_LABEL) else MUNICIPIO_OCOR_LABEL
  )

if (!tem_cep_residencia) {
  cat("  ⚠ Coluna de CEP de residência não encontrada nesta base.\n")
  cat("    → Usando MUNICÍPIO de residência como origem (no SIH/AIH o CEP estará disponível).\n\n")
}
if (!tem_cep_hospital) {
  cat("  ⚠ Base CNES de CEP do hospital não carregada (", arquivo_cnes, ").\n", sep = "")
  cat("    → Usando MUNICÍPIO de ocorrência como destino.\n\n")
}

# ── Fig12: Heatmap origem → polo no fluxo de REFERÊNCIA TERAPÊUTICA ────────────
#     Enxuto: top N origens por polo, escala de cor em raiz quadrada (para não
#     achatar o miolo) e foco no fluxo "limpo" (sem perinatal).
cat("  Fig12: Fluxo origem (CEP/município) → polo, referência terapêutica...\n")

fluxo_cep_grafico <- fluxo_cep_base %>%
  filter(MARCADOR_REFERENCIA, !is.na(ORIGEM)) %>%
  count(ORIGEM, MUNICIPIO_OCOR_LABEL, name = "obitos") %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  arrange(desc(obitos), .by_group = TRUE) %>%
  slice_head(n = N_TOP_ORIGENS) %>%
  ungroup() %>%
  mutate(
    MUNICIPIO_OCOR_LABEL = fct_reorder(MUNICIPIO_OCOR_LABEL, obitos, .fun = sum),
    ORIGEM               = fct_reorder(ORIGEM, obitos, .fun = sum)
  )

rotulo_origem  <- if (tem_cep_residencia) paste0("Área CEP de residência (", N_DIGITOS_CEP, " díg.)") else "Município de residência"
rotulo_destino <- if (tem_cep_hospital)   "Polo receptor (CEP/hospital)" else "Município-polo receptor"
fonte_cep      <- if (tem_cep_hospital) "Fonte: SIM/DATASUS + CNES (CEP do hospital)" else "Fonte: SIM/DATASUS (nível município — CEP indisponível neste extrato)"

g12 <- ggplot(fluxo_cep_grafico, aes(x = MUNICIPIO_OCOR_LABEL, y = ORIGEM, fill = obitos)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = comma(obitos)), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradientn(colors = brewer.pal(9, "BuPu")[2:9], name = "Óbitos",
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

ggsave(file.path(dir_analises, "Fig12_Fluxo_CEP_Origem_Destino_Polos.png"),
       g12, width = 11, height = 9, dpi = 300)

# TABELA NOVA (parte 3): fluxo origem → polo por grupo de CID (com Tipo_fluxo) -
# Mantém TODOS os grupos de alta complexidade (perinatal incluído, identificado
# pela coluna Tipo_fluxo) para permitir filtragem; a figura usa só referência.
tab_fluxo_cep_polos <- fluxo_cep_base %>%
  filter(ALTA_COMPLEXIDADE, !is.na(ORIGEM)) %>%
  group_by(MUNICIPIO_OCOR_LABEL, ORIGEM, GRUPO_CID_POLO, TIPO_FLUXO, GRUPO_ETARIO) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  mutate(perc_no_polo = round(100 * obitos / sum(obitos), 1)) %>%
  ungroup() %>%
  arrange(MUNICIPIO_OCOR_LABEL, desc(obitos)) %>%
  rename(
    Municipio_polo     = MUNICIPIO_OCOR_LABEL,
    Origem             = ORIGEM,
    Grupo_CID          = GRUPO_CID_POLO,
    Tipo_fluxo         = TIPO_FLUXO,
    Faixa_etaria       = GRUPO_ETARIO,
    Obitos             = obitos,
    Percentual_no_polo = perc_no_polo
  )

# (d) Distância de deslocamento (centroides municipais via geobr) --------------
#     Insumo para "vazios assistenciais": quão longe a criança precisa ir até o
#     polo. Calculado por Haversine entre centroides de residência e ocorrência.
dist_polos <- tryCatch({
  centroides <- read_municipality(year = 2020, showProgress = FALSE) %>%
    sf::st_make_valid() %>%
    mutate(cod_mun_6 = substr(as.character(code_muni), 1, 6))
  
  cent_pt <- suppressWarnings(sf::st_centroid(centroides)) %>%
    mutate(lon = sf::st_coordinates(.)[, 1],
           lat = sf::st_coordinates(.)[, 2]) %>%
    sf::st_drop_geometry() %>%
    select(cod_mun_6, lon, lat)
  
  fluxo_polos %>%
    left_join(cent_pt, by = c("COD_MUN_RES" = "cod_mun_6")) %>%
    rename(lon_res = lon, lat_res = lat) %>%
    left_join(cent_pt, by = c("COD_MUN_OCOR" = "cod_mun_6")) %>%
    rename(lon_ocor = lon, lat_ocor = lat) %>%
    filter(!is.na(lon_res), !is.na(lon_ocor)) %>%
    mutate(
      dist_km = 6371 * 2 * asin(pmin(1, sqrt(
        sin((lat_ocor - lat_res) * pi / 180 / 2)^2 +
          cos(lat_res * pi / 180) * cos(lat_ocor * pi / 180) *
          sin((lon_ocor - lon_res) * pi / 180 / 2)^2
      )))
    )
}, error = function(e) { cat("  ⚠ Distâncias não calculadas:", conditionMessage(e), "\n"); NULL })

# TABELA NOVA (parte 4): resumo por polo ---------------------------------------
# Inclui o indicador DISCRIMINANTE de referência terapêutica (sem perinatal),
# que separa polos de alta densidade obstétrica de polos de tratamento.
tab_polos_resumo_cep <- fluxo_polos %>%
  group_by(MUNICIPIO_OCOR_LABEL) %>%
  summarise(
    obitos_recebidos              = n(),
    perc_perinatal                = round(100 * mean(GRUPO_CID_POLO == "Afecções Perinatais"), 1),
    perc_referencia_terapeutica   = round(100 * mean(MARCADOR_REFERENCIA), 1),
    grupo_referencia_predominante = {
      tb <- table(GRUPO_CID_POLO[MARCADOR_REFERENCIA])
      if (length(tb) == 0) NA_character_ else names(sort(tb, decreasing = TRUE))[1]
    },
    faixa_predominante            = names(sort(table(GRUPO_ETARIO[GRUPO_ETARIO != "Sem informação precisa"]),
                                               decreasing = TRUE))[1],
    municipios_origem_distintos   = n_distinct(COD_MUN_RES),
    .groups = "drop"
  ) %>%
  arrange(desc(obitos_recebidos))

if (tem_cep_residencia) {
  areas_cep_polo <- fluxo_cep_base %>%
    filter(!is.na(AREA_CEP_ORIGEM)) %>%
    group_by(MUNICIPIO_OCOR_LABEL) %>%
    summarise(areas_cep_origem_distintas = n_distinct(AREA_CEP_ORIGEM), .groups = "drop")
  tab_polos_resumo_cep <- left_join(tab_polos_resumo_cep, areas_cep_polo, by = "MUNICIPIO_OCOR_LABEL")
}

if (!is.null(dist_polos)) {
  dist_resumo <- dist_polos %>%
    group_by(MUNICIPIO_OCOR_LABEL) %>%
    summarise(
      dist_mediana_km = round(median(dist_km, na.rm = TRUE), 1),
      dist_p90_km     = round(quantile(dist_km, 0.9, na.rm = TRUE), 1),
      .groups = "drop"
    )
  tab_polos_resumo_cep <- left_join(tab_polos_resumo_cep, dist_resumo, by = "MUNICIPIO_OCOR_LABEL")
}

tab_polos_resumo_cep <- tab_polos_resumo_cep %>%
  rename(
    Municipio_polo                = MUNICIPIO_OCOR_LABEL,
    Obitos_recebidos              = obitos_recebidos,
    Perc_perinatal                = perc_perinatal,
    Perc_referencia_terapeutica   = perc_referencia_terapeutica,
    Grupo_referencia_predominante = grupo_referencia_predominante,
    Faixa_predominante            = faixa_predominante,
    Municipios_origem_distintos   = municipios_origem_distintos
  )

# Workbook dedicado das novas análises -----------------------------------------
write_xlsx(
  list(
    "Polo_CID_Faixa"         = tab_polo_cid_faixa,
    "Polo_por_Especialidade" = tab_polo_por_especialidade,
    "Fluxo_CEP_Polos"        = tab_fluxo_cep_polos,
    "Polos_Resumo_CEP"       = tab_polos_resumo_cep
  ),
  path = file.path(dir_analises, "Tabelas_CID_Segmentado_e_Fluxo_CEP.xlsx")
)

cat("  ✓ Novas análises (CID segmentado + especialidade + fluxo CEP + resumo) geradas!\n\n")

# ==============================================================================
# 9) TABELAS EXECUTIVAS (EXCEL)
# ==============================================================================
cat("\n══════════════════════════════════════════════════════════\n")
cat("  ETAPA 6: TABELAS EXECUTIVAS (EXCEL)\n")
cat("══════════════════════════════════════════════════════════\n\n")

tab_evolucao      <- evolucao_etaria %>% pivot_wider(names_from = GRUPO_ETARIO, values_from = n, values_fill = 0) %>% arrange(ANO_OBITO)
tab_causas        <- base_analise %>% count(ANO_OBITO, CAPITULO_DESC) %>% pivot_wider(names_from = CAPITULO_DESC, values_from = n, values_fill = 0) %>% arrange(ANO_OBITO)
tab_prio_faixa    <- causas_prio_faixa %>% select(GRUPO_ETARIO, CAUSA_PRIORITARIA, n, Perc) %>% arrange(GRUPO_ETARIO, desc(Perc))
tab_fluxo         <- fluxo_uf %>% select(NOME_ESTADO, total, fora_mun, perc_fora_mun, fora_uf, perc_fora_uf) %>% arrange(desc(perc_fora_mun))
tab_polos         <- polos %>% select(any_of(c("label", "COD_MUN_OCOR", "obitos_recebidos"))) %>% arrange(desc(obitos_recebidos))
tab_fluxo_interuf <- fluxo_interuf %>% select(NM_RES, NM_OCOR, obitos) %>% arrange(desc(obitos))

tab_taxa_uf <- obitos_uf %>%
  mutate(NOME_ESTADO = mapa_estados[cod_estado]) %>%
  select(NOME_ESTADO, obitos_total, nascidos_vivos = nv_total, taxa_estado) %>%
  arrange(desc(taxa_estado))

tab_taxa_macro <- obitos_macro %>%
  select(MACRORREGIAO, obitos_total, nascidos_vivos = nv_total, taxa_macro) %>%
  arrange(desc(taxa_macro))

abas_excel <- list(
  "1_Evolucao_Idade"       = tab_evolucao,
  "2_Evolucao_Causas"      = tab_causas,
  "3_Prio_por_Faixa"       = tab_prio_faixa,
  "4_Fluxo_por_UF"         = tab_fluxo,
  "5_Polos_Saude_Infantil" = tab_polos,
  "6_Fluxo_InterUF"        = tab_fluxo_interuf,
  "7_Taxa_por_UF"          = tab_taxa_uf,
  "8_Taxa_por_Macro"       = tab_taxa_macro,
  "9_Top10_Receptores"     = tab_top10_receptores_resumo,
  "10_Top10_Detalhado"     = tab_top10_receptores_detalhada,
  "11_Polo_CID_Faixa"      = tab_polo_cid_faixa,
  "12_Polo_Especialidade"  = tab_polo_por_especialidade,
  "13_Fluxo_CEP_Polos"     = tab_fluxo_cep_polos,
  "14_Polos_Resumo_CEP"    = tab_polos_resumo_cep
)

write_xlsx(abas_excel, path = file.path(dir_analises, "Tabelas_Executivas_Mortalidade_v2.xlsx"))

cat("══════════════════════════════════════════════════════════\n")
cat("  PROCESSAMENTO CONCLUÍDO COM SUCESSO!\n")
cat("══════════════════════════════════════════════════════════\n")