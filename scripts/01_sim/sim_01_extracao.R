############################################################
# SIM-DO | MORTALIDADE INFANTO-JUVENIL BRASIL (2015-2024)
# Etapa 1: Download, Estruturação e Limpeza Inicial
# Destino: GitHub / Base para Análises
############################################################

# 1) Pacotes -------------------------------------------------------------------
pacotes <- c("microdatasus", "dplyr", "stringr", "readr")

for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(microdatasus)
library(dplyr)
library(stringr)
library(readr) 

# 2) Parametros e Diretorios ---------------------------------------------------
dir_dados <- "xx"
dir_analises <- "xx"

dir_geral <- file.path(dir_analises, "Geral_Menor_18")
dir_0_a_6 <- file.path(dir_analises, "Subpop_0_6_Anos")

# Criando estrutura de pastas
for (dir in c(dir_dados, dir_geral, dir_0_a_6)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

ufs_brasil <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# 3) Funcoes Auxiliares de Limpeza ---------------------------------------------
calcular_idade_anos <- function(idade_str) {
  idade_str <- as.character(idade_str)
  unidade <- substr(idade_str, 1, 1)
  valor <- suppressWarnings(as.numeric(substr(idade_str, 2, 3)))
  
  case_when(
    is.na(unidade) | is.na(valor) ~ NA_real_,
    unidade %in% c("0", "1", "2", "3") ~ 0, # Menores de 1 ano
    unidade == "4" ~ valor,
    unidade == "5" ~ 100 + valor,
    TRUE ~ NA_real_
  )
}

rot_sexo <- function(x) {
  case_when(x == "1" ~ "Masculino", x == "2" ~ "Feminino", TRUE ~ "Ignorado/Outro")
}

rot_raca <- function(x) {
  case_when(
    x == "1" ~ "Branca", x == "2" ~ "Preta", x == "3" ~ "Parda",
    x == "4" ~ "Amarela", x == "5" ~ "Indígena", TRUE ~ "Sem informação"
  )
}

# 4) Download e Limpeza em Lote (Todas as Variáveis) ---------------------------
cat("Iniciando processamento em lote do SIM-DO Brasil (2015-2024)...\n")
cat("Atenção: Baixando TODAS as variáveis. A extração vai demorar um pouco.\n")

lista_anos_processados <- list()

for (ano in 2015:2024) {
  cat(sprintf("\n>>> Baixando dados de %d...\n", ano))
  
  temp_bruto <- fetch_datasus(
    year_start = ano, 
    year_end = ano, 
    uf = ufs_brasil, 
    information_system = "SIM-DO"
  )
  
  cat(sprintf("Filtrando e processando %d...\n", ano))
  
  temp_limpo <- temp_bruto %>%
    mutate(
      IDADE_ANOS = calcular_idade_anos(IDADE),
      ANO_OBITO = as.numeric(substr(DTOBITO, nchar(DTOBITO)-3, nchar(DTOBITO))),
      SEXO_DESC = rot_sexo(SEXO),
      RACA_DESC = rot_raca(RACACOR),
      CAPITULO_CID = substr(CAUSABAS, 1, 1)
    ) %>%
    filter(!is.na(IDADE_ANOS) & IDADE_ANOS < 18)
  
  lista_anos_processados[[as.character(ano)]] <- temp_limpo
  
  # Força limpeza de RAM
  rm(temp_bruto, temp_limpo)
  gc() 
}

cat("\nJuntando todos os anos em um único dataset...\n")
sim_limpo <- bind_rows(lista_anos_processados)
rm(lista_anos_processados)
gc()

# 5) Divisão das Bases ---------------------------------------------------------
base_0_a_6 <- sim_limpo %>% filter(IDADE_ANOS <= 6)

# 6) Salvando os Arquivos (Tudo na pasta Dados) --------------------------------
cat("Salvando bases no disco (RDS para R, CSV para Excel/Python)...\n")

# Base Geral (< 18)
saveRDS(sim_limpo, file.path(dir_dados, "sim_brasil_menores_18_todas_vars_2015_2024.rds"))
write_csv(sim_limpo, file.path(dir_dados, "sim_brasil_menores_18_todas_vars_2015_2024.csv"))

# Base 0 a 6 anos
saveRDS(base_0_a_6, file.path(dir_dados, "sim_brasil_0_a_6_anos_todas_vars_2015_2024.rds"))
write_csv(base_0_a_6, file.path(dir_dados, "sim_brasil_0_a_6_anos_todas_vars_2015_2024.csv"))

cat("\nETAPA 1 CONCLUÍDA! Arquivos prontos e salvos na pasta Dados.\n")
