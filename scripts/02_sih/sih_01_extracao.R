############################################################
# SIH-RD | INTERNAÇÕES INFANTO-JUVENIS BRASIL (2015-2024)
# Etapa 1: Download em Lotes, Estruturação e Limpeza Inicial
# Destino: Base para Análises
############################################################

# 1) Pacotes -------------------------------------------------------------------
pacotes <- c("microdatasus", "dplyr", "stringr", "readr", "purrr")

for (p in pacotes) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

library(microdatasus)
library(dplyr)
library(stringr)
library(readr)
library(purrr) 

# 2) Parâmetros e Diretórios ---------------------------------------------------
dir_dados <- "xx"
dir_analises <- "xx"

dir_geral <- file.path(dir_analises, "Geral_Menor_18")
dir_0_a_6 <- file.path(dir_analises, "Subpop_0_6_Anos")
dir_temp <- file.path(dir_dados, "Temporarios_UF_Ano") # Pasta para salvar os chunks

# Criando estrutura de pastas
for (dir in c(dir_dados, dir_geral, dir_0_a_6, dir_temp)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

ufs_brasil <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# 3) Download e Limpeza em Lotes (Por Ano e UF) --------------------------------
cat("Iniciando processamento em lote do SIH-RD Brasil (2015-2024)...\n")
cat("Estratégia: Baixando por UF e Ano para preservar a RAM.\n")

for (ano in 2015:2024) {
  for (uf_atual in ufs_brasil) {
    
    # Define o nome do arquivo temporário para este lote
    arquivo_lote <- file.path(dir_temp, sprintf("sih_%s_%d.rds", uf_atual, ano))
    
    # Se o arquivo já existe, pula (útil caso o script seja interrompido e você precise rodar de novo)
    if (file.exists(arquivo_lote)) {
      cat(sprintf("-> Lote %s %d já processado. Pulando...\n", uf_atual, ano))
      next
    }
    
    cat(sprintf("\n>>> Baixando dados: UF %s | Ano %d...\n", uf_atual, ano))
    
    # TryCatch evita que o loop quebre se o DataSUS falhar em um mês/UF específico
    temp_bruto <- tryCatch({
      fetch_datasus(
        year_start = ano, month_start = 1,
        year_end = ano, month_end = 12,
        uf = uf_atual, 
        information_system = "SIH-RD"
      )
    }, error = function(e) {
      cat("Erro na extração. Retornando vazio...\n")
      return(NULL)
    })
    
    if (is.null(temp_bruto) || nrow(temp_bruto) == 0) next
    
    cat("Filtrando e limpando...\n")
    
    # Processamento SIH-RD e criação das variáveis
    temp_limpo <- temp_bruto %>%
      mutate(
        IDADE = as.numeric(IDADE),
        # Lógica de idade do SIH: 2 = Dias, 3 = Meses, 4 = Anos, 5 = > 100 anos
        IDADE_ANOS = case_when(
          COD_IDADE %in% c("2", "3") ~ 0, # Menores de 1 ano
          COD_IDADE == "4" ~ IDADE,
          COD_IDADE == "5" ~ 100 + IDADE,
          TRUE ~ NA_real_
        ),
        # Data de internação no SIH costuma vir como YYYYMMDD
        ANO_INTER = as.numeric(substr(DT_INTER, 1, 4)),
        # Ajuste de sexo para o padrão SIH (M/F ou 1/3)
        SEXO_DESC = case_when(
          SEXO %in% c("1", "M") ~ "Masculino", 
          SEXO %in% c("3", "F") ~ "Feminino", 
          TRUE ~ "Ignorado/Outro"
        ),
        # Ajuste de Raça/Cor do SIH
        RACA_DESC = case_when(
          RACA_COR == "01" ~ "Branca", RACA_COR == "02" ~ "Preta", 
          RACA_COR == "03" ~ "Parda", RACA_COR == "04" ~ "Amarela", 
          RACA_COR == "05" ~ "Indígena", TRUE ~ "Sem informação"
        ),
        CAPITULO_CID = substr(DIAG_PRINC, 1, 1)
      ) %>%
      filter(!is.na(IDADE_ANOS) & IDADE_ANOS < 18)
    
    # Salva o lote em disco e limpa a RAM imediatamente
    saveRDS(temp_limpo, arquivo_lote)
    
    rm(temp_bruto, temp_limpo)
    gc() # Força o R a liberar memória
  }
}

# 4) Consolidação Final --------------------------------------------------------
cat("\nProcesso de download concluído. Juntando todos os lotes temporários...\n")

# Lista todos os arquivos .rds na pasta temporária
arquivos_rds <- list.files(dir_temp, pattern = "\\.rds$", full.names = TRUE)

# Lê e empilha tudo de forma rápida e otimizada (purrr::map_dfr é excelente para isso)
sih_limpo <- map_dfr(arquivos_rds, readRDS)

# 5) Divisão das Bases ---------------------------------------------------------
base_0_a_6 <- sih_limpo %>% filter(IDADE_ANOS <= 6)

# 6) Salvando os Arquivos ------------------------------------------------------
cat("Salvando bases consolidadas no disco...\n")

# Base Geral (< 18)
saveRDS(sih_limpo, file.path(dir_dados, "sih_brasil_menores_18_todas_vars_2015_2024.rds"))
write_csv(sih_limpo, file.path(dir_dados, "sih_brasil_menores_18_todas_vars_2015_2024.csv"))

# Base 0 a 6 anos
saveRDS(base_0_a_6, file.path(dir_dados, "sih_brasil_0_a_6_anos_todas_vars_2015_2024.rds"))
write_csv(base_0_a_6, file.path(dir_dados, "sih_brasil_0_a_6_anos_todas_vars_2015_2024.csv"))

cat("\nETAPA 1 CONCLUÍDA! Arquivos prontos e salvos na pasta Dados.\n")
# Opcional: Você pode apagar a pasta Temporarios_UF_Ano depois para liberar espaço no disco
# unlink(dir_temp, recursive = TRUE)

# PACOTES
library(dplyr)
library(readr)

# 1) Diretórios
dir_dados <- "G:/Meu Drive/INSPER/Trabalho/SIH/Dados"
dir_temp <- file.path(dir_dados, "Temporarios_UF_Ano")

arquivos_rds <- list.files(dir_temp, pattern = "\\.rds$", full.names = TRUE)
arquivo_csv_geral <- file.path(dir_dados, "sih_brasil_menores_18_todas_vars_2015_2024.csv")
arquivo_csv_0_a_6 <- file.path(dir_dados, "sih_brasil_0_a_6_anos_todas_vars_2015_2024.csv")

# Zera os arquivos para não duplicar o que foi feito na tentativa anterior
if (file.exists(arquivo_csv_geral)) file.remove(arquivo_csv_geral)
if (file.exists(arquivo_csv_0_a_6)) file.remove(arquivo_csv_0_a_6)

cat("Iniciando a união BLINDADA contra arquivos corrompidos...\n")

arquivos_corrompidos <- c() # Lista para anotar os que deram pau

# 2) Loop Salva-Vidas com TryCatch
for (i in seq_along(arquivos_rds)) {
  
  cat(sprintf("Processando %d de %d: %s...\n", i, length(arquivos_rds), basename(arquivos_rds[i])))
  
  # Tenta ler o arquivo. Se der erro, ele captura e pula, sem quebrar o script.
  temp_df <- tryCatch({
    readRDS(arquivos_rds[i])
  }, error = function(e) {
    cat(sprintf("⚠️ ERRO CAPTURADO: O arquivo %s está corrompido. Pulando...\n", basename(arquivos_rds[i])))
    return(NULL) # Retorna vazio para a gente saber que falhou
  })
  
  # Se retornou vazio, anota o nome do infeliz e pula pro próximo loop
  if (is.null(temp_df)) {
    arquivos_corrompidos <- c(arquivos_corrompidos, arquivos_rds[i])
    next
  }
  
  # Verifica se o arquivo final já existe (se não existir, é porque é o primeiro arquivo válido, então ele cria o cabeçalho)
  existe_geral <- file.exists(arquivo_csv_geral)
  write_csv(temp_df, arquivo_csv_geral, append = existe_geral)
  
  # ---------------------------------------------------------
  # FILTRA E SALVA NA BASE 0 A 6 ANOS
  temp_0_a_6 <- temp_df %>% filter(IDADE_ANOS <= 6)
  
  if (nrow(temp_0_a_6) > 0) {
    existe_0_a_6 <- file.exists(arquivo_csv_0_a_6)
    write_csv(temp_0_a_6, arquivo_csv_0_a_6, append = existe_0_a_6)
  }
  
  # Limpa a RAM imediatamente
  rm(temp_df, temp_0_a_6)
  gc()
}

cat("\n================ RESUMO DA ÓPERA ================\n")
if(length(arquivos_corrompidos) > 0) {
  cat("Consolidação concluída, MAS os seguintes arquivos estavam corrompidos e ficaram de fora:\n")
  print(basename(arquivos_corrompidos))
  cat("Depois você pode excluir esses rds na mão e rodar o script de download só pra eles.\n")
} else {
  cat("Tudo processado sem erros! Bases consolidadas com sucesso.\n")
}
