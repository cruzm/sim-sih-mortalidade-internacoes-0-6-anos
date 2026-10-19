# ============================================================================
# 01_baixar_populacao.R  (v4 — caminho e layout VERIFICADOS no site do IBGE)
# Denominadores populacionais reais por UF, ano e idade — 0 a 6 anos
# Observatório de Saúde Infantil | INSPER · Hospital Pequeno Príncipe
#
# Gera `populacao_uf_faixa.csv`, usado pelo app como denominador das taxas de
# 1 a 6 anos. Sem ele o app deixa essas taxas em branco — não estima nada.
#
# FONTE (conferida no navegador, não chutada)
#   https://ftp.ibge.gov.br/Projecao_da_Populacao/Projecao_da_Populacao_2024/
#     projecoes_2024_tab1_idade_simples.xlsx        (~4,6 MB)
#   Projeções da População, Revisão 2024 — Brasil, Grandes Regiões e UFs,
#   por sexo e IDADE SIMPLES, 2000 a 2070. Aba única "1) POP_IDADE SIMPLES".
#
# POR QUE AS VERSÕES ANTERIORES FALHARAM
#   v1/v2: o caminho "Projecoes_da_Populacao" não existe — a pasta é
#          "Projecao_da_Populacao" (singular). Daí o HTTP 404.
#   v3: tentei descobrir a pasta raspando o índice, mas ftp.ibge.gov.br é uma
#       página renderizada por JavaScript; o httr recebe 200 com zero links.
#       E o agregado 7358 da API não cobre 2015–2024 — voltava vazio.
#   Agora o caminho é fixo e verificado, e o parser foi escrito contra o
#   layout real do arquivo.
#
# ARMADILHAS DO ARQUIVO, JÁ TRATADAS
#   · A linha 3 é o título "População por sexo e idade simples - 2000/2070".
#     Contém "idade" e um ano, e enganaria uma busca ingênua de cabeçalho.
#     O cabeçalho real é a linha 5:
#       IDADE | SEXO | CÓD. | SIGLA | LOCAL | 2000 | 2001 | ...
#   · Existe coluna SEXO com "Ambos", "Homens" e "Mulheres". Sem filtrar por
#     "Ambos", a população sairia dobrada.
#   · A coluna SIGLA traz BR, as cinco regiões (NO, ND, SD, SU, CO) e as 27
#     UFs. Atenção: "SE" é Sergipe e Sudeste é "SD" — por isso filtramos por
#     lista branca das 27 siglas, nunca por exclusão de regiões.
#
# CONFERÊNCIA (valores lidos do próprio arquivo)
#   Soma das 27 UFs, idades 1 a 6, "Ambos":
#     2015 = 17.490.990   ·   2024 = 16.301.745
#   Batem exatamente com a linha do Brasil no arquivo.
#
# COMO RODAR
#   Rscript 01_baixar_populacao.R
# ============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

CONFIG <- list(
  anos    = 2015:2024,
  saida   = "populacao_uf_faixa.csv",
  cache   = "dados/IBGE",
  arquivo = "projecoes_2024_tab1_idade_simples.xlsx",
  url     = paste0("https://ftp.ibge.gov.br/Projecao_da_Populacao/",
                   "Projecao_da_Populacao_2024/projecoes_2024_tab1_idade_simples.xlsx"),
  # Referências de conferência, extraídas do próprio arquivo.
  ref     = c(`2015` = 17490990, `2024` = 16301745)
)

dir.create(CONFIG$cache, recursive = TRUE, showWarnings = FALSE)

precisa <- function(p) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cloud.r-project.org")
  if (!requireNamespace(p, quietly = TRUE)) stop("Não consegui instalar ", p)
}
precisa("readxl"); precisa("dplyr"); precisa("tidyr")
suppressPackageStartupMessages({library(dplyr); library(tidyr)})

grava_csv <- function(df, path) {
  con <- file(path, open = "w", encoding = "UTF-8"); on.exit(close(con))
  utils::write.csv(df, con, row.names = FALSE, fileEncoding = "")
}

# As 27 UFs. Lista branca: descarta BR e as regiões sem confundir SE (Sergipe)
# com Sudeste (SD).
UFS <- c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE","AL",
         "SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF")

# ----------------------------------------------------------------------------
# 1. Download (reaproveita o arquivo se já estiver na pasta)
# ----------------------------------------------------------------------------
destino <- file.path(CONFIG$cache, CONFIG$arquivo)

# Reaproveita o arquivo local só se ele realmente abrir — assim um download
# interrompido pela metade não passa como válido.
abre_ok <- function(f) {
  file.exists(f) && file.size(f) > 5e4 &&
    !inherits(tryCatch(readxl::excel_sheets(f), error = function(e) e), "error")
}

if (abre_ok(destino)) {
  message("Usando o arquivo já baixado: ", destino,
          " (", round(file.size(destino) / 1e6, 2), " MB)")
} else {
  message("Baixando ", CONFIG$url)
  message("(~4,6 MB — pode levar um minuto)")
  ok <- tryCatch({
    utils::download.file(CONFIG$url, destino, mode = "wb", method = "libcurl")
    abre_ok(destino)
  }, error = function(e) { message("Erro: ", conditionMessage(e)); FALSE },
     warning = function(w) { message("Aviso: ", conditionMessage(w)); FALSE })

  if (!ok) {
    unlink(destino)
    cat("\n--------------------------------------------------------------\n",
        "O DOWNLOAD FALHOU. Faça manualmente, leva 1 minuto:\n\n",
        "  1. Abra no navegador:\n     ", CONFIG$url, "\n\n",
        "  2. Salve o arquivo em:\n     ",
        normalizePath(CONFIG$cache, mustWork = FALSE), "\n\n",
        "  3. Rode este script de novo — ele detecta o arquivo e segue.\n",
        "--------------------------------------------------------------\n", sep = "")
    quit(status = 1)
  }
}

# ----------------------------------------------------------------------------
# 2. Leitura e parsing do layout verificado
# ----------------------------------------------------------------------------
message("\nLendo a planilha ...")
abas <- readxl::excel_sheets(destino)
message("  abas: ", paste(abas, collapse = " · "))
aba <- abas[grepl("idade", abas, ignore.case = TRUE)][1]
if (is.na(aba)) aba <- abas[1]

bruto <- readxl::read_excel(destino, sheet = aba, col_names = FALSE,
                            .name_repair = "minimal")
bruto <- as.data.frame(bruto, stringsAsFactors = FALSE)
message("  dimensões brutas: ", nrow(bruto), " x ", ncol(bruto))

# Cabeçalho de verdade: linha com uma célula exatamente "IDADE" E pelo menos
# três células que sejam exatamente um ano de 4 dígitos. O título da linha 3
# não passa nesse critério — que é justamente o ponto.
eh_ano <- function(v) grepl("^\\s*(19|20)[0-9]{2}(\\.0+)?\\s*$", v)
lin <- NA_integer_
for (i in seq_len(min(40, nrow(bruto)))) {
  cel <- trimws(as.character(unlist(bruto[i, ])))
  cel <- cel[!is.na(cel) & cel != "NA"]
  if (any(grepl("^idade$", cel, ignore.case = TRUE)) && sum(eh_ano(cel)) >= 3) {
    lin <- i; break
  }
}
if (is.na(lin))
  stop("Não localizei a linha de cabeçalho. O layout do IBGE deve ter mudado.")
message("  cabeçalho na linha ", lin)

cab <- trimws(as.character(unlist(bruto[lin, ])))
dados <- bruto[(lin + 1):nrow(bruto), , drop = FALSE]
names(dados) <- ifelse(is.na(cab) | cab == "" | cab == "NA",
                       paste0("V", seq_along(cab)), cab)

acha <- function(rx) {
  i <- which(grepl(rx, names(dados), ignore.case = TRUE))[1]
  if (is.na(i)) NA_character_ else names(dados)[i]
}
c_idade <- acha("^idade$"); c_sexo <- acha("^sexo$"); c_sigla <- acha("^sigla$")
if (is.na(c_idade) || is.na(c_sigla))
  stop("Colunas IDADE/SIGLA não encontradas. Lidas: ",
       paste(head(names(dados), 12), collapse = ", "))

anos_col <- names(dados)[eh_ano(names(dados))]
message("  colunas: idade='", c_idade, "' sexo='", c_sexo %||% "-",
        "' sigla='", c_sigla, "' · ", length(anos_col), " colunas de ano")
if (!length(anos_col)) stop("Nenhuma coluna de ano identificada.")

d <- dados
d$..sigla <- toupper(trimws(as.character(d[[c_sigla]])))
d$..idade <- suppressWarnings(as.integer(gsub("\\D", "", as.character(d[[c_idade]]))))

# Filtro "Ambos" — sem isso a população sairia dobrada.
if (!is.na(c_sexo)) {
  sexos <- unique(trimws(as.character(d[[c_sexo]])))
  message("  valores de SEXO: ", paste(utils::head(stats::na.omit(sexos), 5),
                                       collapse = " · "))
  d <- d[grepl("^ambos", trimws(as.character(d[[c_sexo]])), ignore.case = TRUE), ,
         drop = FALSE]
  message("  linhas após filtrar SEXO = 'Ambos': ", nrow(d))
}

d <- d[d$..sigla %in% UFS & !is.na(d$..idade) & d$..idade <= 6, , drop = FALSE]
message("  linhas de UF com idade 0–6: ", nrow(d))
if (nrow(d) == 0) stop("Nenhuma linha sobrou após os filtros.")

# Como o cabeçalho real fica na linha 5, o readxl lê as colunas de ano com
# tipos misturados (texto nas linhas de título, número nas de dado). Coerção
# explícita antes do pivot, senão o vctrs recusa combinar character com double.
d[anos_col] <- lapply(d[anos_col], function(x)
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x)))))

pop <- d %>%
  select(sigla = ..sigla, idade = ..idade, all_of(anos_col)) %>%
  pivot_longer(-c(sigla, idade), names_to = "ano", values_to = "populacao") %>%
  mutate(ano = suppressWarnings(as.integer(sub("\\..*$", "", ano))),
         populacao = suppressWarnings(as.numeric(populacao))) %>%
  filter(!is.na(ano), !is.na(populacao), populacao > 0)

# ----------------------------------------------------------------------------
# 3. Validações — melhor parar do que gravar um denominador errado
# ----------------------------------------------------------------------------
falta <- setdiff(0:6, unique(pop$idade))
if (length(falta))
  stop("Faltaram idades: ", paste(falta, collapse = ", "),
       ". Denominador incompleto — não vou gravar.")

n_uf <- dplyr::n_distinct(pop$sigla)
if (n_uf != 27) stop("Esperava 27 UFs, encontrei ", n_uf, ". Não vou gravar.")

anos_ok <- intersect(CONFIG$anos, unique(pop$ano))
if (!length(anos_ok))
  stop("A planilha não cobre nenhum ano entre ", min(CONFIG$anos), " e ",
       max(CONFIG$anos), ".")

conf <- pop %>% filter(idade >= 1, idade <= 6) %>%
  group_by(ano) %>% summarise(total = sum(populacao), .groups = "drop")

cat("\nConferência contra os valores lidos do arquivo original do IBGE:\n")
for (a in names(CONFIG$ref)) {
  ai <- as.integer(a)
  if (ai %in% conf$ano) {
    obtido <- conf$total[conf$ano == ai]
    esperado <- unname(CONFIG$ref[a])
    dif <- abs(obtido - esperado) / esperado
    cat(sprintf("  %s: obtido %s · esperado %s · diferença %.4f%%\n", a,
                format(obtido, big.mark = "."), format(esperado, big.mark = "."),
                100 * dif))
    if (dif > 0.01)
      stop("O total de 1 a 6 anos em ", a, " ficou ", round(100 * dif, 1),
           "% fora do valor conferido. Não vou gravar um denominador suspeito.")
  }
}

# ----------------------------------------------------------------------------
# 4. Saída
# ----------------------------------------------------------------------------
saida <- pop %>%
  filter(ano %in% CONFIG$anos) %>%
  mutate(faixa = ifelse(idade == 0, "menores de 1 ano", "1 a 6 anos")) %>%
  group_by(sigla, ano, faixa) %>%
  summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop") %>%
  arrange(sigla, ano, faixa)

grava_csv(saida, CONFIG$saida)

message("\nGravado: ", CONFIG$saida, " (", nrow(saida), " linhas)")
message("UFs: ", dplyr::n_distinct(saida$sigla), " · anos: ",
        paste(range(saida$ano), collapse = "–"))
cat("\nPopulação de 1 a 6 anos, Brasil (soma das 27 UFs):\n")
print(as.data.frame(saida %>% filter(faixa == "1 a 6 anos") %>%
                      group_by(ano) %>%
                      summarise(pop_1a6 = sum(populacao), .groups = "drop")))
cat("\nCopie ", CONFIG$saida, " para a pasta do app e recarregue o Shiny.\n", sep = "")
