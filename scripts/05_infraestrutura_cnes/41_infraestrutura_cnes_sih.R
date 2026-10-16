# =============================================================================
# INFRAESTRUTURA HOSPITALAR PEDIÁTRICA (CNES) × INTERNAÇÕES (SIH) × ÓBITOS (SIM)
# Crianças de 0 a 6 anos · Brasil · 2015, 2019 e 2024
#
# Pergunta: onde as crianças internam e morrem, e esses hospitais têm
# estrutura para recebê-las? (leito pediátrico, UTI neonatal/pediátrica,
# habilitação de referência)
#
# ENTRADAS (todas geradas por scripts anteriores do projeto)
#   DIR_CNES/LT_UF_AAAA.rds   leitos por estabelecimento (CNES-LT, competência dez)
#   DIR_CNES/HB_UF_AAAA.rds   habilitações por estabelecimento (CNES-HB, dez)
#   DIR_CNES/_cadger.rds      nome fantasia dos estabelecimentos
#   DIR_DASH/dados/sih_por_cnes.rds   AIH 0-6 por CNES × ano × faixa × desfecho
#                                     (PARTE 1 do 10_cnes_sia.R)
#   DIR_DASH/populacao_uf_faixa.csv   população por UF/ano (01_baixar_populacao.R)
#   DIR_REPO/bases_finais/obitos_linkados/*.csv.gz   óbitos SIM 0-6 linkados
#   DIR_REPO/bases_finais/resumo_nascidos_vivos_aih.csv   nascidos vivos por UF
#
# SAÍDAS (DIR_OUT)
#   Tabelas_infraestrutura_CNES_SIH.xlsx  (uma aba por tabela)
#   hospitais_nivel_pediatrico_2024.csv   (um hospital por linha)
#   Fig_infra_*.png
#   resumo_infraestrutura.txt             (números citados no texto)
#
# CLASSIFICAÇÃO DOS HOSPITAIS (competência dez/2024)
#   N1 Referência pediátrica de alta complexidade: UTI neonatal ou pediátrica
#      HABILITADA no SUS + ao menos uma habilitação de alta complexidade com
#      foco materno-infantil (cirurgia cardiovascular pediátrica, oncologia
#      pediátrica, neurocirurgia, gestação de alto risco tipo 2/terciária).
#   N2 UTI neonatal/pediátrica habilitada (sem as habilitações acima).
#   N3 Tem leito de UTI neonatal/pediátrica no cadastro, mas sem habilitação
#      SUS (em geral leito privado/filantrópico não credenciado).
#   N4 Só leito pediátrico clínico/cirúrgico ou cuidado intermediário (UCI).
#   N5 Nenhum leito pediátrico: criança interna em leito geral.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(openxlsx)
})

# ---- caminhos (ajuste para a sua máquina) -----------------------------------
DIR_DASH <- Sys.getenv("DIR_DASH", "G:/My Drive/INSPER/Trabalho/Dash")
DIR_CNES <- Sys.getenv("DIR_CNES", file.path(DIR_DASH, "dados", "cnes_bruto"))
DIR_REPO <- Sys.getenv("DIR_REPO", ".")
DIR_OUT  <- Sys.getenv("DIR_OUT",  file.path(DIR_REPO, "outputs", "infraestrutura"))
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

ANOS <- c(2015, 2019, 2024)
UFS  <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT",
          "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
REGIAO <- c(AC="Norte",AM="Norte",AP="Norte",PA="Norte",RO="Norte",RR="Norte",
            TO="Norte",AL="Nordeste",BA="Nordeste",CE="Nordeste",MA="Nordeste",
            PB="Nordeste",PE="Nordeste",PI="Nordeste",RN="Nordeste",SE="Nordeste",
            DF="Centro-Oeste",GO="Centro-Oeste",MS="Centro-Oeste",MT="Centro-Oeste",
            ES="Sudeste",MG="Sudeste",RJ="Sudeste",SP="Sudeste",
            PR="Sul",RS="Sul",SC="Sul")
COD_UF <- c(RO=11,AC=12,AM=13,RR=14,PA=15,AP=16,TO=17,MA=21,PI=22,CE=23,RN=24,
            PB=25,PE=26,AL=27,SE=28,BA=29,MG=31,ES=32,RJ=33,SP=35,PR=41,SC=42,
            RS=43,MS=50,MT=51,GO=52,DF=53)

# ---- dicionários ------------------------------------------------------------
# Leitos (par TP_LEITO-CODLEITO, tabela de domínio do CNES)
LEITO_GRUPO <- c(
  "5-45" = "Pediatria clínica", "5-68" = "Pediatria cirúrgica",
  "3-80" = "UTI neonatal", "3-81" = "UTI neonatal", "3-82" = "UTI neonatal",
  "3-63" = "UTI neonatal",
  "3-77" = "UTI pediátrica", "3-78" = "UTI pediátrica", "3-79" = "UTI pediátrica",
  "3-62" = "UTI pediátrica", "3-52" = "UTI pediátrica",
  "3-92" = "UCI neonatal", "3-93" = "UCI neonatal", "3-65" = "UCI neonatal",
  "3-94" = "UCI pediátrica")
# Habilitações (tabela de domínio CNES-HB)
HAB_UTI_NEO <- c("2602","2605","2610","2611","2697")
HAB_UTI_PED <- c("2603","2606","2698","2613")
HAB_UCI_NEO <- c("2801","2802","2803")
HAB_ALTA <- c(
  "0804" = "Cirurgia cardiovascular pediátrica",
  "1709" = "Oncologia pediátrica", "1711" = "Oncologia pediátrica",
  "1713" = "Oncologia pediátrica",
  "1601" = "Neurocirurgia/neurologia", "1602" = "Neurocirurgia/neurologia",
  "1402" = "Gestação de alto risco (terciária/tipo 2)",
  "1414" = "Gestação de alto risco (terciária/tipo 2)")
HAB_OUTRAS <- c("1404" = "Hospital Amigo da Criança", "1416" = "Hospital Amigo da Criança",
                "1413" = "Gestação de alto risco tipo 1",
                "2701" = "Urgência tipo I-III", "2702" = "Urgência tipo I-III",
                "2703" = "Urgência tipo I-III")
NIVEIS <- c("N1 Referência de alta complexidade", "N2 UTI neo/ped habilitada",
            "N3 UTI neo/ped sem habilitação SUS", "N4 Só leito pediátrico/UCI",
            "N5 Sem leito pediátrico")

le <- function(f) suppressWarnings(readRDS(f))

# ---- 1. LEITOS ----------------------------------------------------------------
leitos <- rbindlist(lapply(ANOS, function(a) rbindlist(lapply(UFS, function(u) {
  f <- file.path(DIR_CNES, sprintf("LT_%s_%d.rds", u, a))
  if (!file.exists(f)) { warning("faltou ", f); return(NULL) }
  x <- as.data.table(le(f))
  x[, .(cnes = CNES, uf = u, ano = a, tp_unid = TP_UNID, munic = CODUFMUN,
        chave = paste0(TP_LEITO, "-", sprintf("%02d", as.integer(CODLEITO))),
        exist = as.numeric(QT_EXIST), sus = as.numeric(QT_SUS))]
}))))
leitos[, grupo := LEITO_GRUPO[chave]]

# ---- 2. HABILITAÇÕES -----------------------------------------------------------
hab <- rbindlist(lapply(ANOS, function(a) rbindlist(lapply(UFS, function(u) {
  f <- file.path(DIR_CNES, sprintf("HB_%s_%d.rds", u, a))
  if (!file.exists(f)) { warning("faltou ", f); return(NULL) }
  x <- as.data.table(le(f))
  x[, .(cnes = CNES, uf = u, ano = a, hab = SGRUPHAB,
        nuleitos = as.numeric(NULEITOS))]
}))))

# ---- 3. PERFIL POR HOSPITAL × ANO -------------------------------------------
GRUPO_COL <- c("Pediatria clínica" = "Pediatria_clinica",
               "Pediatria cirúrgica" = "Pediatria_cirurgica",
               "UTI neonatal" = "UTI_neonatal", "UTI pediátrica" = "UTI_pediatrica",
               "UCI neonatal" = "UCI_neonatal", "UCI pediátrica" = "UCI_pediatrica")
leitos[, gcol := GRUPO_COL[grupo]]
lt_h <- dcast(leitos[!is.na(gcol)], cnes + ano ~ gcol,
              value.var = c("exist", "sus"), fun.aggregate = sum, fill = 0)
tot_h <- leitos[, .(uf = uf[1], munic = munic[1], tp_unid = tp_unid[1],
                    leitos_total = sum(exist), leitos_sus = sum(sus)),
                by = .(cnes, ano)]
hb_h <- hab[, .(
  hab_uti_neo = any(hab %in% HAB_UTI_NEO), hab_uti_ped = any(hab %in% HAB_UTI_PED),
  hab_uci_neo = any(hab %in% HAB_UCI_NEO),
  hab_cardio_ped = any(hab == "0804"),
  hab_onco_ped = any(hab %in% c("1709","1711","1713")),
  hab_neuro = any(hab %in% c("1601","1602")),
  hab_gar = any(hab %in% c("1402","1414")),
  hab_amigo = any(hab %in% c("1404","1416")),
  hab_urgencia = any(hab %in% c("2701","2702","2703")),
  leitos_hab_utin = sum(nuleitos[hab %in% HAB_UTI_NEO]),
  leitos_hab_utip = sum(nuleitos[hab %in% HAB_UTI_PED]),
  habs_alta = paste(sort(unique(HAB_ALTA[hab[hab %in% names(HAB_ALTA)]])), collapse = "; ")
), by = .(cnes, ano)]

hosp <- merge(tot_h, lt_h, by = c("cnes", "ano"), all.x = TRUE)
hosp <- merge(hosp, hb_h, by = c("cnes", "ano"), all.x = TRUE)
num <- names(hosp)[sapply(hosp, is.numeric) & names(hosp) != "ano"]
for (v in num) set(hosp, which(is.na(hosp[[v]])), v, 0)
lg <- names(hosp)[sapply(hosp, is.logical)]
for (v in lg) set(hosp, which(is.na(hosp[[v]])), v, FALSE)
hosp[is.na(habs_alta), habs_alta := ""]
for (g in c("UTI_neonatal","UTI_pediatrica","UCI_neonatal","UCI_pediatrica",
            "Pediatria_clinica","Pediatria_cirurgica"))
  for (p in c("exist_","sus_")) if (!paste0(p, g) %in% names(hosp)) hosp[, (paste0(p, g)) := 0]

hosp[, `:=`(
  uti_np  = exist_UTI_neonatal + exist_UTI_pediatrica,
  ped_tot = exist_Pediatria_clinica + exist_Pediatria_cirurgica,
  uci_np  = exist_UCI_neonatal + exist_UCI_pediatrica)]
hosp[, nivel := fcase(
  (hab_uti_neo | hab_uti_ped) & (hab_cardio_ped | hab_onco_ped | hab_neuro | hab_gar), NIVEIS[1],
  (hab_uti_neo | hab_uti_ped), NIVEIS[2],
  uti_np > 0, NIVEIS[3],
  ped_tot > 0 | uci_np > 0, NIVEIS[4],
  default = NIVEIS[5])]
hosp[, nivel := factor(nivel, levels = NIVEIS)]
hosp[, regiao := REGIAO[uf]]

# ---- 4. SIH: AIH 0-6 POR HOSPITAL ---------------------------------------------
sih <- as.data.table(readRDS(file.path(DIR_DASH, "dados", "sih_por_cnes.rds")))
sih[, faixa := fcase(grepl("^Neonatal", faixa), "Neonatal (0-27 d)",
                     grepl("^Pos", faixa), "Pós-neonatal (28 d-<1 a)",
                     default = "1 a 6 anos")]
FAIXAS <- c("Neonatal (0-27 d)", "Pós-neonatal (28 d-<1 a)", "1 a 6 anos")
sih[, faixa := factor(faixa, levels = FAIXAS)]
sih_y <- sih[ano %in% ANOS]
sih_y <- merge(sih_y, hosp[, .(cnes, ano, nivel)], by = c("cnes", "ano"), all.x = TRUE)
sih_y[is.na(nivel), nivel := "Sem cadastro de leito no CNES"]

# ---- 5. SIM: óbitos 0-6 por estabelecimento ------------------------------------
sim <- rbindlist(lapply(ANOS, function(a) {
  f <- file.path(DIR_REPO, "bases_finais", "obitos_linkados",
                 sprintf("obitos_0_6_linkados_%d.csv.gz", a))
  fread(f, select = c("ano","faixa","uf_res","munic_res","munic_ocor","lococor",
                      "codestab","pareado","universo"),
        colClasses = list(character = c("codestab","munic_res","munic_ocor")))
}))
sim[, codestab := sprintf("%07d", as.integer(codestab))]
sim[, faixa3 := fcase(grepl("^Neonatal", faixa), "Neonatal (0-27 d)",
                      grepl("^Pos", faixa), "Pós-neonatal (28 d-<1 a)",
                      default = "1 a 6 anos")]
sim[, faixa3 := factor(faixa3, levels = FAIXAS)]
sim[, local := fcase(lococor == 1, "Hospital", lococor == 2, "Outro estab. de saúde",
                     lococor == 3, "Domicílio", default = "Via pública/outros")]
sim <- merge(sim, hosp[, .(codestab = cnes, ano, nivel)], by = c("codestab", "ano"),
             all.x = TRUE)
sim[local == "Hospital" & is.na(nivel), nivel := "Sem cadastro de leito no CNES"]

# ---- 6. POPULAÇÃO E NASCIDOS VIVOS ----------------------------------------------
pop <- fread(file.path(DIR_DASH, "populacao_uf_faixa.csv"))
pop <- pop[ano %in% ANOS, .(pop_0_6 = sum(populacao)), by = .(uf = sigla, ano)]
nv <- fread(file.path(DIR_REPO, "bases_finais", "resumo_nascidos_vivos_aih.csv"))
nv <- nv[ano_nasc %in% ANOS, .(nv = sum(nascidos_vivos)), by = .(uf = uf_res, ano = ano_nasc)]

# =============================================================================
# TABELAS
# =============================================================================
br <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")

# T1 — Evolução dos leitos pediátricos no Brasil -------------------------------
T1 <- leitos[!is.na(grupo), .(existentes = sum(exist), sus = sum(sus),
                              hospitais = uniqueN(cnes[exist > 0])), by = .(grupo, ano)]
T1 <- dcast(T1, grupo ~ ano, value.var = c("existentes", "sus", "hospitais"))
T1[, var_2015_2024 := round(100 * (existentes_2024 / existentes_2015 - 1), 1)]
T1[, pct_sus_2024 := round(100 * sus_2024 / existentes_2024, 1)]
ord <- c("Pediatria clínica","Pediatria cirúrgica","UCI neonatal","UCI pediátrica",
         "UTI neonatal","UTI pediátrica")
T1 <- T1[match(ord, grupo)]

# T2 — Hospitais por nível × ano ------------------------------------------------
T2 <- hosp[, .N, by = .(nivel, ano)]
T2 <- dcast(T2, nivel ~ ano, value.var = "N", fill = 0)

# T3 — Leitos por UF, 2024 (taxas) ----------------------------------------------
uf24 <- hosp[ano == 2024, .(
  ped_sus = sum(sus_Pediatria_clinica + sus_Pediatria_cirurgica),
  ped_tot = sum(ped_tot),
  utin_sus = sum(sus_UTI_neonatal), utin_tot = sum(exist_UTI_neonatal),
  utip_sus = sum(sus_UTI_pediatrica), utip_tot = sum(exist_UTI_pediatrica),
  ucin_tot = sum(exist_UCI_neonatal),
  hosp_ref = sum(nivel %in% NIVEIS[1:2])), by = uf]
uf24 <- merge(uf24, pop[ano == 2024, .(uf, pop_0_6)], by = "uf")
uf24 <- merge(uf24, nv[ano == 2024, .(uf, nv)], by = "uf")
uf24[, `:=`(regiao = REGIAO[uf],
            ped_por_10mil = 1e4 * ped_tot / pop_0_6,
            ped_sus_por_10mil = 1e4 * ped_sus / pop_0_6,
            utin_por_mil_nv = 1e3 * utin_tot / nv,
            utin_sus_por_mil_nv = 1e3 * utin_sus / nv,
            utip_por_10mil = 1e4 * utip_tot / pop_0_6)]
setorder(uf24, regiao, uf)

# T4 — Onde as crianças internam (SIH 2024 × nível) -----------------------------
T4 <- sih_y[ano == 2024, .(aih = sum(aih), obitos = sum(aih[desfecho == "Obito"]),
                           hospitais = uniqueN(cnes)), by = .(nivel, faixa)]
T4[, pct_aih_faixa := 100 * aih / sum(aih), by = faixa]
T4[, letalidade := 100 * obitos / aih]
T4[, pct_obitos_faixa := 100 * obitos / sum(obitos), by = faixa]
setorder(T4, faixa, nivel)

# T5 — Onde as crianças morrem (SIM 2024 × nível) -------------------------------
T5 <- sim[ano == 2024, .N, by = .(local, nivel, faixa3)]
T5[is.na(nivel), nivel := "—"]
T5[, pct_faixa := 100 * N / sum(N), by = faixa3]
setorder(T5, faixa3, local, nivel)

# T6 — Óbitos sem AIH por nível (universo SUS) ---------------------------------
T6 <- sim[ano == 2024 & local == "Hospital",
          .(obitos = .N, pareados_aih = sum(pareado %in% c(TRUE, "TRUE"))),
          by = .(nivel, faixa3)]
T6[, pct_sem_aih := 100 * (1 - pareados_aih / obitos)]
setorder(T6, faixa3, nivel)

# T7 — Acesso: o município de residência tem hospital N1/N2? -------------------
ref_mun <- unique(hosp[ano == 2024 & nivel %in% NIVEIS[1:2], .(munic = substr(munic, 1, 6))])
sim24 <- sim[ano == 2024 & !is.na(munic_res)]
sim24[, res_tem_ref := substr(munic_res, 1, 6) %in% ref_mun$munic]
sim24[, morreu_fora := substr(munic_ocor, 1, 6) != substr(munic_res, 1, 6)]
T7 <- sim24[, .(obitos = .N,
                pct_em_hosp_ref = 100 * mean(nivel %in% NIVEIS[1:2]),
                pct_fora_municipio = 100 * mean(morreu_fora, na.rm = TRUE),
                pct_fora_hospital = 100 * mean(local != "Hospital")),
            by = .(res_tem_ref, faixa3)]
setorder(T7, faixa3, -res_tem_ref)

# T8 — Neonatais por UF: % dos óbitos neonatais em hospital com UTIN habilitada
T8 <- sim[ano == 2024 & faixa3 == "Neonatal (0-27 d)",
          .(obitos_neo = .N,
            pct_hosp_ref = 100 * mean(nivel %in% NIVEIS[1:2]),
            pct_hosp_sem_uti = 100 * mean(nivel %in% c(NIVEIS[4:5],
                                            "Sem cadastro de leito no CNES")),
            pct_fora_hosp = 100 * mean(local != "Hospital")), by = .(uf = uf_res)]
T8 <- merge(T8, uf24[, .(uf, regiao, utin_por_mil_nv, utin_sus_por_mil_nv)], by = "uf")
setorder(T8, regiao, uf)

# T9 — Os 25 hospitais que mais internam crianças de 0-6 (2024) ---------------
cad <- as.data.table(le(file.path(DIR_CNES, "_cadger.rds")))[, .(cnes = codestab, nome = FANTASIA)]
top <- sih_y[ano == 2024, .(aih = sum(aih), aih_neo = sum(aih[faixa == FAIXAS[1]]),
                            obitos = sum(aih[desfecho == "Obito"])), by = cnes]
top <- merge(top, hosp[ano == 2024, .(cnes, uf, nivel, uti_neo = exist_UTI_neonatal,
                                       uti_ped = exist_UTI_pediatrica, ped_tot, habs_alta)],
             by = "cnes", all.x = TRUE)
top <- merge(top, cad, by = "cnes", all.x = TRUE)
setorder(top, -aih)
top[, pct_acum := 100 * cumsum(aih) / sum(aih)]
T9 <- top[1:25]
n50 <- top[pct_acum <= 50, .N] + 1

# Saída por hospital
fwrite(merge(hosp[ano == 2024], cad, by = "cnes", all.x = TRUE)[
  , c("cnes","nome","uf","regiao","munic","tp_unid","nivel","leitos_total","leitos_sus",
      grep("^(exist|sus)_", names(hosp), value = TRUE),
      grep("^hab_", names(hosp), value = TRUE), "habs_alta"), with = FALSE],
  file.path(DIR_OUT, "hospitais_nivel_pediatrico_2024.csv"))

# Excel
wb <- createWorkbook()
add <- function(nm, df, nota) {
  addWorksheet(wb, nm); writeData(wb, nm, nota, startRow = 1)
  writeData(wb, nm, df, startRow = 3); setColWidths(wb, nm, 1:ncol(df), "auto")
}
add("T1_leitos_Brasil", T1, "Leitos pediátricos e neonatais existentes e SUS, Brasil, dez/2015, dez/2019, dez/2024 (CNES-LT)")
add("T2_hospitais_nivel", T2, "Estabelecimentos com leito, por nível de complexidade pediátrica")
add("T3_leitos_UF_2024", uf24, "Leitos por UF, 2024: pediátricos por 10 mil crianças 0-6; UTIN por mil nascidos vivos")
add("T4_SIH_nivel_2024", T4, "AIH de 0-6 anos em 2024 por nível do hospital e faixa etária")
add("T5_SIM_local_2024", T5, "Óbitos de 0-6 anos em 2024 por local e nível do hospital de ocorrência")
add("T6_sem_AIH_nivel", T6, "Óbitos hospitalares 2024: % sem AIH pareada por nível do hospital")
add("T7_acesso_municipio", T7, "Óbitos 2024 conforme o município de residência ter (ou não) hospital N1/N2")
add("T8_neonatal_UF", T8, "Óbitos neonatais 2024 por UF de residência e oferta de UTIN")
add("T9_top25_hospitais", T9, "25 hospitais com mais AIH de 0-6 anos em 2024")
saveWorkbook(wb, file.path(DIR_OUT, "Tabelas_infraestrutura_CNES_SIH.xlsx"), overwrite = TRUE)
saveRDS(list(T1=T1,T2=T2,T3=uf24,T4=T4,T5=T5,T6=T6,T7=T7,T8=T8,T9=T9,n50=n50,top=top),
        file.path(DIR_OUT, "tabelas_infraestrutura.rds"))

# =============================================================================
# FIGURAS
# =============================================================================
COR <- c("#1f4e79", "#2e86c1", "#85c1e9", "#f5b041", "#cb4335", "#bdbdbd")
names(COR) <- c(NIVEIS, "Sem cadastro de leito no CNES")
tema <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title.position = "plot")

# Fig A — UTIN por mil NV por UF (2024), com referências 2 e 4/mil
fa <- copy(uf24)[order(utin_por_mil_nv)]
fa[, uf := factor(uf, levels = uf)]
pA <- ggplot(fa) +
  geom_col(aes(uf, utin_por_mil_nv, fill = regiao), width = .7) +
  geom_point(aes(uf, utin_sus_por_mil_nv), shape = 21, fill = "white", size = 2) +
  geom_hline(yintercept = c(2, 4), linetype = c("dashed", "dotted")) +
  coord_flip() + scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(x = NULL, y = "Leitos de UTI neonatal por mil nascidos vivos (barra = total; ponto = SUS)",
       caption = "Linha tracejada: 2 leitos/mil NV (Portaria MS 930/2012). Linha pontilhada: 4/mil (recomendação SBP).") + tema
ggsave(file.path(DIR_OUT, "Fig_infra_01_UTIN_por_UF.png"), pA, width = 7.5, height = 7, dpi = 200)

# Fig B — distribuição das AIH e dos óbitos hospitalares por nível, por faixa
fb <- T4[, .(nivel, faixa, `Internações (AIH)` = pct_aih_faixa,
             `Óbitos hospitalares (AIH)` = pct_obitos_faixa)]
fb <- melt(fb, id.vars = c("nivel", "faixa"))
fb[, nivel := factor(nivel, levels = names(COR))]
pB <- ggplot(fb, aes(faixa, value, fill = nivel)) +
  geom_col(width = .7) + facet_wrap(~variable) +
  scale_fill_manual(values = COR, name = NULL) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL, y = "% do total da faixa etária (2024)") + tema +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(file.path(DIR_OUT, "Fig_infra_02_AIH_obitos_por_nivel.png"), pB, width = 9, height = 5.5, dpi = 200)

# Fig C — evolução dos leitos 2015-2019-2024
fc <- leitos[!is.na(grupo), .(exist = sum(exist), sus = sum(sus)), by = .(grupo, ano)]
fc <- melt(fc, id.vars = c("grupo", "ano"))
fc[, variable := fifelse(variable == "exist", "Existentes", "SUS")]
pC <- ggplot(fc, aes(factor(ano), value, fill = variable)) +
  geom_col(position = "dodge", width = .7) + facet_wrap(~grupo, scales = "free_y") +
  scale_fill_manual(values = c("#1f4e79", "#85c1e9"), name = NULL) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Leitos") + tema
ggsave(file.path(DIR_OUT, "Fig_infra_03_evolucao_leitos.png"), pC, width = 8.5, height = 5.5, dpi = 200)

# =============================================================================
# RESUMO (números citados no texto)
# =============================================================================
s <- character()
p <- function(...) s <<- c(s, sprintf(...))
p("== LEITOS (Brasil) ==")
for (i in seq_len(nrow(T1))) with(T1[i], p("%s: %s (2015) -> %s (2019) -> %s (2024); var %s%%; SUS 2024 %s%%",
  grupo, br(existentes_2015,0), br(existentes_2019,0), br(existentes_2024,0),
  br(var_2015_2024), br(pct_sus_2024)))
p("UTIN/mil NV Brasil 2024: total %s, SUS %s", br(1e3*sum(uf24$utin_tot)/sum(uf24$nv),2),
  br(1e3*sum(uf24$utin_sus)/sum(uf24$nv),2))
p("UFs com UTIN total >=4/mil NV: %s", paste(uf24[utin_por_mil_nv >= 4, uf], collapse=", "))
p("UFs com UTIN total <2/mil NV: %s", paste(uf24[utin_por_mil_nv < 2, uf], collapse=", "))
p("UFs com UTIN SUS <2/mil NV: %s", paste(uf24[utin_sus_por_mil_nv < 2, uf], collapse=", "))
p("Leitos pediátricos/10 mil 0-6: min %s (%s) max %s (%s)",
  br(min(uf24$ped_por_10mil)), uf24[which.min(ped_por_10mil), uf],
  br(max(uf24$ped_por_10mil)), uf24[which.max(ped_por_10mil), uf])
p("== HOSPITAIS POR NÍVEL ==")
for (i in seq_len(nrow(T2))) p("%s: %s", T2$nivel[i], paste(unlist(T2[i, -1]), collapse=" / "))
p("== SIH 2024 ==")
for (i in seq_len(nrow(T4))) with(T4[i], p("%s | %s: AIH %s (%s%%), obitos %s (%s%%), letalidade %s%%, hosp %s",
  faixa, nivel, br(aih,0), br(pct_aih_faixa), br(obitos,0), br(pct_obitos_faixa), br(letalidade,2), hospitais))
p("Hospitais que concentram 50%% das AIH 0-6 em 2024: %d de %d", n50, nrow(top))
p("== SIM 2024 ==")
for (i in seq_len(nrow(T5))) with(T5[i], p("%s | %s | %s: %s (%s%%)", faixa3, local, nivel, N, br(pct_faixa)))
p("== SEM AIH ==")
for (i in seq_len(nrow(T6))) with(T6[i], p("%s | %s: %s obitos, %s%% sem AIH", faixa3, nivel, obitos, br(pct_sem_aih)))
p("== ACESSO ==")
for (i in seq_len(nrow(T7))) with(T7[i], p("%s | res_tem_ref=%s: %s obitos; em hosp ref %s%%; fora do municipio %s%%; fora do hospital %s%%",
  faixa3, res_tem_ref, obitos, br(pct_em_hosp_ref), br(pct_fora_municipio), br(pct_fora_hospital)))
p("Municípios com hospital N1/N2: %d", nrow(ref_mun))
writeLines(s, file.path(DIR_OUT, "resumo_infraestrutura.txt"))
cat(s, sep = "\n")
