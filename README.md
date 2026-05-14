<div align="center">
  <h1>📊 SIM + SIH: Mortalidade e Internações (0 a 6 anos) no Brasil</h1>
  <p><strong>Projeto de análise reprodutível de dados de saúde pública (2015–2024)</strong></p>
</div>

---

## 📌 Resumo Executivo

Este projeto apresenta um mapeamento detalhado da **mortalidade e das internações hospitalares de crianças na primeira infância (0 a 6 anos)** no Brasil, com base em dados públicos oficiais do DataSUS.

O objetivo é fornecer subsídios epidemiológicos e assistenciais para a formulação de políticas públicas, identificação de desigualdades regionais, análise de causas prioritárias e compreensão dos fluxos de atendimento infantil no Sistema Único de Saúde (SUS).

O projeto integra dois grandes módulos analíticos:

* **SIM/SINASC** — mortalidade de crianças de 0 a 6 anos;
* **SIH/SINASC** — internações hospitalares de crianças de 0 a 6 anos.

**Recortes etários analisados:**

* Neonatal precoce: 0–6 dias;
* Neonatal tardia: 7–27 dias;
* Pós-neonatal: 28 dias a <1 ano;
* Crianças de 1 a 6 anos.

---

## 🔬 Nota Metodológica

Para garantir maior precisão epidemiológica, este estudo utiliza o número de **nascidos vivos do SINASC** como denominador para o cálculo das taxas de mortalidade e de internação.

As taxas são expressas por **1.000 nascidos vivos**, permitindo comparações padronizadas entre anos, Unidades Federativas e macrorregiões.

**Fontes de dados oficiais:**

* **SIM** — Sistema de Informações sobre Mortalidade;
* **SIH/SUS** — Sistema de Informações Hospitalares do SUS;
* **SINASC** — Sistema de Informações sobre Nascidos Vivos;
* **Extração e processamento:** DataSUS, TabNet e rotinas em R.

---

## 📁 Scripts do Projeto

Os scripts utilizados para extração, tratamento, análise e geração das visualizações estão disponíveis na pasta `Scripts/`.

### Scripts do SIM — Mortalidade

* [`Scripts/Script Insper_SIM.R`](Scripts/Script%20Insper_SIM.R)  
  Script de extração, organização e preparação das bases do SIM/SINASC.

* [`Scripts/Script Insper_SIM_analises.R`](Scripts/Script%20Insper_SIM_analises.R)  
  Script de análises epidemiológicas, geração de tabelas executivas e visualizações do módulo de mortalidade.

### Scripts do SIH — Internações

* [`Scripts/Script Insper_SIH`](Scripts/Script%20Insper_SIH)  
  Script de extração, organização e preparação das bases do SIH/SINASC.

* [`Scripts/Script Insper_SIH_analises`](Scripts/Script%20Insper_SIH_analises)  
  Script de análises epidemiológicas, geração de tabelas executivas e visualizações do módulo de internações.

---

# ⚰️ SIM/SINASC — Mortalidade de Crianças de 0 a 6 Anos

Esta seção apresenta a análise dos óbitos de crianças de 0 a 6 anos no Brasil, com base no Sistema de Informações sobre Mortalidade (SIM), no período de 2015 a 2024.

As análises incluem evolução temporal, causas de mortalidade, desigualdades regionais, distribuição espacial e fluxos de ocorrência do óbito em relação ao município de residência.

---

## 1. Perfil Temporal e Causal da Mortalidade

### Evolução da Mortalidade por Faixa Etária — Óbitos Absolutos

<div align="center">
  <img src="outputs/SIM/Fig01_Evolucao_Faixa_Etaria_Absoluto.png" width="850">
</div>

### Evolução das Causas de Mortalidade — Taxa por 1.000 Nascidos Vivos

<div align="center">
  <img src="outputs/SIM/Fig02_Evolucao_Causas_Taxa.png" width="850">
</div>

### Causas Prioritárias de Mortalidade por Faixa Etária

<div align="center">
  <img src="outputs/SIM/Fig03_Causas_Prioritarias_por_Faixa.png" width="850">
</div>

---

## 2. Análise Espacial e Desigualdades Regionais da Mortalidade

### Heterogeneidade Regional da Taxa de Mortalidade

<div align="center">
  <img src="outputs/SIM/Fig04_Heterogeneidade_Macro_Taxa.png" width="850">
</div>

### Distribuição Espacial da Taxa de Mortalidade por Estado

<div align="center">
  <img src="outputs/SIM/Fig05_Mapa_Taxa_Estado.png" width="700">
</div>

### Distribuição Espacial da Taxa de Mortalidade por Macrorregião

<div align="center">
  <img src="outputs/SIM/Fig06_Mapa_Taxa_Macrorregiao.png" width="700">
</div>

---

## 3. Fluxos de Mortalidade e Polos de Ocorrência

A análise de fluxo cruza o **município de residência** da criança com o **município de ocorrência do óbito**, permitindo identificar deslocamentos assistenciais, dependência regional e concentração de óbitos em polos de saúde infantil.

### Proporção de Óbitos Fora do Município de Residência

<div align="center">
  <img src="outputs/SIM/Fig07_Fluxo_Obitos_Fora_Municipio.png" width="850">
</div>

### Top 30 Municípios Polo de Saúde Infantil

<div align="center">
  <img src="outputs/SIM/Fig08_Polos_Saude_Infantil_Top30.png" width="850">
</div>

### Fluxo de Mortalidade Infantil entre UFs

<div align="center">
  <img src="outputs/SIM/Fig09_Heatmap_Fluxo_InterUF.png" width="850">
</div>

### Principais Fluxos para os Top 10 Municípios Receptores de Óbitos

<div align="center">
  <img src="outputs/SIM/Fig10_Fluxos_Top10_Municipios_Receptores.png" width="850">
</div>

---

## 📂 Tabelas Executivas — Mortalidade

Os arquivos executivos com os resultados sumarizados da mortalidade estão disponíveis para download:

* [Tabelas Executivas de Mortalidade](outputs/SIM/Tabelas_Executivas_Mortalidade_v2.xlsx)
* [Tabelas dos Top 10 Municípios Receptores — SIM](outputs/SIM/Tabelas_Top10_Municipios_Receptores.xlsx)

---

# 🏥 SIH/SUS — Internações de Crianças de 0 a 6 Anos

Esta seção apresenta a análise das internações hospitalares de crianças de 0 a 6 anos no Brasil, com base no Sistema de Informações Hospitalares do SUS (SIH/SUS), no período de 2015 a 2024.

As análises complementam o módulo de mortalidade do SIM, permitindo avaliar padrões de utilização hospitalar, causas prioritárias de internação, desigualdades regionais, fluxos assistenciais e óbitos hospitalares entre crianças internadas.

---

## 4. Perfil Temporal e Causal das Internações

### Evolução das Internações por Faixa Etária — Internações Absolutas

<div align="center">
  <img src="outputs/SIH/Fig01_Evolucao_Faixa_Etaria_Absoluto.png" width="850">
</div>

### Evolução das Causas de Internação — Taxa por 1.000 Nascidos Vivos

<div align="center">
  <img src="outputs/SIH/Fig02_Evolucao_Causas_Taxa.png" width="850">
</div>

### Causas Prioritárias de Internação por Faixa Etária

<div align="center">
  <img src="outputs/SIH/Fig03_Causas_Prioritarias_por_Faixa.png" width="850">
</div>

---

## 5. Análise Espacial e Desigualdades Regionais nas Internações

### Heterogeneidade Regional da Taxa de Internação

<div align="center">
  <img src="outputs/SIH/Fig04_Heterogeneidade_Macro_Taxa.png" width="850">
</div>

### Distribuição Espacial da Taxa de Internação por Estado

<div align="center">
  <img src="outputs/SIH/Fig05_Mapa_Taxa_Estado.png" width="700">
</div>

### Distribuição Espacial da Taxa de Internação por Macrorregião

<div align="center">
  <img src="outputs/SIH/Fig06_Mapa_Taxa_Macrorregiao.png" width="700">
</div>

---

## 6. Fluxos Assistenciais e Polos de Atendimento Infantil

A análise de fluxo das internações cruza o **município de residência** da criança com o **município de internação**, permitindo identificar deslocamentos assistenciais, concentração de atendimentos em polos regionais e dependência de municípios receptores para o cuidado hospitalar infantil.

### Proporção de Internações Fora do Município de Residência

<div align="center">
  <img src="outputs/SIH/Fig07_Fluxo_Internacoes_Fora_Municipio.png" width="850">
</div>

### Top 30 Municípios Polo de Atendimento Infantil

<div align="center">
  <img src="outputs/SIH/Fig08_Polos_Saude_Infantil_Top30.png" width="850">
</div>

### Fluxo de Internações Infantis entre UFs

<div align="center">
  <img src="outputs/SIH/Fig09_Heatmap_Fluxo_InterUF.png" width="850">
</div>

### Principais Fluxos para os Top 10 Municípios Receptores de Internações

<div align="center">
  <img src="outputs/SIH/Fig10_Fluxos_Top10_Municipios_Receptores_SIH.png" width="850">
</div>

---

## 7. Óbitos Hospitalares entre Internações

Esta análise descreve os óbitos hospitalares registrados entre internações de crianças de 0 a 6 anos, permitindo avaliar a magnitude absoluta dos óbitos em ambiente hospitalar e a letalidade hospitalar ao longo do período.

### Óbitos Hospitalares e Letalidade Hospitalar

<div align="center">
  <img src="outputs/SIH/Fig11_Obitos_Hospitalares_SIH.png" width="850">
</div>

---

## 📂 Tabelas Executivas — Internações

Os arquivos executivos com os resultados sumarizados das internações estão disponíveis para download:

* [Tabelas Executivas de Internações — 0 a 6 anos](outputs/SIH/Tabelas_Executivas_Internacoes_0_6_Anos.xlsx)
* [Tabelas dos Top 10 Municípios Receptores — SIH](outputs/SIH/Tabelas_Top10_Municipios_Receptores_SIH.xlsx)

---

# 📂 Estrutura do Repositório

```text
sim-sih-mortalidade-internacoes-0-6-anos/
│
├── Scripts/
│   ├── Script Insper_SIH
│   ├── Script Insper_SIH_analises
│   ├── Script Insper_SIM.R
│   └── Script Insper_SIM_analises.R
│
├── outputs/
│   │
│   ├── SIM/
│   │   ├── Fig01_Evolucao_Faixa_Etaria_Absoluto.png
│   │   ├── Fig02_Evolucao_Causas_Taxa.png
│   │   ├── Fig03_Causas_Prioritarias_por_Faixa.png
│   │   ├── Fig04_Heterogeneidade_Macro_Taxa.png
│   │   ├── Fig05_Mapa_Taxa_Estado.png
│   │   ├── Fig06_Mapa_Taxa_Macrorregiao.png
│   │   ├── Fig07_Fluxo_Obitos_Fora_Municipio.png
│   │   ├── Fig08_Polos_Saude_Infantil_Top30.png
│   │   ├── Fig09_Heatmap_Fluxo_InterUF.png
│   │   ├── Fig10_Fluxos_Top10_Municipios_Receptores.png
│   │   ├── Tabelas_Executivas_Mortalidade_v2.xlsx
│   │   └── Tabelas_Top10_Municipios_Receptores.xlsx
│   │
│   └── SIH/
│       ├── Fig01_Evolucao_Faixa_Etaria_Absoluto.png
│       ├── Fig02_Evolucao_Causas_Taxa.png
│       ├── Fig03_Causas_Prioritarias_por_Faixa.png
│       ├── Fig04_Heterogeneidade_Macro_Taxa.png
│       ├── Fig05_Mapa_Taxa_Estado.png
│       ├── Fig06_Mapa_Taxa_Macrorregiao.png
│       ├── Fig07_Fluxo_Internacoes_Fora_Municipio.png
│       ├── Fig08_Polos_Saude_Infantil_Top30.png
│       ├── Fig09_Heatmap_Fluxo_InterUF.png
│       ├── Fig10_Fluxos_Top10_Municipios_Receptores_SIH.png
│       ├── Fig11_Obitos_Hospitalares_SIH.png
│       ├── Tabelas_Executivas_Internacoes_0_6_Anos.xlsx
│       └── Tabelas_Top10_Municipios_Receptores_SIH.xlsx
│
├── .gitignore
├── LICENSE
└── README.md
```

---

# 💻 Reprodutibilidade

Para reproduzir as análises, clone o repositório e execute os scripts disponíveis na pasta `Scripts/`.

```r
# Scripts do SIM — Mortalidade
source("Scripts/Script Insper_SIM.R")
source("Scripts/Script Insper_SIM_analises.R")

# Scripts do SIH — Internações
source("Scripts/Script Insper_SIH")
source("Scripts/Script Insper_SIH_analises")
```

---

# 📌 Observações sobre os Dados

Os microdados brutos não são armazenados no repositório, em razão do volume dos arquivos e das boas práticas de organização, versionamento e reprodutibilidade.

As rotinas em R permitem reconstruir as bases analíticas a partir das fontes oficiais do DataSUS, respeitando a estrutura dos sistemas nacionais de informação em saúde.

---

# 📄 Licença

Este projeto está disponibilizado para fins de pesquisa, ensino, auditoria técnica, transparência pública e apoio à formulação de políticas de saúde.
