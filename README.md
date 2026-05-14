<div align="center">
  <h1>📊 SIM + SIH: Mortalidade e Internações (0 a 6 anos) no Brasil</h1>
  <p><strong>Projeto de análise reprodutível de dados de saúde pública (2015–2024)</strong></p>
</div>

---

## 📌 Resumo Executivo

Este projeto apresenta um mapeamento detalhado da **mortalidade e das internações de crianças na primeira infância (0 a 6 anos)** no Brasil, com foco na identificação de padrões temporais, causas prioritárias, desigualdades regionais e fluxos assistenciais relacionados ao Sistema Único de Saúde (SUS).

O objetivo é fornecer subsídios baseados em dados para a formulação de políticas públicas, identificação de gargalos assistenciais e direcionamento de recursos para áreas, causas e territórios mais críticos.

**Recortes etários analisados:**

* Neonatal precoce: 0–6 dias
* Neonatal tardia: 7–27 dias
* Pós-neonatal: 28 dias a <1 ano
* Crianças de 1 a 6 anos

---

## 🔬 Nota Metodológica

Para garantir maior precisão epidemiológica nas estimativas de mortalidade, este estudo utiliza o número de **nascidos vivos do SINASC** como denominador para o cálculo das taxas. As taxas são expressas por **1.000 nascidos vivos**, conforme prática consolidada em análises de mortalidade infantil e na primeira infância.

**Fontes de dados oficiais:**

* **SIM** — Sistema de Informações sobre Mortalidade
* **SINASC** — Sistema de Informações sobre Nascidos Vivos
* **SIH/SUS** — Sistema de Informações Hospitalares
* **Extração e processamento:** DataSUS, TabNet e rotinas em R

---

## 📈 Resultados e Painel de Indicadores

> Todas as análises e rotinas de extração, tratamento e visualização foram desenvolvidas em ambiente R. Os scripts completos estão disponíveis neste repositório.

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

## 2. Análise Espacial e Desigualdades Regionais

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

## 3. Análise de Fluxo e Rede de Atendimento

A análise de fluxo cruza o **município de residência** da criança com o **município de ocorrência do óbito**, permitindo identificar padrões de deslocamento, dependência assistencial, centralização de óbitos em polos regionais e potenciais gargalos da rede de atenção.

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

### Principais Fluxos para os Top 10 Municípios Receptores

<div align="center">
  <img src="outputs/SIM/Fig10_Fluxos_Top10_Municipios_Receptores.png" width="850">
</div>

---

## 📂 Tabelas Executivas

Os arquivos executivos com os resultados sumarizados estão disponíveis para download:

* [Tabelas Executivas de Mortalidade](outputs/SIM/Tabelas_Executivas_Mortalidade_v2.xlsx)
* [Tabelas dos Top 10 Municípios Receptores](outputs/SIM/Tabelas_Top10_Municipios_Receptores.xlsx)

---

## 📂 Estrutura do Repositório

```text
sim-sih-mortalidade-internacoes-0-6-anos/
│
├── Scripts/
│   ├── 01_download_preparo_sim_0a6.R
│   ├── 02_analises_sim_0a6.R
│   └── demais rotinas analíticas
│
├── outputs/
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
│
├── .gitignore
├── LICENSE
└── README.md
