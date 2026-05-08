# REPLICATION README

**Last updated:** May 5, 2026

**Author:** John McIntire

**Paper:** How do time, gender, air quality, temperature and coaching affect distance running performance in California adolescents?

**Repository:** https://github.com/JMcIntireTangier/running-replication

---

## AI assistance

This replication package was developed with the assistance of DeepSeek (https://chat.deepseek.com). The full conversation log is available in `doc/conversation_with_DeepSeek_April2026.txt` (or as PDF in the same folder).

The AI helped with:
- Coding advice (R, fixest, modelsummary, flextable)
- Statistical methodology (two-way clustering, panel data diagnostics)
- Replication package structure
- GitHub version control

**All final code, statistical decisions, and interpretations are the sole responsibility of the author.**

---
## Requirements

- **R version 4.6.0 or higher** (the code checks this automatically)
- Required packages are listed and installed automatically by `LIBRARIES_ARTICLE.R`
- Tested on Windows 11, should work on Mac and Linux

## How to replicate

1. Copy all files from `/R` to the root directory of this package
2. Open `MARKDOWN_RUNNING_CA.Rmd` in RStudio
3. Install required packages (see `LIBRARIES_ARTICLE.R`)
4. Knit to Word (or HTML)

---

## Clone this repository

To replicate this paper, run in your terminal:

```bash
git clone https://github.com/JMcIntireTangier/running-replication.git
cd running-replication
```

## Important note for RStudio users

The main R Markdown file (`MARKDOWN_RUNNING_CA.Rmd`) does **not** 
include `rstudioapi::documentSaveAll()` in its setup chunk. 
This function was removed to ensure compatibility with users who do not use RStudio.

**If you are using RStudio to replicate this paper:**

Add the following line to the setup chunk of `MARKDOWN_RUNNING_CA.Rmd` 
(after `knitr::opts_chunk$set(...)`):

```r
rstudioapi::documentSaveAll()
```

Email: jmcintire@1818alumniwbg.org

Data source
Publicly available race results from:

Mt SAC invitational

California state cross-country championships

Runner names are included as they appear in public race results and are necessary for replicating the two-way clustering (race_name + runner_id) used in all regressions.

Data organization and naming conventions
Two sites
Mt San Antonio College (MTSAC) – near Walnut, CA

Woodward Park (WP) – near Fresno, CA

Two periods for Woodward Park
1987_2023

2000_2023 (distinguished by availability of hourly ozone and temperature data)

One period for MTSAC
2002_2023

Note: There are no data for the year 2020 because of the COVID pandemic.

Object naming conventions
Prefix	Meaning	Example
df_	data frame	df_F_WP_1987_2023
df_F	female data frame	df_F_MTSAC
df_M	male data frame	df_M_WP_2000_2023
dfqnt_	top (fastest) quintile	dfqnt_F_WP_2000_2023_D1D2_Q5
Six basic data frames
df_F_WP_1987_2023

df_M_WP_1987_2023

df_F_WP_2000_2023

df_M_WP_2000_2023

df_F_MTSAC

df_M_MTSAC

School divisions
Divisions are factors with levels: D1, D2, D3, D4, D5

Division 1 – largest schools

Division 5 – smallest schools

Combined divisions:

D1D2 = filter(division == "D1" | division == "D2")

D4D5 = filter(division == "D4" | division == "D5")

At MTSAC, divisions were merged as:

D1D2

D3

D4D5

Top quintile subsets (largest schools)
dfqnt_F_WP_2000_2023_D1D2_Q5

dfqnt_M_WP_2000_2023_D1D2_Q5

dfqnt_F_MTSAC_D1D2_Q5

dfqnt_M_MTSAC_D1D2_Q5

**Clustering**
All regressions use two-way clustering (race_name + runner_id), 
except the fixed effects (FE) model, in which the clustering variable is year.

**Contact**

For questions: jmcintire@1818alumniwbg.org


