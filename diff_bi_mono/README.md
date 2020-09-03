---
title: "Repository specifications - Iliaei et al. (2020)"
author: "Hilary Killam and Rodrigo Dal Ben"
date: "September, 03, 2020"
output: html_document
---

**Article**: Iliaei, S. P., Killam, H., Dal Ben, R., & Byers-Heinlein, K. (under review). Bilingualism affects infant cognition: Insights from new and open data.

This repository contain data, scripts, and analyses from two new datasets (Study 1a & 2) and three reanalyzed open datasets (Study 1b, 1c, 1d).

The reanalyzed datasets come from: 

*Study 1b*: 

D'Souza et al. (2020)

- article: https://doi.org/10.1098/rsos.180191 

- OSF: https://osf.io/53gh2/

- Dryad: https://doi.org/10.5061/dryad.3n5tb2rc6 

*Studies 1c & 1d*: 

Kalashnikova et al. (2020):

- article: https://doi.org/10.1111/desc.13011

- OSF: https://osf.io/k3e9z/

### Directory structure:

The repository is organized into `/input`, `/output`, and `/scripts` subdirectories: 

* `/input`: contain the raw data for Studies 1a and 2 (our own) and **step-by-step** guides to retrieve the raw data from D'Souza et al. (2020; [OSF](https://osf.io/53gh2/), [Dryad](https://doi.org/10.5061/dryad.3n5tb2rc6)) and Kalashnikova et al. (2020; [OSF](https://osf.io/k3e9z/)). The folder also contain `.json` codebooks for each raw dataset.

* `/output`: contain two subdirectories, `/data` and `/table_figure`
  
  * `/data`: contain tidy data for all studies (1a, b, c, d, and 2). Each study have two datasets: *filtered* and *unfiltered*. Studies 1a and 2 have additional files (i.e., `exclusions_1a_2.rda`, `data_tobii_cleaned_1a_2.rda`, `final_sample_mslist_1a_2.rda`, `ms_list_unfiktered_1a_2.rda`) All datasets have an correspondent `.json` codebook.
  
  * `/table_figure`: contains analyses tables and figures for all Studies (1a, b, c, d, and 2).

* `/scripts`: contain three wrangling scripts (`.r`) to transform the raw data (`/input` & OSF) to tidy data (`/output/data`): one for Study 1a and 2, another for 1b, and another for 1c and 1d. It also contain the `analyses.Rmd` script to generate descriptive statistics, inferential statistics, and visualizations for all Studies.

### Feedback and suggestions: 
<kbh.coordinator@concordia.ca>, <dalbenwork@gmail.com>, <k.byers@concordia.ca>
