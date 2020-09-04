---
title: "Step-by-step: raw data from Study 1b - D'Souza et al. (2020)"
author: "Hilary Killam and Rodrigo Dal Ben"
date: "September, 03, 2020"
output: html_document
---

**Article**: Iliaei, S. P., Killam, H., Dal Ben, R., & Byers-Heinlein, K. (under review). Bilingualism affects infant cognition: Evidence from new and reanalyzed data.

**OSF**: https://osf.io/bz8jn/

Step-by-step instructions to retrieve the raw data from D'Souza et al. (2020).

Reference for D'Souza et al. (2020) materials:

- article: https://doi.org/10.1098/rsos.180191 

- OSF: https://osf.io/53gh2/

- Dryad: https://doi.org/10.5061/dryad.3n5tb2rc6 

### Summary: 

Raw (sample-by-sample) data were extracted from **MATLAB** and saved as `.csv` files. These files were then manipulated in **R** to produce a single dataset with experimental and demographic data (see the `/output/data` subdirectory; i.e., `data_full_trials_filtered_1b.Rda` and `data_full_trials_unfiltered_1b.Rda`). The final raw datasets, in `.csv` format are also hosted by D'Souza et al. (2020) at: https://osf.io/53gh2/

### MATLAB

1. D'Souza et al. recorded data on 3 buffers: `events`, `time`, and `samples`; The `Event` buffer had its own timeline; 

2. `Time` and `samples` buffers were combined adapting the authors' matlab script (see `1b_MATLAB` folder on OSF - https://osf.io/bz8jn/);

4. For each participant, one samples file (with a timeline) and one events file (with a timeline) were generated;

5. These files were saved in `.csv` format. To access these individual files (102), you can wither run the MATLAB scripts available on our OSF (`/1b_MATLAB`), or e-mail: <dalbenwork@gmail.com>.

### R

1. Individual `.csv` files for `events` and `samples` were read as data frames;

2. `dplyr::left_join` didn't work for joining these data frames as their timelines do not exactly match; 

3. `purrr::map2` and `fuzzyjoin::fuzzy_left_join` were used to join `.csv` files based on time interval (this took ~ 14 hours of processing);

4. We were unable to identify 10 participants;

5. In 18/06/2020: Additional information was provided by Dean D'Souza by e-mail;

6. In 19/06/2020: We updated participant info, but `A90` was missing; 

7. `A90` was retrieved from MATLAB: One file was generated for D'Souza's *experiment 1* and another for *experiment 2* and *experiment 3*. The file for *experiment 2* and *3* overwrote the file from *experiment 1*;

8. `A90` was added and the full 102-infants sample was identified;

9. Two dataframes with experimental and demographic data were generated (see `/output/data`):

	- `data_full_trials_filtered_1b.Rda`: containing 51 participants included in the final analysis (main article);
	
	- `data_full_trials_unfiltered_1b.Rda`: containing 102 participants included in the additional analysis (supplementary material);


Descriptive statistics, inferential statistics, and visualizations were generated using the single `analyses.Rmd` script available at: `/scripts`.

### Feedback and suggestions: 
<kbh.coordinator@concordia.ca>, <dalbenwork@gmail.com>