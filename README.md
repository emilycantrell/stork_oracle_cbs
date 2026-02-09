## OVERVIEW

This directory contains the code for reproducing the results for our paper "More Data, Modest Gains: Assessing Limits of Prediction in Multidomain Population-Scale Social Data."

The folder `data` is the place for all relevant data files, most of which are not provided here in this repository and are under restricted access conditions. Please see the "DATA" section below for more details. If you and your institution have successfully [applied](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research/applying-for-access-to-microdata) for access to data from the Centraal Bureau voor de Statistiek (CBS), you can log onto CBS's remote access environment from within one of the [approved countries](https://www.cbs.nl/en-gb/faq/microdata/from-which-countries-is-it-allowed-to-log-in-on-the-cbs-microdata-environment-) and find the files within the environment. The folder `data` also contains some intermediate files produced while running the code.

The folder `splits` contains the code for creating indicator variables that split the data into various training and test sets. See more details in the `README.md` within the folder.

The folder `jobfiles` contains specifications for how we prepared the data, fit the catboost models, and evaluated prediction performances. For our paper, we used multiple sets of specifications with different settings for training sample sizes and feature sets, so we had multiple jobfiles. Please see the "JOBFILES" section for how the jobfiles correspond to various parts of the paper.

The folder `model` contains code that gets applied to each jobfile and actually does the data preparation, model fitting, and performance evaluation. Please see the "INSTRUCTIONS FOR RUNNING THE CODE" for more details.

The folder `missingness` contains code that quantifies the amount of missing data. See `README.md` within the folder for more details.

The folder `results` contains outputs of running the code in `model` and `missingness`. We exported these files from the CBS secure environment following a review process.

The folder `analysis` uses the exported data to produce tables and figures for the paper. See `README.md` within the folder for more details.

## SPECS & VERSION NUMBERS

Other than the code within the `analysis` folder (see `README.md` within the folder for more details), all code was run within the heavy server of the CBS secure access environment with the following specs:

| Component | Specification |
|:-----------------------------------|:-----------------------------------|
| **Processor** | Intel(R) Xeon(R) Gold 6442Y 2.60 GHz |
| **Parallelization** | Maximum 4 threads |
| **Memory** | 128 GB |
| **Operating System** | Windows 11 Enterprise |
| **Disk Space** | F: 3.25TB; G: 149GB; H: 120TB; K: 149GB; L: 28.5TB; M: 786GB; T: 3.25TB |
| **catboost** | 1.2.5 |
| **data.table** | 1.16.2 |
| **fastDummies** | 1.7.3 |
| **furrr** | 0.3.1 |
| **glmnet** | 4.1-8 |
| **MLmetrics** | 1.1.3 |
| **pROC** | 1.18.5 |
| **tidymodels** | 1.2.0 |
| **tidyverse** | 2.0.0 |
| **xgboost** | 1.7.7.1 |

It is possible the code will run on other versions as well.

## DATA

The code requires the following datasets as input in the `data` folder:

`train.csv`, official training data provided by PreFer organizers, available via CBS Data Storage. Please contact us for more details.

`GBAPERSOON2020TABV3.csv`, register of basic personal data available via CBS secure environment. We found it at `G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv`.

`GBAHUISHOUDENS2020BUSV1.csv`, register of household data available via CBS secure environment. We found it at `G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv`.

`FAMILIENETWERK2020TABV1.csv`, register of family relations data available via CBS secure environment. We found ti at `G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv`.

`holdout_final_leaderboard.csv`, predictive features from official holdouts set provided by PreFer organizers, similar to `train.csv`, available via CBS Data Storage. Please contact us for more details.

`final_leaderboard_outcomes.csv`, outcomes from official holdouts set provided by PreFer organizers, available via CBS Data Storage. Please contact us for more details.

`Codebook UPD.xlsx`, codebook for `train.csv` provided by PreFer organizers, available directly in `data` folder.

## INSTRUCTIONS FOR RUNNING THE CODE

Please note that this process, especially Step 8, can take quite a while. We expect all 11 jobfiles to take about 10 days to run if the run time had been continuous.

1.  Clone the `special_issue_paper` branch of this repository into the remote access environment.

2.  Populate the `data` folder with the data files mentioned above.

3.  In RStudio, use the command `setwd("FILL_IN_FILEPATH")` to set the working directory to the main directory, the one containing folders `data`, `results`, and others.

4.  Run all of the code in `splits/1_train_and_eval_samples_original.R` and `splits/2_train_and_eval_samples_original.R` in sequence. This should create 40 subsample indicator files in the `data` folder. See more details in the `README.md` within the `splits` folder.

5.  Open `model/run_all.R`, make sure Line 27 contains a jobfile from the `jobfiles` folder.

6.  Open a terminal window.

7.  Using the command `cd FILL_IN_FILEPATH`, set the working directory to the main directory, the one containing folders `data`, `results`, and others.

8.  Run `RScript model/run_all.R "data/GBAPERSOON2020TABV3.csv" "data/GBAHUISHOUDENS2020BUSV1.csv" "data/FAMILIENETWERK2020TABV1.csv" "data/holdout_final_leaderboard.csv" "data/final_leaderboard_outcome.csv" "data/Codebook UPD.xlsx" "data/"`.

9.  Edit Line 27 of `model/run_all.R` to change the jobfile to a jobfile that has not yet been run.

10. Repeat Steps 5-9 to run all 11 jobfiles. This process should create 2,565 modeling files, 11 intermediate results `.csv` files , and one `remove_zero_variance.RDS` (this `.RDS` file is a vestige of older versions of the code and gets overwritten many times. It doesn't actually gets read in the code) in the `data` folder. However, most importantly, it should create 11 `.csv` results file in the `results` folder. During the export review process, we converted these `.csv` files into `.xlsx` files.

11. In RStudio, again, use the command `setwd("FILL_IN_FILEPATH")` to set the working directory to the main directory, the one containing folders `data`, `results`, and others.

12. Run all of the code in `missingness/1_generate_preprocessed_data.R` (this can take an hour or so) and `missingness/2_missingness.R.R` in sequence. This should create `missingness.csv` in the `results` folder. See more details in the `README.md` within the `missingness` folder.

13. Follow instructions in the `README.md` within the `analysis` folder to reproduce all tables and figures in the paper.

## JOBFILES

`jobfile_extrapolation_seed_1_2025-08.R` produces extrapolation training data for Figures 1, 3, C2, C3, C4, C5, C6, and Table 2. This jobfile is slightly different from the one that produced results/results_extrapolation_seed_1_2025-08.xlsx. For that file, we ran the code with 2000 bootstraps and two additional metrics: In_Sample_R2 and AUC. This caused the jobfile to take more than a week to run. We ended up simplifying the jobfile for seeds 2 through 5, as we realized that the extrapolation section of our paper did not really require bootstrapped confidence intervals. To save time for reproduction, we present a simplified jobfile for seed 1 here.

`jobfile_extrapolation_seed_2_2025-08.R`, `jobfile_extrapolation_seed_3_2025-08.R`, and `jobfile_extrapolation_seed_4-5_2025-08.R` produce extrapolation training data for Figures C2, C3, C4, C5, C6, and Table 2.

`jobfile_learning_curves_seed_1_2025-08.R` produces learning curve data for Figure 2 and Table 1. It also supplied extrapolation test data for Figures 1, 3, C2, C3, C4, C5, C6, and Table 2. In addition, it provided data on the age, sex, and family structure baseline and the winning model in Figures 1, 4, 5, 6, D7, D8, D9, D10.

`jobfile_learning_curves_seed_2-5_2025-08.R` produces learning curve data for Table 1. It also supplied extrapolation test data for Figures C2, C3, C4, C5, C6, and Table 2. In addition, they provided data on the age, sex, and family structure baseline and the winning model in Figures 4, 5, 6, D7, D8, D9, and D10.

`jobfile_individual_topics_seed_1_2025-08.R` and `jobfile_individual_topics_seed_2-5_2025-08.R` supplied topic-specific performance data for Figure 4.

`jobfile_baseline_plus_topics_seed_1_2025-08.R` and `jobfile_baseline_plus_topics_seed_2-5_2025-08.R` supplied performance data on individual topics in combination with the age, sex, and family structure baseline for Figures 5, D7, D8, D9, and D10.

`jobfile_family_demography_2025-04.R` supplied performance data on family demography features in Figure E11.
