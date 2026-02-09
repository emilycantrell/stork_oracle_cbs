Please run the following two pieces of code in sequence.

`1_generate_preprocesed_data.R` produces preprocessed datasets.

`2_missingness.R` counts the number of missing data cells for both the outcome an the features. We focus on features without structural missingness. Structural missingness refers to the situation in which a missingness code excludes a defined category of people from having non-missing values or when a feature covers information about a family member the focal individual may not have.

Please set the working directory to the main directory, the one containing folders `data`, `results`, and others.

The code requires the following datasets as input in the `data` folder:

`train.csv`, official training data provided by PreFer organizers, available via CBS Data Storage. Please contact us for more details.

`GBAPERSOON2020TABV3.csv`, register of basic personal data available via CBS secure environment

`GBAHUISHOUDENS2020BUSV1.csv`, register of household data available via CBS secure environment

`FAMILIENETWERK2020TABV1.csv`, register of family relations data available via CBS secure environment

`holdout_final_leaderboard.csv`, predictive features from official holdouts set provided by PreFer organizers, similar to `train.csv`, available via CBS Data Storage. Please contact us for more details.

`final_leaderboard_outcomes.csv`, outcomes from official holdouts set provided by PreFer organizers, available via CBS Data Storage. Please contact us for more details.

`Codebook UPD.xlsx`, codebook for `train.csv` provided by PreFer organizers, available directly in `data` folder.

`train_and_evaluation_samples_seed_1_241016_with_holdout.csv`, data split indicators generated via `splits/1_train_and_eval_samples_original.R`

The code produces the following outputs in the `data` folder:

`data.csv`, preprocessed training data

`metadata.csv`, table indicating feature sets, not actually used by 2_missingness.R

`outcome_data.csv`, preprocessed outcome data

After reading `data.csv` and `outcome_data.csv`, as well as `train.csv`, `final_leaderboard_outcome.csv`, and `holdout_final_leaderboard.csv`, `2_missingness.R` produces a tabulation of missing data for the outcome and the predictor in `missingness.csv` in `data` folder.