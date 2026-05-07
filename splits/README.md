Please run the following two pieces of code in sequence.

`1_train_and_eval_samples_original.R` This file produces indicators that split the data into an official holdout set, an internal validation set (evaluation_test_50_percent_split), an internal selection set (evaluation_test_50_percent_split), and an inner training set (training_set). It also produces some subsample indicators that are not later used.

`2_train_and_eval_samples_for_data_matrix_paper.R` adds indicators for subsamples of the inner training set that were used in the special issue paper

Please set the working directory to the main directory, the one containing folders `data`, `results`, and others.

Both pieces of code require the following datasets as input in the `data` folder:

`train.csv`, official training data provided by PreFer organizers, available via CBS Data Storage. Please contact us for more details.

`holdout_final_leaderboard.csv`, predictive features from official holdouts set provided by PreFer organizers, available via CBS Data Storage. Please contact us for more details.

The first piece of code `1_train_and_eval_samples_original.R` produces 10 `.csv` files in the `data` folder with file names in the format of `train_and_evaluation_samples_seed_X_241016_with_holdout.csv`.

The second piece of code `2_train_and_eval_samples_for_data_matrix_paper.R` takes `data/train_and_evaluation_samples_seed_1_241016.csv` and produces five files with names like `dms_samples_seed_X.csv` to indicate random subsamples of different preset sizes, produced with five different random seeds. For each of these files, the code also takes the 10,000-person subsample and produce subsamples nested within these 10,000 individuals for use in learning curve extrapolation. This creates 25 additional files with file name format `dms_samples_seed_X_subsamples_X.csv`.
