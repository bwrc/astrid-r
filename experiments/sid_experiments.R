library(astrid)
library(e1071)
library(randomForest)
library(foreign)

datapath           <- "data/UCI"
savepath_synthetic <- "results_synthetic"
savepath_uci       <- "results_uci"

do_synthetic <- TRUE
do_uci       <- TRUE

## list of datasets
dataset_list <- c("balance-scale","credit-a", "diabetes", "vehicle", "vote", "vowel", "segment", "kr-vs-kp", "mushroom", "soybean")

## Synthetic dataset
if (do_synthetic) {
    classifier_list_synthetic <- c("svm", "randomForest", "naiveBayes")
    do_experiment_synthetic(classifier_list_synthetic, datapath, savepath_synthetic, R = 100, Rmin = 250, Rmax = 500, alpha = 0.05, prune_singletons = FALSE, early_stopping = FALSE)
}


if (do_uci) {
    classifier_list <- c("svm", "randomForest")
    do_experiment_uci(dataset_list, classifier_list, datapath, savepath_uci, R = 100, Rmin = 250, Rmax = 500, alpha = 0.05, prune_singletons = FALSE, early_stopping = FALSE)
}
