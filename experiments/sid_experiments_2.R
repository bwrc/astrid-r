## Some more analyses
## source("init.R")
library(astrid)
library(e1071)
library(randomForest)
library(foreign)
source("utilities_analysis.R")

datapath <- "data/UCI"

## Where the already calculated results are found
respath <- "results_uci"       ## read data from here ...
savepath <- "results_uci_add"  ## ... and write them back here with more information appended

## Get all ready datasets
datasets_ready_svm          <- get_ready_datasets(respath, classifier = "svm")
datasets_ready_randomforest <- get_ready_datasets(respath, classifier = "randomForest")

## What to do
do_calculate_stats <- TRUE # FALSE

calculate_stats <- function(respath, savepath, classifier) {
    dataset_name_list <- get_ready_datasets(respath, classifier = classifier)

    for (dsname in dataset_name_list) {
        cat(classifier, "\t", dsname, "\n")
        res <- load_result(respath, dsname, classifier)
        best_tree <- get_best_tree(res)

        ## Now calculate the statistics using the best tree
        ## average accuracy.
        out <- list()

        RNGkind("L'Ecuyer-CMRG")

        set.seed(42)
        tmp <- sid_p_tree_es(res$data_train, res$data_validation, tree = best_tree$tree, classifier = get(classifier), Rmin = 250, Rmax = 500, parallel = TRUE, alpha = 0.05, z = 2.6, return_raw = TRUE)

        ## calculate Ojala-Garriga p-value for Test 2
        cl_id <- which(colnames(res$data) == "class")
        tree_og <- lapply(seq.int(res$properties$n_attributes), function(i) c(cl_id, i))

        set.seed(42)
        tmp_og <- sid_p_tree_es(res$data_train, res$data_validation, tree = tree_og, classifier = get(classifier), Rmin = 250, Rmax = 500, parallel = TRUE, alpha = 0.05, z = 2.6, return_raw = TRUE)

        out[["res"]]   <- tmp
        out[["a_ave"]] <- mean(tmp$d)
        out[["p"]]     <- tmp$p
        out[["g0"]]    <- tmp$g0
        out[["sd"]]    <- sd(tmp$d)
        out[["a_min"]] <- min(tmp$d)
        out[["a_max"]] <- max(tmp$d)
        out[["og"]]    <- tmp_og

        res$extra <- out

        save_result(res, savepath)
        ## cat(dsname, "\n")
        ## e(str(out))

    }
}

if (do_calculate_stats) {
    calculate_stats(respath, savepath, "svm")
    calculate_stats(respath, savepath, "randomForest")
}
