library("astrid")
source("utilities_analysis.R")
library(e1071)

res_synthetic    <- TRUE  ## -- tables for the synthetic datasets 
res_uci          <- TRUE  ## -- table showing the summarised results from all of the UCI datasets
res_credit_a_svm <- TRUE  ## -- the credit a dataset (i) grouping and (ii) anonymisation example

## Prefix to prepend to result paths.
path_prefix <- "./"

## ==============================

if (res_synthetic) {
    ## a table with the results for the synthetic dataset
    respath <- "results_synthetic"
    respath <- paste0(path_prefix, respath)
    cat("Using path: ", respath, "\n\n\n")

    print_result_tables(respath, "synthetic", "svm", dataset_specs = FALSE, alpha = 0.05, full_tree = TRUE, R = 100)
    print_result_tables(respath, "synthetic", "randomForest", dataset_specs = FALSE, alpha = 0.05, full_tree = TRUE, R = 100)
    print_result_tables(respath, "synthetic", "naiveBayes", dataset_specs = FALSE, alpha = 0.05, full_tree = TRUE, R = 100)
}

## ==============================

if (res_uci) {
    respath_uci <- "results_uci_add"
    respath_uci <- paste0(path_prefix, respath_uci)
    cat("Using path: ", respath_uci, "\n\n\n")

    cat("SVM\n\n")
    make_grouping_table(respath_uci, "svm")
    cat("\n\nRandom forest\n\n")
    make_grouping_table(respath_uci, "randomForest")
}

## ==============================

if (res_credit_a_svm) {
    ## read the dataset
    respath <- "results_uci"
    respath <- paste0(path_prefix, respath, "/", "credit-a_svm.rds")
    cat("Using path: ", respath, "\n\n\n")
    
    res <- readRDS(respath)

    print_result_tables("results_uci", "credit-a", "svm", dataset_specs = FALSE, alpha = 0.05, full_tree = TRUE, R = 100)
    
    ## choose the tree for which k = 10
    tree_anon <- res$results$tree_p[[9]]

    cat("Using the tree:\n\n")
    tmp <- tree_to_latex(tree_anon$tree)
    tmp <- gsub("\\(", "\\\\set{", tmp)
    tmp <- gsub("\\)", "}", tmp)
    cat(tmp, "\n\n")

   
    ## Calculate baseline p-value
    RNGkind("L'Ecuyer-CMRG")
    set.seed(42)

    p0 <- sid_p_tree_es(res$data_train, res$data_validation, tree = tree_anon$tree, classifier = svm, Rmin = 250, Rmax = 500, parallel = TRUE, alpha = 0.05, z = 2.6)
    cat("p-value:\n\n")
    cat(round(p0, 2), "\n\n")

    cat("baseline goodness (train using unshuffled data):\n\n")
    a0 <- sid_get_goodness(res$data_train, res$data_validation, classifier = svm)
    cat(round(a0, 2), "\n\n")

    cat("anon goodness (train using shuffled data):\n\n")
    a <- sid_tree_goodness(res$data_train, res$data_validation, tree = tree_anon$tree, classifier = svm, R = 100)
    cat(round(a, 2), "\n\n")

    ## generate an anonymised dataset
    anon_quality <- function(data_orig, tree) {
        require(sqldf)
    
        data_anon <- sid_gen_surrogate(tree, data_orig)
        tmp <- sqldf("SELECT * FROM data_anon INTERSECT SELECT * from data_orig")

        nrow(tmp) / nrow(data_orig)
    }

    aq <- replicate(100, anon_quality(res$data_train, tree_anon$tree))

    cat("Quality of anonymisation: \n\n")
    cat(round(100 * mean(aq), 2), "\n\n")
}

## ==============================
