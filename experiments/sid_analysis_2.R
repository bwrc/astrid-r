library(astrid)
source("utilities_analysis.R")
source("utilities_analysis_2.R")

## The synthetic datasets
if (TRUE) {
    respath <- "results_synthetic"
    print_result_tables_3(respath, "synthetic", "svm", dataset_specs = FALSE)
    print_result_tables_3(respath, "synthetic", "randomForest", dataset_specs = FALSE)
    print_result_tables_3(respath, "synthetic", "naiveBayes", dataset_specs = FALSE)
}

if (TRUE) {
    ## The vote dataset for SVM
    respath <- "results_uci"
    print_result_tables_3(respath, "vote", "svm", dataset_specs = FALSE)
}

if (TRUE) {
    ## Summarise the UCI datasets for SVM and RF
    respath_uci <- "results_uci_add"

    cat("SVM\n\n")
    make_grouping_table_3(respath_uci, "svm")
    cat("\n\nRandom forest\n\n")
    make_grouping_table_3(respath_uci, "randomForest")
}
