library(astrid)
library(foreign)
library(xtable)
source("utilities_analysis.R")


## Include running times of the algoritms in the dataset table.
add_running_times <- TRUE

## A helper function for sanitizing column names
san.col.func <- function(x) {
    paste0("\\textbf{", x, "}")
}

## Make a table with the properties of the datasets
path_prefix <- "./"

datapath     <- "data/UCI/"
results_path <- "results_uci/"

## ------------------------------
results_path <- paste0(path_prefix, results_path)
cat("Using path: ", results_path, "\n\n\n")

dataset_list <- list_datasets_sorted(datapath)

dataset_table <- make_dataset_table(datapath, dataset_list)

## Add running times (here divided by 60 to convert from seconds to minutes)
if (add_running_times) {
    time_mat <- calculation_time_table(results_path, dataset_list$names, c("svm", "randomForest"))
    time_mat <- matrix(sprintf("%.1f", time_mat$time / 60), ncol = 2, byrow = FALSE)

    tmp <- colnames(dataset_table)
    dataset_table <- cbind(dataset_table, time_mat)
    colnames(dataset_table) <- c(tmp, c("T(SVM)", "T(RF)"))
}

print.xtable(xtable(dataset_table), booktabs = TRUE, sanitize.text.function = identity,  include.rownames = FALSE, sanitize.colnames.function = san.col.func)

## Running times for the synthetic dataset
print(sprintf("%.1f", calculation_time_table("results_synthetic/", c("synthetic"), c("svm", "randomForest"))$time / 60))
