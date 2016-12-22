#' Read UCI data set stored in arff format and transform all columns
#' to factors.
#'
#' @param dataset The name of the dataset without file suffix.
#' @param datapath The path to the directory containing the datasets,
#'                 with trailing slash.
#' @param factorize Should the columns be transformed to
#'                  factors. Default is FALSE
#' @param na.omit Omit NAs. Default is TRUE.
#' @return The classification accuracy.
#'
#' @export
read_uci_dataset <- function(datapath, dataset, factorize = FALSE, na.omit = TRUE, remove_constant = TRUE) {

    data <- read.arff(file.path(datapath, paste0(dataset, ".arff")))

    if (na.omit)
        data <- na.omit(data)

    if (remove_constant) {
        ind_const <- as.numeric(which(sapply(data[-ncol(data)], function(i) length(unique(i))) == 1))
        if (length(ind_const) > 0)
            data <- data[,-ind_const]

    }

    colnames(data)[ncol(data)] <- "class"

    if(factorize) {
        for(i in colnames(data)) {
            data[,i] <- factor(data[,i])
        }
    }

    names(data) <- gsub("-", "_", names(data))
    names(data) <- gsub(" ", "_", names(data))
    names(data) <- gsub("[?]", "", names(data))
    names(data) <- gsub("/", "", names(data))

    list("name" = dataset, "data" = data)
}


#' Get properties of a dataset.
#'                                       #
#' Get properties of a dataset.
#'
#' @param dataset The dataset (a list).
#'
#' @return The dataset structure with a new list element \code{properties} added containing properties of the dataset.
#'
#' @export
get_dataset_properties <- function(dataset) {

    dataset[["properties"]] <- list("name"          = dataset$name,
                                    "names"         = names(dataset$data)[-ncol(dataset$data)],
                                    "n_classes"     = length(unique(dataset$data$class)),
                                    "n_attributes"  = ncol(dataset$data) - 1,
                                    "n"             = nrow(dataset$data),
                                    "major_class_p" = max(table(dataset$data$class)) / nrow(dataset$data))
    dataset

}


#' Split dataset into training and testing (50/50).
#'                                       #
#' Split dataset into training and testing (50/50).
#'
#' @param dataset The dataset
#' @param classname the name of the class in the dataset. Default is \code{class}.
#'
#' @return The dataset structure with two new elements (\code{data_train} and \code{data_test}).
#'
#' @export
split_dataset <- function(dataset, classname = "class") {
    ind                     <- testsplit(dataset$data, stratify = classname)
    dataset[["data_train"]] <- dataset$data[ind, ]
    dataset[["data_test"]]  <- dataset$data[-ind, ]

    dataset
}


#' Split dataset into 3 parts: training, testing, validation (50/25/25).
#'                                       #
#' Split dataset into 3 parts: training, testing, validation (50/25/25).
#'
#' @param dataset The dataset.
#' @param classname the name of the class in the dataset. Default is \code{class}.
#'
#' @return The dataset structure with two new elements (\code{data_train}, \code{data_test} and \code{data_validation}).
#'
#' @export
split_dataset_ttv <- function(dataset, classname = "class") {

    tmp <- dataset$data
    ind_train <- testsplit(tmp, stratify = classname, fraction = 1/2)
    dataset[["data_train"]] <- tmp[ind_train,]

    tmp <- tmp[-ind_train,]
    ind_test <- testsplit(tmp, stratify = classname, fraction = 1/2)
    dataset[["data_test"]] <- tmp[ind_test,]

    dataset[["data_validation"]] <- tmp[-ind_test,]

    dataset

}


#' List datasets (arff-files) in a given path.
#'                                       #
#' List datasets (arff-files) in a given path.
#'
#' @param datapath A path.
#'
#' @return List of arff-files.
#'
#' @export
list_datasets <- function(datapath) {
    flist <- list.files(datapath, pattern = "*.arff")
    gsub(".arff", "", flist)

}


#' List datasets (arff-files) in a given path sorted by their number of attributes.
#'                                       #
#' List datasets (arff-files) in a given path sorted by their number of attributes.
#'
#' @param datapath A path.
#'
#' @return List of arff-files (sorted).
#'
#' @export
list_datasets_sorted <- function(datapath) {
    dataset_list <- list_datasets(datapath)
    n_attr_list  <- sapply(dataset_list, function(i) get_dataset_properties(read_uci_dataset(datapath, i))$properties$n_attributes)
    list("names" =  dataset_list[order(n_attr_list)],
         "n_attributes" = n_attr_list[order(n_attr_list)])
}


#' Analyze the given dataset.
#'
#' Find the attribute interactions in the given dataset.
#'
#' @param dataset The dataset to analyze
#' @param classname The name of the class attribute in the dataset. Default is \code{class}.
#' @param seed Random seed, default is 42.
#' @param classifier The classifier to be used, as a string. Default is \code{NULL}.
#' @param alpha Significance level (default is 0.05).
#' @param R Number of samples to use for calculating the accuracy. Default is R = 1 + (1 / alpha).
#' @param Rmin Ninimum number of replications to use for calculating p-values. Default is 250.
#' @param Rmax Maximum number of replications to use for calculating p-values. Default is 500.
#' @param z Parameter for confidence band for p-values. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param prune_singletons Should singletons be pruned. Default is \code{TRUE}.
#' @param parallel Calculate p-values in parallel (Boolean, default is \code{TRUE}). If parallel is used the results are not deterministic using the same random seed.
#'
#' @return A results strucure (list with fields).
#'
#' @export
analyze_dataset <- function(dataset, classname = "class", seed = 42, classifier = NULL, alpha = 0.05, R = (1 + ceiling(1 / alpha)), Rmin = 250, Rmax = 500, z = 2.6, prune_singletons = TRUE, parallel = TRUE) {
    if (is.null(classifier))
        stop("No classifier specified!")

  
    classifier_str <- classifier
    classifier     <- get(classifier_str)

    ## Set the random seed
    if (parallel) {
        RNGkind("L'Ecuyer-CMRG")
    }

    set.seed(seed)
    
    ## Set dataset properties
    dataset <- get_dataset_properties(dataset)

    ## Split the dataset into training, testing and validation
    ## dataset <- split_dataset(dataset, classname = classname)
    dataset <- split_dataset_ttv(dataset, classname = classname)

    ## Start timer
    timelist <- list()
    t_start <- Sys.time()

    ## --------------------------------------------------
    ## Perform the analysis
    ## --------------------------------------------------
    ## (1) Sorting step
    cat("Running sorting step.")
    t_tmp                <- Sys.time()
    attributes_sorted    <- sid_sort(dataset$data_train, dataset$data_test, R = R, classifier = classifier, parallel = parallel)
    timelist[["t_sort"]] <- as.numeric(difftime(Sys.time(), t_tmp, units = "secs"))
    cat("\t[DONE]\n")

    ## (2) Grouping step
    cat("Running grouping step.")
    t_tmp                  <- Sys.time()
    res_tree               <- sid_group(dataset$data_train, dataset$data_test, attributes_sorted, R = R, classifier = classifier, parallel = parallel)
    timelist[["t_group"]]  <- as.numeric(difftime(Sys.time(), t_tmp, units = "secs"))
    cat("\t[DONE]\n")
    
    ## (3) Testing step
    cat("Running testing step.")
    t_tmp             <- Sys.time()
    ## res_tree_p        <- sid_p(dataset$data_train, dataset$data_test, res_tree[["trees"]], R = R, classifier = classifier)

    res_tree_p        <- sid_p(dataset$data_train, dataset$data_validation, res_tree[["trees"]], classifier = classifier, Rmin = Rmin, Rmax = Rmax, z = z, alpha = alpha, parallel = parallel)
    timelist[["t_p"]] <- as.numeric(difftime(Sys.time(), t_tmp, units = "secs"))
    cat("\t[DONE]\n\n")

    ## Only keep trees that are "valid" (i.e., no sig diff)
    res_tree_p_filtered <- sid_filter_trees(res_tree_p, alpha = alpha, max_k = FALSE)

    if (prune_singletons) {
        ## Prune the final tree, if possible
        res_tree_p_pruned          <- NULL
        res_tree_p_pruned_filtered <- NULL
        timelist[["t_pruned"]]     <- NULL

        if (length(res_tree_p_filtered) > 0) {
            t_tmp                      <- Sys.time()

            res_tree_p_pruned          <- sid_prune_treelist(res_tree_p_filtered, dataset$data_train, dataset$data_validation, R = R,  Rmin = Rmin, Rmax = Rmax, alpha = alpha, z = z, classifier = classifier, parallel = parallel)
            res_tree_p_pruned_filtered <- sid_filter_trees(res_tree_p_pruned, alpha = alpha, max_k = FALSE)
            timelist[["t_pruned"]]     <- as.numeric(difftime(Sys.time(), t_tmp, units = "secs"))
        }

        dataset[["results"]][["tree_p_pruned"]]          <- res_tree_p_pruned
        dataset[["results"]][["tree_p_pruned_filtered"]] <- res_tree_p_pruned_filtered

    }

    ## --------------------------------------------------

    ## Stop time and duration
    timelist[["t_total"]] <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

    dataset[["results"]][["attributes_sorted"]]      <- attributes_sorted
    dataset[["results"]][["tree"]]                   <- res_tree
    dataset[["results"]][["tree_p"]]                 <- res_tree_p
    dataset[["results"]][["tree_p_filtered"]]        <- res_tree_p_filtered
    dataset[["results"]][["classifier"]]             <- classifier_str
    dataset[["results"]][["original_accuracy"]]      <- sid_get_goodness(dataset$data_train, dataset$data_test, classifier = classifier)
    dataset[["results"]][["number_of_samples"]]      <- R
    dataset[["results"]][["R"]]                      <- R ## same as the number of samples
    dataset[["results"]][["Rmin"]]                   <- Rmin
    dataset[["results"]][["Rmax"]]                   <- Rmax
    dataset[["results"]][["alpha"]]                  <- alpha
    dataset[["results"]][["z"]]                      <- z

    dataset[["results"]][["time"]]                   <- timelist

    dataset
}


#' Save results as rds file.
#'                                       #
#' Save results as rds file, automatically generating the filename based on the dataset name and the used classifier.
#'
#' @param res The results structure.
#' @param savepath Path where to save the resutls.
#'
#' @return Nothing.
#'
#' @export
save_result <- function(res, savepath) {
    ds <- res$properties$name
    cl <- res$results$classifier
    savename <- file.path(savepath, paste0(ds, "_", cl, ".rds"))
    saveRDS(res, file = savename, compress = "xz")
}


#' Load results.
#'                                       #
#' Load results in a given folder (where they were saved using
#' \code{save_result}, given the name of the dataset and the name of
#' the used classifier).
#'
#' @param datapath Directory with the data files.
#' @param dataset The name of the dataset.
#' @param classifier The name of the classifier.
#'
#' @return Nothing.
#'
#' @export
load_result <- function(datapath, dataset, classifier) {
    fname <- file.path(datapath, paste0(dataset, "_", classifier, ".rds"))
    if (file.exists(fname))
        readRDS(fname)
    else
        NULL
}


#' Perform experiments on UCI datasets (in arff format).
#'                                       #
#' Perform experiments on UCI datasets (in arff format).
#'
#' @param dataset_list List of datasets
#' @param classifier_list List of classifiers
#' @param datapath Directory with the data files.
#' @param savepath Path where to save the resutls.
#' @param R Number of samples to use. Default is\code{NULL} in which
#'     case R = 1 + (1 / alpha) will be used by
#'     \code{analyze_dataset}.
#' @param Rmin Ninimum number of replications to use for calculating p-values.
#' @param Rmax Maximum number of replications to use for calculating p-values.
#' @param alpha Significance level (default is 0.05).
#' @param z Parameter for confidence band for p-values. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param prune_singletons Should singletons be pruned. Default is \code{TRUE}.
#' @return Nothing.
#'
#' @export
do_experiment_uci <- function(dataset_list, classifier_list, datapath, savepath, R = NULL, Rmin = NULL, Rmax = NULL, z = NULL, alpha = NULL, prune_singletons = FALSE) {
    for (ds in dataset_list) {
        dataset <- read_uci_dataset(datapath, dataset = ds)

        for (cl in classifier_list) {
            msg <- paste0(ds, " -- ", cl, "\n")
            cat(msg)

            if (is.null(R)) {
                res <- analyze_dataset(dataset, classname = "class", classifier = cl, Rmin = Rmin, Rmax = Rmax, alpha = alpha, z = z, prune_singletons = prune_singletons)
            } else {
                res <- analyze_dataset(dataset, classname = "class", classifier = cl, R = R,  Rmin = Rmin, Rmax = Rmax, alpha = alpha, z = z, prune_singletons = prune_singletons)
            }

            save_result(res, savepath)
        }
    }
}


#' Perform experiments on synthetic datasets.
#'                                       #
#' Perform experiments on synthetic datasets.
#'
#' @param classifier_list List of classifiers
#' @param datapath Directory with the data files.
#' @param savepath Path where to save the resutls.
#' @param R Number of samples to use. Default is\code{NULL} in which
#'     case R = 1 + (1 / alpha) will be used by
#'     \code{analyze_dataset}.
#' @param Rmin Ninimum number of replications to use for calculating p-values.
#' @param Rmax Maximum number of replications to use for calculating p-values.
#' @param alpha Significance level (default is 0.05).
#' @param z Parameter for confidence band for p-values. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param prune_singletons Should singletons be pruned. Default is \code{TRUE}.
#'
#' @return Nothing.
#'
#' @export
do_experiment_synthetic <- function(classifier_list, datapath, savepath, R = NULL, Rmin = NULL, Rmax = NULL, z = NULL, alpha = NULL, prune_singletons = FALSE) {
    dataset <- make_synthetic_dataset(N = 500, seed = 42, mg2 = 0.6)

    for (cl in classifier_list) {
        cat(cl, "\n")
        if (is.null(R)) {
            res <- analyze_dataset(dataset, classname = "class", classifier = cl, Rmin = Rmin, Rmax = Rmax, alpha = alpha, z = z, prune_singletons = prune_singletons)
        } else {
            res <- analyze_dataset(dataset, classname = "class", classifier = cl, R = R, Rmin = Rmin, Rmax = Rmax, alpha = alpha, z = z, prune_singletons = prune_singletons)
        }

        save_result(res, savepath)
    }
}
