#' Add element to list.
#'
#' Add element to list.
#'
#' @param x List.
#' @param s Element to add to list.
#' @return X with s added.
#'
#' @export
add_row <- function(x, s) {
    x[[length(x) + 1]] <- s
    x
}


#' Convert matrix showing the grouping to latex format.
#'
#' Convert matrix showing the grouping to latex format.
#'
#' @param mat Grouping matrix.
#' @param dataset Name of the dataset.
#' @param classifier Name of the used classifier
#' @param res Results structure.
#' 
#' @return Grouping matrix as a latex table.
#' @export
group_matrix_to_latex <- function(mat, dataset = "", classifier = "", res = NULL) {
    nc <- ncol(mat)

    title_string <- paste0(dataset, " -- using ", classifier)
    caption_string <- paste0("Grouping of the \\textrm{", dataset ,"} dataset using ", classifier, ".")


    out <- list()
    out <- add_row(out, "\\setlength{\\tabcolsep}{0.5ex}")
    out <- add_row(out, "\\begin{table}")
    out <- add_row(out, "\\centering")
    out <- add_row(out, paste0("\\begin{tabular}", "{", paste0(rep("c", nc), collapse = ""), "}"))
    out <- add_row(out, "\\toprule")

    if (! is.null(res)) {
        header <- paste0("\\multicolumn{", nc, "}{l}{",
                         "\\texttt{", res$properties$name, "}", "\\hspace*{5mm}",
                         "$\\mathrm{N}_\\mathrm{item}$ : ", res$properties$n, "\\hspace*{3mm}",
                         "$\\mathrm{N}_\\mathrm{attr}$ : ", res$properties$n_attributes, "\\hspace*{3mm}",
                         "$\\mathrm{N}_\\mathrm{class}$ : ", res$properties$n_classes, "\\hspace*{3mm}",
                         "major class : ", sprintf("%.2f", res$properties$major_class_p),
                         "} \\\\")
        out <- add_row(out, header)
        out <- add_row(out, "\\midrule")
    }

    column_labels <- sapply(mat[1,4:nc], function(i) paste0("\\rotatebox{90}{", i, "}"), USE.NAMES = FALSE)
    column_labels <- gsub("_", "-", column_labels)

    out <- add_row(out, paste0(paste0(c("\\textbf{k} & \\textbf{acc} & \\textbf{p}", column_labels), collapse = " & "), "\\\\"))

    out <- add_row(out, paste0("\\cmidrule(lr){1-3}"))
    out <- add_row(out, paste0("\\cmidrule(lr){4-", nc, "}"))

    for (i in seq.int(2, nrow(mat))) {
        out <- add_row(out, paste0("", paste0(paste0(mat[i,], collapse = " & "), "\\\\")))
    }

    out <- add_row(out, "\\bottomrule")
    out <- add_row(out, "\\end{tabular}")
    out <- add_row(out, paste0("\\caption{", caption_string, "}"))
    out <- add_row(out, paste0("\\label{", "res:tab:", dataset, ":", tolower(classifier), "}"))
    out <- add_row(out, "\\end{table}")
    out <- add_row(out, "\\setlength{\\tabcolsep}{6pt}")
    out

}


#' Print latex table
#'
#' Print latex table
#'
#' @param x List where each element is a row for the results table.
#' 
#' @return Nothing.
#' @export
print_res <-function(x) {
    for (i in seq.int(length(x)))
        cat(x[[i]], "\n")
}


#' Get list of ready datasets.
#'
#' Get list of ready datasets.
#'
#' @param datapth Path to folder with datasets (.rds-files).
#' @param classifier String with the name of the classifier for which
#'     to get the ready datasets, e.g., "svm".
#' 
#' @return List with the names of th ready datasets.
#' @export
get_ready_datasets <- function(datapath, classifier) {
    gsub(paste0("_", classifier, ".rds"), "", list.files(datapath, pattern = classifier))
}


#' Print results tables in latex format.
#'
#' Print results tables in latex format.
#'
#' @param respath Path to folder with datasets (.rds-files).
#' @param dataset_list List containing the names of the datasets to print tables for.
#' @param classifier String with the name of the classifier for which
#'     to get the dataets.
#' @param dataset_specs Should dataset specs (size etc) be printed. Default is \code{TRUE}.
#' @param alpha Significance level for filtering groupings. Default is 0.05.
#' @param full_tree Should the full list of trees be printed, not just those for which p >= alpha.
#' 
#' @return Nothing
#' @export
print_result_tables <- function(respath, dataset_list, classifier, dataset_specs = TRUE, alpha = 0.05, full_tree = FALSE) {
    for (ds in dataset_list) {
        res <- load_result(respath, ds, classifier)
        mat <- make_group_matrix(res, alpha = alpha, full_tree = full_tree)

        if (! is.null(mat)) {
            if (! dataset_specs)
                res <- NULL
            out <- group_matrix_to_latex(mat, dataset = ds, classifier = classifier, res = res)
            print_res(out)
            cat("\n\n")
        }
    }
}


#' Make calculation time matrix.
#'
#' Make calculation time matrix.
#'
#' @param respath Path to folder with datasets (.rds-files).
#' @param dataset_list List containing the names of the datasets to print tables for.
#' @param classifier_list List of strings with the names of the classifier for which
#'     to get the dataets.
#' 
#' @return Matrix with calculation times.
#' @export
calculation_time_table <- function(respath, dataset_list, classifier_list) {
    Nc <- length(classifier_list)
    Nr <- length(dataset_list)

    out_time <- matrix(NA, nrow = Nr, ncol = Nc)
    out_find <- matrix(0, nrow = Nr, ncol = Nc)

    for (i in seq.int(Nc)) {
        for (j in seq.int(Nr)) {
            res <- load_result(respath, dataset_list[j], classifier_list[i])

            if (! is.null(res)) {
                if ("t_pruned" %in% names(res$results$time))
                    out_time[j,i] <- res$results$time$t_total - res$results$time$t_pruned
                else
                    out_time[j,i] <- res$results$time$t_total
            }
        }
    }

    colnames(out_time) <- classifier_list
    rownames(out_time) <- dataset_list

    list("time" = out_time)
}


#' Make 
#'
#' Make calculation time matrix.
#'
#' @param respath Path to folder with datasets (.rds-files).
#' @param dataset_list List containing the names of the datasets to print tables for.
#' @param classifier_list List of strings with the names of the classifier for which
#'     to get the dataets.
#' 
#' @return Matrix with calculation times.
#' @export
make_dataset_table <- function(datapath, dataset_list, classname = "class") {
    datasets <- dataset_list$names

    out           <- matrix(ncol = 5, nrow = length(datasets))

    for (i in seq.int(length(datasets))) {
        ds_name  <- datasets[i]
        dataset  <- read_uci_dataset(datapath, dataset = ds_name)

        out[i,1] <- paste0("\\texttt{", dataset$name, "}")
        out[i,2] <- nrow(dataset$data)
        out[i,3] <- length(unique(dataset$data[[classname]]))
        out[i,4] <- ncol(dataset$data) - 1
        out[i,5] <- sprintf("%.2f", max(table(dataset$data$class)) / nrow(dataset$data))
    }
    out <- cbind(seq.int(length(datasets)), out)
    colnames(out) <- c("n", "dataset", "Size", "Classes", "Attributes", "Major class")
    out
}


#' Convert numeric matrix to character matrix.
#'
#' Convert numeric matrix to character matrix.
#'
#' @param x Numeric matrix
#' @param num_form Formatting argument to sprintf.
#' 
#' @return The information in x where numbers are represented as characters.
#' @export
num_matrix_to_char_matrix <- function(x, num_form = "%.2f") {
    out <- matrix(NA, ncol = ncol(x), nrow = nrow(x))
    colnames(out) <- colnames(x)
    rownames(out) <- rownames(x)

    for (i in seq.int(nrow(x)))
        for (j in seq.int(ncol(x)))
            out[i,j] <- sprintf(num_form, x[i,j])

    out
}


#' Print calculation time table.
#'
#' Print calculation time table.
#'
#' @param x Matrix.
#' 
#' @return Nothing.
#' @export
print_calculation_time_table <- function(x) {
    print.xtable(xtable(x),
                 booktabs = TRUE,
                 include.rownames = TRUE,
                 sanitize.text.function = identity,
                 sanitize.rownames.function = function(i) {paste0("\\texttt{", i, "}")},
                 sanitize.colnames.function = function(i) {paste0("\\textbf{", i, "}")}
                 )
}


#' Get the best tree from the results structure
#'
#' Get the best tree from the results structure
#'
#' @param res A results structure.
#' 
#' @return The best tree.
#' @export
get_best_tree <- function(res) {
    if (length(res$results$tree_p_filtered) > 0)
        rev(res$results$tree_p_filtered)[[1]]
    else
        list("tree" = list(c(which(names(res$data) == "class"), which(names(res$data) != "class"))),
             "k" = 1)

}

#' Print a table showing the dataset name and the propoerties of the groupings for multiple datasets for a classifier.
#'
#' Print a table showing the dataset name and the propoerties of the groupings for multiple datasets for a classifier.
#'
#' @param respath Path containing datasets.
#' @param classifier String containing the name of the classifier.
#' 
#' @return The best tree.
#' @export
make_grouping_table <- function(respath, classifier) {
    ## Get the ready datasets
    dataset_list <- get_ready_datasets(respath, classifier = classifier)

    ## Loop over the ready datasets
    for (dsname in dataset_list) {
        cat("\\textbf{", dsname, "}")

        res       <- load_result(respath, dsname, classifier)
        best_tree <- get_best_tree(res)
        prop_tree <- count_tree(best_tree$tree)

        if (! is.null(best_tree))
            nattrs <- sort(c(0, sapply(best_tree$tree, function(i) length(i) -1)), decreasing = TRUE)

        if (is.null(best_tree))
            g <- paste0(rep(" & ", 7), collapse = "")
        else
            g <- paste0(
                res$properties$n_attributes, " & ",
                round(best_tree$k, 0), " & ",
                nattrs[1], " & ",
                nattrs[2], " & ",

                sprintf("%.2f", best_tree$p), " & ",

                sprintf("%.2f", res$extra$g0), " & ",
                sprintf("%.2f", res$extra$a_ave), " & ",
                "[",
                sprintf("%.2f", res$extra$a_min), ", ",
                sprintf("%.2f", res$extra$a_max),
                "] & ",
                sprintf("%.2f", res$extra$sd), " & ",
                sprintf("%.2f", res$extra$og$p))

        cat(" & ", g)
        cat("\\\\")
        cat("\n")
    }
}


#' Get properties of a tree.
#'
#' Get properties of a tree, e.g., number of groups, attribute groups
#' and singleton groups.
#'
#' @param tree A tree (grouping).
#' @param as_string Boolean, default is \code{FALSE} Return string instead of list with numbers.
#' 
#' @return Either list with the propoerties or a string.
#' @export
count_tree <- function(tree, as_string = FALSE) {
    if (length(tree) == 0)
        return(NULL)

    ## count the number of groups
    n_groups <- sum(sapply(tree, function(g) if (length(g) > 2) 1 else 0))

    ## count the number of singletons
    n_sing <- sum(sapply(tree, function(g) if (length(g) == 2) 1 else 0))

    ## count total number of attributes
    n_attr <- length(unlist(tree)) - n_groups - n_sing

    out <- list("n_tot" = n_attr, "n_singleton" = n_sing, "n_groups" = n_groups)

    if (as_string)
        out <- paste0("Na=", out$n_tot, ", Ng=", out$n_groups, ", Ns=", out$n_singleton)
    out

}


#' Print a tree (grouping) in latex format.
#'
#' Print a tree (grouping) in latex format.
#'
#' @param tree A tree (grouping).
#' @param res A results structure.
#' 
#' @return The grouping in latex format.
#' @export
tree_to_latex <- function(tree, res = NULL) {
    tmp <- sapply(tree, function(g) if (g[1] != 0) g[-1])
    ind <- sapply(tmp, function(i) !(is.null(i)))
    if (! all(ind)) {
        ind_keep <- which(ind)
        tree2 <- tmp[ind_keep]
    } else {
        tree2 <- tmp
    }
    ## get attribute names from res if given
    if (! is.null(res)) {
        attr_names <- names(res$data_test)
        attr_names <- tolower(gsub("_", "", attr_names))
        attr_names <- gsub("attribute", "a", attr_names)
        tree2 <- sapply(tree2, function(g) attr_names[g])
    }

    paste0("(", paste0(sapply(tree2, function(g) paste0("(", paste(g, collapse = ", "), ")")), collapse = ", "), ")")

}
