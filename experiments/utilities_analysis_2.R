#' Print results tables in latex format.
#'
#' Print results tables in latex format.
#'
#' @param respath Path to folder with datasets (.rds-files).
#' @param dataset_list List containing the names of the datasets to print tables for.
#' @param classifier String with the name of the classifier for which
#'     to get the dataets.
#' @param dataset_specs Should dataset specs (size etc) be printed. Default is \code{TRUE}.
#'
#' @return Nothing
#' @export
print_result_tables_3 <- function(respath, dataset_list, classifier, dataset_specs = TRUE) {
    for (ds in dataset_list) {
        res <- load_result(respath, ds, classifier)
        mat <- make_group_matrix_3(res, alpha = alpha, full_tree = full_tree)

        if (! is.null(mat)) {
            if (! dataset_specs)
                res <- NULL
            out <- group_matrix_to_latex_3(mat, dataset = ds, classifier = classifier, res = res)
            print_res(out)
            cat("\n\n")
        }
    }
}

make_group_matrix_3 <- function(res, alpha = 0.05, full_tree = FALSE) {

    ind <- res$results$attributes_sorted

    treelist <- res$results$tree_p

    if (length(treelist) < 1) {
        return(NULL)
    }

    if ("goodness" %in% names(treelist))
        treelist <- list(treelist)

    ## Add the original tree with k = 1 to the list
    tree_tmp <- list()
    tree_tmp$tree <- list(unique(unlist(treelist[[1]]$tree)))
    tree_tmp$goodness <- NA
    tree_tmp$k <- 1
    tree_tmp$p <- NA
    tree_tmp$acc_stats$g0 <- treelist[[1]]$acc_stats$g0
    tree_tmp$acc_stats$mean <- treelist[[1]]$acc_stats$g0
    tree_tmp$acc_stats$range <- c(NA, NA)
    tree_tmp$acc_stats$sd <- NA
    tree_tmp$acc_stats$p <- NA
    tree_tmp$acc_stats$ci <- NA


    treelist <- c(list(tree_tmp), treelist)

    names    <- res$properties$names

    nc <- length(names) + 3
    nr <- length(treelist) + 1

    mat <- matrix("", nrow = nr, ncol = nc)

    mat[1, ] <- c("k", "Ci", "", names[ind])

    for (i in seq.int(length(treelist))) {
        tmp <- treelist[[i]]

        mat[(i+1), 1] <- tmp$k


        if (is.na(tmp$acc_stats$ci[1])) {
            mat[(i+1), 2] <- sprintf("%.3f", tmp$acc_stats$g0)
        } else{
            mat[(i+1), 2] <- paste0("[", sprintf("%.3f", tmp$acc_stats$ci[1]), ",\\;", sprintf("%.3f", tmp$acc_stats$ci[2]), "]")
        }

        if ((! is.na(tmp$acc_stats$ci[1])) && (tmp$acc_stats$g0 >= tmp$acc_stats$ci[1]) && (tmp$acc_stats$g0 <= tmp$acc_stats$ci[2])) {
            mat[(i+1), 3] <- "*"
        } else {
            mat[(i+1), 3] <- ""
        }

        mat[(i+1), (4:nc)] <- tree_to_letters(tmp$tree)
    }
    gsub("NA", "", mat)

}


#' Convert matrix showing the grouping to latex format.
#'
#' Convert matrix showing the grouping to latex format.
#'
#' @param mat Grouping matrix.
#' @param dataset Name of the dataset.
#' @param classifier Name of the used classifier
#'
#' @return Grouping matrix as a latex table.
#' @export
group_matrix_to_latex_3 <- function(mat, dataset = "", classifier = "", res = FALSE) {
    nc <- ncol(mat)

    title_string <- paste0(dataset, " -- using ", classifier)
    caption_string <- paste0("Grouping of the \\textrm{", dataset ,"} dataset using ", classifier, ".")


    out <- list()
    out <- add_row(out, "\\setlength{\\tabcolsep}{0.5ex}")
    out <- add_row(out, "\\begin{table}")
    out <- add_row(out, "\\centering")
    out <- add_row(out, paste0("\\begin{tabular}", "{", paste0(rep("c", nc), collapse = ""), "}"))
    out <- add_row(out, "\\toprule")

    column_labels <- sapply(mat[1, 4:nc], function(i) paste0("\\rotatebox{90}{", i, "}"), USE.NAMES = FALSE)
    column_labels <- gsub("_", "-", column_labels)

    out <- add_row(out, paste0(paste0(c("\\textbf{k} & \\textbf{CI} &", column_labels), collapse = " & "), "\\\\"))

    out <- add_row(out, paste0("\\cmidrule(){1-3}"))
    out <- add_row(out, paste0("\\cmidrule(l){4-", nc, "}"))

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



get_best_tree_using_ci <- function(res) {
    ## Original accuracy
    a0 <- res$extra$g0

    ## Find the maximum cardinality grouping for which a0 is within the CIs
    x <- sapply(seq.int(length(res$results$tree_p)), function(i) {ci <- res$results$tree_p[[i]]$acc_stats$ci ; ifelse(ci[2] >= a0, 1, 0) })
    ind <- tail(which(x != 0), 1)

    if (length(ind) >= 1) {
        res$results$tree_p[[ind]]
    } else {
        list("tree" = list(c(which(names(res$data) == "class"), which(names(res$data) != "class"))), "k" = 1)
    }
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
make_grouping_table_3 <- function(respath, classifier, show_specs = TRUE) {
    ## Get the ready datasets
    dataset_list <- get_ready_datasets(respath, classifier = classifier)

    ## Loop over the ready datasets
    for (dsname in dataset_list) {
        cat("\\textbf{", dsname, "}")

        res       <- load_result(respath, dsname, classifier)


        best_tree <- get_best_tree_using_ci(res)
        prop_tree <- count_tree(best_tree$tree)

        ## Get the CI for accuracy
        ci <- best_tree$acc_stats$ci

        if (! is.null(best_tree))
            nattrs <- sort(c(0, sapply(best_tree$tree, function(i) length(i) -1)), decreasing = TRUE)


        if (is.null(best_tree)) {
            g <- paste0(rep(" & ", 7), collapse = "")
        }  else {
            if (show_specs) {
                g <-  paste0(res$properties$n_attributes, " & &")
            } else {
                g <- ""
            }

            g <- paste0(g,
                        round(best_tree$k, 0), " & ",
                        nattrs[1], " & ",
                        nattrs[2], " & ",
                        sprintf("%.3f", res$extra$g0), " & ",
                        "[",
                        sprintf("%.3f", ci[1]), ", ",
                        sprintf("%.3f", ci[2]),
                        "] & ",
                        sprintf("%.2f", res$extra$og$p)
                        )
        }

        cat(" & ", g)
        cat("\\\\")
        cat("\n")
    }
}
