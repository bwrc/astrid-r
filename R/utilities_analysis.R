#' Crate a results matrix from the analysis results of a dataset.
#'
#' Create a matrix summarising the results from the analysis of a
#' datast. The table contains groupings (denoted by letters) and the
#' degree, accuracy and p-value for each grouping.
#'
#' @param res A results structure, i.e., the output from \code{analyze_dataset}.
#' @param alpha The significance level for filtering (showing) trees only above this threshold. The default is 0.05.
#' @param full_tree Return the full tree (also trees for which p < alpha). Default is \code{FALSE}.
#'
#' @return A matrix with the results.
#'
#' @export
make_group_matrix <- function(res, alpha = 0.05, full_tree = FALSE) {

    ind <- res$results$attributes_sorted


    if (full_tree) {
        treelist <- res$results$tree_p

    } else {
        treelist <- res$results$tree_p_filtered
    }

    if (length(treelist) < 1) {
        return(NULL)
    }

    if ("goodness" %in% names(treelist))
        treelist <- list(treelist)

    if (! (full_tree))
        treelist <- sid_filter_trees(treelist, alpha = alpha, max_k = FALSE)

    if ("goodness" %in% names(treelist))
        treelist <- list(treelist)

    if (length(treelist) < 1) {
        return(NULL)
    }

    names    <- res$properties$names

    nc <- length(names) + 3
    nr <- length(treelist) + 1

    mat <- matrix("", nrow = nr, ncol = nc)

    mat[1, ] <- c("k", "acc", "p", names[ind])

    for (i in seq.int(length(treelist))) {
        tmp <- treelist[[i]]

        mat[(i+1), 1] <- tmp$k
        mat[(i+1), 2] <- sprintf("%.2f", tmp$goodness)
        mat[(i+1), 3] <- sprintf("%.2f", tmp$p)
        mat[(i+1), (4:nc)] <- tree_to_letters(tmp$tree)
    }
    mat
}


#' Return full list of trees.
#'
#' Return full list of trees, including trees for which p < alpha.
#'
#' @param res A results structure, i.e., the output from
#'     \code{analyze_dataset}.
#' @param alpha The significance level for filtering (showing) trees
#'     only above this threshold. The default is 0.05.
#'
#' @return Full list of trees.
#'
#' @export
get_full_treelist <- function(res, alpha = 0.05) {
    treelist_out <- res$results$tree_p

    for (k in seq.int(2, (1 + length(res$results$tree_p)))) {
        ind <- get_k_tree(res$results$tree_p_pruned, k)

        if (length(ind) > 0)
            if (res$results$tree_p_pruned[[ind]]$p >= alpha)
                treelist_out[[k-1]] <- res$results$tree_p_pruned[[ind]]
    }
    treelist_out
}


#' Get the k:th tree from a list of trees.
#'
#' Get the k:th tree from a list of trees.
#'
#' @param treelist List of trees.
#' @param k Index of tree to get.
#'
#' @return The k:th tree.
#'
#' @export
get_k_tree <- function(treelist, k) {
    which(sapply(treelist, function(i) i$k) == k)
}


#' Represent a grouping (tree) using letters.
#'
#' Represent a grouping (tree) using letters.
#'
#' @param tree A tree
#'
#' @return The tree represented using letters.
#'
#' @export
tree_to_letters <- function(tree) {
    out <- sapply(seq.int(length(tree)), function(i) {
        g <- tree[[i]]
        if (g[1] > 0)
            s <- LETTERS[i]
        else
            s <- letters[i]
        s <- rep(s, length(g)-1)
        s[1] <- paste0("(", s[1])
        s[length(s)] <- paste0(s[length(s)], ")")
        s
    })
    unlist(out)
}


#' Print the results table in HTML
#'
#' Print the results table in HTML
#'
#' #' @param res A results structure, i.e., the output from
#'     \code{analyze_dataset}.
#'
#' @param alpha The significance level for filtering (showing) trees only above this threshold. The default is 0.05.
#' @param full_tree Return the full tree (also trees for which p < alpha). Default is \code{FALSE}.
#'
#' @return Nothing.
#' 
#' @export
print_result_table_html <- function(res, alpha = 0.05, full_tree = FALSE) {
    require(xtable)
    mat <- make_group_matrix(res, alpha = alpha, full_tree = full_tree)
    colnames(mat) <- mat[1,]
    mat <- mat[2:nrow(mat),]
    print(xtable(mat), type = "html", include.rownames = FALSE)
}
