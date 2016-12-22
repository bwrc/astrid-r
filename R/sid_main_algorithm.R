#' Defactor a numeric.
#'
#' Defactor a numeric factor to a true numeric.
#'
#' @param x A numeric factor to convert to a true numeric.
#'
#' @return A numeric.
#'
#' @export
defactor <- function(x) {
    as.numeric(as.character(x))
}


#' Get the goodness (accuracy) of a classifier.
#'
#' Get the goodness (accuracy) of a classifier.
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#'
#' @return The accuracy of the classifier.
#'
#' @export
sid_get_goodness <- function(data_train, data_test, classifier = svm) {
    model  <- classifier(class ~., data = data_train)
    cl_tmp <- predict(model, newdata = data_test)
    sum(data_test$class == cl_tmp) / length(cl_tmp)
}


#' Singletonize the attributes in the vector x.
#'
#' Singletonize the attributes in the vector x.
#'
#' @param x A vector with attribute numbers.
#' @param cl_id The number of the class attribute.
#'
#' @return A list where the attributes in x are singletons.
#'
#' @export
sid_singletonize <- function(x, cl_id) {
    lapply(x, function(i) c(cl_id, i))
}


#' Generate a surrogate (permuted) dataset.
#'
#' Generate a surrogate (permuted) dataset.
#'
#' @param tree The parametrisation of the permutation.
#' @param data_orig The data to permute.
#'
#' @return The permutation of data_orig using tree.
#'
#' @export
sid_gen_surrogate <- function(tree, data_orig) {
    perm  <- permutation(tree, data = data_orig)
    permutedata(data_orig, perm)
}


#' Convenience function for randomising the data inside a dataset.
#'
#' A convenience function to generate a dataset randomized using a particular tree.
#'
#' @param tree The parametrisation of the permutation.
#' @param dataset A dataset containing the data as (dataset$data).
#'
#' @return The permutation of data_orig using tree.
#'
#' @export
sid_randomize_dataset <- function(tree, dataset) {
    dataset$data <- sid_gen_surrogate(tree, dataset$data)
    dataset
}


#' Get the goodness (accuracy) of a tree.
#'
#' Get the goodness (accuracy) of a tree.
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param R Number of replications. Default is 10.
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The average goodness.
#'
#' @export
sid_tree_goodness <- function(data_train, data_test, tree, classifier, R = 10, parallel = TRUE) {
    ## ----------
    ## make a call to rnorm to consume some randomness
    rnorm(1)
    ## ----------

    if (parallel) {
        out <- simplify2array(mclapply(seq.int(R),
                                       function(i) sid_get_goodness(classifier = classifier, data_train = sid_gen_surrogate(tree, data_train), data_test = data_test),
                                       mc.cores = (detectCores() - 1))
                              )
    }
    else {
        out <- replicate(R,
                         sid_get_goodness(classifier = classifier, data_train = sid_gen_surrogate(tree, data_train), data_test = data_test)
                         )
    }
    mean(out)
}


#' Find attribute to remove from a tree.
#'
#' Testing all possibilities, find the attribute to remove from the
#' grouping that impacts the goodness the least.
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param g_ut List of untested singletons.
#' @param g_s List with already removed attributes as singletons.
#' @param id_cl The number of the class attribute.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param R Number of replications. Default is 10.
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The attribute to remove.
#'
#' @export
sid_find_singleton <- function(data_train, data_test, g_ut, g_s, id_cl, R = 10, classifier, parallel = TRUE) {
    x <- sapply(g_ut, function(i) {
        tree   <- c(list(c(id_cl, setdiff(g_ut, i))), sid_singletonize(c(g_s, i), id_cl))
        sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = R, parallel = parallel)
    })
    g_ut[which.max(x)]
}


#' Rank attributes
#'
#' After ranking, the attributes are ordered so that those belonging
#' to the same group are consecutive.
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param R Number of replications. Default is 10.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The attributes, sorted.
#'
#' @export
sid_sort <- function(data_train, data_test, R = 10, classifier = svm, parallel = TRUE) {
    id_cl   <- which(names(data_train) == "class")
    n_attr  <- ncol(data_train) - 1

    g_ut <- seq.int(n_attr)
    g_s  <- c()

    for (i in seq.int(n_attr)) {
        s    <- sid_find_singleton(data_train, data_test, g_ut, g_s, id_cl, R, classifier, parallel = parallel)
        g_ut <- setdiff(g_ut, s)
        g_s  <- c(g_s, s)
    }
    g_s
}


#' Calculate goodness for 2-group attribute sets.
#'
#' fter ranking, the attributes are ordered so that those belonging to
#' the same group are consecutive. Now calculate the accuracy for each
#' grouping formed by splitting this sorted list of attributes into a
#' tree with 2 groups, for each possible split (number of attributes -
#' 1).
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param attr_rank Sorted list of attributes (from \code{sid_sort}).
#' @param R Number of replications. Default is 10.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The attributes, sorted.
#'
#' @export
sid_halve <- function(data_train, data_test, attr_rank, R = 10, classifier = svm, parallel = parallel) {
    id_cl  <- which(names(data_train) == "class")
    n_attr <- length(attr_rank)

    sapply(seq.int(n_attr-1), function(i) {
        g_l   <- attr_rank[1:i]
        g_r   <- attr_rank[(i+1):n_attr]
        tree  <- c(list(c(id_cl, g_l), c(id_cl, g_r)))
        sid_tree_goodness(data_train, data_test, tree, R = R, classifier = classifier, parallel = parallel)
    })
}


#' Create a valid tree from a list of attribute indices.
#'
#' Create a valid tree from a list of attribute indices by adding the
#' index of the class attribute to each group in the tree.
#'
#' @param tree The tree.
#' @param data The data with the attributes.
#'
#' @return A valid tree.
#'
#' @export
sid_treeify <- function(tree, data) {
    id_cl <- which(names(data) == "class")
    sapply(tree, function(i) unique(c(id_cl, i)), simplify = FALSE)

}


#' Undo treeification.
#'
#' Remove the class index from each group in the tree.
#'
#' @param tree The tree.
#' @param data The data with the attributes.
#'
#' @return A valid tree.
#'
#' @export
sid_untreeify <- function(tree, data) {
    id_cl <- which(names(data) == "class")
    sapply(tree, function(i) setdiff(i, id_cl), simplify = FALSE)
}


#' Prune group.
#'
#' Prune group (singleton or regular group).
#'
#' @param tree The tree.
#' @param data The data with the attributes.
#' @param pg The group to prune.
#'
#' @return A valid tree.
#'
#' @export
sid_prune_group <- function(tree, data, pg) {
    id_cl     <- which(names(data) == "class")
    for (i in pg)
        tree[[i]] <- c(0, setdiff(tree[[i]], id_cl))
    tree
}


#' Partition an array into two.
#'
#' Partition an array x into two lists at the index si.
#'
#' @param x An array.
#' @param si Split index.
#'
#' @return A list with the left and right parts of x split at the
#'     index si.
#'
#' @export
sid_part <- function(x, si) {
    k <- length(si) + 1
    y <- rep(k, length(x))

    for (i in rev(si)) {
        k      <- k - 1
        y[1:i] <- k
    }

    out        <- split(x, y)
    names(out) <- NULL
    out
}

#' Find a grouping from a sorted list of attributes.
#'
#' Find the best grouping from a sorted list of attributes and the goodness.
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param attr_rank Sorted list of attributes (from \code{sid_sort}).
#' @param R Number of replications. Default is 10.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return A list with the left and right parts of x split at the
#'     index si.
#'
#' @export
sid_group <- function(data_train, data_test, attr_rank, R = 10, classifier = svm, parallel = TRUE) {
    g_vec    <- vector(mode = "numeric", length = length(attr_rank))
    tree_vec <- vector(mode = "list", length = length(attr_rank))

    split_acc  <- sid_halve(data_train, data_test, attr_rank, R = R, classifier = classifier, parallel = parallel)
    split_rank <- order(split_acc, decreasing = TRUE)

    ## Find the optimal split and the associated trees
    for (k in seq.int(from = 2, to = length(attr_rank))) {
        ## Optimal split indices for the k-grouping given by the top (k-1) splits
        si <- sort(split_rank[1:(k-1)])

        tree_cl      <- sid_treeify(sid_part(attr_rank, si), data_train)
        goodness_tmp <- sid_tree_goodness(data_train, data_test, tree_cl, R = R, classifier = classifier, parallel = parallel)

        tree_vec[[k]] <- list("tree" = tree_cl, "goodness" = goodness_tmp, "k" = k)
        g_vec[k]      <- goodness_tmp
    }

    list("goodness" = g_vec[-1], "trees" = tree_vec[-1])
}


#' Calculate the p-value for a tree.
#'
#' Calculate the p-value for a tree (grouping).
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param tree The tree
#' @param g0 The Baseline goodness, if known. Can be \code{NULL} (default).
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param R Number of replications. Default is 100.
#' @param histogram Plot histogram (Boolean, default is \code{FALSE}.
#' @param return_raw Return raw data used in the p-value calculation (Boolean, default is \code{FALSE}.
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The p-value for the tree
#'
#' @export
sid_p_tree <- function(data_train, data_test, tree, g0 = NULL, classifier = svm, R = 100, histogram = FALSE, parallel = TRUE, return_raw = FALSE) {
    ## ----------
    ## make a call to rnorm to consume some randomness
    rnorm(1)
    ## ----------

    ## Calculate the original goodness if it is not given
    if (is.null(g0))
        g0 <- sid_get_goodness(data_train, data_test, classifier = classifier)

    if (parallel) {


        d <- simplify2array(mclapply(seq.int(R), function(i) sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = 1, parallel = FALSE), mc.cores = (detectCores() - 1)))
    }
    else {
        d <- replicate(R, sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = 1, parallel = FALSE))
    }

    nu <- sum(d >= g0)
    p <- (1 + nu) / (1 + R)

    if (histogram) {
        x11()
        hist(d, xlim = c(0.5, 1), main = paste0(paste0("p = ", round(p, 5)), "\n", gsub("c", "", paste0(tree, collapse = " "))))
        abline(v = g0, col = "blue", lwd = 5, lty = 3)
    }
    if (return_raw) {
        list("d" = d, "p" = p, "g0" = g0)
    }  else {
        p
    }
}


#' Binomial confidence interval for the p-value
#'
#' binomial confidence interval for the p-value using normal approximation
#'
#' @param p Proportion of successes
#' @param n Number of samples
#' @param z Parameter for confidence band. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#'
#' @return Confidence interval for the p-value
#'
#' @export
sid_p_ci <- function(p, n, z = 1.96) {
    a <- sqrt(0.5*(1/n)*p*(1-p))
    p + c(-a, a)
}


#' Calculate the p-value for a tree (using early stopping)
#'
#' Calculate the p-value for a tree (grouping).
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param tree The tree
#' @param g0 The Baseline goodness, if known. Can be \code{NULL} (default).
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param Rmin Ninimum number of replications. Default is 250.
#' @param Rmax Maximum number of replications. Default is 500.
#' @param return_raw Return raw data used in the p-value calculation (Boolean, default is \code{FALSE}.
#' @param alpha The confidence level at which we are evaluating statistical significance of the tree.
#' @param z Parameter for confidence band. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The p-value for the tree
#'
#' @export
sid_p_tree_es <- function(data_train, data_test, tree, g0 = NULL, classifier = svm, Rmin = 250, Rmax = 500, parallel = TRUE, return_raw = FALSE, alpha = 0.05, z = 2.6) {
    ## ----------
    ## make a call to rnorm to consume some randomness
    rnorm(1)
    ## ----------

    ## Calculate the original goodness if it is not given
    if (is.null(g0))
        g0 <- sid_get_goodness(data_train, data_test, classifier = classifier)

    if (parallel) {
        d <- simplify2array(mclapply(seq.int(Rmin), function(i) sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = 1, parallel = FALSE), mc.cores = (detectCores() - 1)))
    }
    else {
        d <- replicate(Rmin, sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = 1, parallel = FALSE))
    }

    ## check the current confidence interval
    ns <- sum(d >= g0)
    n <- Rmin
    p <- (1 + ns) / (1 + Rmin)

    ## If the band does not cover alpha we can stop here,
    ## otherwise sample until
    ##    -- (1) Rmax or
    ##    -- (2) until band does not cover alpha

    dvec <- d
    for (i in seq.int(Rmax-Rmin)) {
        ## if (! (alpha >= sid_p_ci(p, n+1)[1] && alpha <= sid_p_ci(p, n+1)[2])) {
        ## (1) Stop if the upper limit for the band is lower than alpha --> alpha cannot be inside the band
        ## (2) Also stop if the lower band is higher than alpha --> alpha will always be in the band
        ci_tmp <- sid_p_ci(p, n+1, z)
        if ( (ci_tmp[2] < alpha) || (ci_tmp[1] >= alpha) ) {
            break
        }

        d <- sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = 1, parallel = FALSE)
        dvec <- c(dvec, d)
        n <- n + 1
        if (d >= g0) {
            ns <- ns + 1
        }
        p <- (1 + ns) / (1 + n)
        ## debug printing
        ##   cat(n, "\t", sid_p_ci(p, n+1), "\t", p, "\n")
    }

    ##    cat("done!", "\t", n, "\t", sid_p_ci(p, n+1), "\n")

    if (return_raw) {
        list("d" = dvec, "p" = p, "g0" = g0)
    }  else {
        p
    }
}


#' Calculate the p-values for a list of trees.
#'
#' Calculate the p-value for a list of trees (groupings).
#'
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param tree_list List of trees.
#' @param g0 The Baseline goodness, if known. Can be \code{NULL} (default).
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param Rmin Ninimum number of replications. Default is 250.
#' @param Rmax Maximum number of replications. Default is 500.
#' @param alpha The confidence level at which we are evaluating statistical significance of the tree.
#' @param z Parameter for confidence band. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The treelist with the p-values added.
#'
#' @export
sid_p <- function(data_train, data_test, tree_list, g0 = NULL, classifier = svm, Rmin = 250, Rmax = 500, alpha = 0.05, z = 2.6, parallel = TRUE) {
    if (is.null(g0))
        g0 <- sid_get_goodness(data_train, data_test, classifier = classifier)

    for (i in seq.int(length(tree_list))) {
        ## -- without early stopping -- tree_list[[i]][["p"]] <- sid_p_tree(data_train, data_test, tree = tree_list[[i]][["tree"]], g0 = g0, classifier = classifier, R = R, parallel = parallel)
        tree_list[[i]][["p"]] <- sid_p_tree_es(data_train, data_test, tree = tree_list[[i]][["tree"]], g0 = g0, classifier = classifier, Rmin = Rmin, Rmax = Rmax, z = z, alpha = alpha, parallel = parallel)
    }

    tree_list

}


#' Find trees that are acceptable at the alpha significance level.
#'
#' Find trees that are acceptable at the alpha significance level.
#'
#' @param tree_list List of trees.
#' @param alpha Significance level (default is 0.05).
#' @param max_k Boolean. If \code{TRUE} (default) return only the tree of maximum size.
#'
#' @return List of accetable trees of the maximum size tree.
#'
#' @export
sid_filter_trees <- function(tree_list, alpha = 0.05, max_k = TRUE) {
    id        <- unlist(sapply(seq.int(length(tree_list)), function(i) if (tree_list[[i]][["p"]] < alpha) i))

    if (! is.null(id))
        tree_list <- tree_list[-id]

    if (max_k)
        if (length(tree_list) > 1)
            tree_list <- tree_list[which.max(sapply(tree_list, function(i) i[["k"]]))]

    tree_list
}


#' Prune all trees in the treelist.
#'
#' Prune all trees in the treelist.
#'
#' @param treelist List of trees.
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param R Number of replications. Default is 100..
#' @param alpha Significance level (default is 0.05).
#' @param Rmin Ninimum number of replications for calculating p-values. Default is 250.
#' @param Rmax Maximum number of replications for calculating p-values. Default is 500.
#' @param z Parameter for confidence band. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return Pruned treelist
#'
#' @export
sid_prune_treelist <- function(treelist, data_train, data_test, classifier = svm, R = R, Rmin = 250, Rmax = 500, z = 2.6, alpha = 0.05, parallel = TRUE) {
    tmp <- lapply(treelist,
                  function(i) sid_prune_singletons(i, data_train, data_test, R = Rmin, classifier = classifier, alpha = alpha, parallel = parallel)
                  )

    valid_vals <- which(sapply(tmp, function(i) i$p) >= alpha)
    treelist[valid_vals] <- tmp[valid_vals]

    treelist
}


#' Prune tree.
#'
#' Prune tree.
#'
#' @param tree_res A tree results structure.
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param R Number of replications. Default is 100.
#' @param alpha Significance level (default is 0.05).
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The pruned tree results structure.
#'
#' @export
sid_prune_tree <- function(tree_res, data_train, data_test, classifier = svm, R = 100, alpha = 0.05, parallel = TRUE) {
    id_cl <- which(names(data) == "class")
    tree0 <- sid_treeify(tree_res[["tree"]], data_train)
    ng    <- length(tree0)

    gp   <- c()

    for (i in seq.int(ng)) {
        tree <- sid_prune_group(tree0, data_train, c(gp, i))
        p    <- sid_p_tree(data_train, data_test, tree, g0 = NULL, classifier = classifier, R = R, parallel = parallel)

        if (p >= alpha)
            gp <- c(gp, i)
    }

    if (length(gp) >= 1) {
        tree_res[["tree"]]     <- sid_prune_group(tree0, data_train, gp)
        tree_res[["goodness"]] <- sid_tree_goodness(data_train, data_test, tree_res[["tree"]], classifier = classifier, R = R)
        tree_res[["p"]]        <- sid_p_tree(data_train, data_test, tree_res[["tree"]], g0 = NULL, classifier = classifier, R = R)

    }

    tree_res
}




#' Prune singletons from tree.
#'
#' Prune singletons from tree.
#'
#' @param tree_res A tree results structure.
#' @param data_train Training data.
#' @param data_test Testing data.
#' @param classifier A classifier function, default is svm (from the e1071 package).
#' @param R Number of replications. Default is 100.
#' @param Rmin Ninimum number of replications for calculating p-values. Default is 250.
#' @param Rmax Maximum number of replications for calculating p-values. Default is 500.
#' @param z Parameter for confidence band. Use 1.96 for 95 percent, 2.25 for 97.5 percent and 2.6 for 99 percent. Default is 2.6.
#' @param alpha Significance level (default is 0.05).
#' @param parallel Calculate in parallel (Boolean, default is \code{TRUE}).
#'
#' @return The pruned tree results structure.
#'
#' @export
sid_prune_singletons <- function(tree_res, data_train, data_test, classifier = svm, R = 100, Rmin = 250, Rmax = 500, z = 2.6, alpha = 0.05, parallel = TRUE) {
    id_cl <- which(names(data) == "class")
    tree0 <- sid_treeify(tree_res[["tree"]], data_train)

    ## store indices of singletons in g_ut
    g_ut <- which(sapply(tree0, function(i) length(i)) == 2)

    ## empty list to store pruned singletons
    gp   <- c()

    ## Greedily find the singleton to remove that impacts accuracy the least
    ## Continue as long as p >= alpha

    ## -------------------------------

    for (i in seq.int(length(g_ut))) {
        ## calculate accuracy for all the trees
        x <- sapply(g_ut, function(i) {
            tree <- sid_prune_group(tree0, data_train, c(gp, i))
            sid_tree_goodness(data_train, data_test, tree, classifier = classifier, R = R, parallel = parallel)
        })
        singleton_candidate <- g_ut[which.max(x)]

        ## calculate p-value of the tree
        tree <- sid_prune_group(tree0, data_train, c(gp, singleton_candidate))
        p    <- sid_p_tree_es(data_train, data_test, tree, g0 = NULL, classifier = classifier, Rmin = Rmin, Rmax = Rmax, z = z, alpha = alpha, parallel = parallel)

        if (p >= alpha) {
            gp <- c(gp, singleton_candidate)
            g_ut <- setdiff(g_ut, singleton_candidate)
        } else {
            break
        }

        if (length(gp) >= 1) {
            tree_res[["tree"]]     <- sid_prune_group(tree0, data_train, gp)
            tree_res[["goodness"]] <- sid_tree_goodness(data_train, data_test, tree_res[["tree"]], classifier = classifier, R = R, parallel = parallel)
            tree_res[["p"]]        <- sid_p_tree_es(data_train, data_test, tree_res[["tree"]], g0 = NULL, classifier = classifier, Rmin = Rmin, Rmax = Rmax, z = z, alpha = alpha, parallel = parallel)
        }
    }
    tree_res


}
