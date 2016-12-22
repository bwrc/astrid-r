#' Split data set into training and validation sets, stratifying by
#' groups. Of each class of n items, floor(fraction*n) go to training
#' set and ceiling((1-fraction)*n) go to validation set.
#'
#' @param data The data matrix
#' @param fraction The fraction of data to place in the training and
#'                 testing set.
#' @param size The amount of data to place in the training and testing
#'             set
#' @param replace Should the splitting be with replacement. Default is
#'                 FALSE.
#' @param stratify Variable according to which stratification should
#'                 be performed.
#' @return Indices of the training set.
#'
#' @export
testsplit <- function(data, fraction = 0.5, size = NULL, replace = FALSE, stratify = "Class") {
    if(!is.null(size)) {
        fraction <- size/dim(data)[1]
    }
    if(!is.null(stratify)) {
        res <- c()
        for(idx in tapply(1:dim(data)[1],data[,stratify],function(x) x)) {
            res <- c(res,sample(idx,
                                size=ceiling(fraction*length(idx)),
                                replace=replace))
        }
    }
    else {
        res <- sample.int(dim(data)[1],
                          size=ceiling(fraction*dim(data)[1]),
                          replace=replace)
    }
    res
}

#' Produces a permutation of integers 1:length(vec) such that the
#' permutation leaves vec unchanged, i.e., the permutations are within
#' class labes defined by vec.
#'
#' @param vec Vector defining the permutations
#' @param replace Should the sampling be with or without
#'                replacement. Default is FALSE.
#' @return A permutation.
#'
#' @export
cpermute <- function(vec, replace = FALSE) {
    res <- integer(length(vec))
    for(idx in tapply(1:length(vec),vec,function(x) x)) {
        res[idx] <- if(length(idx)>1) sample(idx,replace=replace) else idx
    }
    res
}


#' Permute the data matrix using the permutation tree.
#'
#' @param tree The permutation tree
#' @param data The data matrix
#' @return A permutation of the indices of the rows in the data matrix
#'
#' @export
permutation <- function(tree, data) {
    res <- matrix(1:dim(data)[1],dim(data)[1],dim(data)[2])
    for(vec in tree) {
        if(vec[1]==0) {
            res[,vec[-1]] <- sample.int(dim(res)[1])
        }
        else {
            res[,vec[-1]] <- cpermute(data[,vec[1]])[res[,vec[1]]]
        }
    }
    res
}


#' Use the permutation to obtain a new data matrix.
#'
#' @param data The data matrix
#' @param perm The permutation
#' @return The permuted data matrix
#'
#' @export
permutedata <- function(data, perm) {
    for(idx in 1:dim(data)[2]) {
        data[,idx] <- data[perm[,idx],idx]
    }
    data
}
