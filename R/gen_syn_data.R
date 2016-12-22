#' Generate covariance matrix.
#'
#' Generate covariance matrix.
#'
#' @param Correlation between the variables.
#' @param N Number of variables.
#'
#' @keywords internal
mksigma <- function(co, N) {
    out <- matrix(data = co, nrow = N, ncol = N)
    diag(out) <- 1
    out
}

#' Generate synthetic dataset.
#'
#' This function generates a synthetic dataset that corresponds to a
#' tree of the form
#' tree <- list(c(5, 1, 2), c(5, 3), c(0, 4))
#'
#' @param N Number of samples in each class.
#' @param mg2 Classwise means of second group (attibute 3)
#' @param seed Random seed. Default is 42.
#'
#' @export
make_synthetic_dataset <- function(N = 500, mg2 = 0.3, seed = 42) {
    set.seed(seed)
    
    require(MASS)

    ## Group 1 : a variable pair
    ## Create two vectors
    ## mu_1_0    <- c(1, 1)
    mu_1_0    <- c(0, 0)
    co_1_0    <- c(0.95)
    sig_1_0   <- mksigma(N = 2, co = co_1_0)
    
    gd_1_0 <- mvrnorm(n = N, mu = mu_1_0, Sigma = sig_1_0)
    gd_1_1 <- mvrnorm(n = N, mu = mu_1_0, Sigma = sig_1_0)
    gd_1_1[,2] <- -gd_1_1[,2]
    
    class <- c(rep(0, N), rep(1, N))
    
    gd_1 <- rbind(gd_1_0, gd_1_1)

    ## Group 2 : a single class-dependent variable 
    gd_2_0 <- rnorm(N, mean = mg2, sd = 1)
    gd_2_1 <- rnorm(N, mean = -mg2, sd = 1)

    gd_2 <- c(gd_2_0, gd_2_1)

    ## Group 3 : a single random variable
    gd_3 <- runif(2*N)

    out <- cbind(gd_1, gd_2, gd_3)
    out <- cbind(out, class)

    colnames(out) <- c(paste0("a", seq.int(4)), "class")
    out <- as.data.frame(out)
    out$class <- as.factor(out$class)

    list("name" = "synthetic", "data" = out)
}

