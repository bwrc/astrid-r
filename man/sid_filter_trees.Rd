% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sid_main_algorithm.R
\name{sid_filter_trees}
\alias{sid_filter_trees}
\title{Find trees that are acceptable at the alpha significance level.}
\usage{
sid_filter_trees(tree_list, alpha = 0.05, max_k = TRUE)
}
\arguments{
\item{tree_list}{List of trees.}

\item{alpha}{Significance level (default is 0.05).}

\item{max_k}{Boolean. If \code{TRUE} (default) return only the tree of maximum size.}
}
\value{
List of accetable trees of the maximum size tree.
}
\description{
Find trees that are acceptable at the alpha significance level.
}

