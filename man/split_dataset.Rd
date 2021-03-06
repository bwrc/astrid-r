% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_experiments.R
\name{split_dataset}
\alias{split_dataset}
\title{Split dataset into training and testing (50/50).
                                      #
Split dataset into training and testing (50/50).}
\usage{
split_dataset(dataset, classname = "class")
}
\arguments{
\item{dataset}{The dataset}

\item{classname}{the name of the class in the dataset. Default is \code{class}.}
}
\value{
The dataset structure with two new elements (\code{data_train} and \code{data_test}).
}
\description{
Split dataset into training and testing (50/50).
                                      #
Split dataset into training and testing (50/50).
}

