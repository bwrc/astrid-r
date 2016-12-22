## Load the library
library(astrid)
library(e1071)
library(randomForest)

## Create a synthetic dataset with the known
## attribute interaction structure
## ((a1, a2), (a_3), (a_4)), where attribute a_4 is just noise.
dataset <- make_synthetic_dataset(N = 500, seed = 42, mg2 = 0.6)

## Perform the analysis using the ASTRID algorithm
res <- analyze_dataset(dataset, classname = "class",  classifier = "svm", parallel = TRUE, R = 100)

## Print the results as an HTML table
print_result_table_html(res, full_tree = TRUE)
