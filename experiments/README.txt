================================================================================

The files in this folder are used to run the experiments for the paper
"Finding Statistically Significant Attribute Interactions"

================================================================================

The files are as follows

------------------------------
1. Experiments
------------------------------

sid_experiments.R                  Run the ASTRID algorithm on the synthetic and UCI datasets.
sid_experiments_2.R                Calculate the p-value for Test 2 in Ojala & Garriga for the datasets.

------------------------------
2. Analyses
------------------------------
sid_analysis_synthetic_figure.R    Create a figure displaying the synthetic dataset.
sid_analysis_dataset_table.R       Create a table with the properties (size, number of attributes etc) and calculation times for the various tables.
sid_analysis.R                     Craete results showing
                                      (i) grouping of synthetic dataset,
                                      (ii) grouping of the credit-a dataset using svm,
				      (iii) grouping of all UCI datasets using svm and randomforest

------------------------------
3. Utilities
------------------------------

utilities_analysis.R               This file contains various utility functions.

------------------------------
