# Final Project — Erasmus+ Exchange Duration

We model the duration of Erasmus+ mobility periods for students. This file quickly highlights what the different files and directories in this directory contain. 

* [project_report_bayesian.pdf](report.pdf): final report in pdf format.
* [erasmus.R](erasmus.R): R script for reading raw downloaded data, cleaning the data, filtering, saving a sample of 15000 points, saving the cleaned data in a csv and making plots for EDA. 
* [models](models): directory of R script for fitting all four models, doing model checking and making plots. 
* [fits](fits): directory of rds files for pre-trained or pre-fitted models via Stan. These are loaded into the R scripts in the aforementioned directory. This way we only needed to train each model once, but can work with the results from the training later without retraining. 
* [stan_models](stan_models): directory of .stan files for fitting the four models in Stan.
* [prior_predictive.R](prior_predictive.R): R script for making prior predictive plot according to model 1. 
* [comparison.R](comparison.R): R script creating tables for section 5 (comparison of models).
* [cleaned.csv](cleaned.csv): final cleaned data from the raw downloaded data. Generated from R script [erasmus.R](erasmus.R) 
* [15kpoints.rds](15kpoints.rds): randomly samples 15000 points from the final cleaned data. Generate from R script [erasmus.R](erasmus.R)
