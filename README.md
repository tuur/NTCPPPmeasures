# README

This repository contains the code used to obtain results in the open access article [The relation between prediction model performance measures and patient selection outcomes for proton therapy in head and neck cancer](https://doi.org/10.1016/j.radonc.2022.109449), published in Radiotherapy and Oncology.

For privacy reasons the patient data is not shared.

However, the following .R scripts are shared:

  `LR.R` ~ Contains a torch implementation of logistic regression (that allows recalibration).

  `MBS simulation.R` ~ Contains the indication protocol simulation (incl. the model-based patient selection procedure).

  `Models LIPPV2_2.R` ~ Contains the models (coefficients and intercept) from the Dutch protocol (v2.2.).

  `Plotting.R` ~ Script used to obtain the plots in the article.

  `data_preparation.R` ~ Preparations / transformation performed to the raw data before analysis.

  `experiment.R` ~ The main scripts running the experiments (incl. calls to the other scripts).
  
If you have any questions regarding the article or code. Feel free to email!
