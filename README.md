[![INFORMS Journal on Computing Logo](https://INFORMSJoC.github.io/logos/INFORMS_Journal_on_Computing_Header.jpg)](https://pubsonline.informs.org/journal/ijoc)

# Synthetic Data and Codes for the Empirical Analysis of the Paper: Bayesian Network Models for PTSD Screening in Veterans

This archive is distributed in association with the [INFORMS Journal on Computing](https://pubsonline.informs.org/journal/ijoc) under the [MIT License](LICENSE).

This repository includes the synthetic data and R codes for the following paper: Tan, Y., Shenoy, P.P., Sherwood, B., Gaddy, M., Oehlert, M., Shenoy, C. [Bayesian Network Models for PTSD Screening in Veterans](https://doi.org/10.1287/ijoc.2021.0174).
INFORMS Journal on Computing.

## Cite

To cite the contents of this repository, please cite both the paper and this repo, using their respective DOIs.
```
@misc{Tan2023PTSD,
  author =        {Tan, Yi and Shenoy, Prakash P and Sherwood, Ben and Gaddy, Melinda and Oehlert, Mary E and Shenoy, Catherine},
  publisher =     {INFORMS Journal on Computing},
  title =         {R Scripts for {B}ayesian Network Models for {PTSD} Screening in Veterans},
  year =          {2023},
  doi =		 {10.1287/ijoc.2021.0174.cd},
  note =           {Available for download at https://github.com/INFORMSJoC/2021.0174},
} 
```

## Description

The goal of this repository is to share a synthetic data and R codes for the empirical analysis of our paper. Our motivation is to present our code and results in a reproducible way and facilitate the coding effort of those who want to improve our model.

## Data files

The original data for our paper is from VA Informatics and Computing Infrastructure, and thus is confidential. Here, we provide a synthetic dataset which is randomly simulated from the BIC score-based Bayesian network model in our paper. We insert the same proportion of missing values as what we have in the original data.

We also provide a file of blacklisted arcs in the structure learning of BIC score-based Bayesian network.

## Code files

There are 3 files.

bic_bn: it includes the train-valid-test split of the data, the construction of BIC score-based Bayesian network model, and the multiple imputation using the BIC score-based Bayesian network model.

gl2_bn: it includes the construction of Gl2-regularized Bayesian network model with the ordering-based search.

fi: it includes the feature importance analysis to evaluate the usefulness of features in PTSD screening.



