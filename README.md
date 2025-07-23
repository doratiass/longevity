# Longevity Predictors Study

This repository contains the complete R code for the manuscript: 
**Machine learning in epidemiology: an introduction, comparison with traditional methods, and a case study of predicting extreme longevity**  
*Dor Atias, Saar Ashri, Uri Goldbourt, Yael Benyamini, Ran Gilad-Bachrach, Tal Hasin, Yariv Gerber, Uri Obolski*  
*Annals of Epidemiology* (in press, 2025) – [doi:10.1016/j.annepidem.2025.07.024](https://doi.org/10.1016/j.annepidem.2025.07.024)

We analyze a historical cohort of 10,058 men recruited in 1963, who were followed up for 5 years to assess ischemic heart disease incidence (Medalie et al. 1973). Our goal is to compare traditional epidemiological methods (e.g., logistic regression) with modern machine learning techniques (LASSO regression and XGBoost) for predicting extreme longevity (near-centenarianism). We also apply explainable AI tools (SHAP) to enhance model interpretability.

------------------------------------------------------------------------

# Repository Overview

-   **Data Privacy:**\
    Due to privacy considerations, the dataset used in this study is not publicly shared.

-   **Code Structure:**\
    All analysis scripts are located in the `/code/scripts` directory. Key scripts include:

    -   **00 - funcs_def.R:** Function definitions and helper routines.
    -   **01 - int.R:** Data import and cleaning.
    -   **02 - ml_models.R:** Model fitting using tidymodels (logistic regression, LASSO, XGBoost).
    -   **03 - model_assesment.R:** Model discrimination and calibration assessment.
    -   **04 - XAI.R:** SHAP analysis for model interpretability.
    -   **05 - fig_3_dp.R:** Partial dependency plots.
    -   **06 - interaction_check.R:** Exploration and visualization of interactions.
    -   **07 - supplement.R:** Supplementary material and additional tables.

-   **Results and Outputs:**

    -   The full output of our study is available [here](https://doratiass.github.io/longevity/).
    -   The manuscript, titled “Machine learning in epidemiology: an introduction, comparison with traditional methods, and a case study of predicting extreme longevity”, is currently under review.
    -   An interactive SHAP values exploration tool can be accessed [here](https://dorati-longevity-shap.hf.space).

------------------------------------------------------------------------

For questions, feedback, or collaboration inquiries, please contact 

Lead analyst:

Dr. Dor Atias

Tel Aviv University, Grays School of Public Health

Email: [doratias\@mail.tau.ac.il](mailto:doratias@mail.tau.ac.il){.email}


Corresponding author:

Prof. Uri Obolski, Ph.D.

Tel Aviv University, Grays School of Public Health

Email: [uriobols\@tauex.tau.ac.il](mailto:uriobols@tauex.tau.ac.il){.email}
