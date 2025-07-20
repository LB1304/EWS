<h1 align="center">Early Warning Systems using hidden Markov and binary regression models</h1>
<p align="center"> <span style="font-size: 14px;"><em><strong>Luca Brusa &middot; Fulvia Pennoni &middot; Romina Peruilh Bagolini &middot; Francesco Bartolucci</strong></em></span> </p>
<br>


In this repository, we illustrate the R code used to implement Early Warning Systems based on different probabilistic models.
Assuming a univariate response variable that indicates the occurrence of an event of interest (e.g., financial crises), we consider hidden Markov models, with covariates included in the measurement submodel via a logistic parameterization of the conditional response probabilities, and three variants of binary regression models: pooled logit, pooled probit, and fixed-effects models.
In all cases, we perform in-sample prediction and out-of-sample forecasting based on the estimated event probabilities provided by each model, and we evaluate predictive performance using standard classification metrics such as accuracy, precision, recall, and F1 score.



In particular, this repository contains: 
1. the file [`EWS_Functions.R`](EWS_Functions.R), which includes all the functions required to perform in-sample prediction and out-of-sample forecasting, as well as to assess predictive performance using standard evaluation metrics. Each function is documented within the file;
2. the file [`EWS_Data.RData`](EWS_Data.RData), which contains a simulated dataset used for illustrative purposes. The dataset includes a binary response variable (indicating the occurrence of the event of interest) and four continuous covariates. It consists of 95 units observed over 15 time periods and is structured in long format;
3. the folder [`Example`](Example), which provides example scripts with code and output to replicate the main results.



