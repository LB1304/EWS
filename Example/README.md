<h1>Early Warning Systems using hidden Markov model</h1>

We load the necessary packages, source the function definitions, and load the simulated dataset. The training set consists of the first T−1 time points, while the test set includes the final time point.

```r
require(dplyr)
require(LMest)
source("EWS_Functions.R")
load("EWS_Data.RData")

# Split the data into training (T−1 time points) and test (final time point) sets
Data.Train <- Data %>% 
  filter(Time != max(Time))
Data.Test <- Data %>% 
  filter(Time == max(Time)) %>% 
  mutate(Time = Time - max(Time) + 1)

# Extract covariates and response variables
Covariates.Train <- Data.Train[, 4:7]
Covariates.Test <- Data.Test[, 4:7]
Response.Train <- Data.Train$Y
Response.Test <- Data.Test$Y
Time <- Data.Train$Time
Id <- Data.Train$Id
```


<h3>Hidden Markov model estimation</h3>

We fit a hidden Markov model with 3 latent states using the training data. The model includes covariates into the measurement sub-model through a logistic regression on the conditional response probabilities.

```r
RespForm <- formula(Y ~ X1 + X2 + X3 + X4)
ModEst <- lmest(responsesFormula = RespForm, data = Data.Train, index = c("Id", "Time"), 
                k = 3, start = 0, tol = 1e-6, maxit = 1e5, out_se = T, output = T)
```


<h3>In-sample prediction</h3>

We estimate conditional response probabilities and compute the crisis probabilities for each sample unit and each time occasion in the training set. Then, we evaluate in-sample prediction performance using different cut-off values (based on ROC and PR curves), and report the associated evaluation metrics.

```r
Psi.C1 <- computePsi(Id = Id, Time = Time, Covariates = Covariates.Train, Model = ModEst)
ProbCrisis <- computeProbCrisis(Model = ModEst, Psi.C1 = Psi.C1)
Metrics.in <- computeCutOff(ProbCrisis = ProbCrisis, True = Response.Train)
```

<div align="center">
  <table>
    <tr>
      <td></td><td>PR curve</td><td>ROC curve</td>
    </tr>
    <tr>
      <td>Optimal cut-off</td> <td>0.31</td> <td>0.21</td>
    </tr>
    <tr>
      <td>True positive</td> <td>112</td> <td>118</td>
    </tr>
    <tr>
      <td>False positive</td> <td>16</td> <td>46</td>
    </tr>
    <tr>
      <td>Sensitivity/Recall</td> <td>0.91</td> <td>0.96</td>
    </tr>
    <tr>
      <td>Specificity</td> <td>0.99</td> <td>0.96</td>
    </tr>
    <tr>
      <td>Precision</td> <td>0.88</td> <td>0.72</td>
    </tr>
    <tr>
      <td>Accuracy</td> <td>0.98</td> <td>0.96</td>
    </tr>
    <tr>
      <td>F1</td> <td>0.89</td> <td>0.82</td>
    </tr>
  </table>
  <caption>
    <em>Table 1.</em> In-sample prediction evaluation metrics
  </caption>
</div>


<h3>Out-of-sample forecast</h3>

We compute the crisis probability for each sample unit in the test set and forecast the event of interest using the previously selected optimal cut-offs. We report the standard evaluation metrics to assess the performance of out-of-sample forecast.

```r
PredProbCrisis <- forecastProbCrisis(Id = Id, Covariates.Train = Covariates.Train, 
                                     Covariates.Test = Covariates.Test, Model = ModEst)
PredCrisis.F1 <- ifelse(PredProbCrisis < Metrics.in$Res.Optimal$res_F1[1], 0, 1)
PredCrisis.J <- ifelse(PredProbCrisis < Metrics.in$Res.Optimal$res_J[1], 0, 1)

df <- data.frame(True = Data.Test$Y, Pred.F1 = PredCrisis.F1, Pred.J = PredCrisis.J)
Metrics.out <- data.frame(res_F1 = computeMetrics(True = df$True, Pred = df$Pred.F1), 
                          res_J = computeMetrics(True = df$True, Pred = df$Pred.J))
```

<div align="center">
  <table>
    <tr>
      <td></td><td>PR curve</td><td>ROC curve</td>
    </tr>
    <tr>
      <td>True positive</td> <td>6</td> <td>6</td>
    </tr>
    <tr>
      <td>False positive</td> <td>1</td> <td>2</td>
    </tr>
    <tr>
      <td>Sensitivity/Recall</td> <td>0.86</td> <td>0.86</td>
    </tr>
    <tr>
      <td>Specificity</td> <td>0.99</td> <td>0.98</td>
    </tr>
    <tr>
      <td>Precision</td> <td>0.86</td> <td>0.75</td>
    </tr>
    <tr>
      <td>Accuracy</td> <td>0.98</td> <td>0.97</td>
    </tr>
    <tr>
      <td>F1</td> <td>0.86</td> <td>0.80</td>
    </tr>
  </table>
  <caption>
    <em>Table 2.</em> Out-of-sample forecast evaluation metrics
  </caption>
</div>



<h2>Early Warning Systems using binary regression models</h2>

Preparing Data.1 for fixed effect model dropping all countries that do not present crises during the hole period and Data.2 for the other two models





