require(dplyr)
require(LMest)
source("EWS_Functions.R")
load("EWS_Data.RData")


## Dataset preparation
Data.Train <- Data %>% 
  filter(Time != max(Time))
Data.Test <- Data %>% 
  filter(Time == max(Time)) %>% 
  mutate(Time = Time - max(Time) + 1)

Covariates.Train <- Data.Train[, 4:7]
Covariates.Test <- Data.Test[, 4:7]
Response.Train <- Data.Train$Y
Response.Test <- Data.Test$Y
Time <- Data.Train$Time
Id <- Data.Train$Id


## Model estimation
RespForm <- formula(Y ~ X1 + X2 + X3 + X4)
ModEst <- lmest(responsesFormula = RespForm, data = Data.Train, index = c("Id", "Time"), 
                k = 3, start = 0, tol = 1e-6, maxit = 1e5, out_se = T, output = T)


## In-sample prediction
Psi.C1 <- computePsi(Id = Id, Time = Time, Covariates = Covariates.Train, Model = ModEst)
ProbCrisis <- computeProbCrisis(Model = ModEst, Psi.C1 = Psi.C1)
Metrics.in <- computeCutOff(ProbCrisis = ProbCrisis, True = Response.Train)


## Out-of-sample forecast
PredProbCrisis <- forecastProbCrisis(Id = Id, Covariates.Train = Covariates.Train, 
                                     Covariates.Test = Covariates.Test, Model = ModEst)
PredCrisis.F1 <- ifelse(PredProbCrisis < Metrics.in$Res.Optimal$res_F1[1], 0, 1)
PredCrisis.J <- ifelse(PredProbCrisis < Metrics.in$Res.Optimal$res_J[1], 0, 1)

df <- data.frame(True = Data.Test$Y, Pred.F1 = PredCrisis.F1, Pred.J = PredCrisis.J)
Metrics.out <- data.frame(res_F1 = computeMetrics(True = df$True, Pred = df$Pred.F1), 
                          res_J = computeMetrics(True = df$True, Pred = df$Pred.J))



