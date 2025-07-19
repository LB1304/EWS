require(dplyr)
require(ModelMetrics)
source("EWS_Functions.R")
load("EWS_Data.RData")


## Dataset preparation
Data.1 <- Data %>%
  mutate(Y = as.factor(Y))
Data.Train.1 <- Data.1 %>% 
  filter(Time != max(Time))
Data.Test.1 <- Data.1 %>% 
  filter(Time == max(Time)) %>% 
  mutate(Time = Time - max(Time) + 1)

Data.2 <- Data %>%
  group_by(Id) %>%
  filter(any(Y == 1, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(Id = dense_rank(Id)) %>% 
  mutate(Y = as.factor(Y), Id = as.factor(Id))
Data.Train.2 <- Data.2 %>% 
  filter(Time != max(Time))
Data.Test.2 <- Data.2 %>% 
  filter(Time == max(Time)) %>% 
  mutate(Time = Time - max(Time) + 1)


## Model estimation
ModEst.PL <- glm(Y ~ X1 + X2 + X3 + X4, data = Data.Train.1, family = binomial(link = "logit"))
ModEst.PP <- glm(Y ~ X1 + X2 + X3 + X4, data = Data.Train.1, family = binomial(link = "logit"))
ModEst.FE <- glm(Y ~ X1 + X2 + X3 + X4 + Id, data = Data.Train.2, family = binomial(link = "probit"))


## In-sample prediction
ProbCrisis.PL <- ModEst.PL$fitted.values
Metrics.in.PL <- computeCutOff(ProbCrisis = ProbCrisis.PL, True = Data.Train.1$Y)
ProbCrisis.PP <- ModEst.PP$fitted.values
Metrics.in.PP <- computeCutOff(ProbCrisis = ProbCrisis.PP, True = Data.Train.1$Y)
ProbCrisis.FE <- ModEst.FE$fitted.values
Metrics.in.FE <- computeCutOff(ProbCrisis = ProbCrisis.FE, True = Data.Train.2$Y)


## Out-of-sample forecast
PredProbCrisis.PL <- predict(ModEst.PL, newdata = Data.Test.1, type = "response")
PredProbCrisis.PP <- predict(ModEst.PP, newdata = Data.Test.1, type = "response")
PredProbCrisis.FE <- predict(ModEst.FE, newdata = Data.Test.2, type = "response")

PredCrisis.PL.F1 <- ifelse(PredProbCrisis.PL < Metrics.in.PL$Res.Optimal$res_F1[1], 0, 1)
PredCrisis.PL.J <- ifelse(PredProbCrisis.PL < Metrics.in.PL$Res.Optimal$res_J[1], 0, 1)
PredCrisis.PP.F1 <- ifelse(PredProbCrisis.PP < Metrics.in.PP$Res.Optimal$res_F1[1], 0, 1)
PredCrisis.PP.J <- ifelse(PredProbCrisis.PP < Metrics.in.PP$Res.Optimal$res_J[1], 0, 1)
PredCrisis.FE.F1 <- ifelse(PredProbCrisis.FE < Metrics.in.FE$Res.Optimal$res_F1[1], 0, 1)
PredCrisis.FE.J <- ifelse(PredProbCrisis.FE < Metrics.in.FE$Res.Optimal$res_J[1], 0, 1)

df <- data.frame(True = Data.Test.1$Y, Pred.F1 = PredCrisis.PL.F1, Pred.J = PredCrisis.PL.J)
Metrics.out.PL <- data.frame(res_F1 = computeMetrics(True = df$True, Pred = df$Pred.F1), 
                             res_J = computeMetrics(True = df$True, Pred = df$Pred.J))
df <- data.frame(True = Data.Test.1$Y, Pred.F1 = PredCrisis.PP.F1, Pred.J = PredCrisis.PP.J)
Metrics.out.PP <- data.frame(res_F1 = computeMetrics(True = df$True, Pred = df$Pred.F1), 
                             res_J = computeMetrics(True = df$True, Pred = df$Pred.J))
df <- data.frame(True = Data.Test.2$Y, Pred.F1 = PredCrisis.FE.F1, Pred.J = PredCrisis.FE.J)
Metrics.out.FE <- data.frame(res_F1 = computeMetrics(True = df$True, Pred = df$Pred.F1), 
                             res_J = computeMetrics(True = df$True, Pred = df$Pred.J))



