## ----------------------------------------------------------------------------
## This function computes the conditional response probabilities for each unit, 
## time point, and latent state, and for category 1 of the response variable
## (i.e. crisis occurrence): P(Y_i^(t) = 1 | U_i^(t) = u, X_i^(t) = x)
## ----------------------------------------------------------------------------
# Arguments: 
#   - Id: numeric vector of unit ids in long format
#   - Time: numeric vector of time points in long format
#   - Covariates: data.frame of covariates in long format
#   - Model: estimated hidden Markov model (obtained using the lmest function)
# Output: 
#   - Psi.C1: array of the estimated conditional response probabilities
## ----------------------------------------------------------------------------

computePsi <- function(Id, Time, Covariates, Model) {
  k <- Model$k
  n <- length(unique(Id))
  TT <- length(unique(Time))
  
  mu <- array(Model$mu, dim = c(n, TT, k))
  al <- array(1, dim = c(n, TT)) %o% Model$al
  be <- matrix(Model$be, ncol = 1)
  X <- cbind(Intercept = 1, as.matrix(Covariates))
  be.X <- matrix(X %*% be, ncol = TT, nrow = n, byrow = T) %o% rep(1, k)
  
  Psi.C1 <- exp(mu + al + be.X)
  Psi.C1 <- Psi.C1/(1 + Psi.C1)
  Psi.C1[is.nan(Psi.C1)] <- 1
  
  return(Psi.C1)
}



## ----------------------------------------------------------------------------
## This function computes the probabilities of crisis occurrence for each unit 
## and time point: P(Y_i^(t) = 1 | X_i^(t) = x) 
## ----------------------------------------------------------------------------
# Arguments: 
#   - Model: estimated hidden Markov model (obtained using the lmest function)
#   - Psi.C1: array of the estimated conditional response probabilities
# Output: 
#   - ProbCrisis: matrix of the estimated crisis probabilities
## ----------------------------------------------------------------------------

computeProbCrisis <- function(Model, Psi.C1) {
  if (Model$k == 1) return(Psi.C1[, , 1])
  
  Psi.C1 <- aperm(Psi.C1, c(1, 3, 2))
  ProbCrisis <- Model$V * Psi.C1
  ProbCrisis <- apply(ProbCrisis, c(1, 3), sum)
  
  return(ProbCrisis)
}



## ----------------------------------------------------------------------------
## This function computes the main evaluation metrics to assess the quality of
## prediction: number of true and false positive, sensitivity, specificity, 
## precision, recall, accuracy, Youden's J statistics, and F1 score
## ----------------------------------------------------------------------------
# Arguments: 
#   - True: vector of true category (0 or 1) for the response variable
#   - Pred: vector of predicted category (0 or 1) for the response variable
# Output: 
#   - Res: numeric vector containing the evaluation metrics introduced above
## ----------------------------------------------------------------------------

computeMetrics <- function(True, Pred) {
  u <- union(Pred, True)
  Tbl <- table(factor(Pred, u), factor(True, u))
  TP <- Tbl["1", "1"]; FP <- Tbl["1", "0"]; FN <- Tbl["0", "1"]; TN <- Tbl["0", "0"]
  
  SENS = TP/(TP+FN)
  SPEC = TN/(TN+FP)
  PREC = TP/(TP+FP)
  REC = TP/(TP+FN)
  ACC = (TP+TN)/(TP+FP+TN+FN)
  F1 <- (2*PREC*REC)/(PREC+REC)
  J <- SENS+SPEC-1
  
  Res <- c(SENS, SPEC, PREC, REC, ACC, F1, J, TP, FP)
  names(Res) <- c("Sensitivity", "Specificity", "Precision", "Recall", 
                  "Accuracy", "F1", "J", "PredictedCrisis", "FalseAlarms")
  
  return(Res)
}



## ----------------------------------------------------------------------------
## This function computes the optimal cut-off value to categorize the predicted
## probabilities into the two categories (0 and 1) of the response variable.
## It takes into consideration both the ROC (through the Youden's J statistics) 
## and the PR (through the F1 score) curves
## ----------------------------------------------------------------------------
# Arguments: 
#   - ProbCrisis: matrix of the estimated crisis probabilities
#   - True: vector of true category (0 or 1) for the response variable
#   - CutOffs: vector of cut-off values to evaluate
# Output: 
#   - Res.CutOffs: value of F1 score and Youden's J statistics for each cut-off
#   - Res.Optimal: evaluation metrics for the optimal cut-off
## ----------------------------------------------------------------------------

computeCutOff <- function(ProbCrisis, True, CutOffs = seq(0, 1, by = 0.01)) {
  Res.CutOffs <- data.frame(CutOff = CutOffs, F1 = 0, J = 0)
  res_F1 <- res_J <- rep(0, 10)
  
  for (i in 1:length(CutOffs)) {
    PredCrisis <- ifelse(as.numeric(t(ProbCrisis)) < CutOffs[i], 0, 1)
    Metrics <- computeMetrics(True = True, Pred = PredCrisis)
    
    Res.CutOffs$F1[i] <- Metrics[6]
    Res.CutOffs$J[i] <- Metrics[7]
    
    if (!is.na(Metrics[6]) && Metrics[6] > res_F1[7]) {
      res_F1 <- c(CutOffs[i], Metrics)
    }
    if (!is.na(Metrics[7]) && Metrics[7] > res_J[8]) {
      res_J <- c(CutOffs[i], Metrics)
    }
  }
  Res.Optimal <- data.frame(res_F1 = res_F1, res_J = res_J)
  rownames(Res.Optimal)[1] <- "CutOff"
  
  return(list(Res.CutOffs = Res.CutOffs, Res.Optimal = Res.Optimal))
}



## ----------------------------------------------------------------------------
## This function forecast the probabilities of crisis occurrence for each unit
## and at a future time point
## ----------------------------------------------------------------------------
# Arguments: 
#   - Id: numeric vector of unit ids in long format
#   - Covariates.Train: data.frame of covariates in the training set
#   - Covariates.Test: data.frame of covariates in the test set
#   - Model: estimated hidden Markov model (obtained using the lmest function)
# Output: 
#   - ProbCrisis: matrix of the predicted crisis probabilities at a future time
## ----------------------------------------------------------------------------

forecastProbCrisis <- function(Id, Covariates.Train, Covariates.Test, Model) {
  n <- length(unique(Id))
  TT <- nrow(Covariates.Train)/n
  k <- Model$k
  PI <- Model$PI
  V <- Model$V[, , TT]
  
  if (k == 1) {
    Psi_New <- computePsi(Id, 1, Covariates.Test, Model)[, 1, ]
    ProbCrisis <- Psi_New
    return(ProbCrisis)
  }
  
  Psi_New <- computePsi(Id, 1, Covariates.Test, Model)[, 1, ]
  V_New <- V %*% PI
  ProbCrisis <- rowSums(V_New * Psi_New)
  
  return(ProbCrisis)
}



