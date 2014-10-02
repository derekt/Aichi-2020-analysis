#-------------------------------------------------------------------------------
# Holds the functions to calculate Wald CIs
# Method from A. R. Gallant (1987); nonlinear statistical models
# Derek Tittensor
# Last updated 28th January 2014
#-------------------------------------------------------------------------------

### Helper functions to assist with processes common to all models
#-------------------------------------------------------------------------------
HelperFunction <- function(PredictYears)
{
  # Convert the years to start at 1
  PredictYearsStartAt1 = ConvertYearsToStartAtZero(PredictYears)
  
  # Variables to hold the confidence limits and SEs
  cis<-matrix(nrow = 2,ncol = length(PredictYearsStartAt1))
  cis99<-matrix(nrow = 2,ncol = length(PredictYearsStartAt1))
  se<-vector(mode="numeric",length=length(PredictYearsStartAt1))
  
  list(PredictYearsStartAt1 = PredictYearsStartAt1, cis = cis, cis99 = cis99, se = se)
}

# Calculate SEs and CIs based on the vector of first partial derivatives, gamma
# and the variance-covariance matrix. Note that the variance-covariance matrix
# is already pre-multiplied by sigma^2
CalculateSEsCIs <- function(helperList, hhat, gammah, vcovm, nummodelparams, num_data_points)
{  
  for (ii in 1:length(helperList$PredictYearsStartAt1))
  {
    cifn<-sqrt(hhat[ii,] %*% vcovm %*% hhat[ii,])
    margin_error<-qt(0.975,num_data_points - nummodelparams) * cifn
    margin_error99<-qt(0.995,num_data_points - nummodelparams) * cifn
    helperList$cis[,ii]<-c(gammah[ii] - margin_error,gammah[ii] + margin_error)
    helperList$cis99[,ii]<-c(gammah[ii] - margin_error99,gammah[ii] + margin_error99)
    helperList$se[ii]<-cifn

  }
  
  list(cis = helperList$cis, cis99 = helperList$cis99, se = helperList$se)  
}
#-------------------------------------------------------------------------------

### Functions associated with each model

#-------------------------------------------------------------------------------
# MODEL 1: FIRST ORDER POLYNOMIAL
WaldLimitspoly1fun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] + cofer[2] * helperList$PredictYearsStartAt1
  Hhat<-cbind(1, helperList$PredictYearsStartAt1)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListpoly1fun <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MODEL 2: 4.2.30 FROM RATKOWSKY (1983)
WaldLimitsratk423 <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * (1 +  helperList$PredictYearsStartAt1)^cofer[2]
  Hhat<-cbind((helperList$PredictYearsStartAt1 + 1)^cofer[2], cofer[1] * (helperList$PredictYearsStartAt1 + 1) ^ cofer[2] * log(helperList$PredictYearsStartAt1 + 1))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  
  CIList2ndOrderPoly  <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}

#-------------------------------------------------------------------------------
# MODEL 3: 4.3.33 FROM RATKOWSKY (1983)
WaldLimitsratk4333 <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
                                                   
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * exp(cofer[2] / (helperList$PredictYearsStartAt1 + cofer[3]))
  Hhat<-cbind(exp(cofer[2] / (cofer[3] + helperList$PredictYearsStartAt1)), cofer[1] * exp(cofer[2] / (cofer[3] + helperList$PredictYearsStartAt1)) / (cofer[3] + helperList$PredictYearsStartAt1),-cofer[1] * cofer[2] * exp(cofer[2] / (cofer[3] + helperList$PredictYearsStartAt1)) / (cofer[3] + helperList$PredictYearsStartAt1)^2)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIList3rdOrderPoly <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 4: HYPERBOLIC
WaldLimitshyperfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] / (cofer[2] + helperList$PredictYearsStartAt1)
  Hhat<-cbind(1 / (cofer[2] + helperList$PredictYearsStartAt1), -cofer[1] / (cofer[2] + helperList$PredictYearsStartAt1)^2)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListHpyer <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 5: MICHAELIS-MENTEN
WaldLimitsmmfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * helperList$PredictYearsStartAt1) / (cofer[2] + helperList$PredictYearsStartAt1)
  Hhat<-cbind(helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1), -cofer[1] * helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1)^2)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListMM <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 6: HOLLING TYPE III
WaldLimitshiiifun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * helperList$PredictYearsStartAt1^2) / (cofer[2] + helperList$PredictYearsStartAt1^2)
  Hhat<-cbind(helperList$PredictYearsStartAt1^2 / (cofer[2] + helperList$PredictYearsStartAt1^2), -cofer[1] * helperList$PredictYearsStartAt1^2 / (cofer[2] + helperList$PredictYearsStartAt1^2)^2)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListHIII <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 7: HOLLING TYPE IV
WaldLimitshivfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * helperList$PredictYearsStartAt1^2) / (cofer[2] + cofer[3] * helperList$PredictYearsStartAt1 + helperList$PredictYearsStartAt1^2)
  Hhat<-cbind(helperList$PredictYearsStartAt1^2 / (cofer[3] * helperList$PredictYearsStartAt1 + helperList$PredictYearsStartAt1^2 + cofer[2]), -cofer[1] * helperList$PredictYearsStartAt1^2 / (cofer[3] * helperList$PredictYearsStartAt1 + helperList$PredictYearsStartAt1^2 + cofer[2])^2, -cofer[1] * helperList$PredictYearsStartAt1^3 / (cofer[3] * helperList$PredictYearsStartAt1 + helperList$PredictYearsStartAt1^2 + cofer[2])^2)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListHIV <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 8: RATIONAL
WaldLimitsrationalfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-((cofer[1] + (cofer[2] * helperList$PredictYearsStartAt1)) / (1 + cofer[3] * helperList$PredictYearsStartAt1))
  Hhat<-cbind(1 / (cofer[3] * helperList$PredictYearsStartAt1 + 1), helperList$PredictYearsStartAt1 / (cofer[3] * helperList$PredictYearsStartAt1 + 1), -(cofer[2] * helperList$PredictYearsStartAt1 + cofer[1]) * helperList$PredictYearsStartAt1 / (cofer[3] * helperList$PredictYearsStartAt1 + 1)^2)    

  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListRational <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 9: EXPONENTIAL
WaldLimitsexpfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * (exp(helperList$PredictYearsStartAt1 * cofer[2]))
  Hhat<-cbind(exp(cofer[2] * helperList$PredictYearsStartAt1), cofer[1] * helperList$PredictYearsStartAt1 * exp(cofer[2] * helperList$PredictYearsStartAt1))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListExp <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 10: MONOMOLECULAR
WaldLimitsmonofun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * (1 - exp(cofer[2] * helperList$PredictYearsStartAt1))
  Hhat<-cbind(-exp(cofer[2] * helperList$PredictYearsStartAt1) + 1, -cofer[1] * helperList$PredictYearsStartAt1 * exp(cofer[2] * helperList$PredictYearsStartAt1))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListMono <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 11: RICKER
WaldLimitsrickerfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * helperList$PredictYearsStartAt1  * exp(cofer[2] * helperList$PredictYearsStartAt1)
  Hhat<-cbind(helperList$PredictYearsStartAt1 * exp(cofer[2] * helperList$PredictYearsStartAt1), cofer[1] * helperList$PredictYearsStartAt1^2 * exp(cofer[2] * helperList$PredictYearsStartAt1))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListRicker <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 12: GOMPERTZ
WaldLimitsgompertzfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-exp(cofer[1] * exp(cofer[2] * (helperList$PredictYearsStartAt1 - cofer[3])))
  Hhat<-cbind(exp(-cofer[2] * (cofer[3] - helperList$PredictYearsStartAt1) + cofer[1] * exp(-cofer[2] * (cofer[3] - helperList$PredictYearsStartAt1))), -cofer[1] * (cofer[3] * helperList$PredictYearsStartAt1) * exp(-cofer[2] * (cofer[3] - helperList$PredictYearsStartAt1) + cofer[1] * exp(-cofer[2] * (cofer[3] - helperList$PredictYearsStartAt1))), -cofer[1] * cofer[2] * exp(cofer[2] * (cofer[3] - helperList$PredictYearsStartAt1) + cofer[1] * exp(-cofer[2] * (cofer[3] - helperList$PredictYearsStartAt1))))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListGompertz <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 13: CHAPMAN-RICHARDS
WaldLimitschaprichfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * (1 - exp(cofer[2] * helperList$PredictYearsStartAt1) ^ cofer[3])
  Hhat<-cbind(-(exp(cofer[2] * helperList$PredictYearsStartAt1))^cofer[3] + 1, -cofer[1] * cofer[3] * helperList$PredictYearsStartAt1 * (exp(cofer[2] * helperList$PredictYearsStartAt1))^(cofer[3] - 1) * exp(cofer[2] * helperList$PredictYearsStartAt1), -cofer[1] * (exp(cofer[2] * helperList$PredictYearsStartAt1))^cofer[1] * log(exp(cofer[2] * helperList$PredictYearsStartAt1)))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListChapRich <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 14: WEIBULL  NOW 4-PARAMETER GOMPERTZ
WaldLimitsweibullfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  

  
  # Calculate estimates of gamma and first derivative matrix
  #gammahat<-cofer[1] * (1 - exp(cofer[2] * (helperList$PredictYearsStartAt1 - cofer[3])^cofer[4]))
  gammahat <- cofer[1] + cofer[2] * exp(-exp(cofer[3] - cofer[4] * helperList$PredictYearsStartAt1))
  #Hhat<-cbind(-exp(cofer[2] * (-cofer[3] + helperList$PredictYearsStartAt1)^cofer[4]) + 1, -cofer[1] * (-cofer[3] + helperList$PredictYearsStartAt1)^cofer[4] * exp(cofer[2] * (-cofer[3] + helperList$PredictYearsStartAt1)^cofer[4]), cofer[1] * cofer[2] * (-cofer[3] + helperList$PredictYearsStartAt1) ^ (cofer[4] - 1) * cofer[4] * exp(cofer[2] * (-cofer[3] + helperList$PredictYearsStartAt1)^cofer[4]), 7)
  
  Hhat <- cbind(1, exp(-exp(-cofer[4] * helperList$PredictYearsStartAt1 + cofer[3])), - cofer[2] * exp(-cofer[4] * helperList$PredictYearsStartAt1 + cofer[3]  - exp(-cofer[4] * helperList$PredictYearsStartAt1 + cofer[3])), cofer[2] * helperList$PredictYearsStartAt1 * exp(-cofer[4] * helperList$PredictYearsStartAt1 + cofer[3] - exp(-cofer[4] * helperList$PredictYearsStartAt1 + cofer[3])))  
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListWeibull <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MODEL 15: POWER-LAW
WaldLimitspowerfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] * helperList$PredictYearsStartAt1 ^ cofer[2]
  Hhat<-cbind(helperList$PredictYearsStartAt1^cofer[2], cofer[1] * helperList$PredictYearsStartAt1^cofer[2] * log(helperList$PredictYearsStartAt1))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListPower <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MODEL 16: ASYMPTOTIC
WaldLimitsasymptoticfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-cofer[1] - (cofer[2] * cofer[3] ^ helperList$PredictYearsStartAt1)
  Hhat<-cbind(1, -cofer[3]^helperList$PredictYearsStartAt1, -helperList$PredictYearsStartAt1 * cofer[2] * cofer[3]^(helperList$PredictYearsStartAt1 - 1))    

  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListAsymptotic <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 17: SHEPHERD
WaldLimitsshepherdfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * helperList$PredictYearsStartAt1) / (cofer[2] + helperList$PredictYearsStartAt1^cofer[3])
  Hhat<-cbind(helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1^cofer[3]), -cofer[1] * helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1^cofer[3])^2, -cofer[1] * helperList$PredictYearsStartAt1 * helperList$PredictYearsStartAt1^cofer[3] * log(helperList$PredictYearsStartAt1) / (cofer[2] + helperList$PredictYearsStartAt1^cofer[3])^2)    

  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListShepherd <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MODEL 18: HASSELL
WaldLimitshassellfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1)^cofer[3])
  Hhat<-cbind(helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1)^cofer[3], -cofer[1] * (cofer[2] + helperList$PredictYearsStartAt1)^(cofer[3] - 1) * cofer[3] * helperList$PredictYearsStartAt1 / ((cofer[2] + helperList$PredictYearsStartAt1)^cofer[3])^2, -cofer[1] * helperList$PredictYearsStartAt1 * log(cofer[2] + helperList$PredictYearsStartAt1) / (cofer[2] + helperList$PredictYearsStartAt1)^cofer[3])    

  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListHassell <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MODEL 19: HYPERBOLIC
WaldLimitshyperbolafun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-((cofer[1] + (cofer[2] * helperList$PredictYearsStartAt1))^(-cofer[3]))
  Hhat<-cbind(-(cofer[2] * helperList$PredictYearsStartAt1 + cofer[1])^(-cofer[3] -1) * cofer[3], -(cofer[2] * helperList$PredictYearsStartAt1 + cofer[1])^(-cofer[3] - 1) * cofer[3] * helperList$PredictYearsStartAt1,  -(cofer[2] * helperList$PredictYearsStartAt1 + cofer[1])^(-cofer[3]) * log(cofer[2] * helperList$PredictYearsStartAt1 + cofer[1]))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListHyperbolicfun <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# MODEL 20: NEGEXP2
WaldLimitsnegexp2fun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * (1 - exp(-cofer[2] * (helperList$PredictYearsStartAt1))))
  Hhat<-cbind(-exp(-cofer[2] * helperList$PredictYearsStartAt1) + 1, cofer[1] * helperList$PredictYearsStartAt1 * exp(-cofer[2] * helperList$PredictYearsStartAt1))
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListNegExp2fun <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# MODEL 21: NEGTANH
WaldLimitsnegtanhfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] * tanh(cofer[2] * (helperList$PredictYearsStartAt1) / cofer[1]))
  Hhat<-cbind(0, -(tanh(cofer[2] * helperList$PredictYearsStartAt1)^2 - 1) * helperList$PredictYearsStartAt1)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListNegtanhfun <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# MODEL 22: NEGMM
WaldLimitsnegmmfun <- function(cofer, PredictYears, vcovmatrix, nummodelparams, num_data_points)
{
  # Set up variables
  helperList <- HelperFunction(PredictYears)
  
  # Calculate estimates of gamma and first derivative matrix
  gammahat<-(cofer[1] - (cofer[1] - cofer[2]) * (helperList$PredictYearsStartAt1) / (cofer[3] + helperList$PredictYearsStartAt1))
  Hhat<-cbind(-helperList$PredictYearsStartAt1 / (cofer[3] + helperList$PredictYearsStartAt1) + 1, helperList$PredictYearsStartAt1 / (cofer[3] + helperList$PredictYearsStartAt1), (cofer[1] - cofer[2]) * helperList$PredictYearsStartAt1 / (cofer[2] + helperList$PredictYearsStartAt1)^2)
  
  # Calculate CIs and SEs based on these estimates
  CIsSEs <- CalculateSEsCIs(helperList, Hhat, gammahat, vcovmatrix, nummodelparams, num_data_points)
  
  CIListNegmmfun <- list(LowCI = CIsSEs[[1]][1,], HighCI = CIsSEs[[1]][2,], Low99CI = CIsSEs[[2]][1,], High99CI = CIsSEs[[2]][2,], SEs = CIsSEs[[3]])
}
#-------------------------------------------------------------------------------







