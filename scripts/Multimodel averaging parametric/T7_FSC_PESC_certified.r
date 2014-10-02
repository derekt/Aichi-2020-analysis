# DiagnosticPlots.r modified from: http://ucfagls.wordpress.com/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
  
  # Load required libraries
  library(mgcv)
  library(nlme)
  library(MASS)
  library(emdbook)
  
  script.dir <- dirname(sys.frame(1)$ofile)
  setwd(script.dir)

  # Load required functions
  source("common functions/Bootstrapping.r")
  source("common functions/DiagnosticPlots.r")
  source("common functions/ParametricFunctions.r")
  source("common functions/WaldParametricFunctions.r")
  source("common functions/PlotIndicatorProjections.r")
  source("common functions/PlotIndicatorProjections.r")
  source("common functions/Fit_autocorrelated_models.r")
  source("common functions/writepdfsandresults.r")

# Set working directory for data
setwd("../../data/Target 7")

    output_name = "T7_FSC_PEFC_Cert_Forest"

# Read in and attach data
eafs<-read.csv("fsc_and_pefc_certified_area.csv")
eafs <-eafs[1:(dim(eafs)[1]),]

# Convert to million hectares
eafs[,2] = eafs[,2] / 1000000

colnames(eafs) = c("Year", "data")


    # Num data points
  num_data_points = sum(!is.na(eafs[,2]))

minx = min(eafs[,2])
# Transform data
back_transform <- function(x)
{
  x * minx
}

eafs[,2] = eafs[,2] / minx

attach(eafs)
                                                  

# Get the years for which to predict values
PredictYears = c(min(Year):2020)
PredictYearsDataFrame = data.frame(Year = PredictYears)

# Years which are projected
PredictionSpan = which(PredictYears == (max(Year) + 1)):length(PredictYears)

# Target and plot values                                              
target_value = -99999999999
y_limit = 600
target_value_text = -999999999999
ylabel = "Million hectares"
maintitle = "Area of FSC and PEFC certified forest"

#-------------------------------------------------------------------------------------
### MODELLING

m <- list()

plot(Year - min(Year) + 1, data, pch = 20)

#-------------------------------------------------------------------------------------
# MODEL 1: FIRST ORDER POLYNOMIAL
# No autocorrelation function
m[[1]] <- FitAutocorrelatedModel("poly1fun", "(a,b,Year)", 2, "eafs", "list(a =-22, b = 50)", "coef(m1)", "coef(m1)", TRUE, fitAR2 = FALSE, num_data_points = num_data_points, ylabel, "First order polynomial")
 
  # MODEL 2: 4.2.30 FROM RATKOWSKY
  m[[2]] <- FitAutocorrelatedModel("ratk423", "(a,b,Year)", 3, "eafs", "list(a = 100, b = 1)", "coef(m1)", "coef(m1)", TRUE, fitAR2 = FALSE,  num_data_points = num_data_points, ylabel, "2 parameter negative exponential")
  
  #-------------------------------------------------------------------------------------
  # MODEL 3: 4.3.33 FROM RATKOWSKY
  m[[3]] <- FitAutocorrelatedModel("ratk4333", "(a,b,c,Year)", 4, "eafs", "list(a = 100, b = -1, c = 0.5)", "coef(m1)", "coef(m2)", TRUE, fitAR2 = FALSE,  num_data_points = num_data_points, ylabel, "3 parameter negative exponential")
  
#--------------------------------------------------------------------------------------
# MODEL 4: HYPERBOLIC
m[[4]] <- FitAutocorrelatedModel("hyperfun", "(a,b,Year)", 2, "eafs", "list(a = 200, b = -1)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Hyperbolic")

#--------------------------------------------------------------------------------------
# MODEL 5: MICHAELIS-MENTEN
m[[5]] <- FitAutocorrelatedModel("mmfun", "(a,b,Year)", 2, "eafs", "list(a = 10, b = 1)", "coef(m1)", "list(a = 1.3, b = 1.3)", fitAR1 = FALSE, TRUE, num_data_points = num_data_points, ylabel, "Michaelis-Menten",0.1)

#--------------------------------------------------------------------------------------
# MODEL 6: HOLLING TYPE III
m[[6]] <- FitAutocorrelatedModel("hiiifun", "(a,b,Year)", 2, "eafs", "list(a = 1.1, b = 1)", "coef(m1)", "list(a = 1.1, b = 0.03)", TRUE, fitAR1 = FALSE, num_data_points = num_data_points, ylabel, "Hollings Type III")

#--------------------------------------------------------------------------------------
# MODEL 7: HOLLING TYPE IV
m[[7]] <- FitAutocorrelatedModel("hivfun", "(a,b,c,Year)", 3, "eafs", "list(a = 1.1, b = 1, c = 1)", "coef(m1)", "list(a = 1.1, b = 0.03, c = 1)", TRUE, fitAR1 = FALSE, num_data_points = num_data_points, ylabel, "Hollings Type IV")

#--------------------------------------------------------------------------------------
# MODEL 8: RATIONAL
m[[8]] <- FitAutocorrelatedModel("rationalfun", "(a,b,d,Year)", 3, "eafs", "list(a = 1.1, b = 1, d = 1)", "coef(m1)", "list(a = 1.1, b = 0.03, d = 1)", TRUE, num_data_points = num_data_points, ylabel, "Rational")

#-------------------------------------------------------------------------------------
## MODEL 9: EXPONENTIAL  
m[[9]] <- FitAutocorrelatedModel("expfun", "(a,b,Year)", 2, "eafs", "list(a = 20, b = -1)", "coef(m1)", "coef(m1)", TRUE, fitAR2 = FALSE, num_data_points = num_data_points, ylabel, "Exponential")

#--------------------------------------------------------------------------------------
# MODEL 10: MONOMOLECULAR
m[[10]] <- FitAutocorrelatedModel("monofun", "(a,b,Year)", 2, "eafs", "list(a = 130, b = -0.1)", "coef(m1)", "coef(m2)", TRUE, fitAR2 = FALSE, num_data_points = num_data_points, ylabel, "Monomolecular")

#--------------------------------------------------------------------------------------
# MODEL 11: RICKER
m[[11]] <- FitAutocorrelatedModel("rickerfun", "(a,b,Year)", 2, "eafs", "list(a = 1.1, b = 1)", "coef(m1)", "coef(m2)", TRUE, fitAR2 = FALSE, num_data_points = num_data_points, ylabel, "Ricker")

#--------------------------------------------------------------------------------------
# MODEL 12: GOMPERETZ
m[[12]] <- FitAutocorrelatedModel("gompertzfun", "(a,b,d,Year)", 3, "eafs", "list(a = 1.1, b =0.1,d = 0.1)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Gompertz")

#--------------------------------------------------------------------------------------
# MODEL 13: CHAPMAN-RICHARDS
m[[13]] <- FitAutocorrelatedModel("chaprichfun", "(a,b,d,Year)", 3, "eafs", "list(a = 12, b = 0.2,d = 0.02)", "list(a = 1.1, b = 0.1,d = 5)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Chapman-Richards")

#---------------------------------------------------------------------------------------
# MODEL 14: WEBIFULL
m[[14]] <- FitAutocorrelatedModel("weibullfun", "(a,b,d,e,Year)", 4, "eafs", "list(a = 2, b = 2,d = 2, e= 0.01)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Weibull")

#---------------------------------------------------------------------------------------
# MODEL 15: POWER-LAW
m[[15]] <- FitAutocorrelatedModel("powerfun", "(a,b,Year)", 2, "eafs", "list(a = 200, b = -0.8)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Power", 0.01, 0.1, 0.1)

#--------------------------------------------------------------------------------------
# MODEL 16: ASYMPTOTIC
m[[16]] <- FitAutocorrelatedModel("asymptoticfun", "(a,b,d,Year)", 3, "eafs", "list(a = 1000, b = 2, d = 1)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Asymptotic", 0.1)

#--------------------------------------------------------------------------------------
# MODEL 17: SHEPHERD
m[[17]] <- FitAutocorrelatedModel("shepherdfun", "(a,b,d,Year)", 3, "eafs", "list(a = 2, b = 2, d = 0.98)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Shepherd")

#--------------------------------------------------------------------------------------
# MODEL 18: HASSELL
m[[18]] <- FitAutocorrelatedModel("hassellfun", "(a,b,d,Year)", 3, "eafs", "list(a = 28, b = -0.02, d = 0.-2)", "list(a = 28, b = -0.02, d = 0.-2)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Hassell")





#--------------------------------------------------------------------------------------
# MULTI-MODEL AVERAGING

# Create a data frame to hold the results
Modelnames = vector()
for (ii in 1:18)
  Modelnames[[ii]] = m[[ii]]$modelname
  
# Calculate small sample size AIC value
ssAIC <- function(model)
{
  num_params = length(coef(model)) + 1 + length(unlist(model$modelStruct))
  AIC(model) + (2 * num_params * (num_params + 1)) / (num_data_points - num_params - 1)   
}

ModelAICs = vector()
for (ii in 1:18)
{
  if (!is.na(m[[ii]]$model[1]))
    ModelAICs[[ii]] = ssAIC(m[[ii]]$model)
  else
    ModelAICs[[ii]] = NA
} 
Results = data.frame(Modelnames, ModelAICs) 

# Filter out NA values
Results_noNAs = Results[!is.na(Results[,2]),]

# Sort models by AIC value
sorted_models = sort(Results_noNAs[,2], index.return = T)$ix
sorted_models_original_list = order(Results[,2])[1:length(sorted_models)]

# Get the minimum AIC
min_AIC<-min(Results_noNAs$ModelAICs[sorted_models], na.rm = T)

# Get the delta AICs
delta_AIC<-Results_noNAs$ModelAICs[sorted_models] - min_AIC

# Keep only models for which delta AIC < 2
num_models_to_be_averaged = length(which(delta_AIC < 2))

# Calculate the Akaike weights
temp_val<-sum(exp(-delta_AIC/2),na.rm=TRUE)
akaike_weights<-exp(-delta_AIC/2) / temp_val

# Calculate normalised AIC weights
norm_akaike_weights = akaike_weights[1:num_models_to_be_averaged]
norm_akaike_weights = norm_akaike_weights / sum(norm_akaike_weights)

# Combine predictions from all models to be averaged into a matrix
modelpred = matrix(nrow = num_models_to_be_averaged, ncol= length(PredictYears))
for (ii in 1:num_models_to_be_averaged)
  modelpred[ii,] = as.vector(m[[sorted_models_original_list[ii]]]$prediction)
  
# Calculate model-weighted averages
model_average<-matrix(nrow = 1,ncol = length(PredictYears))
    
for (ii in 1:length(PredictYears))
{
  model_average[ii]<-sum(modelpred[,ii] * norm_akaike_weights)
}      

# Combine standard errors from all models to be averaged into a matrix
modelses = matrix(nrow = length(sorted_models), ncol= length(PredictYears))
for (ii in 1:num_models_to_be_averaged)
  modelses[ii,] = as.vector(m[[sorted_models_original_list[ii]]]$CIs$SEs) 

# Now calculate the unconditional variance for the fitted values                                           S
fitted_se<-vector(mode="numeric",length=length(PredictYears));
  for (jj in 1:length(PredictYears))
  {
  for (ii in 1:num_models_to_be_averaged)
  {
       fitted_se[jj]<-fitted_se[jj] + norm_akaike_weights[ii] * sqrt(modelses[ii,jj]^2  + ( modelpred[ii,jj] - model_average[jj])^2)
  }
}    

nparams <- vector(mode = "numeric", length = length(num_models_to_be_averaged))
for (ii in 1: num_models_to_be_averaged)
{
  nparams[ii] = length(coef(m[[sorted_models_original_list[ii]]]$model)) + length(unlist(m[[sorted_models_original_list[ii]]]$model$modelStruct))
}
nparams = mean(nparams)
nparams


fitted_var<-fitted_se^2
model_avg_fitted_se<-sqrt(fitted_var)
model_avg_fitted_ci<-rbind(model_average - qt(0.975,num_data_points - nparams) * model_avg_fitted_se, model_average + qt(0.975,num_data_points - nparams) * model_avg_fitted_se)
model_avg_fitted_ci99<-rbind(model_average - qt(0.995,num_data_points - nparams) * model_avg_fitted_se, model_average + qt(0.995,num_data_points - nparams) * model_avg_fitted_se)   
   
   
# Write out images of fit and results matrix containing fit and bootstrapped values
writepdfsandresults(output_name, 0)