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
setwd("../../data/Target 5")

# Turn warnings off for attempting model fits
options(warn = -1)

  output_name = "T5_Pct_natural_habitat"


# Read in and attach data
eafs<-read.csv("Percent_land_area_natural_habitat.csv")
colnames(eafs) = c("Year", "data")

  back_transform <- function(x)
  {
    x
  }
  attach(eafs)


# Get the years for which to predict values
PredictYears = c(Year, (max(Year) + 1):2020)
PredictYearsDataFrame = data.frame(Year = PredictYears)

# Years which are projected
PredictionSpan = which(PredictYears == (max(Year) + 1)):length(PredictYears)


    # Num data points
  num_data_points = sum(!is.na(eafs[,2]))
  
# Target and plot values                                              
target_value = -9999
target_value_text = -9999
y_limit = 75
ylabel = "Percent global area"
maintitle = "Natural habitat extent"

#-------------------------------------------------------------------------------------
### MODELLING

m <- list()

#-------------------------------------------------------------------------------------
# MODEL 1: FIRST ORDER POLYNOMIAL

m[[1]] <- FitAutocorrelatedModel("poly1fun", "(a,b,Year, FALSE)", 2, "eafs", "list(a =1, b = 1)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "First order polynomial", 0.01, 0.01, 1)

 
# MODEL 2: 4.2.30 FROM RATKOWSKY
m[[2]] <- FitAutocorrelatedModel("ratk423", "(a,b,Year)", 2, "eafs", "list(a = 100, b = 1)", "coef(m1)", "coef(m1)", TRUE, num_data_points = num_data_points, ylabel, "2 parameter negative exponential")

#-------------------------------------------------------------------------------------
# MODEL 3: 4.3.33 FROM RATKOWSKY
m[[3]] <- FitAutocorrelatedModel("ratk4333", "(a,b,c,Year)", 3, "eafs", "list(a = 100, b = -1, c = 0.5)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "3 parameter negative exponential")

#--------------------------------------------------------------------------------------
# MODEL 4: HYPERBOLIC
m[[4]] <- FitAutocorrelatedModel("hyperfun", "(a,b,Year)", 2, "eafs", "list(a = 1, b = 1)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Hyperbolic")

#--------------------------------------------------------------------------------------
# MODEL 5: MICHAELIS-MENTEN
m[[5]] <- FitAutocorrelatedModel("mmfun", "(a,b,Year)", 2, "eafs", "list(a = 1.1, b = 1)", "coef(m1)", "list(a = 1.3, b = 1.3)", TRUE, num_data_points = num_data_points, ylabel, "Michaelis-Menten")

#--------------------------------------------------------------------------------------
# MODEL 6: HOLLING TYPE III
m[[6]] <- FitAutocorrelatedModel("hiiifun", "(a,b,Year)", 2, "eafs", "list(a = 1.1, b = 1)", "coef(m1)", "list(a = 1.1, b = 0.03)", TRUE, num_data_points = num_data_points, ylabel, "Hollings Type III")

#--------------------------------------------------------------------------------------
# MODEL 7: HOLLING TYPE IV
m[[7]] <- FitAutocorrelatedModel("hivfun", "(a,b,c,Year)", 3, "eafs", "list(a = 1.1, b = 1, c = 1)", "coef(m1)", "list(a = 1.1, b = 0.03, c = 1)", TRUE, num_data_points = num_data_points, ylabel, "Hollings Type IV")

#--------------------------------------------------------------------------------------
# MODEL 8: RATIONAL
m[[8]] <- FitAutocorrelatedModel("rationalfun", "(a,b,d,Year)", 3, "eafs", "list(a = 1.1, b = 1, d = 1)", "coef(m1)", "list(a = 1.1, b = 0.03, d = 1)", TRUE,  num_data_points = num_data_points, ylabel, "Rational")

#-------------------------------------------------------------------------------------
## MODEL 9: EXPONENTIAL   
m[[9]] <- FitAutocorrelatedModel("expfun", "(a,b,Year)", 2, "eafs", "list(a = 50, b = -0.05)", "coef(m1)", "coef(m2)", TRUE,  num_data_points = num_data_points, ylabel, "Exponential")

#--------------------------------------------------------------------------------------
# MODEL 10: MONOMOLECULAR
m[[10]] <- FitAutocorrelatedModel("monofun", "(a,b,Year)", 2, "eafs", "list(a = 50, b = -0.005)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Monomolecular")

#--------------------------------------------------------------------------------------
# MODEL 11: RICKER 
m[[11]] <- FitAutocorrelatedModel("rickerfun", "(a,b,Year)", 2, "eafs", "list(a = 0.1, b = 0.1)", "list(a = 0.1, b = 0.1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Ricker")

#--------------------------------------------------------------------------------------
# MODEL 12: GOMPERETZ
m[[12]] <- FitAutocorrelatedModel("gompertzfun", "(a,b,d,Year)", 3, "eafs", "list(a = 1.1, b =-0.1,d = 0.1)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Gompertz")

#--------------------------------------------------------------------------------------
# MODEL 13: CHAPMAN-RICHARDS
m[[13]] <- FitAutocorrelatedModel("chaprichfun", "(a,b,d,Year)", 3, "eafs", "list(a = 40, b = 0.2,d = 0.2)", "list(a = 1.1, b = 0.1,d = 5)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Chapman-Richards")

#---------------------------------------------------------------------------------------
# MODEL 14: WEBIFULL
m[[14]] <- FitAutocorrelatedModel("weibullfun", "(a,b,d,e,Year)", 4, "eafs", "list(a = 2, b = 2,d = 2, e= 0.01)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Weibull")

#---------------------------------------------------------------------------------------
# MODEL 15: POWER-LAW
m[[15]] <- FitAutocorrelatedModel("powerfun", "(a,b,Year)", 2, "eafs", "list(a = 2, b = 2)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Power")

#--------------------------------------------------------------------------------------
# MODEL 16: ASYMPTOTIC
m[[16]] <- FitAutocorrelatedModel("asymptoticfun", "(a,b,d,Year)", 3, "eafs", "list(a = 0.55, b = 0.08, d = 1.1)", "coef(m1)", "coef(m2)", TRU, ylabel, "Asymptotic", 0.001)

#--------------------------------------------------------------------------------------
# MODEL 17: SHEPHERD
m[[17]] <- FitAutocorrelatedModel("shepherdfun", "(a,b,d,Year)", 3, "eafs", "list(a = 2, b = 2, d = 0.98)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Shepherd")

#--------------------------------------------------------------------------------------
# MODEL 18: HASSELL
m[[18]] <- FitAutocorrelatedModel("hassellfun", "(a,b,d,Year)", 3, "eafs", "list(a = 2, b = 2, d = 0.98)", "coef(m1)", "coef(m2)", TRUE, num_data_points = num_data_points, ylabel, "Hassell")






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
   
# Plot model average
# Plot projections


  
  png(filename=paste("../../images/", output_name, ".png", sep = ""), res = 1200, width = 4, height = 4, units = "in", pointsize = 4)
  par(mar = c(5,5,2,2), xaxs = "i", yaxs = "i", cex = 1.2, cex.axis = 1.6, cex.lab = 1.6, cex.main = 1.6)
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, maintitle, TRUE, min_y_limit = 55, cexl = 1, cexp = 2, DataYears = eafs[,1])
  dev.off()
  
   pdf(file=paste("../../images/", output_name, ".pdf", sep = ""))
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, maintitle, TRUE, min_y_limit = 55, DataYears = eafs[,1])
  dev.off()
  
  
png(filename=paste("../../images/", output_name, "_multimodel.png", sep = ""), res = 1200, width = 4, height = 4, units = "in", pointsize = 4)
  par(mar = c(5,5,2,2), xaxs = "i", yaxs = "i", cex = 2, cex.axis = 1.75, cex.lab = 1.75, cex.main = 1.75)
  
  # Plot model averaged plus top 5 models
  newpar <- par(mfrow=c(3,2),  oma=c(0,1,1,1)) 
  
  # Plot projections
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text,y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, "Model averaged prediction", TRUE,   min_y_limit = 55, cexl = 0.4, cexp = 1.5, DataYears = eafs[,1])
  
    if (num_models_to_be_averaged > 5)
      num_models_to_be_averaged = 5
      
  for (ii in 1:num_models_to_be_averaged)
  {
    mtoplot = m[[sorted_models_original_list[ii]]]
    PlotIndicatorProjections(back_transform(as.vector(mtoplot$prediction)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(mtoplot$CIs$LowCI), back_transform(mtoplot$CIs$HighCI), back_transform(mtoplot$CIs$Low99CI), back_transform(mtoplot$CIs$High99CI),ylabel, paste(Results[sorted_models_original_list[ii],1], ": weight = ", format(norm_akaike_weights[ii], digits = 2), sep = "") , FALSE, min_y_limit = 55, DataYears = eafs[,1])
  }
  dev.off()
  
  pdf(file=paste("../../images/", output_name, "_multimodel.pdf", sep = ""))
  
  # Plot model averaged plus top 5 models
  newpar <- par(mfrow=c(3,2),  oma=c(0,1,1,1)) 
  
  # Plot projections
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text,y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, "Model averaged prediction", TRUE,   min_y_limit = 55, DataYears = eafs[,1])
  
    if (num_models_to_be_averaged > 5)
      num_models_to_be_averaged = 5
      
  for (ii in 1:num_models_to_be_averaged)
  {
    mtoplot = m[[sorted_models_original_list[ii]]]
    PlotIndicatorProjections(back_transform(as.vector(mtoplot$prediction)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(mtoplot$CIs$LowCI), back_transform(mtoplot$CIs$HighCI), back_transform(mtoplot$CIs$Low99CI), back_transform(mtoplot$CIs$High99CI),ylabel, paste(Results[sorted_models_original_list[ii],1], ": weight = ", format(norm_akaike_weights[ii], digits = 2), sep = "") , FALSE, min_y_limit = 55, DataYears = eafs[,1])
  }
  dev.off()



  
 
datamatrix = matrix(nrow = 7, ncol = length(PredictYears))
datamatrix[1,] = PredictYears
Year2010 = which(datamatrix[1,] == 2010)
datamatrix[2,] = c(rep(NA, PredictionSpan[1] - 1), PredictionSpan)
datamatrix[3,] = back_transform(as.vector(model_average))
Val2010 = datamatrix[3,Year2010]
datamatrix[4,] = back_transform(model_avg_fitted_ci[1,])
datamatrix[5,] = back_transform(model_avg_fitted_ci[2,])
datamatrix[6,] = back_transform(as.vector(model_avg_fitted_se))
tempdata = back_transform(eafs[,2])
for (ii in 1:length(tempdata))
{
  jj = 1;
  while (datamatrix[1,jj] != eafs[ii,1])
    jj = jj + 1;
  datamatrix[7,jj] = tempdata[ii] 
}

write.csv(datamatrix, file = paste("../../results/untransformed/untransformed_", output_name, ".csv", sep=""))

# Calculate bootstrapped values for use in the rates plot
n_bootstrap = 10000
temp_matrix = matrix(nrow = n_bootstrap, ncol = length(PredictYears))
# Loop through years
for (hh in 1:length(PredictYears))
{
  results_vector = vector(length = n_bootstrap)
  # This calculate a sorted set of estimates for those points
  temp_matrix[,hh] = model_avg_fitted_se[hh] * rt(n_bootstrap,num_data_points - nparams) + model_average[hh]
  temp_matrix[,hh] = back_transform(temp_matrix[,hh])
  temp_matrix[,hh] = (temp_matrix[,hh] - Val2010) / Val2010
}

datamatrix[3,] = (datamatrix[3,] - Val2010) / Val2010
datamatrix[4,] = (datamatrix[4,] - Val2010) / Val2010
datamatrix[5,] = (datamatrix[5,] - Val2010) / Val2010
datamatrix = datamatrix[1:5,]

datamatrix = rbind(datamatrix, temp_matrix)

write.csv(datamatrix, file = paste("../../results/transformed/transformed_", output_name, ".csv", sep=""))