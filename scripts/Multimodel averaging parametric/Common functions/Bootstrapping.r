# Calculate bootstrapped CIs using a bootstrapping of residuals
# Inputs are original data, span over which to predict, and a model object
# Currently only works with model objects with 4 or 5 arguments including
# starting parameter list and optionally a correlation function.
ResidBootstrap <- function(data, PredictYears, model)
{
  # Create a string to hold the new model call, which is the same as the old but using "newdata" instead of whatever the old data were called
  newcall = ""
  
  # List of starting parameter values
  startlist = "start = list("
  for (ii in 2:length(as.character(model$call[[4]])))
  {
    startlist = paste(startlist, as.character(names(model$call[[4]][ii])), " = ", as.character(model$call[[4]][ii]), sep="")
    if (ii == length(as.character(model$call[[4]])))
    {
      startlist = paste(startlist, ")", sep="")
    }
    else
    {
      startlist = paste(startlist, ", ", sep="")
    }
  }
  newcall = paste(as.character(model$call[[1]]), "(", "newdata ~ ", as.character(model$call[[2]][3]), ", ", startlist,sep="")
  
  # Correlation function
  if (length(model$call) == 5)
    newcall = paste(newcall, ", correlation = ", as.character(model$call[[5]])[1], "(form = ", as.character(model$call[[5]])[2], ", p = ", as.character(model$call[[5]])[3], ")", sep="")
  if (length(model$call) > 5)
    print("ERROR! MODEL CALL > 5 ELEMENTS!")
    
  newcall = paste(newcall, ")", sep="")
  print("New model call for residual bootstrapping is: ")
  print(newcall)
  
  # Get the residuals
  modResid = residuals(model)

  # Create a matrix to hold the model results
  bootstrapped = matrix(nrow = 1000, ncol = length(PredictYears))
  
  # Do the resampling with replacement and refit the model
  for (ii in 1:1000)
  {
    newdata = data + sample(modResid, replace=TRUE)
    eval(parse(text = paste("newmodel <- ", newcall, sep="")))
    bootstrapped[ii,] = as.vector(predict(newmodel, newdata = data.frame(Year = PredictYears)))
  }
  
  # Return the confidence intervals and standard errors (standard deviation of boostrapped output)
  CILow = vector(length = length(PredictYears))
  CIHigh = vector(length = length(PredictYears))
  SEs = vector(length = length(PredictYears))
  for (ii in 1:length(PredictYears))
  {
    nn = sort(bootstrapped[,ii])
    CILow[ii] = nn[25]
    CIHigh[ii] = nn[975]
    SEs[ii] = sd(nn)
  }
  list(CILow = CILow, CIHigh = CIHigh, SEs = SEs)
}