#-------------------------------------------------------------------------------
# 3rd April 2014, Derek Tittensor, Greg Britten & Dan Boyce
# Use a random walk + noise model to look at the signal to noise ratio in the 
# data. See Petris 2010.
#
# Inputs: input_data is a 2 column file where the first column is named Year and
# the second column is named data
#
# Outputs: PNGs and PDFs of fits and fit checking assessments
# Plus a list containing full fit plus 95% CIs and AIC value of the model
#-------------------------------------------------------------------------------

DLM_Signal_To_Noise <- function(input_data)
{

  #-------------------------------------------------------------------------------
  # Calculate basic information about data
  num_data_points = dim(input_data)[1]
  
  # Extract data and create vectors for extrapolation
  x <- as.numeric(input_data$Year)
  y <- input_data[,2]

  #-------------------------------------------------------------------------------
    
  
  #-------------------------------------------------------------------------------
  # Set up model structure
  mod1 <- dlmModPoly(order = 1, dV = 1, dW = 1)
  
  # Optimisation function for estimable parameters
  build_mod1 <- function(parm) 
  	{
  		mod1$V <- exp(parm[1]) 	#OBSERVATION ERROR
  		mod1$W <- exp(parm[2])	#PROCESS ERROR ON SLOPE
  		return(mod1)
  	}
  
  # Using MLE to estimate parameters
  rwunifit <- dlmMLE(y, 		
  	parm=rep(0,2), #NUMBER OF ESTIMABLE PARAMETERS 	
  	build=build_mod1, 
  	method="Nelder-Mead")
  
  model_parm <- rwunifit$par              
  mod1$V <- exp(model_parm[1])	#OBSERVATION ERROR VARIANCE
  mod1$W <- exp(model_parm[2])	#INTERCEPT PROCESS VARIANCE
  mod1negLL = rwunifit$value
  
  # Now fit model where there is no observation variance, and do a likelihood ratio
  # test to see if the states-space approach is appropriate (do not use
  # when there is no evidence for observation error)
  mod2<- dlmModPoly(order = 1, dV = 0, dW = 1)
  
    # Optimisation function for estimable parameters
  build_mod2 <- function(parm) 
  	{
  		mod2$W <- exp(parm[1])	#PROCESS ERROR ON SLOPE
  		return(mod2)
  	}
  
  # Using MLE to estimate parameters
  rwunifit2 <- dlmMLE(y, 		
  	parm=rep(0,1), #NUMBER OF ESTIMABLE PARAMETERS 	
  	build=build_mod2, 
  	method="BFGS")
  
  model_parm2 <- rwunifit2$par            
  mod2$W <- exp(model_parm2[1])	#INTERCEPT PROCESS VARIANCE
  mod2negLL = rwunifit2$value

  print("Model 1 (estimated observation variance) negative log-likelihood: ")
  print(mod1negLL)
  print("Model 2 (no observation variance) negative log-likelihood: ")
  print(mod2negLL)
  print("Likelihood ratio test statistic: ")
  print(2 * mod2negLL - 2 * mod1negLL)
  print("Likelihood ratio test p value: ")
  print(pchisq(2 * mod2negLL - 2 * mod1negLL, 1))
  print("Signal variance")
  print(mod1$W)
  print("Noise variance")
  print(mod1$V)
  print("Signal to noise ratio")
  print(mod1$W / mod1$V)
  #dev.new()
  #plot(input_data)
  #-------------------------------------------------------------------------------
  
}