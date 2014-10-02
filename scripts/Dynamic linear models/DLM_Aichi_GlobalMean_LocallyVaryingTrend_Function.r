#-------------------------------------------------------------------------------
# 23 March 2014, Derek Tittensor, Greg Britten & Dan Boyce
# A function to perform a DLM extrapolation based on a global mean
# locally varying trend model
#
# Inputs: input_data is a 2 column file where the first column is named Year and
# the second column is named data
# Output_filename is a string containing the filename for output images
#
# Outputs: PNGs and PDFs of fits and fit checking assessments
# Plus a list containing full fit plus 95% CIs and AIC value of the model
#-------------------------------------------------------------------------------

DLM_GM_LVT <- function(input_data, output_filename, ylimits)
{

  #-------------------------------------------------------------------------------
  # Calculate basic information about data
  num_data_points = dim(input_data)[1]
  num_points_to_extrapolate = 2020 - max(input_data$Year)
  
  # Number of parameters to estimate is the number of states (2) plus the number 
  # of parameters to estimate (2)
  num_parameters_to_estimate = 4
  
  # Extract data and create vectors for extrapolation
  x <- as.numeric(input_data$Year)
  newx = x[length(x)] + 0:num_points_to_extrapolate
  fullx = c(x, newx[2:length(newx)])
  y <- input_data[,2]
  #-------------------------------------------------------------------------------
    
  
  #-------------------------------------------------------------------------------
  # Set up model structure
  mod1 <- dlmModPoly(order = 2, dV = 1, dW = c(0,1))
  
  # Optimisation function for estimable parameters
  build_mod1 <- function(parm) 
  	{
  		mod1$V <- exp(parm[1]) 	#OBSERVATION ERROR
  		mod1$W[2,2] <- exp(parm[2])	#PROCESS ERROR ON SLOPE
  		return(mod1)
  	}
  
  # Using MLE to estimate parameters
  rwunifit <- dlmMLE(y, 		
  	parm=rep(0,2), #NUMBER OF ESTIMABLE PARAMETERS 	
  	build=build_mod1, 
  	method="Nelder-Mead")
  
  model_parm <- rwunifit$par              
  mod1$V <- exp(model_parm[1])	#OBSERVATION ERROR VARIANCE
  mod1$W[2,2] <- exp(model_parm[2])	#INTERCEPT PROCESS VARIANCE
  mod1negLL = rwunifit$value

  #-------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------
  # Run the Kalman filter
  mod1Filt <- dlmFilter(y, mod1)
  

  # Print diagnostic plot
  png(filename=paste("../../images/", output_filename, "_GM_LVT_diagnostics.png", sep = ""))
  tsdiag(mod1Filt)
  dev.off()
  pdf(file=paste("../../images/", output_filename, "_GM_LVT_diagnostics.pdf", sep = ""))
  tsdiag(mod1Filt)
  dev.off()

  
  # Extract state estimates
  b0_Filt <- dropFirst(mod1Filt$m[,1]) 	#Position
  b1_Filt <- dropFirst(mod1Filt$m[,2])	#Slope
  
  # Extract covariance matrix from singular values
  state_var <- dlmSvd2var(mod1Filt$U.C, mod1Filt$D.C)
  
  # Extract variance of individual states (based on parameter estimates)
  b0_Filt_var <- unlist(lapply(state_var,"[",1))[-1] 	
  b1_Filt_var <- unlist(lapply(state_var,"[",4))[-1] 	
  covar1 <- unlist(lapply(state_var,"[",2))[-1] 
  
  # Confidence intervals on states
  b0_Filt_u <- b0_Filt + qnorm(0.975, sd=sqrt(b0_Filt_var))
  b0_Filt_l <- b0_Filt + qnorm(0.025, sd=sqrt(b0_Filt_var))
  b1_Filt_u <- b1_Filt + qnorm(0.975, sd=sqrt(b1_Filt_var))
  b1_Filt_l <- b1_Filt + qnorm(0.025, sd=sqrt(b1_Filt_var))
  
  # Calculate value and variance of state in which we are interested
  y_Filt	= b0_Filt;
  var1 = vector(length = num_data_points)
  var1 = b0_Filt_var# + b1_Filt_var + 2 * covar1
  # THIS IS b0 IS MU PLUS LAMBDA, WHILE b1 IS LAMBDA????
  
  # Calculate analytic CIs
  y_Filt_u = y_Filt + qnorm(0.975, sd = sqrt(var1))
  y_Filt_l = y_Filt + qnorm(0.025, sd = sqrt(var1))
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # As above but compare adding in NAs to get predictions. Not strictly necessary
  # to do this separately, but done just as a precaution and for comparison to
  # other extrapolation approaches (e.g. iterating forward Kalman filter 
  # manually)
  x2 = c(x, x[length(x)]:(x[length(x)] + num_points_to_extrapolate))
  y2 = c(y, rep(NA, num_points_to_extrapolate))
  
  mod2 <- dlmModPoly(order = 2, dV = 1, dW = c(0,1))
  
  # Optimisation function for estimable parameters
  build_mod2 <- function(parm) 
  	{
  		mod2$V <- exp(parm[1]) 	#OBSERVATION ERROR
  		mod2$W[2,2] <- exp(parm[2])	#PROCESS ERROR ON INTERCEPT
  		return(mod2)
  	}
  
  rwunifit2 <- dlmMLE(y2, 		
  	parm=rep(0,2), #NUMBER OF ESTIMABLE PARAMETERS 	
  	build=build_mod2, 
  	method="Nelder-Mead")
  
  
  model_parm2 <- rwunifit2$par              
  mod2$V <- exp(model_parm[1])	#OBSERVATION ERROR VARIANCE
  mod2$W[2,2] <- exp(model_parm[2])	#INTERCEPT PROCESS VARIANCE
  
  # Run the Kalman filter
  mod2Filt <- dlmFilter(y2, mod2)
  
  # Extract state estimates
  b0_Filt2 <- dropFirst(mod2Filt$m[,1]) #Position
  b1_Filt2 <- dropFirst(mod2Filt$m[,2])	#Slope
  
  # Extract covariance matrix from singular values
  state_var2 <- dlmSvd2var(mod2Filt$U.C, mod2Filt$D.C)
  
  # Extract variance of individual states (based on parameter estimates)
  b0_Filt_var2 <- unlist(lapply(state_var2,"[",1))[-1] 	
  b1_Filt_var2 <- unlist(lapply(state_var2,"[",4))[-1] 	
  covar2 <- unlist(lapply(state_var2,"[",2))[-1] 
  
  # Calculate value and variance of state in which we are interested
  y_Filt2	= b0_Filt2;
  var2 = vector(length = num_data_points)
  var2 = b0_Filt_var2# + b1_Filt_var2 + 2 * covar2
  
  # Calculate analytic CIs
  y_Filt_u2 = y_Filt2 + qnorm(0.975, sd = sqrt(var2))
  y_Filt_l2 = y_Filt2 + qnorm(0.025, sd = sqrt(var2))
  #-------------------------------------------------------------------------------	 
  
  #-------------------------------------------------------------------------------
  # Smoother
  mod1Smooth <- dlmSmooth(y, mod1)
  
  # Extract state estimates
  b0_Smooth <- dropFirst(mod1Smooth$s[,1]) 	
  b1_Smooth <- dropFirst(mod1Smooth$s[,2])
  
  # Extract covariance matrix from singular values
  state_var <- dlmSvd2var(mod1Smooth$U.S, mod1Smooth$D.S)
  
  # Extract variance of individual states (based on parameter estimates)	
  b0_Smooth_var <- unlist(lapply(state_var,"[",1))[-1] 	
  b1_Smooth_var <- unlist(lapply(state_var,"[",4))[-1] 	
  covar3 <- unlist(lapply(state_var,"[",2))[-1]
  
  # Confidence intervals on states
  b0_Smooth_u <- b0_Smooth + qnorm(0.975, sd=sqrt(b0_Smooth_var))
  b0_Smooth_l <- b0_Smooth + qnorm(0.025, sd=sqrt(b0_Smooth_var))	
  
  # Calculate value and variance of state in which we are interested
  y_Smooth	= b0_Smooth;
  var3 = vector(length = num_data_points)
  var3 = b0_Smooth_var# + b1_Smooth_var + 2 * covar3
  
  # Calculate analytic CIs
  y_Smooth_u = y_Smooth + qnorm(0.975, sd = sqrt(var3))
  y_Smooth_l = y_Smooth + qnorm(0.025, sd = sqrt(var3))
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Note that extrapolated output includes last fit data point for plotting
  # purposes
  GM_LVT = list(xvalsfit = x, meanfit = y_Smooth, u95fit = y_Smooth_u, l95fit = y_Smooth_l, xvals_extrap = newx, meanextrap = y_Filt2[num_data_points: (num_data_points + num_points_to_extrapolate)], u95extrap = y_Filt_u2[num_data_points: (num_data_points + num_points_to_extrapolate)], l95extrap = y_Filt_l2[num_data_points: (num_data_points + num_points_to_extrapolate)], se_fit = sqrt(var3), se_extrap = sqrt(var2[num_data_points:(num_data_points + num_points_to_extrapolate)]))
}