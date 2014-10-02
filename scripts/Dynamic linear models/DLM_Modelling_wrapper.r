#-------------------------------------------------------------------------------
# Wrapper script for preparing data, extracting data, plotting and saving
# results for a single DLM run
#
# Derek Tittensor 23 March 2014
#-------------------------------------------------------------------------------

DLM_modelling_wrapper <- function(input_data, output_name, output_title, ylimits, yaxislabel, do_asin_transform, do_log_transform, logaxes = FALSE)
{
      
percent = FALSE
  # Transform bounded data
  if (do_asin_transform)
  {
    if (max(input_data[,2], na.rm=T) > 1)
    {
      input_data[,2] = input_data[,2] / 100
      percent = TRUE
    }
    input_data[,2] = asin(sqrt(input_data[,2]))
  }
  # Transform non-bounded data
  else
  {
    if (do_log_transform)
    {
      input_data[,2] = log(input_data[,2])
    }
    
    # Standardise to z-scores
    idmean = mean(input_data[,2], na.rm=T)
    idsd = sd(input_data[,2], na.rm = T)
    input_data[,2] = ((input_data[,2] - idmean) / idsd)
  }
  
  # Do the modelling
  model_output = DLM_GM_LVT(input_data, output_name, ylimits)

 
   # Calculate bootstrapped values for use in the rates plot
    n_bootstrap = 10000

    temp_matrix = matrix(nrow = n_bootstrap, ncol = length(c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])))
    # Loop through years
    for (hh in 1:length(c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])))
    {
      results_vector = vector(length = n_bootstrap)
      # This calculate a sorted set of estimates for those points
      temp_matrix[,hh] = c(model_output$se_fit, model_output$se_extrap[2:length(model_output$se_extrap)])[hh] * rnorm(n_bootstrap) + c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])[hh]
    }
 
   
  # Back transform
  if (do_asin_transform)
  {
    input_data[,2] = sin(input_data[,2])^2
    model_output$meanfit = sin(model_output$meanfit)^2
    model_output$u95fit = sin(model_output$u95fit)^2
    model_output$l95fit = sin(model_output$l95fit)^2
    model_output$meanextrap = sin(model_output$meanextrap)^2
    model_output$u95extrap = sin(model_output$u95extrap)^2
    model_output$l95extrap = sin(model_output$l95extrap)^2
    model_output$se_fit = sin(model_output$se_fit)^2
    model_output$se_extrap = sin(model_output$se_extrap)^2  

    for (hh in 1:length(c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])))
    {
      temp_matrix[,hh] = sin(temp_matrix[,hh])^2
    }
      
    
    if (percent == TRUE)
    {
      input_data[,2] = input_data[,2] * 100
      model_output$meanfit = model_output$meanfit * 100
      model_output$u95fit = model_output$u95fit * 100
      model_output$l95fit = model_output$l95fit * 100
      model_output$meanextrap = model_output$meanextrap * 100
      model_output$u95extrap = model_output$u95extrap * 100
      model_output$l95extrap  = model_output$l95extrap * 100
      model_output$se_fit = model_output$se_fit * 100
      model_output$se_extrap = model_output$se_extrap * 100
    
      for (hh in 1:length(c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])))
    {
      temp_matrix[,hh] = temp_matrix[,hh] * 100
    }
    
    }
  }
  else
  {
    input_data[,2] = input_data[,2] * idsd + idmean 
    model_output$meanfit = model_output$meanfit * idsd + idmean
    model_output$u95fit = model_output$u95fit * idsd + idmean
    model_output$l95fit = model_output$l95fit * idsd + idmean
    model_output$meanextrap = model_output$meanextrap * idsd + idmean
    model_output$u95extrap = model_output$u95extrap * idsd + idmean
    model_output$l95extrap = model_output$l95extrap * idsd + idmean
    model_output$se_fit = model_output$se_fit * idsd + idmean
    model_output$se_extrap = model_output$se_extrap * idsd + idmean
    for (hh in 1:length(c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])))
    {
      temp_matrix[,hh] = temp_matrix[,hh] * idsd + idmean
    }
            
    if (do_log_transform)
    {
      input_data[,2] = exp(input_data[,2])       
      model_output$meanfit = exp(model_output$meanfit)
      model_output$u95fit = exp(model_output$u95fit)
      model_output$l95fit = exp(model_output$l95fit)
      model_output$meanextrap = exp(model_output$meanextrap)
      model_output$u95extrap = exp(model_output$u95extrap)
      model_output$l95extrap = exp(model_output$l95extrap)
      model_output$se_fit = exp(model_output$se_fit)
      model_output$se_extrap = exp(model_output$se_extrap)
      for (hh in 1:length(c(model_output$meanfit, model_output$meanextrap[2:length(model_output$meanextrap)])))
    {
      temp_matrix[,hh] = exp(temp_matrix[,hh])
    }
    
    }
  }
  
  attach(model_output, warn.conflicts = FALSE)
    
    #-------------------------------------------------------------------------------
  # Plot results.
  
  cexp = 2
  png(filename=paste("../../images/", output_name, ".png", sep = ""), res = 1200, width = 4, height = 4, units = "in", pointsize = 4)
  par(mar = c(5,5,2,2), xaxs = "i", yaxs = "i", cex = 1.2, cex.axis = 1.6, cex.lab = 1.6, cex.main = 1.6)
     if (logaxes)
  		matplot(cbind(xvalsfit, xvalsfit, xvalsfit),cbind(meanfit, u95fit, l95fit),ty="l",col="black",lty=c(1,3,3), xlim = c(min(xvalsfit), 2020),  ylim = ylimits, main=paste(output_title),xlab = "Year", ylab=yaxislabel, lwd = 2, log = "y")
  		else
   		matplot(cbind(xvalsfit, xvalsfit, xvalsfit),cbind(meanfit, u95fit, l95fit),ty="l",col="black",lty=c(1,3,3), xlim = c(min(xvalsfit), 2020),  ylim = ylimits, main=paste(output_title),xlab = "Year", ylab=yaxislabel, lwd = 2) 		

  matplot(cbind(xvals_extrap, xvals_extrap, xvals_extrap), cbind(meanextrap, u95extrap, l95extrap), ty = "l", col = "black", lty = c(2, 3, 3), lwd = 2, add = T)
    points(xvalsfit,input_data[,2], pch = 19, cex = 0.7 * cexp)
    if (max(xvalsfit) < 2010)
    {
    if (logaxes)
    abline(meanextrap[2010 - max(xvalsfit) + 1],0, col="grey", lty = "12", untf=TRUE)
    else
      abline(meanextrap[2010 - max(xvalsfit) + 1],0, col="grey", lty = "12")
    }  
      
    else
    {
    if (logaxes)
        abline(meanfit[which(xvalsfit == 2010)],0,  col = "grey", lty = "12", untf=TRUE)
    else
      abline(meanfit[which(xvalsfit == 2010)],0,  col = "grey", lty = "12")
     }
     
     	matplot(cbind(xvalsfit, xvalsfit, xvalsfit),cbind(meanfit, u95fit, l95fit),ty="l",col="black",lty=c(1,3,3), lwd = 2, add = T)

  matplot(cbind(xvals_extrap, xvals_extrap, xvals_extrap), cbind(meanextrap, u95extrap, l95extrap), ty = "l", col = "black", lty = c(2, 3, 3), lwd = 2, add = T)
  dev.off()
  pdf(file=paste("../../images/", output_name, ".pdf", sep = ""))
    		if (logaxes)
  		matplot(cbind(xvalsfit, xvalsfit, xvalsfit),cbind(meanfit, u95fit, l95fit),ty="l",col="black",lty=c(1,3,3), xlim = c(min(xvalsfit), 2020), ylim = ylimits, main=paste(output_title),xlab = "Year", ylab=yaxislabel, lwd = 2, log = "y")
    else
  		  	matplot(cbind(xvalsfit, xvalsfit, xvalsfit),cbind(meanfit, u95fit, l95fit),ty="l",col="black",lty=c(1,3,3), xlim = c(min(xvalsfit), 2020), ylim = ylimits, main=paste(output_title),xlab = "Year", ylab=yaxislabel, lwd = 2)

  matplot(cbind(xvals_extrap, xvals_extrap, xvals_extrap), cbind(meanextrap,u95extrap, l95extrap), ty = "l", col = "black", lty = c(2, 3, 3), lwd = 2, add = T)
    points(xvalsfit,input_data[,2], pch = 19, cex = 0.75)
        if (max(xvalsfit) < 2010)
      {
        if (logaxes)
          abline(meanextrap[2010 - max(xvalsfit) + 1],0,lty = 3, untf=TRUE)
        else
          abline(meanextrap[2010 - max(xvalsfit) + 1],0,lty = 3)
      }
    else
    {
      if (logaxes)
        abline(meanfit[which(xvalsfit == 2010)],0, lty = 3, untf = TRUE)
      else
       
        abline(meanfit[which(xvalsfit == 2010)],0, lty = 3)
    }
  dev.off()
  #-------------------------------------------------------------------------------


datamatrix = matrix(nrow = 7, ncol = length(c(meanfit, meanextrap[2:length(meanextrap)])))
datamatrix[1,] = min(xvalsfit):2020
Year2010 = which(datamatrix[1,] == 2010)
  datamatrix[2,] = c(rep(NA, length(xvalsfit)), (length(xvalsfit) + 1):(length(xvalsfit) + length(xvals_extrap) - 1))
  datamatrix[3,] = c(meanfit, meanextrap[2:length(meanextrap)])
    datamatrix[4,] = c(l95fit, l95extrap[2:length(meanextrap)])
  datamatrix[5,] = c(u95fit, u95extrap[2:length(meanextrap)])
  datamatrix[6,] = c(se_fit, se_extrap[2:length(se_extrap)])
  
  for (ii in 1:length(input_data[,1]))
{
  jj = 1
  while (datamatrix[1,jj] != input_data[ii,1])
    jj = jj + 1
  datamatrix[7,jj] = input_data[ii,2] 
}


write.csv(datamatrix, file = paste("../../results/untransformed/untransformed_", output_name, ".csv", sep=""))  


  Val2010 = datamatrix[3,Year2010]
  

for (hh in 1:length(c(meanfit, meanextrap[2:length(meanextrap)])))
{
  temp_matrix[,hh] = (temp_matrix[,hh] - Val2010) / Val2010
}


datamatrix[3,] = (datamatrix[3,] - Val2010) / Val2010
datamatrix[4,] = (datamatrix[4,] - Val2010) / Val2010
datamatrix[5,] = (datamatrix[5,] - Val2010) / Val2010
datamatrix = datamatrix[1:5,]

datamatrix = rbind(datamatrix, temp_matrix)

write.csv(datamatrix, file = paste("../../results/transformed/transformed_", output_name, ".csv", sep=""))
}