writepdfsandresults <- function(output_name, minimum_y_limit, logyx = FALSE)
{
  
  png(filename=paste("../../images/", output_name, ".png", sep = ""), res = 1200, width = 4, height = 4, units = "in", pointsize = 4)
  par(mar = c(5,5,2,2), xaxs = "i", yaxs = "i", cex = 1.2, cex.axis = 1.6, cex.lab = 1.6, cex.main = 1.6)
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, maintitle, TRUE, min_y_limit = minimum_y_limit, cexl = 1, cexp = 2, DataYears = eafs[,1], logyaxis = logyx)
  dev.off()
  
  pdf(file=paste("../../images/", output_name, ".pdf", sep = ""))
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, maintitle, TRUE, min_y_limit = minimum_y_limit, DataYears = eafs[,1], logyaxis = logyx)
 dev.off()
  
  
  png(filename=paste("../../images/", output_name, "_multimodel.png", sep = ""), res = 1200, width = 4, height = 4, units = "in", pointsize = 4)
  par(mar = c(5,5,2,2), xaxs = "i", yaxs = "i", cex = 2, cex.axis = 1.75, cex.lab = 1.75, cex.main = 1.75)
  
  # Plot model averaged plus top 5 models
  newpar <- par(mfrow=c(3,2),  oma=c(0,1,1,1)) 
  
  # Plot projections
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text,y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, "Model averaged prediction", TRUE, min_y_limit = minimum_y_limit, cexl = 0.4, cexp = 1.5, DataYears = eafs[,1], logyaxis = logyx)
  
    if (num_models_to_be_averaged > 5)
      num_models_to_be_averaged = 5
      
  for (ii in 1:num_models_to_be_averaged)
  {
    mtoplot = m[[sorted_models_original_list[ii]]]
    PlotIndicatorProjections(back_transform(as.vector(mtoplot$prediction)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(mtoplot$CIs$LowCI), back_transform(mtoplot$CIs$HighCI), back_transform(mtoplot$CIs$Low99CI), back_transform(mtoplot$CIs$High99CI),ylabel, paste(Results[sorted_models_original_list[ii],1], ": weight = ", format(norm_akaike_weights[ii], digits = 2), sep = "") , FALSE, min_y_limit = minimum_y_limit, DataYears = eafs[,1], logyaxis = logyx)
  }
  dev.off()
  
  pdf(file=paste("../../images/", output_name, "_multimodel.pdf", sep = ""))
  
  # Plot model averaged plus top 5 models
  newpar <- par(mfrow=c(3,2),  oma=c(0,1,1,1)) 
  
  # Plot projections
  PlotIndicatorProjections(back_transform(as.vector(model_average)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text,y_limit, back_transform(model_avg_fitted_ci[1,]), back_transform(model_avg_fitted_ci[2,]),  back_transform(model_avg_fitted_ci99[1,]), back_transform(model_avg_fitted_ci99[2,]),ylabel, "Model averaged prediction", TRUE, min_y_limit = minimum_y_limit, DataYears = eafs[,1], logyaxis = logyx)
  
    if (num_models_to_be_averaged > 5)
      num_models_to_be_averaged = 5
      
  for (ii in 1:num_models_to_be_averaged)
  {
    mtoplot = m[[sorted_models_original_list[ii]]]
    PlotIndicatorProjections(back_transform(as.vector(mtoplot$prediction)), PredictYears, PredictionSpan, back_transform(data), target_value, target_value_text, y_limit, back_transform(mtoplot$CIs$LowCI), back_transform(mtoplot$CIs$HighCI), back_transform(mtoplot$CIs$Low99CI), back_transform(mtoplot$CIs$High99CI),ylabel, paste(Results[sorted_models_original_list[ii],1], ": weight = ", format(norm_akaike_weights[ii], digits = 2), sep = "") , FALSE, min_y_limit = minimum_y_limit, DataYears = eafs[,1], logyaxis = logyx)
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
  #temp_matrix[,hh] = model_avg_fitted_se[hh] * rt(n_bootstrap,num_data_points - nparams) + model_average[hh]
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
}