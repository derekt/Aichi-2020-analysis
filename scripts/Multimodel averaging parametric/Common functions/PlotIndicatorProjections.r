# Plot indicator projections

PlotIndicatorProjections <- function(glsPrediction, PredictYears, PredictionSpan, rawdata, target_value,target_value_text, y_limit, low_ci, high_ci, low99_ci, high99_ci, ylabel, maintitle, model_averaged_prediction, normalized = FALSE, xlimits = c(min(PredictYears), max(PredictYears)), text_x_loc = PredictYears[length(PredictYears) - 5], plot_data = TRUE, min_y_limit = 0, netadd = FALSE, cexl = 1, cexp = 1, DataYears = PredictYears[1:(PredictionSpan[1] - 1)], logyaxis = FALSE)
{

  # Create a 'loop' around the x values and y values for CIs
  X.Vec <- c(PredictYears, max(PredictYears), rev(PredictYears), min(PredictYears))
  Y.Vec <- c(low_ci, tail(high_ci, 1), rev(high_ci), low_ci[1])
  Y.Vec2 = c(low99_ci, tail(high99_ci, 1), rev(high99_ci), low99_ci[1])
           
  if (netadd == FALSE)        
  {
  if (logyaxis)
    plot(glsPrediction[1:(PredictionSpan[1] - 1)] ~ PredictYears[1:(PredictionSpan[1] - 1)], type = "l", xlab="Year", ylab=ylabel, lwd = 2 * cexl, xlim = xlimits, ylim = c(min_y_limit, y_limit), main = maintitle, log = "y")
  else
  plot(glsPrediction[1:(PredictionSpan[1] - 1)] ~ PredictYears[1:(PredictionSpan[1] - 1)], type = "l", xlab="Year", ylab=ylabel, lwd = 2 * cexl, xlim = xlimits, ylim = c(min_y_limit, y_limit), main = maintitle)
    if (logyaxis)
      abline(glsPrediction[PredictYears == 2010],0, lty = "12",  col="grey", untf = TRUE)
    else
      abline(glsPrediction[PredictYears == 2010],0, lty = "12",  col="grey")
    if (plot_data)
    points(DataYears, rawdata, pch = 19, cex = 0.7 * cexp)  
    
  lines(PredictYears[(PredictionSpan[1] - 1):length(PredictYears)], glsPrediction[(PredictionSpan[1] - 1):length(PredictYears)], lwd = 2 * cexl,  col="black", lty = 2)
  lines(glsPrediction[1:(PredictionSpan[1] - 1)] ~ PredictYears[1:(PredictionSpan[1] - 1)], lwd = 2 * cexl)
  lines(PredictYears, low_ci, lwd = 2 * cexl, lty =3)
  
  lines(PredictYears, high_ci, lwd = 2 * cexl, lty = 3)

   }                                                 
  else
  {
      lines(PredictYears, glsPrediction, lwd = 2 * cexl)
    if (plot_data)
    points(PredictYears[1:(PredictionSpan[1] - 1)], rawdata, pch = 19, cex = 0.7 * cexp)
    }
  if (model_averaged_prediction)
  {
    #polygon(X.Vec, Y.Vec2, col = "cornflowerblue", border = NA)
    #polygon(X.Vec, Y.Vec, col = "lightskyblue", border = NA)
    #polygon(X.Vec, Y.Vec2, col = "dimgrey", border = NA)
    #polygon(X.Vec, Y.Vec, col = "darkgrey", border = NA)
  }
  else
  {
    if (normalized)
    {
    #polygon(X.Vec, Y.Vec2, col = "slategray4", border = NA)
     # polygon(X.Vec, Y.Vec, col = "slategray3", border = NA)
    }
    else
    {
     # polygon(X.Vec, Y.Vec2, col = "chartreuse4", border = NA)
      #polygon(X.Vec, Y.Vec, col = "chartreuse3", border = NA)
    }
  }  
     if (plot_data)
    points(DataYears, rawdata, pch = 19, cex = 0.7 * cexp) 
  #lines(PredictYears[1:(PredictionSpan[1] - 1)], glsPrediction[1:(PredictionSpan[1] - 1)], type = "l", lwd = 2, lend = 1)
  #lines(PredictYears[c(PredictionSpan[1] - 1, PredictionSpan)], glsPrediction[c(PredictionSpan[1] - 1, PredictionSpan)], type = "l", lwd = 2, col = "black", lty = 5, lend = 1)
  lines(c(0,3000), c(target_value,target_value), lty = 2)
  #lines(c(min(PredictYears) - 1, max(PredictYears) + 1), c(1.5, 1.5), lty = 2)
  text(text_x_loc, target_value_text, "2020 Target")
  
  box()
}