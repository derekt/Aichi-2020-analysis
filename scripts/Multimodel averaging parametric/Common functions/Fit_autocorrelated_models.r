# A general function to fit three autocorrelated models of various AR forms
FitAutocorrelatedModel <- function(functional_form, param_names, num_parameters, input_data, nocor_startlist, ar1_startlist, ar2_startlist, diagnostic_on = FALSE, indicator = "", functional_formfullname = "", nocor_nlsTol = 0.01, ar1_nlsTol = 0.01, ar2_nlsTol = 0.01, nofit = FALSE, fitAR1 = TRUE, fitAR2 = TRUE, num_data_points)
{
   if(nofit)
        eval(parse(text = paste("list(model =", NA, ", prediction =", NA, CIs =", NA, modelname = functional_formfullname)", sep="")))
   else
   {
     if (!fitAR1)
     {
      #print("No correlation model")    
      eval(parse(text = paste("m1 <- try(gnls(data ~ ", functional_form, param_names, ", data = ", input_data, ", start = ", nocor_startlist,", control = gnlsControl(nlsTol = ", nocor_nlsTol,")), silent = TRUE)", sep = "")))
           
      if (length(regexpr('Error', m1)) == sum(regexpr('Error', m1) == -1))
      {
         
            return_model = "m1"
             
            if (diagnostic_on)
            {
              eval(parse(text = paste("DiagnosticPlots(m1, Year, data, \"", indicator,": ", functional_formfullname, " no temporal autocorrelation\")", sep="")))
            }
            #print(functional_form)
            #if (return_model == "m1")
              #print("No correlation structure")
      
            # Calculate the predicted values
            eval(parse(text = paste("glsPrediction = predict(", return_model, ", newdata = PredictYearsDataFrame)", sep="")))
    
            # Calculate the Wald CIs. Method from Gallant (1983)
            eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters, ",", num_data_points, ")", sep="")))
             
            # Return a list of outputs and predictions from the best model. 
            eval(parse(text = paste("list(model =", return_model, ", prediction = glsPrediction, CIs = CIList, modelname = functional_formfullname)", sep="")))    
      }
      else
      {
        # Return no fits 
        eval(parse(text = paste("list(model =", NA, ", prediction =", NA, CIs =", NA, modelname = functional_formfullname)", sep=""))) 
      }
 
    }
    else if (!fitAR2)
    {
      #print("No correlation model")    
       eval(parse(text = paste("m1 <- try(gnls(data ~ ", functional_form, param_names, ", data = ", input_data, ", start = ", nocor_startlist,", control = gnlsControl(nlsTol = ", nocor_nlsTol,")), silent = TRUE)", sep = "")))
       
       #print("AR1 model")
       eval(parse(text = paste("m2 <- try(gnls(data ~ ", functional_form, param_names, ", data = ", input_data, ", start = ", ar1_startlist,", correlation = corARMA(form = ~ Year, p = 1), control = gnlsControl(nlsTol = ", ar1_nlsTol,")), silent = TRUE)", sep = "")))
  
       if ((regexpr('Error', m1) > 0) || (regexpr('Error', m2) > 0))
       {
          
            # Return no fits 
            eval(parse(text = paste("list(model =", NA, ", prediction =", NA, CIs =", NA, modelname = functional_formfullname)", sep=""))) 
       }
       else
       {
           
            print(anova(m1,m2))
            return_model = paste("m",which.min(c(AIC(m1), AIC(m2))), sep="")
  
  
            if (diagnostic_on)
            {
              eval(parse(text = paste("DiagnosticPlots(m1, Year, data, \"", indicator,": ", functional_formfullname, " no temporal autocorrelation\")", sep="")))
              
              eval(parse(text = paste("DiagnosticPlots(m2, Year, data, \"", indicator,": ", functional_formfullname, " AR1\")", sep=""))) 
            }
            print(functional_form)
            if (return_model == "m1")
              print("Best model: no correlation structure")
            else if (return_model == "m2")
              print("Best model: AR1")
      
            # Calculate the predicted values
            eval(parse(text = paste("glsPrediction = predict(", return_model, ", newdata = PredictYearsDataFrame)", sep="")))
    
            # Calculate the Wald CIs. Method from Gallant (1987)
            if (return_model == "m1")
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters, ",", num_data_points, ")", sep="")))
            else
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters + 1, ",", num_data_points, ")", sep="")))
             
            # Return a list of outputs and predictions from the best model. 
            eval(parse(text = paste("list(model =", return_model, ", prediction = glsPrediction, CIs = CIList, modelname = functional_formfullname)", sep="")))    
       }
    }
     else
     {
       #print("No correlation model")    
       eval(parse(text = paste("m1 <- try(gnls(data ~ ", functional_form, param_names, ", data = ", input_data, ", start = ", nocor_startlist,", control = gnlsControl(nlsTol = ", nocor_nlsTol,")), silent = TRUE)", sep = "")))
       
       #print("AR1 model")
       eval(parse(text = paste("m2 <- try(gnls(data ~ ", functional_form, param_names, ", data = ", input_data, ", start = ", ar1_startlist,", correlation = corARMA(form = ~ Year, p = 1), control = gnlsControl(nlsTol = ", ar1_nlsTol,")), silent = TRUE)", sep = "")))
       #print("AR2 model")
           eval(parse(text = paste("m3 <- try(gnls(data ~ ", functional_form, param_names, ", data = ", input_data, ", start = ", ar2_startlist,", correlation = corARMA(form = ~ Year, p = 2), control = gnlsControl(nlsTol = ", ar2_nlsTol,")), silent = TRUE)", sep = "")))
  
       if ((regexpr('Error', m1) > 0) || (regexpr('Error', m2) > 0) || (regexpr('Error', m3) > 0))
       {
          if ((length(regexpr('Error', m1)) == sum(regexpr('Error', m1) == -1)) && (length(regexpr('Error', m2)) == sum(regexpr('Error', m2) == -1)))
          {
          
            print(anova(m1,m2))
            return_model = paste("m",which.min(c(AIC(m1), AIC(m2))), sep="")
  
  
            if (diagnostic_on)
            {
              eval(parse(text = paste("DiagnosticPlots(m1, Year, data, \"", indicator,": ", functional_formfullname, " no temporal autocorrelation\")", sep=""))) 
             
              eval(parse(text = paste("DiagnosticPlots(m2, Year, data, \"", indicator,": ", functional_formfullname, " AR1\")", sep=""))) 
            }
            print(functional_form)
            if (return_model == "m1")
              print("Best model: no correlation structure")
            else if (return_model == "m2")
              print("Best model: AR1")
      
            # Calculate the predicted values
            eval(parse(text = paste("glsPrediction = predict(", return_model, ", newdata = PredictYearsDataFrame)", sep="")))
    
            # Calculate the Wald CIs. Method from Gallant (1987)
            if (return_model == "m1")
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters, ",", num_data_points, ")", sep="")))
            else
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters + 1, ",", num_data_points, ")", sep="")))
             
            # Return a list of outputs and predictions from the best model. 
            eval(parse(text = paste("list(model =", return_model, ", prediction = glsPrediction, CIs = CIList, modelname = functional_formfullname)", sep="")))    
          }
          else
          {
            # Return no fits 
            eval(parse(text = paste("list(model =", NA, ", prediction =", NA, CIs =", NA, modelname = functional_formfullname)", sep=""))) 
          }
      }
      else
      {
            print(anova(m1, m2, m3))
          if (diagnostic_on)
          {
            eval(parse(text = paste("DiagnosticPlots(m1, Year, data, \"", indicator,": ", functional_formfullname, " no temporal autocorrelation\")", sep=""))) 

            eval(parse(text = paste("DiagnosticPlots(m2, Year, data, \"", indicator,": ", functional_formfullname, " AR1\")", sep=""))) 
            
            eval(parse(text = paste("DiagnosticPlots(m3, Year, data, \"", indicator,": ", functional_formfullname, " AR2\")", sep=""))) 
          }
      
      
      return_model = paste("m",which.min(c(AIC(m1), AIC(m2), AIC(m3))), sep="")
      
      print(functional_form)
      if (return_model == "m1")
        print("Best model: no correlation structure")
      else if (return_model == "m2")
        print("Best model: AR1")
      else
        print("Best model: AR2")
      
      # Calculate the predicted values
      eval(parse(text = paste("glsPrediction = predict(", return_model, ", newdata = PredictYearsDataFrame)", sep="")))
    
            # Calculate the Wald CIs. Method from Gallant (1987)
            if (return_model == "m1")
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters, ",", num_data_points, ")", sep="")))
            else if (return_model == "m2")
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters + 1, ",", num_data_points, ")", sep="")))
            else
              eval(parse(text = paste("CIList <- WaldLimits", functional_form, "(coef(" , return_model, "), PredictYears, vcov(", return_model, "),", num_parameters + 2, ",", num_data_points, ")", sep="")))
            
      if(nofit)
        eval(parse(text = paste("list(model =", NA, ", prediction =", NA, CIs =", NA, modelname = functional_formfullname)", sep=""))) 
      else           
        # Return a list of outputs and predictions from the best model. 
        eval(parse(text = paste("list(model =", return_model, ", prediction = glsPrediction, CIs = CIList, modelname = functional_formfullname)", sep="")))       
    }
    }
  }
}
