#-------------------------------------------------------------------------------
# Wrapper script to run prep data for an Aichi signal-to-noise calculation
#
# Derek Tittensor 23 March 2014
#-------------------------------------------------------------------------------
DLM_Signal_To_Noise_Wrapper <- function(input_data, do_asin_transform, do_log_transform)
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
    idmean = mean(input_data[,2], na.rm=TRUE)
    idsd = sd(input_data[,2], na.rm=TRUE)
    input_data[,2] = ((input_data[,2] - idmean) / idsd)
  }
  
  # Calculate signal to noise ratio
  DLM_Signal_To_Noise(input_data)
}