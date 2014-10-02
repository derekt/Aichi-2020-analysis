# Short function to expand / normalise data
data_standardise <- function(input_data)
{
  names(input_data) = c("Year", "Data")
  minyear = min(input_data$Year)
  maxyear = max(input_data$Year)
  if (dim(input_data)[1] < (maxyear - minyear + 1))
  {
    newyears = minyear:maxyear
    newdata = rep(NA, (maxyear - minyear + 1))
    for (ii in 1:length(newyears))
    {
      for (jj in 1:dim(input_data)[1])
      {
        if (input_data$Year[jj] == newyears[ii])
        {
          newdata[ii] = input_data$Data[jj]
          break;
        }  
      }
    }
    newdf = data.frame(Year = newyears, Data = newdata)
  }
  else
    input_data  
}