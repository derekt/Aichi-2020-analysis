# Only reads data from year 2000
read_data <- function(input_data, invert = FALSE)
{
   
  eval(parse(text = paste("bb = as.matrix(read.csv(\"../../results/transformed/transformed_", input_data, "\", row.names = NULL))", sep="")))
  bb = bb[,2:dim(bb)[2]]
  Loc2010 = which(bb[1,] == 2010)
  Val2010 = bb[3, Loc2010]

  if (min(bb[1,]) < 2000)
  {
    Loc2000 = which(bb[1,] == 2000)
    bb = bb[,Loc2000:dim(bb)[2]]
  }
  if (invert)
  {
    bb[3:dim(bb)[1],] = -bb[3:dim(bb)[1],]
    temp = bb[4,]
    bb[4,] = bb[5,]
    bb[5,] = temp
  }
  bb
}

# Reads all data
read_all_data <- function(input_data, invert = FALSE)
{
   
  eval(parse(text = paste("bb = as.matrix(read.csv(\"../../results/transformed/transformed_", input_data, "\", row.names = NULL))", sep="")))
  bb = bb[,2:dim(bb)[2]]
  Loc2010 = which(bb[1,] == 2010)
  Val2010 = bb[3, Loc2010]

  if (invert)
  {
    bb[3:dim(bb)[1],] = -bb[3:dim(bb)[1],]
    temp = bb[4,]
    bb[4,] = bb[5,]
    bb[5,] = temp
  }
  bb
}

pad_missing <- function(input_data)
{
  # Pad with NAs
  gg = 2000:2020
  missing_years = setdiff(gg, intersect(input_data[1,], gg))
  new_matrix = matrix(data = NA, nrow = dim(input_data)[1], ncol = 21)
  for (ii in 2000:2020)
  {
    if (ii %in% missing_years)
    {
      new_matrix[1,ii - 1999] = ii;
    }
    else
    {
      new_matrix[,ii - 1999] = input_data[,which(input_data[1,] == ii)]
    }
  }
  
  new_matrix
}


pad_missing_all_data <- function(input_data)
{
  # Pad with NAs
  if(input_data[1,1] < 2000)
  {
    years = input_data[1,1]:2020
    missing_years = setdiff(years, intersect(input_data[1,], years))
    new_matrix = matrix(data = NA, nrow = dim(input_data)[1], ncol = length(years))
    for (ii in 1:length(years))
    {
      if (years[ii] %in% missing_years)
      {
        new_matrix[1,ii] = years[ii];
      }
      else
      {
        new_matrix[,ii] = input_data[,which(input_data[1,] == years[ii])]
      }
    }
  }
  else
    new_matrix = pad_missing(input_data)
  
  new_matrix
}

create_polygon_loop <- function(input_data)
{
  # Create a 'loop' around the x values and y values for CIs
  eval(parse(text = paste("X.Vec <- c(", input_data, "[1,], max(", input_data, "[1,]), rev(", input_data, "[1,]), min(", input_data, "[1,]))", sep = "")))
  eval(parse(text = paste("Y.Vec <- c(", input_data,"[4,], tail(", input_data, "[5,], 1), rev(", input_data,"[5,]), ", input_data, "[4,][1])", sep = "")))
  polygon(X.Vec, Y.Vec, col = alpha("dimgrey", 0.25), border = NA)
}

plot_line <- function(input_data, input_colour, significant)
{
  if (significant)
    eval(parse(text = paste("lines(as.numeric(", input_data, "[1,]), as.numeric(", input_data, "[3,]), col = alpha(\"", input_colour, "\", 0.9), lwd = 2)", sep="")))
  else
  eval(parse(text = paste("lines(as.numeric(", input_data, "[1,]), as.numeric(", input_data, "[3,]), col = alpha(\"", input_colour, "\", 0.9), lwd = 1, lty = 3)", sep="")))
}

# Create plots of inverse-weighted mean
calculate_meta_analytic <- function(...)
{
  as.list( sys.call() ) 
  print(nargs())
  print(as.list(sys.call())[2])
  print(as.list(sys.call())[3])
  eval(parse(text = paste("print(", as.list(sys.call())[2],"[3,])", sep="")))
  eval(parse(text = paste("allyears = ", as.list(sys.call())[2], "[1,]", sep="")))
  eval(parse(text = paste("combined_normalised_variances = ", as.list(sys.call())[2], "[6,]^2", sep="")))
  eval(parse(text = paste("combined_normalised_projections = ", as.list(sys.call())[2], "[3,]", sep="")))
  
  for (ii in 2:nargs())
  {
  
    eval(parse(text = paste("allyears = rbind(allyears, ", as.list(sys.call())[ii+1], "[1,]", ")", sep="")))
    eval(parse(text = paste("combined_normalised_variances = rbind(combined_normalised_variances, ", as.list(sys.call())[ii+1], "[6,]^2", ")", sep="")))
    eval(parse(text = paste("combined_normalised_projections = rbind(combined_normalised_projections, ", as.list(sys.call())[ii+1], "[3,]", ")", sep="")))
  }

  weighted_projection = vector(length = length(allyears[1,]))
  weighted_variance = vector(length = length(allyears[1,]))
  weights_matrix =matrix(nrow = nargs(), ncol = length(allyears[1,]))
  
  # Do this in stages. Calculate the weights first
  for (ii in 1:length(weighted_projection))
  {
    for (jj in 1:dim(allyears)[1])
    {
      if (is.na(allyears[jj,ii]))
        weights_matrix[jj,ii] = 0
      else
        weights_matrix[jj,ii] = 1 / combined_normalised_variances[jj,ii]
    }
  }

  # Calculate weighted means and variances
  for (ii in 1:length(weighted_projection))
  {
    weighted_projection[ii] = sum(weights_matrix[,ii] * combined_normalised_projections[,ii], na.rm = T) / sum(weights_matrix[,ii], na.rm = T)
    weighted_variance[ii] = 1 / sum(weights_matrix[,ii], na.rm = T)
  }
  
  list(combined_normalised_variances = combined_normalised_variances, weighted_projection = weighted_projection, weighted_variance = weighted_variance, allyears = allyears)
}

plotbox1 <- function(addin = F)
{
  # State
  plot(1, axes = FALSE, type = "n", ylim = c(-1,1), xlim = c(2000, 2020) )
  #par(cex = 1)
  axis(side = 1, at = c(2000, 2005, 2010, 2015, 2020))
  #par(cex = 0.4)
  amaths1 <- expression(phantom(.) %<-% phantom(.))
  amaths2 <- expression(phantom(.) %->% phantom(.))
 if (!addin)
 {
 #axis(2, col = "grey40", col.axis = "grey20", at = c(-1, 0, 1), lab = c("100%", "No change", "+100%"), cex = 0.5)
 }
 else
 {
 axis(4, col = "grey40", col.axis = "grey20", at = c(-1, 0, 1), lab = c("-100%", "", "+100%"), las = 2, cex = 0.5)
 }
 abline(0,0,lty = 3)
  box()
}

add_graph_details <- function(input_string = "")
{
  abline(a = 0, b = 0, lty = 3, lwd = 2)
  mtext(input_string, side = 3, line = 0.5, cex = 1.1)

  # Add in a vertical line
  #abline(v = 2010, lty = 2, lwd = 1)
}

