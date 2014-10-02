# z-transform
  # Export the model-averaged output as a z-score
z_standardise = function(input_matrix)
{
  idmean = mean(input_matrix[3,])
  idsd = sd(input_matrix[3,])
  input_matrix[3,] = (input_matrix[3,] - idmean) / idsd   
  input_matrix[4,] = (input_matrix[4,] - idmean) / idsd
  input_matrix[5,] = (input_matrix[5,] - idmean) / idsd
  input_matrix[6,] = (input_matrix[6,]) / idsd
  input_matrix
}
