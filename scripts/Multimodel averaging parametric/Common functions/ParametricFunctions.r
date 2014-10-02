# Parametric functions for each of the models used in the
# Aichi model averaging approach.
#
# Thanks to Ben Bolker for elements of the code / approach here

ConvertYearsToStartAtZero <- function(Year)
{
  Year - min(Year) + 1
}

#----------------------------------------------------
# MODEL 1: FIRST ORDER POLYNOMIAL
poly1fun <- function(a,b,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a + b * Year2
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
    
  r
}
#----------------------------------------------------
                        
           
#----------------------------------------------------
# MODEL 2: 4.2.30 FROM RATKOWSKY
ratk423 <- function(a,b,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * (1 + Year2)^b
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 3: 4.3.33 FROM RATKOWSKY
ratk4333 <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  #r <- a + b * Year2 + d * Year2^2 + e * Year2^3
  #r <- a + b * Year2^d
  r <- a * exp(b / (Year2 + d))
  if (debug) cat(mean(Year2), a, b, d,  mean(r), "\n")
  r   
}
#-----------------------------------------------------

#----------------------------------------------------
# MODEL 4: HYPERBOLIC
hyperfun <- function(a,b,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a / (b + Year2)
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r     
}
#-----------------------------------------------------

#-----------------------------------------------------
# MODEL 5: MICHAELIS-MENTEN
mmfun <- function(a,b,Year, debug = FALSE)
{
  Year2 = Year - min(Year) + 1
  r <- (a * Year2) / (b + Year2)
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r   
}
#-----------------------------------------------------

#----------------------------------------------------
# MODEL 6: HOLLING TYPE III
hiiifun <- function(a,b,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- (a * Year2^2)/ (b + Year2^2)
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r     
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 7: HOLLING TYPE IV
hivfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- (a * Year2^2)/ (b + d * Year2 + Year2^2)
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r     
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 8: RATIONAL
rationalfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- (a + (b * Year2)) / (1 + (d * Year2))
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 9: EXPONENTIAL
expfun <- function(a,b, Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * exp(b * Year2)
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 10: MONOMOLECULAR
monofun <- function(a,b, Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * (1 - exp(b * Year2))
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 11: RICKER
rickerfun <- function(a,b, Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * Year2 * exp(b * Year2)
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 12: GOMPERTZ
gompertzfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- exp(a * exp(b * (Year2 - d)))
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 13: CHAPMAN-RICHARDS
chaprichfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * (1 - exp(b * Year2))^d
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 14: 4-PARAMETER GOMPERTZ
weibullfun <- function(a,b,d,e,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  #r <- a * (1 - exp(-(b * (Year2 - d))^e))
  r <- a + b * exp(-exp(d - e * Year2))
  if (debug) cat(mean(Year2), a, b, d, e, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 15: POWER-LAW
powerfun <- function(a,b, Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * Year2^b
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r   
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 16: ASYMPTOTIC
asymptoticfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a - (b * d^Year2)
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 17: SHEPHERD
shepherdfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- (a * Year2) / (b + Year2^d)
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r
}
#----------------------------------------------------

#----------------------------------------------------
# MODEL 18: HASSELL
hassellfun <- function(a,b,d,Year, debug = FALSE)
{
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- (a * Year2) / (b + Year2)^d
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r
}
#----------------------------------------------------























# Unused models
 #---------------------------------------------------
 # MODEL 19: HYPERBOLA
 hyperbolafun <- function(a,b, c,Year, debug = FALSE)
 {
  Year2 = ConvertYearsToStartAtZero(Year)
  print(Year2)
  r <- (a + b * Year2)^(-c)
  print(r)
  r
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
 }
 
 # MODEL 20:
 negexp2fun <- function(a,b, Year, debug= FALSE)
 {
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * (1 - exp(-b * (Year2)))
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r
   }
 
  # MODEL 21:
 negtanhfun <- function(a,b, Year, debug= FALSE)
 {
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a * tanh(b * (Year2) / a)
  if (debug) cat(mean(Year2), a, b, d, mean(r), "\n")
  r
   }
 
   # MODEL 22:
 negmmfun <- function(a,b,d, Year, debug= FALSE)
 {
  Year2 = ConvertYearsToStartAtZero(Year)
  r <- a - (a - b) * Year2 / (d + Year2)
    if (debug) cat(mean(Year2), a, b, d,e, mean(r), "\n")

  r
 }
 
 # MODEL 23:
 
 




# (actual) Negative exponential
negexpfun <- function(a,b,Year, debug = FALSE)
{
  Year2 = Year - min(Year) + 1
  r <- a * (1 - exp(-b * Year2))
  if (debug) cat(mean(Year2), a, b, mean(r), "\n")
  r
}