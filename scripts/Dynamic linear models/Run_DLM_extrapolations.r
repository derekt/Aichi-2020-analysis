#-------------------------------------------------------------------------------
# Wrapper script to run the Aichi DLM extrapolations
#
# Derek Tittensor 23 March 2014
#-------------------------------------------------------------------------------

remove(list = ls())

# Load libraries
library(dlm)

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
  
# Source functions
source("DLM_Aichi_GlobalMean_LocallyVaryingTrend_Function.r")
source("DLM_Modelling_wrapper.r")                                                                           
source("../common functions/data_standardise.r")

######################
# T1: Funds for environmental education global aid
temp_data =read.csv("../../data/Target 1/Environmental_education_global_aid_funds.csv") 
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T1_Funds_environmental_education", "Funds for environmental education and training", c(1.0, 1000), "Constant USD (millions)", FALSE, TRUE, TRUE)

######################
# T2: Global funds EIA
temp_data =read.csv("../../data/Target 2/Global_funds_EIA.csv")
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T2_Global_funds_EIA",  "Funds for environmental impact assessments", c(1.0, 5000), "Constant USD (millions)", FALSE, TRUE, TRUE)

########################
# T3: Funds for institutional capacity building fishing
temp_data =read.csv("../../data/Target 3/funds_development_sustainable_fisheries_fishing_practices.csv") 
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T3_Funds_institution_capacity_building_fishing",  "Funds towards institutional capacity building in fishing", c(1.0, 10000), "Constant USD (millions)", FALSE, TRUE, TRUE)

# T6: Propn fish stocks safe biological limits
temp_data =read.csv("../../data/Target 6/Proportion_fish_stocks_safe_biological_limits.csv") 
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T6_prop_fish_stocks_safe_bl_statespace","Percentage of fish stocks within safe biological limits", c(0, 100), "Percent", FALSE, FALSE, FALSE)

# T10: Glacier mass balance
temp_data =read.csv("../../data/Target 10/glaciermassbalance.csv") 
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T10_glacier_mass_balance", "Glacier mass balance", c(-1500,100), "Mass balance (mm water equivalent)", FALSE, FALSE, FALSE)

# T10: Mean ice extent
temp_data =read.csv("../../data/Target 10/Sum_N&S_mean_sea_ice_extent.csv") 
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T10_sea_ice_extent", "Mean polar sea ice extent", c(22,25), "Mean extent (million sq km)", FALSE, FALSE, FALSE)

# T11: Funds towards nature reserves
temp_data =read.csv("../../data/Target 11/funds_towards_nature_reserves.csv") 
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T11_funds_towards_nature_reserves",  "Funds towards nature reserves", c(1.0, 10000), "Constant USD (millions)", FALSE, TRUE, TRUE)

# T12: Funds towards species protection
temp_data =read.csv("../../data/Target 12/funds_species_protection.csv") 
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T12_funds_towards_species_protection",  "Funds towards species protection", c(1.0, 10000), "Constant USD (millions)", FALSE, TRUE, TRUE)

# T20: GEF funding
temp_data =read.csv("../../data/Target 20/gef_funding_total_with_cofinancing.csv") 
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T20_GEF_funding",  "GEF funding", c(100, 3000), "Constant USD (millions), including co-financing", FALSE, TRUE, TRUE)

# T20: Funding EPL
temp_data =read.csv("../../data/Target 20/Global_funds_environmental_policy_laws_regulations_economic_instruments.csv") 
temp_data[,2] = temp_data[,2] / 1000000
temp_data = DLM_modelling_wrapper(data_standardise(temp_data), "T20_funding_EPL",  "Global funds towards environmental policy and laws", c(1, 14000), "Constant million USD", FALSE, TRUE, TRUE)


