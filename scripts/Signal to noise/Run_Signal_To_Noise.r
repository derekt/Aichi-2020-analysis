#-------------------------------------------------------------------------------
# Wrapper script to run the Aichi signal-to-noise calculations
#
# Derek Tittensor 23 March 2014
#-------------------------------------------------------------------------------

remove(list = ls())

# Load libraries
library(dlm)

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

# Source functions
source("DLM_Signal_To_Noise.r")
source("DLM_Signal_To_Noise_Wrapper.r")                                                                           
source("../common functions/data_standardise.r")

# Set working directory for data
setwd("../../data")
options(warn=-1)

######################
# T1: Biodiversity barometer: heard of biodiversity
print("")
print("Heard of biodiv")
temp_data =read.csv("Target 1/Biodiversity_barometer_%heardofbiodiversity.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T1: Biodiversity barometer: % knew correct definition of diversity
print("")
print("Correct def biodiv")
temp_data =read.csv("Target 1/Biodiversity_barometer_%knewcorrectdefinitionbiodiversity.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T1: Funds for environmental education global aid
print(" ")
print("Funds env. ed.")
temp_data =read.csv("Target 1/Environmental_education_global_aid_funds.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T1: Google trends
print("")
print("Online interest biodiv.")
temp_data =read.csv("Target 1/googletrends_final.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)


######################
# T2: Global funds EIA
print("")
print("Global funds EIA")
temp_data =read.csv("Target 2/Global_funds_EIA.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T2: Number of research studies involving economic valuation
print("")
print("Research studie economic val.")
temp_data =read.csv("Target 2/Number_of_Research_studies_involving_economic_valuation.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)


######################

# T3: Funds for the development of sustainable fishing
print("")
print("Funds sust. fish.")
temp_data =read.csv("Target 3/funds_development_sustainable_fisheries_fishing_practices.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T3: WTO Green box
print("")
print("WTO Green box.")
temp_data =read.csv("Target 3/WTO_greenboxspending_limited_countries.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#######################
# T4: Ecological footprint
print("")
print("Eco. foot.")
temp_data =read.csv("Target 4/Ecological.footprint.number.earths.demanded_incl.carbon.built.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T4: Trends driven by use
print("")
print("Trends driv. by use.")
temp_data =read.csv("Target 4/trends_driven_by_use_limited.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T4: HANPP
print("")
print("HANPP")
temp_data =read.csv("Target 4/HANPP.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)


# T4: Water footprint
print("")
print("Water footprint")
temp_data =read.csv("Target 4/water footprint.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T4: Percent category 1 CITES nations
print("")
print("% cat 1")
temp_data =read.csv("Target 4/Percent_cat1_nations_CITES.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#-------------------------
# T5: Natural habitat extent
temp_data =read.csv("Target 5/Percent_land_area_natural_habitat.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T5: WBI data
print("")
print("WBI hab. dep. birds")
temp_data =read.csv("Target 5/WBI_habitat_dep_birds.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T5: Wetland LPI
print("")
print("Wetland LPI")
temp_data =read.csv("Target 5/wetland_lpi_limited.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#-------------------------------
# T6: Tonnage MSC engaged fisheries
print("")
print("MSC engaged")
temp_data =read.csv("Target 6/AllMSCEngaged.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T6: Propn fish stocks safe biological limits
print("")
print("Fish stocks bio. limits")
temp_data =read.csv("Target 6/proportion_fish_stocks_safe_biological_limits.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T6: RLI for seabirds
print("")
print("RLI for seabirds")
temp_data =read.csv("Target 6/RLI_seabirds_limited.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T6: Trawl effective effort
print("")
print("Trawl effort")
temp_data =read.csv("Target 6/trawl_effective_effort.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)
#----------------------------------------

#-------------------------------

# T7: FSC + PESC certified forest
print("")
print("Cert. forest. area")
temp_data =read.csv("c:\\users\\derekt\\work\\research\\dropbox\\finaldatagbo4\\target_7\\FSC_and_PEFC_certified_area.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T7: Farmland WBI
print("")
print("Farmland WBI")
temp_data =read.csv("Target 7/farmland_wbi.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T7: Organic agriculture
print("")
print("Organ. agric.")
temp_data =read.csv("Target 7/Organic_agriculture.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T7: Conservation agriculture
print("")
print("Cons. agric.")
temp_data =read.csv("Target 7/area_conservation_agriculture.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#--------------------------------
# T8: Insecticide use
print("")
print("Insec. use")
temp_data =read.csv("Target 8/insecticide_use_cutdown.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T8: RLI pollution impacts birds
print("")
print("RLI pollution")
temp_data =read.csv("Target 8/RLI_birds_impacted_by_pollution_limited.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T8: Nitrogen surplus
print("")
print("Nitr. surp.")
temp_data =read.csv("Target 8/nitrogen_surplus.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#---------------------------------------
# T9: RLI for IAS on birds only
print("")
print("RLI IAS")
temp_data =read.csv("Target 9/RLI_birds_impacted_alien_invasive_spp_limited.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T9 IAS 21 countries
print("")
print("IAS 21 countries")
temp_data = read.csv("Target 9/BIP indicator IAS 21 countries2.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T9: Trends in endangered species legislation
print("")
print("End. spp. legis.")
temp_data =read.csv("c:\\users\\derekt\\work\\research\\dropbox\\finaldatagbo4\\target_9\\trends_invasive_legislation.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

#---------------------------------------
# T10: Glacier mass balance
print("")
print("Glac. m. b.")
temp_data =read.csv("Target 10/glaciermassbalance.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T10: Sum mean sea-ice extent
print("")
print("Sum mn. sea-ice extnt.")
temp_data =read.csv("Target 10/sum_n&s_mean_sea_ice_extent.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T10: Coral reef data
print("")
print("Reef data")
temp_data =read.csv("Target 10/coral_cover.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#------------------------------------
# T11: Terrestrial PAs
print("")
print("Terr. PAs")
#temp_data =read.csv("Target 11/WDPA_limited.csv") 
#temp_data = temp_data[,1:2]
temp_data =read.csv("Target 11/terrestrial_PAs.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T11: Marine PAs
print("")
print("Mar. PAs")
#temp_data =read.csv("Target 11/WDPA_limited.csv") 
#temp_data = temp_data[,c(1,3)]
temp_data =read.csv("Target 11/marine_PAs.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T11: PAME: Assessments per year
print("")
print("PAMEs")
temp_data =read.csv("Target 11/pame_assessments_limited.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T11: Funds towards nature reserves
print("")
print("Fund. nat. res.")
temp_data =read.csv("Target 11/funds_towards_nature_reserves.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T11: PA coverage species
print("")
print("PA coverage spp. dist. mean")
temp_data =read.csv("Target 11/pa_coverage_spp_dist_mean.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

# T11: PA coverage freshwater ecosystems
print("")
print("PA freshwater")
temp_data =read.csv("Target 11/coverage_freshwater_ecosystems.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

# T11: PA coverage ecoregions
print("")
print("PA terrestrial")
#temp_data =read.csv("Target 11/PA_coverage_terrestrial_ecoregions.csv") 
temp_data =read.csv("Target 11/PA_coverage_terrestrial_ecoregions_updated.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

# T11: PA coverage ecoregions
print("")
print("PA marine")
#temp_data =read.csv("Target 11/PA_coverage_marine_ecoregions.csv") 
temp_data =read.csv("Target 11/PA_coverage_marine_ecoregions_updated.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

# T11: PA coverage IBAs  
print("")
print("PA IBAs")
temp_data =read.csv("Target 11/PA_coverage_AZEs.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

# T11: PA coverage AZEs
print("")
print("PA AZEs")
temp_data =read.csv("Target 11/PA_coverage_AZEs.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), TRUE, FALSE)

#-----------------------------------
# T12: LPI
print("")
print("LPI")
temp_data =read.csv("Target 12/LPI.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T12: RLI 
print("")
print("RLI")
temp_data =read.csv("Target 12/RLI_all.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T12: Mammal and bird extinctions
print("")
print("Mamm. bird. ext.")
temp_data =read.csv("Target 12/mammal_bird_extinctions_update.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T12: Global funds for species protection
print("")
print("Funds spec. proc.")
temp_data =read.csv("Target 12/funds_species_protection.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#-----------------------------------
# T13: Genetic diversity domesticated animals
print("")
print("Gen. div.")
temp_data =read.csv("Target 13/gen_div_terr_dom_percentage.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#-----------------------------------
# T14: RLI for pollinator birds
print("")
print("RLI pollinators.")
temp_data =read.csv("Target 14/rli_pollinators_limited.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#-----------------------------------
# T19: Funds for environmental research
print("")
print("Funds env. res.")
temp_data =read.csv("Target 19/funds_for_environmental_research.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, TRUE)

# T19: WOS num papers biodiversity in title
print("")
print("WOS papers")
temp_data =read.csv("Target 19/WOS_num_papers_biodiversity_title_limited.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T19: GBIF records over time
print("")
print("GBIF recs.")
temp_data =read.csv("Target 19/gbif_num_records.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

#-------------------------------------
# T20: ODA convention
print("")
print("ODA conv.")
temp_data =read.csv("Target 20/oda_biodiv.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, FALSE)

# T20: GEF funding
print("")
print("GEF fund.")
temp_data =read.csv("Target 20/gef_funding_total_with_cofinancing.csv") 
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, TRUE)

# T20: Funding for environmental policy and laws
print("")
print("Fund. env. pol.")
temp_data = read.csv("Target 20/Global_funds_environmental_policy_laws_regulations_economic_instruments.csv")
DLM_Signal_To_Noise_Wrapper(data_standardise(temp_data), FALSE, TRUE)