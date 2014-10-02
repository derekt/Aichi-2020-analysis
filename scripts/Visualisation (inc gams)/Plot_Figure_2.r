remove(list = ls())

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
             
             
source("common functions/graphics_helper_functions.r")

# Plot figure 3
par(mar = c(1,1,3,1), oma = c(1,1,1,1))
par(tcl = -0.25)
par(mgp = c(1, 0.6, 0))

library(scales)
m <-rbind(c(1,2,3))
layout(m)

#--------------------------------------

# PRESSURES
  
        
pressure1 = pad_missing(read_data("T4_ecofoot.csv"))
pressure2 = pad_missing(read_data("T4_HANPP.csv"))
pressure3 = pad_missing(read_data("T4_water_footprint.csv"))
pressure4 = pad_missing(read_data("T6_trawl_effort.csv"))
pressure5 = pad_missing(read_data("T8_insecticide_use.csv"))
pressure6 = pad_missing(read_data("T8_nitrogen_surplus.csv"))
pressure7 = pad_missing(read_data("T9_introduction_events.csv"))

plotbox1()

plot_line("pressure1", "red", T)
plot_line("pressure2", "red", F)
plot_line("pressure3", "red", T)
plot_line("pressure4", "red", T)
plot_line("pressure5", "red", F)
plot_line("pressure6", "red", T)
plot_line("pressure7", "red", T)


# STATES
state2 = pad_missing(read_data("T5_pct_natural_habitat.csv"))
state3 = pad_missing(read_data("T5_WBI_habitat_specialists.csv"))
state4 = pad_missing(read_data("T5_wetland_LPI.csv"))
state5 = pad_missing(read_data("T6_prop_fish_stocks_safe_bl_statespace.csv"))
state6 = pad_missing(read_data("T6_RLI_seabirds.csv"))
state7 = pad_missing(read_data("T7_farmland_WBI.csv"))
state8 = pad_missing(read_data("T8_rli_pollution_birds.csv"))
state9 = pad_missing(read_data("T9_RLI_invasive_birds.csv"))
state10 = pad_missing(read_data("T10_glacier_mass_balance.csv", invert = T))
state11 = pad_missing(read_data("T10_sea_ice_extent.csv"))
state12 = pad_missing(read_data("T12_LPI.csv"))
state13 = pad_missing(read_data("T12_bird_mammal_extinctions.csv"))
state14 = pad_missing(read_data("T12_rli.csv")) 
state17 = pad_missing(read_data("T10_coral_reef_cover.csv"))


plotbox1()

plot_line("state2", "orange", F)                  
plot_line("state3", "orange", F)
plot_line("state4", "orange", T)
plot_line("state5", "orange", F)
plot_line("state6", "orange", T)
plot_line("state7", "orange", T)
plot_line("state8", "orange", T)
plot_line("state9", "orange", T)
plot_line("state10", "orange", F)
plot_line("state11", "orange", T)
plot_line("state12", "orange", T)
plot_line("state13", "orange", F)
plot_line("state14", "orange", T)
plot_line("state17", "orange", F)


# BENEFITS

benefit1 = pad_missing(read_data("T13_domesticated_genetic_diversity.csv", invert = TRUE))
benefit2 = pad_missing(read_data("T14_RLI_pollinators.csv")) 
benefit3 = pad_missing(read_data("T4_trends_driven_by_use.csv"))

plotbox1()

plot_line("benefit1", "blue", T)
plot_line("benefit2", "blue", T)
plot_line("benefit3", "blue", T)


# RESPONSES

dev.new()
m <-rbind(c(1,2,3))
layout(m)

response1 = pad_missing(read_data("T1_cor_def_biodiv.csv"))
response2 = pad_missing(read_data("T1_google_trends.csv"))
response3 = pad_missing(read_data("T1_heard_of_biodiv.csv"))
response4 = pad_missing(read_data("T1_funds_environmental_education.csv"))
response5 = pad_missing(read_data("T2_global_funds_EIA.csv"))
response6 = pad_missing(read_data("T2_research_studies_involving_economic_valuation.csv"))
response7 = pad_missing(read_data("T3_funds_institution_capacity_building_fishing.csv"))
response8 = pad_missing(read_data("T3_WTO_greenbox.csv"))
response9 = pad_missing(read_data("T4_cat_1_cites_countries.csv"))
response10 = pad_missing(read_data("T6_msc_engaged_tonnes.csv"))
response11 = pad_missing(read_data("T7_FSC_PEFC_cert_forest.csv"))
response12 = pad_missing(read_data("T7_area_Conservation_agriculture.csv"))
response13 = pad_missing(read_data("T7_area_organic_agriculture.csv"))
response14 = pad_missing(read_data("T9_invasives_national_legislation.csv"))
response15 = pad_missing(read_data("T11_PA_coverage_birds_mammals_amphibians.csv"))
response16 = pad_missing(read_data("T11_pame_Assessments_per_year.csv"))
response17 = pad_missing(read_data("T11_terrestrial_ecoregion_coverage.csv"))
response18 = pad_missing(read_data("T11_terrestrial_PA_coverage.csv"))
response19 = pad_missing(read_data("T11_marine_PA_coverage.csv"))
response20 = pad_missing(read_data("T11_pa_coverage_azes.csv"))
response21 = pad_missing(read_data("T11_pa_coverage_ibas.csv"))
response22 = pad_missing(read_data("T11_freshwater_ecoregion_coverage.csv"))
response23 = pad_missing(read_data("T11_funds_towards_nature_reserves.csv"))
response24 = pad_missing(read_data("T11_marine_ecoregion_coverage.csv"))
response25 = pad_missing(read_data("T12_funds_towards_species_protection.csv"))
response27 = pad_missing(read_data("T19_funds_for_environmental_research.csv"))
response28 = pad_missing(read_data("T19_gbif_records.csv"))
response29 = pad_missing(read_data("T19_wos_biodiv_papers.csv"))
response30 = pad_missing(read_data("T20_gef_funding.csv"))
response31 = pad_missing(read_data("T20_global_funds_environmental_policy_and_laws.csv"))
response32 = pad_missing(read_data("T20_ODA_biodiversity.csv"))

plotbox1()
plot_line("response1", "green", T)
plot_line("response2", "green", F)          
plot_line("response3", "green", T)                
plot_line("response4", "green", FALSE)
plot_line("response5", "green", F)
plot_line("response6", "green", T)
plot_line("response7", "green", F)
plot_line("response8", "green", T)
plot_line("response9", "green", T)
plot_line("response10", "green", T)
plot_line("response11", "green", T)
plot_line("response12", "green", T)
plot_line("response13", "green", T)
plot_line("response14", "green", F)
plot_line("response15", "green", T)
plot_line("response16", "green", T)
plot_line("response17", "green", T)
plot_line("response18", "green", T)
plot_line("response19", "green", T)
plot_line("response20", "green", T)
plot_line("response21", "green", T)
plot_line("response22", "green", T)
plot_line("response23", "green", F)
plot_line("response24", "green", T)
plot_line("response25", "green", F)
plot_line("response27", "green", F)
plot_line("response28", "green", T)
plot_line("response29", "green", F)
plot_line("response30", "green", F)
plot_line("response31", "green", F)
plot_line("response32", "green", F)




