remove(list = ls())

script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

source("common functions/graphics_helper_functions.r")

# Plot figure 2 by goal
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


library(scales)
m <-rbind(1,2, 3, 4, 5, 6, 7, 8)
layout(m)




plot_end_points <- function(input_data, input_data_type, input_string, ylimset, yticks, yticklabels)
{

  ndata = length(input_data)
  amaths1 <- expression(phantom(.) %<-% phantom(.))
  amaths2 <- expression(phantom(.) %->% phantom(.))
 
  # Set up the plot
  plot(1, axes = FALSE, type = "n", ylim = ylimset, xlim = c(0.04, 0.96 ))   
  axis(2, col = "grey40", col.axis = "grey20", at = yticks, labels = yticklabels, las = 2)
       box()
  num_pressure = sum(input_data_type == 1)
  num_state = sum(input_data_type == 2)
  num_benefit = sum(input_data_type == 3)
  num_response = sum(input_data_type == 4)
  
  
      # Plot the pressures
  if (num_pressure > 0)
  {
    positions = (1:num_pressure) / (4 * (num_pressure + 1)) 
    for (ii in (1:num_pressure))
    {
      eval(parse(text = paste("arrows(positions[ii] ,", input_data[[ii]], "[5, dim(", input_data[[ii]], ")[2]], positions[ii], ",   input_data[[ii]], "[4, dim(", input_data[[ii]], ")[2]], angle=90, code=3, length = 0.1, col = \"black\", lwd = 1)", sep = "")))    
      eval(parse(text = paste("points(positions[ii],", input_data[[ii]], "[3, dim(", input_data[[ii]], ")[2]], pch = 21, col = \"black\", bg = \"red\", cex = 2)")))   
    }
  }
  
  # Plot the states
  if (num_state > 0)
  {
    positions = 1 / 4 + (1:num_state) / (4 * (num_state + 1)) 
    for (ii in 1:num_state)
  for (ii in (1 + num_pressure):(num_state + num_pressure))
    {   
      eval(parse(text = paste("arrows(positions[ii - num_pressure] ,", input_data[[ii]], "[5, dim(", input_data[[ii]], ")[2]], positions[ii] - num_pressure, ",   input_data[[ii]], "[4, dim(", input_data[[ii]], ")[2]], angle=90, code=3, length = 0.1, col = \"black\", lwd = 1)", sep = "")))   
      eval(parse(text = paste("points(positions[ii - num_pressure],", input_data[[ii]], "[3, dim(", input_data[[ii]], ")[2]], pch = 21, col = \"black\", bg = \"orange\", cex = 2)")))   
    }
  }

  # Plot the benefits
  if (num_benefit > 0)
  {
    positions = 1 / 2 + (1:num_benefit) / (4 * (num_benefit + 1)) 
    for (ii in 1:num_benefit)
  for (ii in (1 + num_pressure + num_state):(num_state + num_pressure + num_benefit))
    {   
      eval(parse(text = paste("arrows(positions[ii - (num_state + num_pressure)] ,", input_data[[ii]], "[5, dim(", input_data[[ii]], ")[2]], positions[ii] - (num_state + num_pressure), ",   input_data[[ii]], "[4, dim(", input_data[[ii]], ")[2]], angle=90, code=3, length = 0.1, col = \"black\", lwd = 1)", sep = "")))   
      eval(parse(text = paste("points(positions[ii - (num_state + num_pressure)],", input_data[[ii]], "[3, dim(", input_data[[ii]], ")[2]], pch = 21, col = \"black\", bg = \"blue\", cex = 2)")))   
    }
  }  
      # Plot the responses
  if (num_response > 0)
  {
    positions = 3/4 + (1:num_response) / (4 * (num_response + 1)) 
    for (ii in (1 + num_state + num_pressure + num_benefit):(num_state + num_pressure + num_benefit + num_response))
    {
      eval(parse(text = paste("arrows(positions[ii - (num_state + num_pressure + num_benefit)] ,", input_data[[ii]], "[5, dim(", input_data[[ii]], ")[2]], positions[ii - (num_state + num_pressure + num_benefit)], ",   input_data[[ii]], "[4, dim(", input_data[[ii]], ")[2]], angle=90, code=3, length = 0.1, col = \"black\", lwd = 1)", sep = "")))    
      eval(parse(text = paste("points(positions[ii - (num_state + num_pressure + num_benefit)],", input_data[[ii]], "[3, dim(", input_data[[ii]], ")[2]], pch = 21, col = \"black\", bg = \"green\", cex = 2)")))   
    }
  }
  
    
 
  # Plot additional lines
  abline(0, 0, lty = 2)
  abline(v = 1/4, lty = 3)
  abline(v = 2/4, lty = 3)
  abline(v = 3/4, lty = 3)
  mtext(input_string, side = 3, line = 0.5, cex = 1.1)
}



#--------------------------------------
# TARGET 1
response1 = pad_missing(read_data("T1_Cor_def_biodiv.csv"))
response2 = pad_missing(read_data("T1_google_trends.csv"))
response3 = pad_missing(read_data("T1_heard_of_biodiv.csv"))
response4 = pad_missing(read_data("T1_funds_environmental_education.csv"))

plot_end_points(c("response1", "response2", "response3", "response4"), c(4,4,4,4), "Projected state in 2020 (relative to 2010)", c(-1,1.2), c(-1,1), c("-100%", "+100%"))


# TARGET 2
response5 = pad_missing(read_data("T2_global_funds_EIA.csv"))
response6 = pad_missing(read_data("T2_research_studies_involving_economic_valuation.csv"))

plot_end_points(c("response5", "response6"), c(4,4), "", c(-1,6), c(-1,5), c("-100%", "+500%"))

# TARGET 3
response7 = pad_missing(read_data("T3_funds_institution_capacity_building_fishing.csv"))
response8 = pad_missing(read_data("T3_WTO_greenbox.csv"))

plot_end_points(c("response7", "response8"), c(4,4), "", c(-1,12), c(-1,10), c("-100%", "+1000%"))

# TARGET 4
benefit1 = pad_missing(read_data("T4_trends_driven_by_use.csv"))
response9 = pad_missing(read_data("T4_cat_1_cites_countries.csv"))
pressure1 = pad_missing(read_data("T4_ecofoot.csv"))
pressure2 = pad_missing(read_data("T4_HANPP.csv"))
pressure3 = pad_missing(read_data("T4_water_footprint.csv"))

plot_end_points(c("pressure1", "pressure2", "pressure3", "benefit1", "response9"),  c(1,1,1,3,4), "", c(-0.4,1), c(-0.2,0.8), c("-20%", "+80%"))

# TARGET 5

state2 = pad_missing(read_data("T5_pct_natural_habitat.csv"))
state3 = pad_missing(read_data("T5_WBI_habitat_specialists.csv"))
state4 = pad_missing(read_data("T5_wetland_LPI.csv"))

plot_end_points(c("state2", "state3", "state4"),  c(2,2,2), "", c(-0.6,0.3), c(-0.5,0.2), c("-50%", "+20%"))

# TARGET 6

state5 = pad_missing(read_data("T6_prop_fish_stocks_safe_bl_statespace.csv"))
state6 = pad_missing(read_data("T6_RLI_seabirds.csv"))
pressure4 = pad_missing(read_data("T6_trawl_effort.csv"))
response10 = pad_missing(read_data("T6_msc_engaged_tonnes.csv"))

plot_end_points(c("pressure4", "state5" , "state6",  "response10"),  c(1,2,2,4), "", c(-0.4,2), c(-0.2,1.8), c("-20%", "+80%"))

# TARGET 7

state7 = pad_missing(read_data("T7_farmland_WBI.csv"))
response11 = pad_missing(read_data("T7_FSC_PEFC_cert_forest.csv"))
response12 = pad_missing(read_data("T7_area_Conservation_agriculture.csv"))
response13 = pad_missing(read_data("T7_area_organic_agriculture.csv"))

plot_end_points(c("state7", "response11", "response12", "response13"),  c(2,4,4,4), "", c(-0.4,1.7), c(-0.2,1.5), c("-20%", "+150%"))

# TARGET 8
state8 = pad_missing(read_data("T8_rli_pollution_birds.csv"))
pressure5 = pad_missing(read_data("T8_insecticide_use.csv"))
pressure6 = pad_missing(read_data("T8_nitrogen_surplus.csv"))

plot_end_points(c("pressure5", "pressure6", "state8"),  c(1,1,2), "", c(-0.4, 0.4), c(-0.3,0.3), c("-30%", "+30%"))


dev.new()
# Plot figure 2 by goal
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

library(scales)
m <-rbind(1,2, 3, 4, 5, 6, 7, 8)
layout(m)




# TARGET 9
state9 = pad_missing(read_data("T9_RLI_invasive_birds.csv"))
pressure7 = pad_missing(read_data("T9_introduction_events.csv"))
response14 = pad_missing(read_data("T9_invasives_national_legislation.csv"))

plot_end_points(c("pressure7", "state9", "response14"),  c(1,2,4), "", c(-0.2,0.7), c(-0.1,0.5), c("-10%", "+50%"))

# TARGET 10
state10 = pad_missing(read_data("T10_glacier_mass_balance.csv", invert = TRUE))
state11 = pad_missing(read_data("T10_sea_ice_extent.csv"))
state245 = pad_missing(read_data("T10_coral_reef_cover.csv"))

plot_end_points(c("state245", "state10", "state11"),  c(2,2,2), "", c(-0.8,0.4), c(-0.6,0.2), c("-60%", "+20%"))

# TARGET 11
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


plot_end_points(c("response15", "response16", "response17", "response18", "response19", "response20", "response21", "response22", "response23", "response24"),  c(4,4,4,4,4,4,4,4,4), "", c(-1,6), c(-1,5), c("-100%", "+500%"))

# TARGET 12
state12 = pad_missing(read_data("T12_lpi.csv"))
state13 = pad_missing(read_data("T12_bird_mammal_extinctions.csv", invert = TRUE))
state14 = pad_missing(read_data("T12_rli.csv"))
response25 = pad_missing(read_data("T12_funds_towards_species_protection.csv"))


plot_end_points(c("state12", "state13", "state14", "response25"),  c(2,2,2,4), "", c(-0.6,5), c(-0.4,4), c("-40%", "+500%"))

# TARGET 12

benefit2 = pad_missing(read_data("T13_domesticated_genetic_diversity.csv", invert = TRUE))

plot_end_points(c("benefit2"),  c(3), "", c(-0.4,0.4), c(-0.3,0.3), c("-30%", "+30%"))


# TARGET 14
benefit3 = pad_missing(read_data("T14_RLI_pollinators.csv"))

plot_end_points(c("benefit3"),  c(3), "", c(-0.4,0.4), c(-0.3,0.3), c("-30%", "+30%"))


# TARGET 19
response27 = pad_missing(read_data("T19_funds_for_environmental_research.csv"))
response28 = pad_missing(read_data("T19_gbif_records.csv"))
response29 = pad_missing(read_data("T19_wos_biodiv_papers.csv"))

plot_end_points(c("response27", "response28", "response29"),  c(4,4,4), "", c(-1,32), c(-1,30), c("-100%", "+3000%"))

# TARGET 20
response30 = pad_missing(read_data("T20_gef_funding.csv"))
response31 = pad_missing(read_data("T20_global_funds_environmental_policy_and_laws.csv"))
response32 = pad_missing(read_data("T20_ODA_biodiversity.csv"))

plot_end_points(c("response30", "response31", "response32"),  c(4,4,4), "", c(-0.5,6), c(-0.5,5), c("-50%", "+500%"))





 #mtext("1", side = 2, outer = TRUE, cex = 1.25, line = 2, at = 0.875, las = 1)
 #mtext("2", side = 2, outer = TRUE, cex = 1.25, line = 2, at = 0.625, las = 1)
 #mtext("3", side = 2, outer = TRUE, cex = 1.25, line = 2, at = 0.375, las = 1)
 #mtext("4", side = 2, outer = TRUE, cex = 1.25, line = 2, at = 0.125, las = 1)
 
 
 
 
 
 
 