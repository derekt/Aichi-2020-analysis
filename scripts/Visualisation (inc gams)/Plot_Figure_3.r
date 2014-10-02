remove(list = ls())                 
       
         script.dir <- dirname(sys.frame(1)$ofile)
  setwd(script.dir)
             
source("common functions/graphics_helper_functions.r")
library(scales)
library(shape)

par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

m <-cbind(1,2,3)
layout(m)

overlap <- function(range1, range2) 
{
  ff = FALSE
  if((range1[1] > range2[1]) && (range1[1] < range2[2]))
    ff = TRUE 
 if((range2[1] > range1[1]) && (range2[1] < range1[2]))
     ff = TRUE;
  ff
}


plot_end_points <- function(input_data, input_data_type, input_string, xlimset, xticks, xticklabels)
{

  threshold = 0.2

  ndata = length(input_data)
  amaths1 <- expression(phantom(.) %<-% phantom(.))
  amaths2 <- expression(phantom(.) %->% phantom(.))
 
  # Set up the plot
  plot(1, axes = FALSE, type = "n", xlim = xlimset, ylim = c(0, 1 ))   
  axis(1, col = "grey40", col.axis = "grey20", at = xticks, labels = xticklabels, las = 2)
       box()
  
  num_pressure = sum(input_data_type == 1)
  num_state = sum(input_data_type == 2)
  num_benefit = sum(input_data_type == 3)
  num_response = sum(input_data_type == 4)

    # Plot the pressures
  if (num_pressure > 0)
  {
    positions = (1:num_pressure) / ((30 + 1)) 
    for (ii in (1:num_pressure))
    {   
      eval(parse(text = paste("gg = calculate_rates(", input_data[ii], ")", sep = "")))
      fname = paste("../results/bootstrapped/", input_data[ii], sep="")
      save(gg, file = fname)
      points(gg$pre2010, positions[ii], pch = 15, col = "red", cex = 1.5)    

      if (overlap(c(0, 0), gg$post2010cis))
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]), col = "grey")      
      else
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]))
    }
  }
  
    # Plot the states
  if (num_state > 0)
  {
    positions = (num_pressure + (1:num_state)) / ((30 + 1))

    for (ii in (1 + num_pressure):(num_pressure + num_state))
    {   
      eval(parse(text = paste("gg = calculate_rates(", input_data[ii], ")", sep = "")))
      
      # Write out the bootstrapped results
           fname = paste("../results/bootstrapped/", input_data[ii], sep="")
      save(gg, file = fname)
     points(gg$pre2010, positions[ii],  pch = 15, col = "orange", cex = 1.5)  
      
       if (overlap(c(0, 0), gg$post2010cis))
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]), col = "grey")      
      else
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]))
    }
  }
  
      # Plot the benefits
  if (num_benefit > 0)
  {
    positions = ((num_pressure + num_state) + (1:num_benefit)) / ((30 + 1)) 

    for (ii in (1 + num_pressure + num_state):(num_pressure + num_state + num_benefit))
    {   
      eval(parse(text = paste("gg = calculate_rates(", input_data[ii], ")", sep = "")))
      
      # Write out the bootstrapped results
           fname = paste("../results/bootstrapped/", input_data[ii], sep="")
      save(gg, file = fname)
     points(gg$pre2010, positions[ii],  pch = 15, col = "blue", cex = 1.5)  
      
       if (overlap(c(0, 0), gg$post2010cis))
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]), col = "grey")      
      else
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]))
    }
  }

  
    abline(v = 0, lty = 2)  
    
      # Plot the responses
  if (num_response > 0)
  {
    dev.new()
     # Set up the plot
     par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

m <-cbind(1,2,3)
layout(m)
  plot(1, axes = FALSE, type = "n", xlim = xlimset, ylim = c(0, 1 ))   
  axis(1, col = "grey40", col.axis = "grey20", at = xticks, labels = xticklabels, las = 2)
       box()
    positions = (1:num_response) / ((num_response + 1)) 
    for (ii in (1 + num_pressure + num_state + num_benefit):(num_pressure + num_state + num_benefit + num_response))
    {
      eval(parse(text = paste("gg = calculate_rates(", input_data[ii], ")", sep = "")))
      
           fname = paste("../results/bootstrapped/", input_data[ii], sep="")
      save(gg, file = fname)
     
     points(gg$pre2010, positions[ii], pch = 15, col = "green", cex = 1.5)
      if (overlap(c(0, 0), gg$post2010cis))
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]), col = "grey")      
      else
        lines(c(gg$pre2010, gg$post2010), c(positions[ii], positions[ii]))
    }
  }
                     
      abline(v = 0, lty = 2)  
}

# Calculate rates between 2001 - 2010 and 2011 - 2020
calculate_rates <- function(input_data)
{
  year1_begin = 2001
  year1_end = 2010
  year2_begin = 2011
  year2_end = 2020
  
  # Calculate mean rate
  year1startloc = which(input_data[1,] == year1_begin)
  while(is.na(input_data[3,year1startloc]))
  {
    year1startloc = year1startloc + 1
  }
  if (input_data[1,year1startloc] >= 2010)
    stop("No pre-2010 start year value")

  year1endloc = which(input_data[1,] == year1_end)
  while(is.na(input_data[3,year1endloc]))
    year1endloc = year1endloc + 1
  if (input_data[1,year1endloc] > 2010)
    stop("No 2010 end year value")
    
  year2startloc = which(input_data[1,] == year2_begin)
  while(is.na(input_data[3,year2startloc]))
  {
    year2startloc = year2startloc + 1
    print(year2startloc)
    print(input_data[3,year2startloc])
  }
  if (input_data[1,year2startloc] > 2019)
    stop("No 2011 start year value")

  year2endloc = which(input_data[1,] == year2_end)
  while(is.na(input_data[3,year2endloc]))
    year2endloc = year2endloc + 1
  if (input_data[1,year2endloc] > 2020)
    stop("No 2020 end year value")

  pre2010annualrate = (input_data[3,year1endloc] - input_data[3,year1startloc]) / (year1endloc - year1startloc)
  post2010annualrate = (input_data[3,year2endloc] - input_data[3,year2startloc]) / (year2endloc - year2startloc)  
  
  # Calculate bootstrapped rates
  n_bootstrap = dim(input_data)[1] - 5
  pre2010slopes = vector(length = n_bootstrap)
  post2010slopes = vector(length = n_bootstrap)
  if ((dim(input_data)[1] - 5) != 10000)
    stop("DON'T RUN WITHOUT CHANGING THE 2.5% and 97.5% INDICES BELOW IN The PLOT_SPR_RATES CODE")
  for (ii in 1:n_bootstrap)
  {
      # Calculate the pre2010 bootstrapped slopes
       pre2010slopes[ii] = getslope(input_data[ii+5, year1startloc:year1endloc],input_data[1, year1startloc:year1endloc])
      post2010slopes[ii] = getslope(input_data[ii + 5, year2startloc:year2endloc], input_data[1, year2startloc:year2endloc])
  }
  
  pre2010slopes = sort(pre2010slopes)
  post2010slopes = sort(post2010slopes)
  
  # Calculate the 2.5 and 97.5 CIs
  pre2010slopescis = c(pre2010slopes[250], pre2010slopes[9750])
  post2010slopescis = c(post2010slopes[250], post2010slopes[9750])
  
  list(pre2010 = pre2010annualrate, post2010 = post2010annualrate, pre2010cis = pre2010slopescis, post2010cis = post2010slopescis)
}

getslope <- function(input_data, year)
{
  bb = lm(input_data ~ year)
  as.numeric(coef(bb)[2])
}

#--------------------------------------
# PRESSURES
pressure1 = pad_missing(read_data("T4_ecofoot.csv"))
pressure2 = pad_missing(read_data("T4_HANPP.csv"))
pressure3 = pad_missing(read_data("T4_water_footprint.csv"))
pressure4 = pad_missing(read_data("T6_trawl_effort.csv"))
pressure5 = pad_missing(read_data("T8_insecticide_use.csv"))
pressure6 = pad_missing(read_data("T8_nitrogen_surplus.csv"))
pressure7 = pad_missing(read_data("T9_introduction_events.csv"))

plot_end_points(c("pressure6", "pressure1", "pressure5", "pressure2", "pressure7", "pressure3", "pressure4"),  c(1,1,1,1,1,1,1), "",c(-0.05,0.05), c(-0.025,0.025), c("-2.5%", "+2.5%"))
abline(v = 0, lty = 2)

# STATES

state2 = pad_missing(read_data("T5_pct_natural_habitat.csv"))
state3 = pad_missing(read_data("T5_WBI_habitat_specialists.csv"))
state4 = pad_missing(read_data("T5_wetland_LPI.csv"))
state5 = pad_missing(read_data("T6_prop_fish_stocks_safe_bl_statespace.csv"))
state6 = pad_missing(read_data("T6_RLI_seabirds.csv"))
state7 = pad_missing(read_data("T7_farmland_WBI.csv"))
state8 = pad_missing(read_data("T8_rli_pollution_birds.csv"))
state9 = pad_missing(read_data("T9_RLI_invasive_birds.csv"))
state10 = pad_missing(read_data("T10_glacier_mass_balance.csv", invert = TRUE))
state11 = pad_missing(read_data("T10_sea_ice_extent.csv"))
state12 = pad_missing(read_data("T12_lpi.csv"))
state13 = pad_missing(read_data("T12_bird_mammal_extinctions.csv", invert = TRUE))
state14 = pad_missing(read_data("T12_rli.csv"))
state17 = pad_missing(read_data("T10_coral_reef_cover.csv"))

#plot_end_points(c("state17", "state7", "state3", "state12", "state10", "state5", "state13",  "state2", "state11", "state14", "state8", "state6",  "state9", "state4"),  c(2,2,2,2,2,2,2,2,2,2,2,2,2,2), "",c(-0.06,0.06), c(-0.05,0.05), c("-5%", "+5%"))
#abline(v = 0, lty = 2)

# BENEFITS
benefit1 = pad_missing(read_data("T4_trends_driven_by_use.csv"))
benefit2 = pad_missing(read_data("T13_domesticated_genetic_diversity.csv", invert = TRUE))
benefit3 = pad_missing(read_data("T14_RLI_pollinators.csv"))
#plot_end_points(c("benefit2", "benefit1", "benefit3"),  c(3,3,3), "",c(-0.06,0.06), c(-0.05,0.05), c("-5%", "+5%"))

# RESPONSES
response2 = pad_missing(read_data("T1_google_trends.csv"))
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
response31 = pad_missing_all_data(read_all_data("T20_funding_EPL.csv"))
response32 = pad_missing(read_data("T20_ODA_biodiversity.csv"))

#plot_end_points(c("response4", "response25", "response11", "response2", "response13",  "response32", "response5", "response14", "response29", "response15", "response18", "response20", "response17",   "response9", "response21",  "response12","response22",   "response24", "response8", "response10", "response30", "response6", "response19","response16", "response7", "response28", "response23", "response31", "response27"),  rep(4, 29), "", c(-0.6,0.6), c(-0.3,0.3), c("-30%", "+30%"))
#abline(v = 0, lty = 2)                                                  