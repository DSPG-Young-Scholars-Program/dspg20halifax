
library(sf)
library(dplyr)
library(leaflet)
library(tigris)
library(ggplot2)

## Read in opportunity insights data and filter to Halifax
op_insights <- readr::read_csv(here::here("data", "original", "tract_outcomes_simple.csv"))
halifax_op_insights <- op_insights %>% filter(state == 51, county == 83) %>% mutate(GEOID = paste0("51083", tract))

## Read in spatial data for Halifax
halifax_tracts <- tracts(state = "VA", county = "Halifax")
halifax_tracts <- st_as_sf(halifax_tracts)

## Join op insights data with polygons
halifax_sf <- left_join(halifax_op_insights, halifax_tracts) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

#
#
# Visualizing Uncertainty --------------------------------------------------------------------------------------------------------------------
#
#

## Just for exploration, create new columns where you add the SE to sub-median estimates and subtract a SE from above-median estimates
## Do this for both 1 SE and 2 SE
halifax_sf <- halifax_sf %>%
  mutate(se_adjusted_jail_p25_1 = case_when(jail_pooled_pooled_p25 < median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 + jail_pooled_pooled_p25_se,
                                            jail_pooled_pooled_p25 >= median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 - jail_pooled_pooled_p25_se),
         se_adjusted_jail_p25_2 =  case_when(jail_pooled_pooled_p25 < median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 + 2*jail_pooled_pooled_p25_se,
                                             jail_pooled_pooled_p25 >= median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 - 2*jail_pooled_pooled_p25_se))

## Fixed palette based on original estimates
pal <- colorBin("BuPu", range(halifax_sf$jail_pooled_pooled_p25, na.rm = TRUE), bins = 5)

## Add layers for estimates and the two SE-adjusted layers to display how much of a difference it makes
leaflet(halifax_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(jail_pooled_pooled_p25),
              fillOpacity = 0.5,
              opacity = 0.3,
              weight = 2,
              color = "gray",
              label = ~round(jail_pooled_pooled_p25, 2),
              group = "Not Adjusted") %>%
  addPolygons(fillColor = ~pal(se_adjusted_jail_p25_1),
              fillOpacity = 0.5,
              opacity = 0.3,
              weight = 2,
              color = "gray",
              label = ~round(se_adjusted_jail_p25_1, 2),
              group = "Adjusted 1") %>%
  addPolygons(fillColor = ~pal(se_adjusted_jail_p25_2),
              fillOpacity = 0.5,
              opacity = 0.3,
              weight = 2,
              color = "gray",
              label = ~round(se_adjusted_jail_p25_2, 2),
              group = "Adjusted 2") %>%
  addLayersControl(baseGroups = c("Not Adjusted", "Adjusted 1", "Adjusted 2")) %>%
  addLegend("bottomright",
            pal = pal,
            values = halifax_sf$jail_pooled_pooled_p25)

## This isn't the whole story though - I guess it ought to be a MVN distribution?
## Just artificially adding to the low ones and subtracting from the high ones really seems conservative

#
#
# Generating samples based on estimates and SEs recorded ------------------------------------------------------------------------------------
#
#

## Helper function used in map_samples to add sample polygons 
addAdjPolygons <- function(map, variable, group_name, palette) {
  addPolygons(map, color = "#444444", weight = 2, smoothFactor = 0.5,
              opacity = 0.4, fillOpacity = 0.5,
              fillColor = ~pal(variable),
              group = group_name,
              label = ~round(variable, 2))
}

## This function takes samples of the variable given and plots them on a single map so the user can toggle between them
## An attempt to visualize uncertainty in small-sample ACS estimates.
## For sampling it assumes that each value is independent of other values (obviously a simplification...)

map_samples <- function(data, ## sf object with data being plotted and column of SE estimates for variable being plotted
                        var, ## string name of variable of interest
                        se_var, ## string name of variable storing standard error values
                        x, ## Number of samples for each region
                        palette) {
  
  ## Create matrix of sample values
  samps <- matrix(nrow = nrow(data), ncol = x)
  colnames(samps) <- paste("Sample", seq(1:x), sep = "_")
  
  ## Sample based on estimates and SEs in the data
  ## Assumes there is a column in the data labeled with the same name as the estimates plus an additional _se
  for (i in seq(1, nrow(data))) {
    samp <- rnorm(x, mean = data[[i,var]], sd = data[[i, se_var]])
    samps[i,] <- samp
  }
  
  ## Add sampled data to sf object
  samp_data <- samps %>% 
    cbind(data)
  
  ## Generate base map based on actual estimates
  base_map <- leaflet(samp_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(samp_data[[var]]),
                fillOpacity = 0.5,
                opacity = 0.4,
                smoothFactor = 0.5,
                weight = 2,
                color = "#444444",
                label = ~round(samp_data[[var]], 2),
                group = "Estimates") %>%
    addLegend("bottomright",
              pal = pal,
              values = samp_data[[var]])
  
  ## Add all sampled values as separate layers
  for (i in seq(1, x)) {
    variable <- paste("Sample", i, sep = "_")
    base_map <- base_map %>% addAdjPolygons(var = samps[,variable], group_name = variable, palette = pal)
  }
  
  ## Add layers control for toggling
  group_names <- colnames(samps)
  final_map <- base_map %>% addLayersControl(baseGroups = c("Estimates", group_names))
  
  return(final_map)
  
}
 

# pal <- colorBin("BuPu", c(0, max(halifax_sf$jail_pooled_pooled_p25) + max(halifax_sf$jail_pooled_pooled_p25_se)), bins = 5, na.color = "#fafafa")

# map_samples(data = halifax_sf, var = "jail_pooled_pooled_p25", se_var = "jail_pooled_pooled_p25_se", x = 10, palette = pal)

