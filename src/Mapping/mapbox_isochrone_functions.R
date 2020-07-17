
library(httr)
library(jsonlite)

## Construct api request path for mapbox isochrones
create_path <- function(profile, lat, lon, minutes, token) {
  api_path <- paste0("https://api.mapbox.com/isochrone/v1/mapbox/", profile, "/", lon, "%2C", lat, "?contours_minutes=", minutes, "&polygons=true&access_token=", token)
  return(api_path)
}

## Make single mapbox API request for isochrone
get_isochrone <- function(profile, lat, lon, minutes, token) {
  
  ## HTTP request from API
  path <- create_path(profile, lat, lon, minutes, token)
  req <- GET(path)
  status <- http_status(req)
  
  ## Convert response to sf format if request successful
  if (status$category == "Success") {
    req_sf <- st_read(content(req, as = "text"))  
  } else {
    stop(status$message)
  }
  
  return(req_sf)
  
}

## Function to pull isochrones from the mapbox API for a set of profiles, coordinates, and travel times
get_multi_isochrones <- function(profiles, ## vector of profiles desired (driving, cycling, walking) 
                                 coords, ## data frame with three columns: column 1 = the label for that point location, column 2 = latitude, column 3 = longitude
                                 coord_lat_col = "lat", ## Name of latitude column in data
                                 coord_lon_col = "lon", ## Name of longitude column in data
                                 coord_id_col, ## column name that identifies each point
                                 minutes, ## numeric vector of minutes for the isochrones. Order matters for plotting
                                 token ## API token
) {
  
  ## Initial values for iteration
  isochrones_list <- list()
  i <- 1
  
  ## Iterate through input vectors
  for (profile in profiles) {
    for (coord_pair in seq(1, nrow(coords)))  {
      for (minute in minutes) {
        
        ## Get point labels and latitudes for the curent point
        points <- coords[coord_pair,]
        
        label <- points[[coord_id_col]]
        
        lat <- points[[coord_lat_col]]
        lon <- points[[coord_lon_col]]
        
        ## Get isochrones from API and add to list. Remove unnecessary columns (can be input by user later) and store info about profile and label for each polygon
        isochrones_list[[i]] <- get_isochrone(profile, lat, lon, minute, token) %>% 
          select(-all_of(c("fillOpacity", "color", "fill", "fillColor", "opacity", "fill.opacity"))) %>%
          mutate(profile = profile, label = label)
        
        i <- i + 1
        
      }
    }
  }
  
  ## Combine into single source
  isochrones_sf <- do.call(bind_rows, isochrones_list)
  
  return(isochrones_sf)
}

## Function to be used with a leaflet map to plot isochrone polygons
plot_multi_isochrones <- function(map,
                                  data, ## sf object of isochrone polygons, like the ones returned by get_multi_isochrones
                                  color_var = "contour", ## variable to color isochrones by. Most likely will be the contour (time) column returned by the mapbox API
                                  label_var = "none", ## variable to label isochrone polygons with
                                  group_var = label_var, ## variable to group by for the basemap layers control
                                  palette, ## color palette for polygons
                                  opacity = 1,
                                  weight = 1,
                                  fillOpacity = 0.5) {
  
  if (label_var == "none") {
    for (poly in nrow(data)) {
      map <- map %>%
        addPolygons(data = data,
                    fillColor = ~palette(data[[color_var]]),
                    color = ~palette(data[[color_var]]),
                    opacity = opacity,
                    weight = weight,
                    fillOpacity = fillOpacity,
                    group = ~data[[group_var]],
                    highlightOptions = highlightOptions(opacity = 1, fillOpacity = ifelse(fillOpacity >= 0.8, 1, fillOpacity + 0.2)))
    }
    
  } else {
    for (poly in nrow(data)) {
      map <- map %>%
        addPolygons(data = data,
                    fillColor = ~palette(data[[color_var]]),
                    color = ~palette(data[[color_var]]),
                    opacity = opacity,
                    weight = weight,
                    fillOpacity = fillOpacity,
                    label = ~data[[label_var]],
                    group = ~data[[group_var]],
                    highlightOptions = highlightOptions(opacity = 1, fillOpacity = ifelse(fillOpacity >= 0.8, 1, fillOpacity + 0.2)))
    }
  }
  
  return(map)
  
}





