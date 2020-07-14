
library(httr)
library(jsonlite)

## Construct api request path for mapbox isochrones
create_path <- function(profile, lat, lon, minutes, token) {
  api_path <- paste0("https://api.mapbox.com/isochrone/v1/mapbox/", profile, "/", lon, "%2C", lat, "?contours_minutes=", minutes, "&polygons=true&access_token=", token)
  return(api_path)
}

## Make API request for isochrone
get_isochrone <- function(profile, lat, lon, minutes, token) {
  
  path <- create_path(profile, lat, lon, minutes, token)
  req <- GET(path)
  status <- http_status(req)
  
  if (status$category == "Success") {
    req_sf <- st_read(content(req, as = "text"))  
  } else {
    stop(status$message)
  }
  
  return(req_sf)
  
}

get_multi_isochrones <- function(profiles, ## vector of profiles desired (driving, cycling, walking) 
                                 coords, ## data frame with three columns: column 1 = the label for that point location, column 2 = latitude, column 3 = longitude
                                 minutes, ## numeric vector of minutes for the isochrones. Order matters for plotting
                                 token ## API token
) {
  
  isochrones_list <- list()
  i <- 1
  
  for (profile in profiles) {
    for (coord_pair in seq(1, nrow(coords)))  {
      for (minute in minutes) {
        
        points <- coords[coord_pair,]
        
        label <- points[[1]]
        
        lat <- points[[2]]
        lon <- points[[3]]
        
        isochrones_list[[i]] <- get_isochrone(profile, lat, lon, minute, token) %>% 
          select(-all_of(c("fillOpacity", "color", "fill", "fillColor", "opacity", "fill.opacity"))) %>%
          mutate(profile = profile, label = label)
        
        i <- i + 1
        
      }
    }
  }
  
  isochrones_sf <- do.call(bind_rows, isochrones_list)
  
  return(isochrones_sf)
}

plot_multi_isochrones <- function(map,
                            data,
                            color_var,
                            label_var,
                            group_var = label_var,
                            palette,
                            color = "gray",
                            opacity = 1,
                            weight = 1,
                            fillOpacity = 0.5) {
  
  for (lab in unique(data[[label_var]])) {
    
    map <- map %>%
      addPolygons(data = data,
                  fillColor = ~palette(data[[color_var]]),
                  color = color,
                  opacity = opacity,
                  weight = weight,
                  fillOpacity = fillOpacity,
                  label = ~data[[label_var]],
                  group = ~data[[group_var]])
    
    
  }
  
  return(map)

}





