
library(rvest)
library(stringr)

#
#
# This script scapes the ICPSR website to get a dictionary of mappings between counties and local police ORI agency codes
# Can be used to join on VABeyond Crime data (or other crime data where only ORI is reported without county)
# Easily scaled to other states, which are stored in other similar website links.
#
#


## Read in website with ORI/County pairings for VA
website <- read_html("https://www.icpsr.umich.edu/files/NACJD/ORIs/51oris.html")

## Extract county names (contained in h3 headings)
headers <- website %>% html_nodes("h3 a")
counties <- html_text(headers) %>%
  str_split(" \\(")

## Convert county names to vector
counties <- unlist(lapply(counties, function(x) x[[1]]))

## Extract ORI numbers
oris <- website %>% 
  html_nodes("body pre") %>%
  html_text()

## Clean whitespace, split at start list of agencies in the county and pull out string of these agencies
oris_trimmed <- str_replace_all(oris, " ", "")
oris_trimmed <- lapply(str_split(oris_trimmed, "\\n\\n"), function(x) x[[2]])

## Extract 9-digit ORIs for each agency. Some are state police ORIs with "SP" in the middle, others are just 7 digits after a "VA"
sp_oris <- str_extract_all(oris_trimmed, "(VA)[0-9]{3}SP00")
non_sp_oris <- str_extract_all(oris_trimmed, "(VA)[0-9]{7}")

## Attach county names to the ORI list
names(sp_oris) <- counties
names(non_sp_oris) <- counties

## Join the lists of state police ORIs and local agency ORIs for each county
keys <- unique(c(names(sp_oris), names(non_sp_oris)))

county_ori_map <- purrr::map2(sp_oris[keys], non_sp_oris[keys], c) %>% 
  set_names(keys) %>%
  unlist() %>%
  as.data.frame()

## Clean up dataframe for later joining
county_ori_map$county <- str_extract(paste0(row.names(county_ori_map), "9"), "^(.*?)(?=[0-9])")
row.names(county_ori_map) <- seq(1, nrow(county_ori_map))
colnames(county_ori_map) <- c("ORI", "COUNTY")

# readr::write_csv(county_ori_map, here::here("data", "original", "Crime", "full_va_crime", "ori_county_mappings.csv"))

