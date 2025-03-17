# BEM March 2025
library(sf) # 1.0

# converts the `sp` class objects from version 1.3 to the `sf` class objects
# in version 2.0, which is still a supported library

sp_list <- ClelandEcoregions::Cleland2007_eco_map

prov_poly <- sp_list[[1]]
prov_poly <- sf::st_as_sf(prov_poly)
# make sure it is a normal crs, try 4326
prov_poly <- sf::st_transform(prov_poly, 'EPSG:4326')
sf::st_is_valid(prov_poly)
# poly 18 is invalid
prov_poly <- sf::st_make_valid(prov_poly)
stopifnot(all(sf::st_is_valid(prov_poly)))

sect_poly <- sp_list[[2]]
sect_poly <- sf::st_as_sf(sect_poly)
sect_poly <- sf::st_transform(sect_poly, 'EPSG:4326')
sf::st_is_valid(sect_poly)
sect_poly <- sf::st_make_valid(sect_poly)
stopifnot(all(sf::st_is_valid(sect_poly)))

subsect_poly <- sp_list[[3]]
subsect_poly <- sf::st_as_sf(subsect_poly)
subsect_poly <- sf::st_transform(subsect_poly, 'EPSG:4326')
sf::st_is_valid(subsect_poly)
subsect_poly <- sf::st_make_valid(subsect_poly)
stopifnot(all(sf::st_is_valid(subsect_poly)))

# names of list should be province, sections, subsects
Cleland2007_eco_map <- list(prov_poly, sect_poly, subsect_poly)
names(Cleland2007_eco_map) <- c('province', 'sections', 'subsects')
usethis::use_data(Cleland2007_eco_map, overwrite = T)
