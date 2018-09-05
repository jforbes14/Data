## This script maps each electorate at election time, onto its composition at census dates either side of the election.
## The idea is so that we can impute the profiles of the election boundaries, had they been in place at Census time.

## Only relevant for election that do not fall on census years: 2004, 2007, 2010, 2013
## Census for 2006 does not have a map of electorates for 2006, instead it has two version: 2004 and 2007. Use the year that is closer to the election of interest, but still interpolate from 2006.

## This script requires: sFsmall_abs01, sFsmall_abs06e04, sFsmall_abs06e07, sFsmall_abs11, sFsmall_abs16, sFsmall_aec10, sFsmall_aec13, sFsmall_aec16
## This script generates: Mapping_aec04, Mapping_aec07, Mapping_aec10, Mapping_aec13

library(sp)
library(tidyverse)
library(rgeos)
library(ggplot2)

# --- SIMPLIFY SHAPEFILES FOR RELEVANT CENSUS AND ELECTIONS ---
simp_aec04 <- as(gSimplify(sFsmall_abs06e04, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_aec04@data <- sFsmall_abs06e04@data

simp_aec07 <- as(gSimplify(sFsmall_abs06e07, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_aec07@data <- sFsmall_abs06e07@data

simp_aec10 <- as(gSimplify(sFsmall_aec10, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_aec10@data <- sFsmall_aec10@data

simp_aec13 <- as(gSimplify(sFsmall_aec13, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_aec13@data <- sFsmall_aec13@data

simp_aec16 <- as(gSimplify(sFsmall_aec16, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_aec16@data <- sFsmall_aec16@data

simp_abs01 <- as(gSimplify(sFsmall_abs01, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_abs01@data <- sFsmall_abs01@data

simp_abs11 <- as(gSimplify(sFsmall_abs11, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_abs11@data <- sFsmall_abs11@data

simp_abs16 <- as(gSimplify(sFsmall_abs16, tol = 0.0005), "SpatialPolygonsDataFrame")
simp_abs16@data <- sFsmall_abs16@data


# --- Loading required functions --- #
# Centroid
centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}
# Euclidean distance
cdist <- function(lat1, long1, lat2, long2) {
  dist_btwn = sqrt((lat2-lat1)^2 + (long2-long1)^2)
  return(dist_btwn)
}


# --- Add centroids to all shapefiles --- #
temp_polys <- as(simp_aec04, "SpatialPolygons")
centroids_aec04 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_aec04@data <- cbind(simp_aec04@data, centroids_aec04)

temp_polys <- as(simp_aec07, "SpatialPolygons")
centroids_aec07 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_aec07@data <- cbind(simp_aec07@data, centroids_aec07)

temp_polys <- as(simp_aec10, "SpatialPolygons")
centroids_aec10 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_aec10@data <- cbind(simp_aec10@data, centroids_aec10)

temp_polys <- as(simp_aec13, "SpatialPolygons")
centroids_aec13 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_aec13@data <- cbind(simp_aec13@data, centroids_aec13)

temp_polys <- as(simp_aec16, "SpatialPolygons")
centroids_aec16 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_aec16@data <- cbind(simp_aec16@data, centroids_aec16)

simp_abs04 <- simp_aec04
simp_abs07 <- simp_aec07

temp_polys <- as(simp_abs01, "SpatialPolygons")
centroids_abs01 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_abs01@data <- cbind(simp_abs01@data, centroids_abs01)

temp_polys <- as(simp_abs11, "SpatialPolygons")
centroids_abs11 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_abs11@data <- cbind(simp_abs11@data, centroids_abs11)

temp_polys <- as(simp_abs16, "SpatialPolygons")
centroids_abs16 <- seq_along(temp_polys) %>% purrr::map_df(centroid, polys=temp_polys) #centroids
simp_abs16@data <- cbind(simp_abs16@data, centroids_abs16)

remove(temp_polys)

save(simp_abs07, file = "data/simp_abs07.rda")
save(simp_abs11, file = "data/simp_abs11.rda")
save(simp_abs16, file = "data/simp_abs16.rda")


# --- 2004 Election --- #
# Placeholder
Mapping_aec04 <- data.frame(Election_Division = 0, Census_Division = 0, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area = 0)[-1,]

# For the previous census
elect_names <- simp_aec04$NAME_2004

# Loop for breakdown of electorates
for (i in 1:length(elect_names)) {
  
  elec_name <- elect_names[i]
  elec_poly <- simp_aec04 %>% subset(NAME_2004 == elec_name)
  elec_lat_c <- elec_poly$lat_c
  elec_long_c <- elec_poly$long_c
  elec_area <- gArea(elec_poly)
  
  # Load comparison electorates (2001 Census)
  comp <- simp_abs01@data %>%
    select(CED_NAME_2001, long_c, lat_c) %>% rowwise %>%
    mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
  
  comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
  
  cens_names_near <- comp$CED_NAME_2001[1:35] #names of 35 'nearest' electorates
  cens_mapped <- data.frame(Election_Division = elec_name, Census_Division = cens_names_near, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area=elec_area)
  
  for (j in 1:length(cens_names_near)) {
    cens_poly <- simp_abs01 %>% subset(CED_NAME_2001 == cens_names_near[j])
    poly_intersect <- gIntersection(elec_poly, cens_poly)
    if (!is.null(poly_intersect)) {
      area_intersect <- gArea(poly_intersect)
      cens_mapped$Intersect_Area[j] = area_intersect
      
    }
    
    # break if sum of intersection areas is over 99.5%
    if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > 0.995 ) break

  }
  
  cens_mapped$Year = "2001"  
  
  Mapping_aec04 <- rbind(Mapping_aec04, cens_mapped)
  
}

# Remove zero intersections
Mapping_aec04 <- filter(Mapping_aec04, Intersect_Area > 0)

# Adding area of Census divsions
cens_area_01 <- data.frame(Census_Division = simp_abs01$CED_NAME_2001, Census_Area = 0)

for (i in 1:nrow(cens_area_01)) {
  cens_area_01$Census_Area[i] = gArea(simp_abs01 %>% subset(CED_NAME_2001 == cens_area_01[i,1]))
}

for (i in 1:nrow(Mapping_aec04)) {
  Mapping_aec04$Census_Area[i] = cens_area_01[which(cens_area_01$Census_Division == Mapping_aec04$Census_Division[i]), 2]
}


# --- 2007 Election --- #
# Placeholder
Mapping_aec07 <- data.frame(Election_Division = 0, Census_Division = 0, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area = 0)[-1,]

# For the previous census
elect_names <- simp_aec07$NAME_2007

# Loop for breakdown of electorates
for (i in 1:length(elect_names)) {
  
  elec_name <- elect_names[i]
  elec_poly <- simp_aec07 %>% subset(NAME_2007 == elec_name)
  elec_lat_c <- elec_poly$lat_c
  elec_long_c <- elec_poly$long_c
  elec_area <- gArea(elec_poly)
  
  # Comparison with 2006 not required as the 2006 Census data is already in 2007 electorate format
  
  # Load comparison electorates (2011 Census)
  comp <- simp_abs11@data %>%
    select(CED_NAME, long_c, lat_c) %>% rowwise %>%
    mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
  
  comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
  
  cens_names_near <- comp$CED_NAME[1:35] #names of 35 'nearest' electorates
  cens_mapped <- data.frame(Election_Division = elec_name, Census_Division = cens_names_near, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area=elec_area)
  
  for (j in 1:length(cens_names_near)) {
    cens_poly <- simp_abs11 %>% subset(CED_NAME == cens_names_near[j])
    poly_intersect <- gIntersection(elec_poly, cens_poly)
    if (!is.null(poly_intersect)) {
      area_intersect <- gArea(poly_intersect)
      cens_mapped$Intersect_Area[j] = area_intersect
      cens_mapped$Census_Area[j] = gArea(cens_poly)
      
    }

    # break if sum of intersection areas is over 99.5%
    if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > 0.995 ) break
        
  }
  
  cens_mapped$Year = "2011"
  
  Mapping_aec07 <- rbind(Mapping_aec07, cens_mapped)
  
}

# Remove zero intersections
Mapping_aec07 <- filter(Mapping_aec07, Intersect_Area > 0)

# Adding area of 2011 Census divsions
cens_area_11 <- data.frame(Census_Division = simp_abs11$CED_NAME, Census_Area = 0)

for (i in 1:nrow(cens_area_11)) {
  cens_area_11$Census_Area[i] = gArea(simp_abs11 %>% subset(CED_NAME == cens_area_11[i,1]))
}

for (i in 1:nrow(Mapping_aec07)) {
  Mapping_aec07$Census_Area[i] = cens_area_11[which(cens_area_11$Census_Division == Mapping_aec07$Census_Division[i]), 2]
}



# --- 2010 Election --- #
# Placeholder
Mapping_aec10 <- data.frame(Election_Division = 0, Census_Division = 0, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area = 0)[-1,]

# For the previous census
elect_names <- simp_aec10$ELECT_DIV

# Loop for breakdown of electorates
for (i in 1:length(elect_names)) {

  elec_name <- elect_names[i]
  elec_poly <- simp_aec10 %>% subset(ELECT_DIV == elec_name)
  elec_lat_c <- elec_poly$lat_c
  elec_long_c <- elec_poly$long_c
  elec_area <- gArea(elec_poly)
  
  # Load comparison electorates (2006 Census - 2007 Election Format)
  comp <- simp_abs07@data %>%
    select(NAME_2007, long_c, lat_c) %>% rowwise %>%
    mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
  
  comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
  
  cens_names_near <- comp$NAME_2007[1:35] #names of 35 'nearest' electorates
  cens_mapped <- data.frame(Election_Division = elec_name, Census_Division = cens_names_near, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area=elec_area)
  
  for (j in 1:length(cens_names_near)) {
    cens_poly <- simp_abs07 %>% subset(NAME_2007 == cens_names_near[j])
    poly_intersect <- gIntersection(elec_poly, cens_poly)
    if (!is.null(poly_intersect)) {
      area_intersect <- gArea(poly_intersect)
      cens_mapped$Intersect_Area[j] = area_intersect
      cens_mapped$Census_Area[j] = gArea(cens_poly)
    }

    # break if sum of intersection areas is over 99.5%
    if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > 0.995 ) break
    
        
  }
  
  cens_mapped$Year = "2006"
  
  Mapping_aec10 <- rbind(Mapping_aec10, cens_mapped)
  
  
  # Load comparison electorates (2011 Census)
  comp <- simp_abs11@data %>%
    select(CED_NAME, long_c, lat_c) %>% rowwise %>%
    mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
  
  comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
  
  cens_names_near <- comp$CED_NAME[1:35] #names of 35 'nearest' electorates
  cens_mapped <- data.frame(Election_Division = elec_name, Census_Division = cens_names_near, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area=elec_area)
  
  
  for (j in 1:length(cens_names_near)) {
    cens_poly <- simp_abs11 %>% subset(CED_NAME == cens_names_near[j])
    poly_intersect <- gIntersection(elec_poly, cens_poly)
    if (!is.null(poly_intersect)) {
      area_intersect <- gArea(poly_intersect)
      cens_mapped$Intersect_Area[j] = area_intersect
      cens_mapped$Census_Area[j] = gArea(cens_poly)
    }
    
    # break if sum of intersection areas is over 99.5%
    if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > 0.995 ) break
    
  }
  
  cens_mapped$Year = "2011"
  Mapping_aec10 <- rbind(Mapping_aec10, cens_mapped)

}  

# Remove zero intersections
Mapping_aec10 <- filter(Mapping_aec10, Intersect_Area > 0)

# 2006 Census (2007 Format) Divsion Areas
cens_area_06 <- data.frame(Census_Division = simp_abs07$NAME_2007, Census_Area = 0)
  
  for (i in 1:nrow(cens_area_06)) {
    cens_area_06$Census_Area[i] = gArea(simp_abs07 %>% subset(NAME_2007 == cens_area_06[i,1]))
  }
  

# --- 2013 Election --- #
# Placeholder
Mapping_aec13 <- data.frame(Election_Division = 0, Census_Division = 0, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area = 0)[-1,]

# For the previous census
elect_names <- simp_aec13$Elect_div

# Loop for breakdown of electorates
for (i in 1:length(elect_names)) {
  
  elec_name <- elect_names[i]
  elec_poly <- simp_aec13 %>% subset(Elect_div == elec_name)
  elec_lat_c <- elec_poly$lat_c
  elec_long_c <- elec_poly$long_c
  elec_area <- gArea(elec_poly)
  
  # Load comparison electorates (2011 Census)
  comp <- simp_abs11@data %>%
    select(CED_NAME, long_c, lat_c) %>% rowwise %>%
    mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
  
  comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
  
  cens_names_near <- comp$CED_NAME[1:35] #names of 35 'nearest' electorates
  cens_mapped <- data.frame(Election_Division = elec_name, Census_Division = cens_names_near, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area=elec_area)
  
  for (j in 1:length(cens_names_near)) {
    cens_poly <- simp_abs11 %>% subset(CED_NAME == cens_names_near[j])
    poly_intersect <- gIntersection(elec_poly, cens_poly)
    if (!is.null(poly_intersect)) {
      area_intersect <- gArea(poly_intersect)
      cens_mapped$Intersect_Area[j] = area_intersect
      cens_mapped$Census_Area[j] = gArea(cens_poly)
    }

    # break if sum of intersection areas is over 99.5%
    if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > 0.995 ) break

  }
  
  cens_mapped$Year = "2011"  
  Mapping_aec13 <- rbind(Mapping_aec13, cens_mapped)
  
  # Load comparison electorates (2016 Census)
  comp <- simp_abs16@data %>%
    select(CED_NAME, long_c, lat_c) %>% rowwise %>%
    mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
  
  comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
  
  cens_names_near <- comp$CED_NAME[1:35] #names of 35 'nearest' electorates
  cens_mapped <- data.frame(Election_Division = elec_name, Census_Division = cens_names_near, Year = 0, Intersect_Area = 0, Census_Area = 0, Election_Area=elec_area)
  
  for (j in 1:length(cens_names_near)) {
    cens_poly <- simp_abs16 %>% subset(CED_NAME == cens_names_near[j])
    poly_intersect <- gIntersection(elec_poly, cens_poly)
    if (!is.null(poly_intersect)) {
      area_intersect <- gArea(poly_intersect)
      cens_mapped$Intersect_Area[j] = area_intersect
      cens_mapped$Census_Area[j] = gArea(cens_poly)
    }
    
    # break if sum of intersection areas is over 99.5%
    if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > 0.995 ) break
    
  }
  
  cens_mapped$Year = "2016"
  Mapping_aec13 <- rbind(Mapping_aec13, cens_mapped)
  
}

# Remove zero intersections
Mapping_aec13 <- filter(Mapping_aec13, Intersect_Area > 0)


  
# Add Election year labels
Mapping_aec13$Election_Year = "2013"
Mapping_aec10$Election_Year = "2010"
Mapping_aec07$Election_Year = "2007"
Mapping_aec04$Election_Year = "2004"




# --- CALCULATE PERCENTAGE BREAKDOWN OF AREAS --- #
# Adding percentage of Election division made up by intersection, and percentage of Census division made up by intersection
Mapping_aec13 <- Mapping_aec13 %>%
  mutate(Percent_Elec_Composition = Intersect_Area/Election_Area,
         Percent_Cens_Composition = Intersect_Area/Census_Area)

Mapping_aec10 <- Mapping_aec10 %>%
  mutate(Percent_Elec_Composition = Intersect_Area/Election_Area,
         Percent_Cens_Composition = Intersect_Area/Census_Area)

Mapping_aec07 <- Mapping_aec07 %>%
  mutate(Percent_Elec_Composition = Intersect_Area/Election_Area,
         Percent_Cens_Composition = Intersect_Area/Census_Area)

Mapping_aec04 <- Mapping_aec04 %>%
  mutate(Percent_Elec_Composition = Intersect_Area/Election_Area,
         Percent_Cens_Composition = Intersect_Area/Census_Area)


# --- STORING MAPPING_AEC DUPLICATES FOR SAFETY --- #
#STORE_Mapping_aec13 <- Mapping_aec13
#STORE_Mapping_aec10 <- Mapping_aec10
#STORE_Mapping_aec07 <- Mapping_aec07
#STORE_Mapping_aec04 <- Mapping_aec04

# --- Export --- #
save(Mapping_aec04, file="Clean/Maps/Mapping_aec04.rda")
save(Mapping_aec07, file="Clean/Maps/Mapping_aec07.rda")
save(Mapping_aec10, file="Clean/Maps/Mapping_aec10.rda")
save(Mapping_aec13, file="Clean/Maps/Mapping_aec13.rda")
