########################################################################

# This script contains the function to determine the postal area mapping
# for electorates. It uses Censuses on both sides of the election.

########################################################################

library(tidyverse)
library(sp)
library(rgeos)




########################################################################

# FUNCTION TO DO MAPPING FOR EACH ELECTION USING POA

########################################################################



Mapping_POA_fn <- function(simp_aec, simp_abs_POA_1, simp_abs_POA_2, 
                           area_thres = 0.995, census_year_1, census_year_2) {
  
  # Placeholder
  Mapping_POA_aec <- data.frame(Election_Division = 0, POA = 0, Year = 0, Intersect_Area = 0, POA_Area = 0, Election_Area = 0)[-1,]
  
  ## CENSUS BEFORE
  
  # For the previous census
  elect_names <- simp_aec$Electorate
  
  # Loop for breakdown of electorates
  for (i in 1:length(elect_names)) {
    
    elec_name <- elect_names[i]
    elec_poly <- simp_aec %>% subset(as.character(Electorate) == as.character(elec_name))
    elec_lat_c <- elec_poly$lat_c
    elec_long_c <- elec_poly$long_c
    elec_area <- gArea(elec_poly)
    
    
    # Load comparison electorates (2001 Census)
    comp <- simp_abs_POA_1@data %>%
      select(POA_CODE, long_c, lat_c) %>% rowwise %>%
      mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
    
    comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
    
    cens_names_near <- comp$POA_CODE #names of postal areas - can change to nearest X if desired
    cens_mapped <- data.frame(Election_Division = elec_name, POA = cens_names_near, Year = 0, Intersect_Area = 0, POA_Area = 0, Election_Area=elec_area)
    
    # Loop for that electorate until break
    for (j in 1:length(cens_names_near)) {
      cens_poly <- simp_abs_POA_1 %>% subset(POA_CODE == cens_names_near[j])
      
      if (gIntersects(elec_poly, cens_poly)) { # Only if polygons intersect
        poly_intersect <- gIntersection(elec_poly, cens_poly)
        cens_mapped$Intersect_Area[j] = gArea(poly_intersect)
      }
      
      # break if sum of intersection areas is over threshold (area_thres)
      if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > area_thres ) break
      
    }
    
    print(paste(census_year_1,i))
    
    cens_mapped$Year = census_year_1  
    
    Mapping_POA_aec <- rbind(Mapping_POA_aec, cens_mapped)
    
  }
  
  # Remove zero intersections
  Mapping_POA_aec <- filter(Mapping_POA_aec, Intersect_Area > 0)
  
  # Adding area of Census divsions
  cens_area_1 <- data.frame(POA = simp_abs_POA_1$POA_CODE, POA_Area = 0)
  
  for (i in 1:nrow(cens_area_1)) {
    cens_area_1$POA_Area[i] = gArea(simp_abs_POA_1 %>% subset(POA_CODE == cens_area_1[i,1]))
  }
  
  for (i in 1:nrow(Mapping_POA_aec)) {
    Mapping_POA_aec$POA_Area[i] = cens_area_1[which(cens_area_1$POA == Mapping_POA_aec$POA[i]), 2]
  }
  
  ## CENSUS AFTER
  
  # Loop for breakdown of electorates
  for (i in 1:length(elect_names)) {
    
    elec_name <- elect_names[i]
    elec_poly <- simp_aec %>% subset(as.character(Electorate) == as.character(elec_name))
    elec_lat_c <- elec_poly$lat_c
    elec_long_c <- elec_poly$long_c
    elec_area <- gArea(elec_poly)
    
    
    # Load comparison electorates (2006 Census)
    comp <- simp_abs_POA_2@data %>%
      select(POA_CODE, long_c, lat_c) %>% rowwise %>%
      mutate(dist_centroid = cdist(lat_c, long_c, elec_lat_c, elec_long_c))
    
    comp <- comp[order(comp$dist_centroid),] #order by distance to centroid
    
    cens_names_near <- comp$POA_CODE #names of postal areas - can change to nearest X if desired
    cens_mapped <- data.frame(Election_Division = elec_name, POA = cens_names_near, Year = 0, Intersect_Area = 0, POA_Area = 0, Election_Area=elec_area)
    
    # Loop for that electorate until break
    for (j in 1:length(cens_names_near)) {
      
      cens_poly <- simp_abs_POA_2 %>% subset(POA_CODE == cens_names_near[j])
      
      if (gIntersects(elec_poly, cens_poly)) { # Only if polygons intersect
        poly_intersect <- gIntersection(elec_poly, cens_poly)
        cens_mapped$Intersect_Area[j] = gArea(poly_intersect)
      }
      
      # break if sum of intersection areas is over threshold (area_thres)
      if ( (sum(cens_mapped$Intersect_Area[1:j])/elec_area) > area_thres ) break

    }
    
    
    cens_mapped$Year = census_year_2  
    
    Mapping_POA_aec <- rbind(Mapping_POA_aec, cens_mapped)
    
    print(paste(census_year_2,i))
  }
  
  
  # Remove zero intersections
  Mapping_POA_aec <- filter(Mapping_POA_aec, Intersect_Area > 0)

  # Adding areas from Census 2006
  cens_area_2 <- data.frame(POA = simp_abs_POA_2$POA_CODE, POA_Area = 0)
  
  for (i in 1:nrow(cens_area_2)) {
    cens_area_2$POA_Area[i] = gArea(simp_abs_POA_2 %>% subset(POA_CODE == cens_area_2[i,1]))
  }
  
  for (i in 1:nrow(Mapping_POA_aec)) {
    if (Mapping_POA_aec$Year[i] == census_year_2) {
      Mapping_POA_aec$POA_Area[i] = cens_area_2[which(as.character(cens_area_2$POA) == as.character(Mapping_POA_aec$POA[i])), 2]    
    }
  }
  
  ### Adding percentage of Census and Electorate intersections
  Mapping_POA_aec <- Mapping_POA_aec %>%
    mutate(Percent_Elec_Composition = Intersect_Area/Election_Area,
           Percent_POA_Composition = Intersect_Area/POA_Area)
  
  return(Mapping_POA_aec)
}

########################################################################

# Running for each election

########################################################################


# Election 2004
Mapping_POA_aec04 <- Mapping_POA_fn(simp_aec = simp_aec04, simp_abs_POA_1 = simp_abs01_POA, 
                              simp_abs_POA_2 = simp_abs06_POA, area_thres = 0.995,
                               census_year_1 = "2001", census_year_2 = "2006")

# Election 2007
Mapping_POA_aec07 <- Mapping_POA_fn(simp_aec = simp_aec07, simp_abs_POA_1 = simp_abs06_POA, 
                               simp_abs_POA_2 = simp_abs11_POA, area_thres = 0.995,
                               census_year_1 = "2006", census_year_2 = "2011")

# Election 2010
Mapping_POA_aec10 <- Mapping_POA_fn(simp_aec = simp_aec10, simp_abs_POA_1 = simp_abs06_POA, 
                               simp_abs_POA_2 = simp_abs11_POA, area_thres = 0.995,
                               census_year_1 = "2006", census_year_2 = "2011")

# Election 2013
Mapping_POA_aec13 <- Mapping_POA_fn(simp_aec = simp_aec13, simp_abs_POA_1 = simp_abs11_POA, 
                               simp_abs_POA_2 = simp_abs16_POA, area_thres = 0.995,
                               census_year_1 = "2011", census_year_2 = "2016")


# Add Election year labels
Mapping_POA_aec13$Election_Year = "2013"
Mapping_POA_aec10$Election_Year = "2010"
Mapping_POA_aec07$Election_Year = "2007"
Mapping_POA_aec04$Election_Year = "2004"


# --- Export --- #
save(Mapping_POA_aec04, file="Clean/Maps/Mapping_POA_aec04.rda")
save(Mapping_POA_aec07, file="Clean/Maps/Mapping_POA_aec07.rda")
save(Mapping_POA_aec10, file="Clean/Maps/Mapping_POA_aec10.rda")
save(Mapping_POA_aec13, file="Clean/Maps/Mapping_POA_aec13.rda")



##################################################################################
# POA: Running for 2011 using 2006 and 2016 for the comparison of methods
##################################################################################
#simp_aec11 <- simp_abs11
#simp_aec11@data <- simp_abs11@data %>% 
#  rename(Electorate = CED_NAME)

#Mapping_POA_abs11 <- Mapping_POA_fn(simp_aec = simp_aec11, simp_abs_POA_1 = simp_abs06_POA, 
#                                    simp_abs_POA_2 = simp_abs16_POA, area_thres = 0.995,
#                                    census_year_1 = "2006", census_year_2 = "2016")


#save(Mapping_POA_abs11, file="data/Mapping_POA_abs11.rda")


##################################################################################
# CED: Running for 2011 using 2006 and 2016 for the comparison of methods
##################################################################################
#simp_aec11 <- simp_abs11
#simp_aec11@data <- simp_abs11@data %>% 
#  rename(Electorate = CED_NAME)

#use_06 <- simp_abs07
#use_06@data <- use_06@data %>% 
#  rename(POA_CODE = NAME_2007)

#use_16 <- simp_abs16
#use_16@data <- use_16@data %>% 
#  rename(POA_CODE = CED_NAME)

#Mapping_abs11 <- Mapping_POA_fn(simp_aec = simp_aec11, simp_abs_POA_1 = use_06, 
#                                    simp_abs_POA_2 = use_16, area_thres = 0.995,
#                                    census_year_1 = "2006", census_year_2 = "2016")


#save(Mapping_abs11, file="data/Mapping_abs11.rda")

#### CASE OF 2011 AGGREGATING UP
#use_11 <- simp_abs11
#use_11@data <- use_11@data %>% 
#  rename(POA_CODE = CED_NAME)
#testing_2011_case <- Mapping_POA_fn(simp_aec = simp_aec11, simp_abs_POA_1 = simp_abs11_POA, 
#                                simp_abs_POA_2 = simp_abs11_POA, area_thres = 0.995,
#                                census_year_1 = "2006", census_year_2 = "2016")

#save(testing_2011_case, file="data/testing_2011_case.rda")

