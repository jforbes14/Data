
----------------------------------------------------------------------------------------
  
## README
  
# This file is the final step in imputing sociodemographic information for each division at election time. 

# For each election year, we will interpolate between the censuses either side of the election. The census profiles contain information about each division, at census time. 

# We have previously computed how much of each Census divisions is made up by the overlapping election divisions (different points in time), and use this mapping to impute Census information so that it aligns with the election division boundaries.

# A reminder - when a census and election fall on the same year, we directly join the census information to the election.

# The required files are:
  
# - Mapping_POA_aecXX: which determines the area composition of each election division, by census divisions in the corresponding election division boundary. XX represents the election year suffix {04, 07, 10 and 13}.

# - POA_abs20XX: census profiles for each census division (sociodemographics). XX represents the census year suffix {01, 06, 11 and 16}.


----------------------------------------------------------------------------------------
  
library(tidyverse)

load("./Clean/POA_abs2001.rda")
POA_abs2001$year = "abs2001"

load("./Clean/POA_abs2006.rda")
POA_abs2006$year = "abs2006"

load("./Clean/POA_abs2011.rda")
POA_abs2011$year = "abs2011"

load("./Clean/POA_abs2016.rda")
POA_abs2016$year = "abs2016"

load("./Clean/Maps/Mapping_POA_aec04.rda")
load("./Clean/Maps/Mapping_POA_aec07.rda")
load("./Clean/Maps/Mapping_POA_aec10.rda")
load("./Clean/Maps/Mapping_POA_aec13.rda")

###########################################################################

# Writing a function to do the imputation

###########################################################################

#election_year, year_census_1, year_census_2
Mapping_POA_aec = Mapping_POA_aec04
year_election = 2004
POA_abs1 = POA_abs2001
POA_abs2 = POA_abs2006
year_census_1 = 2001
year_census_2 = 2006

interpolate_census <- function(Mapping_POA_aec, year_election, 
                               POA_abs1, POA_abs2, 
                               year_census_1, year_census_2) {

# Formatting
  year_census_1 <- as.character(year_census_1)
  year_census_2 <- as.character(year_census_2)
  Mapping_POA_aec$Election_Division <- droplevels(Mapping_POA_aec$Election_Division)
  
  Mapping_POA_aec$POA %>% as.character() ->
    Mapping_POA_aec$POA
  POA_abs1$POA %>% as.character() ->
    POA_abs1$POA
  POA_abs2$POA %>% as.character() ->
    POA_abs2$POA
  
# 2001 Census POAs for 2004 Election

# Filter for 2001 and those that have Census data
p1 <- Mapping_POA_aec %>%
  filter(Year == year_census_1,
         POA %in% POA_abs1$POA) %>%
  mutate(Census_Population = 0, Imputed_Population = 0)

for (i in 1:nrow(p1)) {
  p1$Census_Population[i] = POA_abs1$Population[which(POA_abs1$POA == p1$POA[i])]
  # Imputed population (from Census division i) = (Census Population X Area intersection/Area Census division)
  p1$Imputed_Population[i] = p1$Census_Population[i]*p1$Percent_POA_Composition[i]
}

# Proportion of total imputed population
Total_pop <- aggregate(Imputed_Population ~ Election_Division, data = p1, sum)

p1$Total_Imputed_Population = 0

for (i in 1:nrow(p1)) {
  p1$Total_Imputed_Population[i] = Total_pop$Imputed_Population[which(Total_pop$Election_Division == p1$Election_Division[i])]
}

p1$Proportion_Imputed_Population = p1$Imputed_Population/p1$Total_Imputed_Population

# Remove imputed populations making up less than 1% of total imputed population
#p1 <- filter(p1, Proportion_Imputed_Population >= 0.01)


# Impute sociodemographics
# Election Divisions as factor
p1$Election_Division <- as.factor(p1$Election_Division)

# POA_abs1 data frame with numeric variables only 
num_POA_abs1 <- POA_abs1[,unlist(lapply(POA_abs1, is.numeric))]

# Empty data frame for 2001 imputed socio-demographics for 2004 election
sd_1 <- num_POA_abs1[1:150,] #150 rows
sd_1[,] = 0
sd_1$Election_Division = 0

# Loop

for (i in 1:length(levels(p1$Election_Division))) {
  Electorate_Name = levels(p1$Election_Division)[i]
  Intersect_POAs = filter(p1, Election_Division == Electorate_Name) %>%
    select(POA, Proportion_Imputed_Population, Total_Imputed_Population)
  Population = Intersect_POAs$Total_Imputed_Population[1]
  
  sd_profiles = num_POA_abs1[1:nrow(Intersect_POAs),]
  sd_profiles[,]=0
  
  for (j in 1:nrow(Intersect_POAs)) {
    POA = Intersect_POAs$POA[j]
    sd_profiles[j,] = num_POA_abs1[which(POA_abs1$POA == POA),]
  }
  
  weighted_sd_profiles = sd_profiles[1,]
  weighted_sd_profiles[,] = 0
  
  for (k in 1:ncol(weighted_sd_profiles)) {
    
    weighted_sd_profiles[,k] = sum(
      sd_profiles[!is.na(sd_profiles[,k]), k] * (Intersect_POAs$Proportion_Imputed_Population[which(!is.na(sd_profiles[,k]))] 
                                                 / sum(Intersect_POAs$Proportion_Imputed_Population[which(!is.na(sd_profiles[,k]))]))
    )
    # weighted average of those that aren't NA
  }
  
  rownames(weighted_sd_profiles) = NULL
  
  sd_1$Election_Division[i] = Electorate_Name
  sd_1[i, 1:(ncol(sd_1) - 1)] = weighted_sd_profiles
  
  sd_1$Population[i] = Population
  
}

sd_1$Election_Division <- as.factor(sd_1$Election_Division)


# 2006 Census POAs for 2004 Election

# Filter for 2006 and those that have Census data
p2 <- Mapping_POA_aec %>%
  filter(Year == year_census_2,
         POA %in% POA_abs2$POA) %>%
  mutate(Census_Population = 0, Imputed_Population = 0)

for (i in 1:nrow(p2)) {
  p2$Census_Population[i] = POA_abs2$Population[which(POA_abs2$POA == p2$POA[i])]
  # Imputed population (from Census division i) = (Census Population X Area intersection/Area Census division)
  p2$Imputed_Population[i] = p2$Census_Population[i]*p2$Percent_POA_Composition[i]
}

# Proportion of total imputed population
Total_pop <- aggregate(Imputed_Population ~ Election_Division, data = p2, sum)

p2$Total_Imputed_Population = 0

for (i in 1:nrow(p2)) {
  p2$Total_Imputed_Population[i] = Total_pop$Imputed_Population[which(Total_pop$Election_Division == p2$Election_Division[i])]
}

p2$Proportion_Imputed_Population = p2$Imputed_Population/p2$Total_Imputed_Population

# Remove imputed populations making up less than 1% of total imputed population
#p2 <- filter(p2, Proportion_Imputed_Population >= 0.01)


# Imputing sociodemographics
# Election Divisions as factor
p2$Election_Division <- as.factor(p2$Election_Division)

# POA_abs2 data frame with numeric variables only 
num_POA_abs2 <- POA_abs2[,unlist(lapply(POA_abs2, is.numeric))]

# Empty data frame for 2006 imputed socio-demographics for 2004 election
sd_2 <- num_POA_abs2[1:150,] #150 rows
sd_2[,] = 0
sd_2$Election_Division = 0

# Loop

for (i in 1:length(levels(p2$Election_Division))) {
  Electorate_Name = levels(p2$Election_Division)[i]
  Intersect_POAs = filter(p2, Election_Division == Electorate_Name) %>%
    select(POA, Proportion_Imputed_Population, Total_Imputed_Population)
  Population = Intersect_POAs$Total_Imputed_Population[1]
  
  sd_profiles = num_POA_abs2[1:nrow(Intersect_POAs),]
  sd_profiles[,]=0
  
  for (j in 1:nrow(Intersect_POAs)) {
    POA = Intersect_POAs$POA[j]
    sd_profiles[j,] = num_POA_abs2[which(POA_abs2$POA == POA),]
  }
  
  weighted_sd_profiles = sd_profiles[1,]
  weighted_sd_profiles[,] = 0
  
  for (k in 1:ncol(weighted_sd_profiles)) {
    
    weighted_sd_profiles[,k] = sum(
      sd_profiles[!is.na(sd_profiles[,k]) ,k] * Intersect_POAs$Proportion_Imputed_Population[which(!is.na(sd_profiles[,k]))]
    ) / sum(Intersect_POAs$Proportion_Imputed_Population[which(!is.na(sd_profiles[,k]))])
    # weighted average of those that aren't NA
    
  }
  
  rownames(weighted_sd_profiles) = NULL
  
  sd_2$Election_Division[i] = Electorate_Name
  sd_2[i, 1:(ncol(sd_2) - 1)] = weighted_sd_profiles
  
  sd_2$Population[i] = Population
}

sd_2$Election_Division <- as.factor(sd_2$Election_Division)


# Interpolate
year_census_1 <- as.numeric(year_census_1)
year_census_2 <- as.numeric(year_census_2)
year_election <- as.numeric(year_election)

# Rows are matching
sd_1 <- sd_1[match(levels(sd_1$Election_Division), sd_1$Election_Division),]
sd_2 <- sd_2[match(levels(sd_2$Election_Division), sd_2$Election_Division),]

if (sum(sd_1$Election_Division == sd_2$Election_Division) != 150) {
  print("WARNING: Election divisions do not match!")
} #print if rows have not been aligned correctly

# Columns are matching
sd_1 <- sd_1[,order(colnames(sd_1))]
sd_2 <- sd_2[,order(colnames(sd_2))]

if (sum(names(sd_1) != names(sd_2)) != 0) {
  print("WARNING: Column names do not match!")
} #print if rows have not been aligned correctly


w1 = (1 - abs(year_election - year_census_1)/(year_census_2 - year_census_1))
w2 = (1 - abs(year_election - year_census_2)/(year_census_2 - year_census_1))

# Interpolate over time (between Census years)
sd_electorates <- w1*(sd_1 %>% select(-Election_Division)) + w2*(sd_2 %>% select(-Election_Division))

sd_electorates$Election_Division = sd_1$Election_Division


return(sd_electorates)

}



###########################################################################

# Adjust for inflation

###########################################################################


# Sourced from Reserve Bank of Australia: Inflation Calculation
# https://www.rba.gov.au/calculator/annualDecimal.html

# Has now been built into the original abs dataframes
POA_abs2001 <- POA_abs2001[ , order(names(POA_abs2001))]
POA_abs2006 <- POA_abs2006[ , order(names(POA_abs2006))]
POA_abs2011 <- POA_abs2011[ , order(names(POA_abs2011))] 
POA_abs2016 <- POA_abs2016[ , order(names(POA_abs2016))]


###########################################################################

# Run for each election

###########################################################################


# 2004
sd_POA_2004 <- interpolate_census(Mapping_POA_aec04, 2004, POA_abs2001, POA_abs2006, 2001, 2006)

# 2007
sd_POA_2007 <- interpolate_census(Mapping_POA_aec07, 2007, POA_abs2006, POA_abs2011, 2006, 2011)

# 2010
sd_POA_2010 <- interpolate_census(Mapping_POA_aec10, 2010, POA_abs2006, POA_abs2011, 2006, 2011)

# 2013
sd_POA_2013 <- interpolate_census(Mapping_POA_aec13, 2013, POA_abs2011, POA_abs2016, 2011, 2016)

# Order electorates alphabetically and convert to upper case
sd_POA_2004 <- sd_POA_2004 %>% 
  arrange(Election_Division) %>% 
  mutate(Election_Division = toupper(Election_Division))
sd_POA_2007 <- sd_POA_2007 %>% 
  arrange(Election_Division) %>% 
  mutate(Election_Division = toupper(Election_Division))
sd_POA_2010 <- sd_POA_2010 %>% 
  arrange(Election_Division) %>% 
  mutate(Election_Division = toupper(Election_Division))
sd_POA_2013 <- sd_POA_2013 %>% 
  arrange(Election_Division) %>% 
  mutate(Election_Division = toupper(Election_Division))

save(sd_POA_2004, file = "Clean/sd_POA_2004.rda")
save(sd_POA_2007, file = "Clean/sd_POA_2007.rda")
save(sd_POA_2010, file = "Clean/sd_POA_2010.rda")
save(sd_POA_2013, file = "Clean/sd_POA_2013.rda")

##############################################################################
# POA: Running for 2011 using 2006 and 2016 for the comparison of methods
##############################################################################
load("./Clean/Maps/Mapping_POA_abs11.rda")
sd_POA_2011 <- interpolate_census(Mapping_POA_aec = Mapping_POA_abs11, year_election = 2011, 
                                  POA_abs1 = POA_abs2006, POA_abs2 = POA_abs2016, 
                                  year_census_1 = 2006, year_census_2 = 2016)
save(sd_POA_2011, file = "data/sd_POA_2011.rda")
##############################################################################
# CED: Running for 2011 using 2006 and 2016 for the comparison of methods
##############################################################################
load("./Clean/Maps/Mapping_abs11.rda")
load("data/abs2006_e07.rda")
load("data/abs2016.rda")
abs2006 <- abs2006_e07
abs2006 <- abs2006[ , order(names(abs2006))]
abs2016 <- abs2016[ , order(names(abs2016))]

use_06 <- abs2006 %>% 
  select(-c(ID, State)) %>% 
  rename(POA = Electorate) %>% 
  mutate(year = "abs2006")

use_16 <- abs2016 %>% 
  select(-c(ID, State)) %>% 
  rename(POA = Electorate) %>% 
  mutate(year = "abs2016")

Mapping_abs11$POA <- toupper(Mapping_abs11$POA)

sd_2011 <- interpolate_census(Mapping_POA_aec = Mapping_abs11, year_election = 2011, 
                              POA_abs1 = use_06, POA_abs2 = use_16, 
                              year_census_1 = 2006, year_census_2 = 2016)
save(sd_2011, file = "data/sd_2011.rda")
##############################################################################
# CED: Running for 2011 using 2006 and 2016 for the comparison of methods
##############################################################################
load("./Clean/Maps/Mapping_abs11.rda")
load("data/abs2006_e07.rda")
load("data/abs2016.rda")
abs2006 <- abs2006_e07
abs2006 <- abs2006[ , order(names(abs2006))]
abs2016 <- abs2016[ , order(names(abs2016))]

use_06 <- abs2006 %>% 
  select(-c(ID, State)) %>% 
  rename(POA = Electorate) %>% 
  mutate(year = "abs2006")

use_16 <- abs2016 %>% 
  select(-c(ID, State)) %>% 
  rename(POA = Electorate) %>% 
  mutate(year = "abs2016")

Mapping_abs11$POA <- toupper(Mapping_abs11$POA)

sd_2011 <- interpolate_census(Mapping_POA_aec = Mapping_abs11, year_election = 2011, 
                                  POA_abs1 = use_06, POA_abs2 = use_16, 
                                  year_census_1 = 2006, year_census_2 = 2016)
save(sd_2011, file = "data/sd_2011.rda")

##############################################################################
# Test case of aggregating up 2011
##############################################################################

load("/Users/Jeremy/Documents/R/Shapefiles/data/testing_2011_case.rda")
test_imput_11 <- interpolate_census(Mapping_POA_aec = testing_2011_case, year_election = 2011, 
                                  POA_abs1 = POA_abs2011, POA_abs2 = POA_abs2011, 
                                  year_census_1 = 2006, year_census_2 = 2016)
abs2011 <- abs2011[ , order(names(abs2011))]

test_case_res <- (test_imput_11 %>% select(-Election_Division))