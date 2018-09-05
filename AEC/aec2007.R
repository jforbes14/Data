## Importing federal election results for 2007, and forming a dataframe for each vote count
## Vote counts are: first preference, two candidate preferred (2cp) and two party preferred (2pp)
## Download from: http://results.aec.gov.au/13745/Website/HouseDownloadsMenu-13745-csv.htm

library(tidyverse)

#--- FIRST PREFERENCES ---#

all_content <- readLines("./Raw/HouseDopByDivision2007.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
pref07 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

fp07 <- pref07[seq(2, nrow(pref07), 4), ] %>%
  filter(CountNumber == 0) #takes only % of votes



#--- TWO CANDIDATE PREFERRED ---#
# Distribution of preferences to the two candidates who came first and second in the election
tcp07 <- pref07[seq(2, nrow(pref07), 4), ] %>%
  group_by(DivisionID, PartyAb) %>%
  filter(CountNumber == max(CountNumber)) %>%
  arrange() %>%
  filter(CalculationValue != 0)



#--- TWO PARTY PREFERRED ---#
# Preferences distribution only to Labor (ALP) and Coalition (LP, NP, LNQ, CLP)
# A distribution of preferences where, by convention, comparisons are made between the ALP and the leading Liberal/National candidates. In seats where the final two candidates are not from the ALP and the Liberal or National parties, a two party preferred count may be conducted to find the result of preference flows to the ALP and the Liberal/National candidates.
all_content <- readLines("./Raw/HouseTppByDivision2007.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
tpp07 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

tpp07 <- tpp07 %>%
  arrange(DivisionID)



#---- COUNT VOTES PER ELECTORATE ----
#votes07 <- pref07[seq(1, nrow(pref07), 4), ] %>%
#  filter(CountNumber == 0) %>% #takes number of first preferences
#  group_by(StateAb, DivisionID, DivisionNm) %>%
#  summarise(TotalVotes07 = sum(CalculationValue))
#votes07$DivisionNm <- toupper(votes07$DivisionNm)


#---- MAKE ALL ELECTORATE NAMES UPPER CASE ----
fp07$DivisionNm <- toupper(fp07$DivisionNm)
tcp07$DivisionNm <- toupper(tcp07$DivisionNm)
tpp07$DivisionNm <- toupper(tpp07$DivisionNm)


#---- SAVE ----
save(fp07, file = "Clean/fp07.rda")
save(tpp07, file = "Clean/tpp07.rda")
save(tcp07, file = "Clean/tcp07.rda")