## Importing federal election results for 2010, and forming a dataframe for each vote count
## Vote counts are: first preference, two candidate preferred (2cp) and two party preferred (2pp)
## Download from: http://results.aec.gov.au/15508/Website/HouseDownloadsMenu-15508-csv.htm

library(tidyverse)

#--- FIRST PREFERENCES ---#

all_content <- readLines("./Raw/HouseDopByDivision2010.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
pref10 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

fp10 <- pref10 %>% 
  filter(CalculationType %in% c("Preference Count", "Preference Percent")) %>% 
  group_by(StateAb, DivisionID, DivisionNm, CountNumber, BallotPosition, CandidateID, Surname, GivenNm, PartyAb, PartyNm, Elected, HistoricElected) %>% 
  spread(key = CalculationType, value = CalculationValue) %>%
  filter(CountNumber == 0) %>% 
  ungroup() %>% 
  select(-CountNumber) %>% #takes only % of first preference votes
  rename(Count = `Preference Count`, Percent = `Preference Percent`)


#--- TWO CANDIDATE PREFERRED ---#
# Distribution of preferences to the two candidates who came first and second in the election
tcp10 <- pref10[seq(2, nrow(pref10), 4), ] %>%
  group_by(DivisionID, PartyAb) %>%
  filter(CountNumber == max(CountNumber)) %>%
  arrange() %>%
  filter(CalculationValue != 0)



#--- TWO PARTY PREFERRED ---#
# Preferences distribution only to Labor (ALP) and Coalition (LP, NP, LNQ, CLP)
# A distribution of preferences where, by convention, comparisons are made between the ALP and the leading Liberal/National candidates. In seats where the final two candidates are not from the ALP and the Liberal or National parties, a two party preferred count may be conducted to find the result of preference flows to the ALP and the Liberal/National candidates.
all_content <- readLines("./Raw/HouseTppByDivision2010.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
tpp10 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

tpp10 <- tpp10 %>%
  arrange(DivisionID)



#---- COUNT VOTES PER ELECTORATE ----
#votes10 <- pref10[seq(1, nrow(pref13), 4), ] %>%
#  filter(CountNumber == 0) %>% #takes number of first preferences
#  group_by(StateAb, DivisionID, DivisionNm) %>%
#  summarise(TotalVotes10 = sum(CalculationValue))
#votes10$DivisionNm <- toupper(votes10$DivisionNm)


#---- MAKE ALL ELECTORATE NAMES UPPER CASE ----
fp10$DivisionNm <- toupper(fp10$DivisionNm)
tcp10$DivisionNm <- toupper(tcp10$DivisionNm)
tpp10$DivisionNm <- toupper(tpp10$DivisionNm)


#---- RELABEL PARTY NAMES ----

# Function in aec2016.R

# Apply

fp10 <- fp10 %>% relabel_parties() %>% reabbrev_parties()
tcp10 <- tcp10 %>% relabel_parties() %>% reabbrev_parties()
tpp10 <- tpp10 %>% reabbrev_parties()


#---- SAVE ----
save(fp10, file = "Clean/fp10.rda")
save(tpp10, file = "Clean/tpp10.rda")
save(tcp10, file = "Clean/tcp10.rda")