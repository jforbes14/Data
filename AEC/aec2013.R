## Importing federal election results for 2013, and forming a dataframe for each vote count
## Vote counts are: first preference, two candidate preferred (2cp) and two party preferred (2pp)
## Download from: http://results.aec.gov.au/17496/Website/HouseDownloadsMenu-17496-csv.htm

library(tidyverse)

#--- FIRST PREFERENCES ---#

all_content <- readLines("./Raw/HouseDopByDivision2013.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
pref13 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

fp13 <- pref13 %>% 
  filter(CalculationType %in% c("Preference Count", "Preference Percent")) %>% 
  group_by(StateAb, DivisionID, DivisionNm, CountNumber, BallotPosition, CandidateID, Surname, GivenNm, PartyAb, PartyNm, Elected, HistoricElected) %>% 
  spread(key = CalculationType, value = CalculationValue) %>%
  filter(CountNumber == 0) %>% 
  ungroup() %>% 
  select(-CountNumber) %>% #takes only % of first preference votes
  rename(Count = `Preference Count`, Percent = `Preference Percent`)


#--- TWO CANDIDATE PREFERRED ---#
# Distribution of preferences to the two candidates who came first and second in the election
tcp13 <- pref13[seq(2, nrow(pref13), 4), ] %>%
  group_by(DivisionID, PartyAb) %>%
  filter(CountNumber == max(CountNumber)) %>%
  arrange() %>%
  filter(CalculationValue != 0)



#--- TWO PARTY PREFERRED ---#
# Preferences distribution only to Labor (ALP) and Coalition (LP, NP, LNQ, CLP)
# A distribution of preferences where, by convention, comparisons are made between the ALP and the leading Liberal/National candidates. In seats where the final two candidates are not from the ALP and the Liberal or National parties, a two party preferred count may be conducted to find the result of preference flows to the ALP and the Liberal/National candidates.
all_content <- readLines("./Raw/HouseTppByDivision2013.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
tpp13 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

tpp13 <- tpp13 %>%
  arrange(DivisionID)



#---- COUNT VOTES PER ELECTORATE ----
#votes13 <- pref13[seq(1, nrow(pref13), 4), ] %>%
#  filter(CountNumber == 0) %>% #takes number of first preferences
#  group_by(StateAb, DivisionID, DivisionNm) %>%
#  summarise(TotalVotes13 = sum(CalculationValue))
# votes13$DivisionNm <- toupper(votes13$DivisionNm)

#---- MAKE ALL ELECTORATE NAMES UPPER CASE ----
fp13$DivisionNm <- toupper(fp13$DivisionNm)
tcp13$DivisionNm <- toupper(tcp13$DivisionNm)
tpp13$DivisionNm <- toupper(tpp13$DivisionNm)

#---- RELABEL PARTY NAMES ----

# Function in aec2016.R

# Apply

fp13 <- fp13 %>% relabel_parties() %>% reabbrev_parties()
tcp13 <- tcp13 %>% relabel_parties() %>% reabbrev_parties()
tpp13 <- tpp13 %>% reabbrev_parties()

#---- SAVE ----
save(fp13, file = "Clean/fp13.rda")
save(tpp13, file = "Clean/tpp13.rda")
save(tcp13, file = "Clean/tcp13.rda")