## Importing federal election results for 2016, and forming a dataframe for each vote count
## Vote counts are: first preference, two candidate preferred (2cp) and two party preferred (2pp)
## Download from: http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm

library(tidyverse)

#---- FIRST PREFERENCES ----

all_content <- readLines("./Raw/HouseDopByDivision2016.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
pref16 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

fp16 <- pref16[seq(2, nrow(pref16), 4), ] %>%
  filter(CountNumber == 0) #takes only % of votes


#---- TWO CANDIDATE PREFERRED ----
# Distribution of preferences to the two candidates who came first and second in the election
tcp16 <- pref16[seq(2, nrow(pref16), 4), ] %>%
  group_by(DivisionID, PartyAb) %>%
  filter(CountNumber == max(CountNumber)) %>%
  arrange() %>%
  filter(CalculationValue != 0)



#---- TWO PARTY PREFERRED ----
# Preferences distribution only to Labor (ALP) and Coalition (LP, NP, LNQ, CLP)
# A distribution of preferences where, by convention, comparisons are made between the ALP and the leading Liberal/National candidates. In seats where the final two candidates are not from the ALP and the Liberal or National parties, a two party preferred count may be conducted to find the result of preference flows to the ALP and the Liberal/National candidates.
all_content <- readLines("./Raw/HouseTppByDivision2016.csv") #to remove first row and load correct column headers
skip_first <- all_content[-1]
tpp16 <- read.csv(textConnection(skip_first), header = TRUE, stringsAsFactors = FALSE)

tpp16 <- tpp16 %>%
  arrange(DivisionID)


#---- Test that votes tally 100% ----
#test <- fp16 %>%
#  group_by(DivisionID) %>%
#  summarize(percentage = sum(CalculationValue))



#---- COUNT VOTES PER ELECTORATE ----
#votes16 <- pref16[seq(1, nrow(pref16), 4), ] %>%
#  filter(CountNumber == 0) %>% #takes number of first preferences
#  group_by(StateAb, DivisionID, DivisionNm) %>%
#  summarise(TotalVotes16 = sum(CalculationValue))
#votes16$DivisionNm <- toupper(votes16$DivisionNm)



#---- MAKE ALL ELECTORATE NAMES UPPER CASE ----
fp16$DivisionNm <- toupper(fp16$DivisionNm)
tcp16$DivisionNm <- toupper(tcp16$DivisionNm)
tpp16$DivisionNm <- toupper(tpp16$DivisionNm)


#---- SAVE ----
save(fp16, file = "Clean/fp16.rda")
save(tpp16, file = "Clean/tpp16.rda")
save(tcp16, file = "Clean/tcp16.rda")