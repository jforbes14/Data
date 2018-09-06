## Adding first preference votes, and winning party to data_mod

library(tidyverse)

load("Clean/fp16.rda")
load("Clean/fp13.rda")
load("Clean/fp10.rda")
load("Clean/fp07.rda")
load("Clean/fp04.rda")
load("Clean/fp01.rda")

fp01$DivisionNm[which(fp01$DivisionNm == "PROSPECT")] = "MCMAHON"
fp04$DivisionNm[which(fp04$DivisionNm == "PROSPECT")] = "MCMAHON"
fp07$DivisionNm[which(fp07$DivisionNm == "PROSPECT")] = "MCMAHON"

## Winners and FP
fp_all <- bind_rows(fp01 %>% select(StateAb, DivisionNm, PartyAb, Elected, CalculationValue) %>% mutate(year = "2001"),
                    fp04 %>% select(StateAb, DivisionNm, PartyAb, Elected, CalculationValue) %>% mutate(year = "2004"),
                    fp07 %>% select(StateAb, DivisionNm, PartyAb, Elected, CalculationValue) %>% mutate(year = "2007"),
                    fp10 %>% select(StateAb, DivisionNm, PartyAb, Elected, CalculationValue) %>% mutate(year = "2010"),
                    fp13 %>% select(StateAb, DivisionNm, PartyAb, Elected, CalculationValue) %>% mutate(year = "2013"),
                    fp16 %>% select(StateAb, DivisionNm, PartyAb, Elected, CalculationValue) %>% mutate(year = "2016")
) %>% 
  rename(firstpref = CalculationValue) %>% 
  mutate(PartyAb = ifelse(PartyAb %in% c("CLR", "ALP"), "ALP", 
                          ifelse(PartyAb %in% c("LP", "LNP", "NP", "CLP"), "LNP",
                                 ifelse(PartyAb %in% c("GRN", "GWA", "TG"), "GRN", 
                                        ifelse(PartyAb %in% c("HAN", "ON"), "ON", 
                                               ifelse(PartyAb %in% c("IND"), "IND", "Other")))))) %>% 
  group_by(year, StateAb, DivisionNm, PartyAb) %>% 
  summarise(firstpref = sum(firstpref), Elected = sum(Elected == "Y")) %>% 
  group_by(year, DivisionNm) %>% 
  mutate(ElectedParty = PartyAb[which(Elected == 1)]) %>% 
  select(-Elected) %>% 
  spread(key = "PartyAb", value = "firstpref")
         
save(fp_all, file = "Clean/fp_all.rda")

