# Polling place locations and results

# -------------------------------------

# 2016

pollplace_16 <- read_csv("https://results.aec.gov.au/20499/Website/Downloads/GeneralPollingPlacesDownload-20499.csv", skip = 1)

# 2013

pollplace_13 <- read_csv("http://results.aec.gov.au/17496/Website/Downloads/GeneralPollingPlacesDownload-17496.csv", skip=1)

# 2010

pollplace_10 <- read_csv("https://results.aec.gov.au/15508/Website/Downloads/GeneralPollingPlacesDownload-15508.csv", skip = 1)

# 2007

pollplace_07 <- read_csv("https://results.aec.gov.au/13745/website/Downloads/GeneralPollingPlacesDownload-13745.csv", skip = 1)

# 2004
# Doesn't have lat and long but if IDs and name match other years, it is the same location

pollplace_04_blank <- read_csv("https://results.aec.gov.au/12246/results/Downloads/GeneralPollingPlacesDownload-12246.csv", skip = 1) %>% 
  left_join(pollplace_07 %>% select(PollingPlaceID, Latitude, Longitude, PollingPlaceNm), by = c("PollingPlaceNm", "PollingPlaceID"))
    
add1 <- left_join(pollplace_04_blank %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_07 %>% select(PollingPlaceNm, PremisesNm, PremisesPostCode, State, Latitude, Longitude), by = c("PremisesNm","PollingPlaceNm", "PremisesPostCode", "State"))

add2 <- left_join(add1 %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_10 %>% select(PollingPlaceID, PollingPlaceNm, Latitude, Longitude), by = c("PollingPlaceNm", "PollingPlaceID"))

add3 <- left_join(add2 %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_10 %>% select(PollingPlaceNm, PremisesNm, PremisesPostCode, Latitude, Longitude), by = c("PremisesNm", "PremisesPostCode", "PollingPlaceNm"))

add4 <- left_join(add3 %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_13 %>% select(PollingPlaceID, PollingPlaceNm, Latitude, Longitude), by = c("PollingPlaceNm", "PollingPlaceID"))

add5 <- left_join(add4 %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_13 %>% select(PollingPlaceNm, PremisesNm, PremisesPostCode, Latitude, Longitude), by = c("PremisesNm", "PremisesPostCode", "PollingPlaceNm"))

add6 <- left_join(add5 %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_16 %>% select(PollingPlaceID, PollingPlaceNm, Latitude, Longitude), by = c("PollingPlaceNm", "PollingPlaceID"))

add7 <- left_join(add6 %>% filter(is.na(Longitude)) %>% select(-c(Longitude, Latitude)),
  pollplace_16 %>% select(PollingPlaceNm, PremisesNm, PremisesPostCode, Latitude, Longitude), by = c("PremisesNm", "PremisesPostCode", "PollingPlaceNm"))

# For the 600 without a location, default to the postcode lat and long
# http://www.corra.com.au/downloads/Australian_Post_Codes_Lat_Lon.zip

postcodes <- read_csv("/Users/Jeremy/Documents/R/Data/Raw/Australian_Post_Codes_Lat_Lon.csv")

postcodes <- postcodes[match(unique(postcodes$postcode), postcodes$postcode),]

add8 <- left_join(add7 %>% filter(is.na(Longitude)) %>% 
    select(-c(Longitude, Latitude)) %>% rename(postcode = PremisesPostCode),
  postcodes %>% rename(Latitude = lat, Longitude = lon) 
  %>% select(postcode, Latitude, Longitude), by = "postcode") %>% 
  mutate(postcode = "Yes")

# Now combine into (almost) complete dataset

pollplace_04 <- pollplace_04_blank %>% filter(!is.na(Longitude)) %>% 
  bind_rows(add1 %>% filter(!is.na(Longitude))) %>% 
  bind_rows(add2 %>% filter(!is.na(Longitude))) %>% 
  bind_rows(add3 %>% filter(!is.na(Longitude))) %>% 
  bind_rows(add4 %>% filter(!is.na(Longitude))) %>% 
  bind_rows(add5 %>% filter(!is.na(Longitude))) %>% 
  bind_rows(add6 %>% filter(!is.na(Longitude))) %>% 
  bind_rows(add7 %>% filter(!is.na(Longitude))) %>%
  mutate(postcode = "No") %>% 
  bind_rows(add8 %>% filter(!is.na(Longitude)))

# 2001
# Taken from https://pappubahry.com/electionmaps/#download
pollplace_01 <- read_csv("/Users/Jeremy/Documents/R/Data/Raw/DB-electionmaps/booths_data/2001.csv")

# -------------------------------------

# Function to re-label parties into larger parties and other
relabel_parties <- function(df, PartyAb = PartyAb) {
  out <- df %>% 
    mutate(PartyAb = ifelse(
      PartyAb %in% c("CLP", "LP", "LNP", "NP"), "LNP", 
      ifelse(PartyAb == "ALP", "ALP",
        ifelse(PartyAb == "IND", "IND", 
          ifelse(PartyAb == "GRN", "GRN",
            ifelse(PartyAb %in% c("HAN","ON"), "ON",
              "Other"))))))
  return(out)
}

# -------------------------------------

# Download polling place division of preferences, two party preferred and two candidate preferred (where available)
# In long format, where each candidate has their own row. Alternative is wide format with code in next section.

# 2016

tcp_pp16 <- read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-20499.csv", skip = 1) %>% 
  relabel_parties() %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(TCP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, TCP) %>%
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))


tpp_pp16 <- read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseTppByPollingPlaceDownload-20499.csv", skip = 1) %>%
  rename(LNP_Votes = `Liberal/National Coalition Votes`,
    LNP_Percent = `Liberal/National Coalition Percentage`,
    ALP_Votes = `Australian Labor Party Votes`,
    ALP_Percent = `Australian Labor Party Percentage`)

fp_pp16 <- read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-NSW.csv", skip = 1) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-VIC.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-QLD.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-SA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-TAS.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-WA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-NT.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-20499-ACT.csv", skip = 1)) %>% 
  relabel_parties() %>% 
  mutate(PartyAb = ifelse(is.na(PartyAb), "Informal", PartyAb)) %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(FP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, FP) %>% 
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    Informal = replace_na(Informal, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))
  
# 2013

tcp_pp13 <- read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-17496.csv", skip = 1) %>% 
  relabel_parties() %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(TCP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, TCP) %>%
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    LNP = replace_na(LNP, 0), 
    ON = 0, # No One Nation in TCP for 2013
    Other = replace_na(Other, 0))


tpp_pp13 <- read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseTppByPollingPlaceDownload-17496.csv", skip = 1) %>%
  rename(LNP_Votes = `Liberal/National Coalition Votes`,
    LNP_Percent = `Liberal/National Coalition Percentage`,
    ALP_Votes = `Australian Labor Party Votes`,
    ALP_Percent = `Australian Labor Party Percentage`)

fp_pp13 <- read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-NSW.csv", skip = 1) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-VIC.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-QLD.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-SA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-TAS.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-WA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-NT.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/17496/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-17496-ACT.csv", skip = 1)) %>% 
  relabel_parties() %>% 
  mutate(PartyAb = ifelse(is.na(PartyAb), "Informal", PartyAb)) %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(FP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, FP) %>% 
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    Informal = replace_na(Informal, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))

# 2010

tcp_pp10 <- read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-15508.csv", skip = 1) %>% 
  relabel_parties() %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(TCP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, TCP) %>%
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    LNP = replace_na(LNP, 0), 
    ON = 0,  # No One Nation in TCP for 2010
    Other = replace_na(Other, 0))

tpp_pp10 <- read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseTppByPollingPlaceDownload-15508.csv", skip = 1) %>%
  rename(LNP_Votes = `Liberal/National Coalition Votes`,
    LNP_Percent = `Liberal/National Coalition Percentage`,
    ALP_Votes = `Australian Labor Party Votes`,
    ALP_Percent = `Australian Labor Party Percentage`)

fp_pp10 <- read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-NSW.csv", skip = 1) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-VIC.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-QLD.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-SA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-TAS.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-WA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-NT.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/15508/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-15508-ACT.csv", skip = 1)) %>% 
  relabel_parties() %>% 
  mutate(PartyAb = ifelse(is.na(PartyAb), "Informal", PartyAb)) %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(FP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, FP) %>% 
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    Informal = replace_na(Informal, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))

# 2007

tcp_pp07 <- read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-13745.csv", skip = 1) %>% 
  relabel_parties() %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(TCP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, TCP) %>%
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    LNP = replace_na(LNP, 0), 
    ON = 0, # No One Nation in TCP for 2007
    Other = 0) # No other parties in TCP for 2007

tpp_pp07 <- read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseTppByPollingPlaceDownload-13745.csv", skip = 1) %>%
  rename(LNP_Votes = `Liberal/National Coalition Votes`,
    LNP_Percent = `Liberal/National Coalition Percentage`,
    ALP_Votes = `Australian Labor Party Votes`,
    ALP_Percent = `Australian Labor Party Percentage`)

fp_pp07 <- read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-NSW.csv", skip = 1) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-VIC.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-QLD.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-SA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-TAS.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-WA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-NT.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/13745/Website/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-13745-ACT.csv", skip = 1)) %>% 
  relabel_parties() %>% 
  mutate(PartyAb = ifelse(is.na(PartyAb), "Informal", PartyAb)) %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(FP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, FP) %>% 
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    Informal = replace_na(Informal, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))

# 2004

tcp_pp04 <- read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseTcpByCandidateByPollingPlaceDownload-12246.csv", skip = 1) %>% 
  relabel_parties() %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(TCP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, TCP) %>%
  mutate(ALP = replace_na(ALP, 0), 
    GRN = 0, # No Greens in TCP for 2004
    IND = replace_na(IND, 0), 
    LNP = replace_na(LNP, 0), 
    ON = 0, # No One Nation in TCP for 2004
    Other = 0) # No other parties in TCP for 2004

tpp_pp04 <- read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseTppByPollingPlaceDownload-12246.csv", skip = 1) %>%
  rename(LNP_Votes = `Liberal/National Coalition Votes`,
    LNP_Percent = `Liberal/National Coalition Percentage`,
    ALP_Votes = `Australian Labor Party Votes`,
    ALP_Percent = `Australian Labor Party Percentage`)

fp_pp04 <- read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-NSW.csv", skip = 1) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-VIC.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-QLD.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-SA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-TAS.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-WA.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-NT.csv", skip = 1)) %>% 
  bind_rows(read_csv("https://results.aec.gov.au/12246/results/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-12246-ACT.csv", skip = 1)) %>% 
  relabel_parties() %>% 
  mutate(PartyAb = ifelse(is.na(PartyAb), "Informal", PartyAb)) %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(FP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, FP) %>% 
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    Informal = replace_na(Informal, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))

# 2001

# Sourced from David Barry (https://pappubahry.com/electionmaps/)

votes_pp01 <- read_csv("/Users/Jeremy/Documents/R/Data/Raw/pollingplace2001.csv") %>% 
  mutate(ALP_prim = replace_na(ALP_prim, 0), 
    GRN_prim = replace_na(GRN_prim, 0), 
    IND_prim = replace_na(IND_prim, 0), 
    LP_prim = replace_na(LP_prim, 0), 
    NP_prim = replace_na(NP_prim, 0), 
    HAN_prim = replace_na(HAN_prim, 0)) %>% 
  mutate(LNP = LP_prim + NP_prim) %>% 
  select(-c(LP_prim, NP_prim, DEM_prim, TCP_1, TCP_2)) %>% 
  rename(StateAb = State, DivisionID = Seat,PollingPlace = Booth, 
    ALP = ALP_prim, ON = HAN_prim, IND = IND_prim, GRN = GRN_prim,
    Latitude = Lat, Longitude = Long) %>% 
  mutate(Other = round(100 - ALP - LNP - GRN - IND, 2))

fp_pp01 <- votes_pp01 %>% 
  select(-c(ALP_2PP, LNP_2PP))

tpp_pp01 <- votes_pp01 %>% 
  select(-c(ALP, LNP, GRN, Other, IND)) %>% 
  rename(ALP = ALP_2PP, LNP = LNP_2PP)

# -------------------------------------

# To change to WIDE format
# Independents are grouped together, and minor parties are lumped into "Other"

tcp_pp16 <- read_csv("https://results.aec.gov.au/20499/Website/Downloads/HouseTcpByCandidateByPollingPlaceDownload-20499.csv", skip = 1) %>% 
  relabel_parties() %>% 
  select(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb, OrdinaryVotes) %>% 
  group_by(StateAb, DivisionID, DivisionNm, PollingPlaceID, PollingPlace, PartyAb) %>% 
  summarise(TCP = sum(OrdinaryVotes)) %>% 
  spread(PartyAb, TCP) %>%
  mutate(ALP = replace_na(ALP, 0), 
    GRN = replace_na(GRN, 0), 
    IND = replace_na(IND, 0), 
    LNP = replace_na(LNP, 0), 
    ON = replace_na(ON, 0), 
    Other = replace_na(Other, 0))

# -------------------------------------

# Adding locations of polling places

fp_pp16 <- fp_pp16 %>% 
  left_join(pollplace_16 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tcp_pp16 <- tcp_pp16 %>% 
  left_join(pollplace_16 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tpp_pp16 <- tpp_pp16 %>% 
  left_join(pollplace_16 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")

fp_pp13 <- fp_pp13 %>% 
  left_join(pollplace_13 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tcp_pp13 <- tcp_pp13 %>% 
  left_join(pollplace_13 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tpp_pp13 <- tpp_pp13 %>% 
  left_join(pollplace_13 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")

fp_pp10 <- fp_pp10 %>% 
  left_join(pollplace_10 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tcp_pp10 <- tcp_pp10 %>% 
  left_join(pollplace_10 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tpp_pp10 <- tpp_pp10 %>% 
  left_join(pollplace_10 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")

fp_pp07 <- fp_pp07 %>% 
  left_join(pollplace_07 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tcp_pp07 <- tcp_pp07 %>% 
  left_join(pollplace_07 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tpp_pp07 <- tpp_pp07 %>% 
  left_join(pollplace_07 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")

fp_pp04 <- fp_pp04 %>% 
  left_join(pollplace_04 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tcp_pp04 <- tcp_pp04 %>% 
  left_join(pollplace_04 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")
tpp_pp04 <- tpp_pp04 %>% 
  left_join(pollplace_04 %>% select(PollingPlaceID, PremisesPostCode, Latitude, Longitude), by = "PollingPlaceID")

# Save
save(fp_pp16, file = "/Users/Jeremy/Documents/R/Data/Clean/fp_pp16.rda")
save(tcp_pp16, file = "/Users/Jeremy/Documents/R/Data/Clean/tcp_pp16.rda")
save(tpp_pp16, file = "/Users/Jeremy/Documents/R/Data/Clean/tpp_pp16.rda")

save(fp_pp13, file = "/Users/Jeremy/Documents/R/Data/Clean/fp_pp13.rda")
save(tcp_pp13, file = "/Users/Jeremy/Documents/R/Data/Clean/tcp_pp13.rda")
save(tpp_pp13, file = "/Users/Jeremy/Documents/R/Data/Clean/tpp_pp13.rda")

save(fp_pp10, file = "/Users/Jeremy/Documents/R/Data/Clean/fp_pp10.rda")
save(tcp_pp10, file = "/Users/Jeremy/Documents/R/Data/Clean/tcp_pp10.rda")
save(tpp_pp10, file = "/Users/Jeremy/Documents/R/Data/Clean/tpp_pp10.rda")

save(fp_pp07, file = "/Users/Jeremy/Documents/R/Data/Clean/fp_pp07.rda")
save(tcp_pp07, file = "/Users/Jeremy/Documents/R/Data/Clean/tcp_pp07.rda")
save(tpp_pp07, file = "/Users/Jeremy/Documents/R/Data/Clean/tpp_pp07.rda")

save(fp_pp04, file = "/Users/Jeremy/Documents/R/Data/Clean/fp_pp04.rda")
save(tcp_pp04, file = "/Users/Jeremy/Documents/R/Data/Clean/tcp_pp04.rda")
save(tpp_pp04, file = "/Users/Jeremy/Documents/R/Data/Clean/tpp_pp04.rda")

save(fp_pp01, file = "/Users/Jeremy/Documents/R/Data/Clean/fp_pp01.rda")
save(tpp_pp01, file = "/Users/Jeremy/Documents/R/Data/Clean/tpp_pp01.rda")

