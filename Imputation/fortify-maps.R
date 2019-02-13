## Fortify electorate maps

library(tidyverse)

## 2016
fortified_2016 <- ggplot2::fortify(simp_abs16) %>% 
  mutate(year = "2016",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_abs16@data %>% select(CED_NAME, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2016 <- left_join(fortified_2016, nms, by="id") %>% 
  mutate(Election_Division =  toupper(CED_NAME)) %>% 
  select(-CED_NAME)

## 2013
fortified_2013 <- ggplot2::fortify(simp_aec13) %>% 
  mutate(year = "2013",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_aec13@data %>% select(Elect_div, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2013 <- left_join(fortified_2013, nms, by="id") %>% 
  mutate(Election_Division =  toupper(Elect_div)) %>% 
  select(-Elect_div)

## 2010
fortified_2010 <- ggplot2::fortify(simp_aec10) %>% 
  mutate(year = "2010",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_aec10@data %>% select(ELECT_DIV, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2010 <- left_join(fortified_2010, nms, by="id") %>% 
  mutate(Election_Division =  toupper(ELECT_DIV)) %>% 
  select(-ELECT_DIV)

## 2007
fortified_2007 <- ggplot2::fortify(simp_abs06e07) %>% 
  mutate(year = "2007",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_abs06e07@data %>% select(NAME_2007, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2007 <- left_join(fortified_2007, nms, by="id") %>% 
  mutate(Election_Division =  toupper(NAME_2007)) %>% 
  select(-NAME_2007)

## 2004
fortified_2004 <- ggplot2::fortify(simp_abs06e04) %>% 
  mutate(year = "2004",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_abs06e04@data %>% select(NAME_2004, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2004 <- left_join(fortified_2004, nms, by="id") %>% 
  mutate(Election_Division =  toupper(NAME_2004)) %>% 
  select(-NAME_2004)

## 2001
fortified_2001 <- ggplot2::fortify(simp_abs01) %>% 
  mutate(year = "2001",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_abs01@data %>% select(CED_NAME_2001, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2001 <- left_join(fortified_2001, nms, by="id") %>% 
  mutate(Election_Division =  toupper(CED_NAME_2001)) %>% 
  select(-CED_NAME_2001)

## Census 2011
fortified_2011 <- ggplot2::fortify(simp_abs11) %>% 
  mutate(year = "2011",
         group = paste0("g.", group),
         piece = paste0("p.", piece))
nms <- simp_abs11@data %>% select(CED_NAME, long_c, lat_c)
nms$id <- as.character(1:150)
fortified_2011 <- left_join(fortified_2011, nms, by="id") %>% 
  mutate(Election_Division =  toupper(CED_NAME)) %>% 
  select(-CED_NAME)

## Save
save(fortified_2001, file = "Clean/Maps/fortified_2001.rda")
save(fortified_2004, file = "Clean/Maps/fortified_2004.rda")
save(fortified_2007, file = "Clean/Maps/fortified_2007.rda")
save(fortified_2010, file = "Clean/Maps/fortified_2010.rda")
save(fortified_2011, file = "Clean/Maps/fortified_2011.rda")
save(fortified_2013, file = "Clean/Maps/fortified_2013.rda")
save(fortified_2016, file = "Clean/Maps/fortified_2016.rda")
