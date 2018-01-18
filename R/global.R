rm(list = ls())

library(tidyverse)
library(gridExtra)
library(pander)
library(shiny)
library(DT)
library(readxl)
library(stringr)
library(LBSPR)
library(TropFishR)
library(lubridate)
library(zoo)
library(curl)

#df_length <- read_csv("data/data_length.csv",progress=TRUE)
# df_length <- read_csv("R/data/data_catch.csv",progress=TRUE) %>%
#   mutate(species=replace(species, species=="caesio cuning", "Caesio cuning"),
#          species=replace(species, species=="Bolbometopon Muricatum", "Bolbometopon muricatum"))s
# write_csv(df_length,"R/data/data_catch.csv")
# df_catch <- read_csv("R/data/data_catch.csv",progress=TRUE) %>%
#   mutate(species=replace(species, species=="caesio cuning", "Caesio cuning"),
#          species=replace(species, species=="Bolbometopon Muricatum", "Bolbometopon muricatum"))
# write_csv(df_catch,"R/data/data_catch.csv")
# df_length <- read_csv("data/data_catch.csv",progress=TRUE) %>%
#   mutate(species=replace(species, species=="caesio cuning", "Caesio cuning"),
#          species=replace(species, species=="Bolbometopon Muricatum", "Bolbometopon muricatum"))
# df_catch <- read_csv("data/data_catch.csv",progress=TRUE) %>%
#   mutate(species=replace(species, species=="caesio cuning", "Caesio cuning"),
#          species=replace(species, species=="Bolbometopon Muricatum", "Bolbometopon muricatum"))
# df_length <- read_csv("R/data/data_catch.csv",progress=TRUE) %>%
#   filter(!is.na(length_cm)) %>%
#   dplyr::select(site,year,date,gear,inside_area,species,length_cm)
# write_csv(df_length,"R/data/df_length.csv")
df_length <- read_csv("data/data_length.csv")
df_catch <- read_csv("data/data_catch.csv")
df_biomass <- read_csv("data/data_biomass.csv")
df_density <- read_csv("data/data_density.csv")
indicatorTable <- read_csv("data/indicatorTable.csv")
# df_catch <- read_csv("R/data/data_catch.csv",progress=TRUE)
# df_catch <- df_catch %>%
#   mutate(days = 1) %>%
#   mutate(gear = replace(gear, gear=="handline", "Handline")) %>%
#   mutate(gear = replace(gear, gear=="Gill Net", "Gillnet")) %>%
#   mutate(gear = replace(gear, gear=="Gill net", "Gillnet")) %>%
#   mutate(inside_area = replace(inside_area, inside_area=="outside", "Outside")) %>%
#   mutate(inside_area = replace(inside_area, inside_area=="inside", "Inside"))
# gearEffortTable <- data.frame(gear = unique(df_catch$gear),fisher_days = c(2,18,1,2,2,1,2))
# df_catch <- df_catch %>%
#   left_join(gearEffortTable,by="gear")
# write_csv(df_catch,"R/data/data_catch.csv")
  
phils_lhi_metadata <- read_xlsx("data/Philippines Species Life History.xlsx",sheet=2,na=c("","N/A"))
colnames(phils_lhi_metadata) = phils_lhi_metadata[1,]
colnames(phils_lhi_metadata)[1] = "Marker"
phils_lhi_metadata <- phils_lhi_metadata[-1,] %>%
  mutate(`Scientific Name` = na.locf(`Scientific Name`),
         `Common Name` = na.locf(`Common Name`),
         `Species Identification Code` = na.locf(`Species Identification Code`)) %>%
  filter(Marker == "Reference(s)") %>%
  mutate(Country = "Philippines")

moz_lhi_metadata <- read_xlsx("data/Mozambique Life History Database.xlsx",sheet=2,na=c("","N/A"))
colnames(moz_lhi_metadata) = moz_lhi_metadata[1,]
colnames(moz_lhi_metadata)[1] = "Marker"
moz_lhi_metadata <- moz_lhi_metadata[-1,] %>%
  mutate(`Scientific Name` = na.locf(`Scientific Name`),
         `Common Name` = na.locf(`Common Name`),
         `Species Identification Code` = na.locf(`Species Identification Code`)) %>%
  filter(Marker == "Reference(s)") %>%
  mutate(Country = "Mozambique")

indo_lhi_metadata <- read_xlsx("data/Indonesia Life History Database.xlsx",sheet=2,na=c("","N/A"))
colnames(indo_lhi_metadata) = indo_lhi_metadata[1,]
colnames(indo_lhi_metadata)[1] = "Marker"
indo_lhi_metadata <- indo_lhi_metadata[-1,] %>%
  mutate(`Scientific Name` = na.locf(`Scientific Name`),
         `Common Name` = na.locf(`Common Name`),
         `Species Identification Code` = na.locf(`Species Identification Code`)) %>%
  filter(Marker == "Reference(s)") %>%
  mutate(Country = "Indonesia")

brazil_lhi_metadata <- read_xlsx("data/Brazil Life History Database.xlsx",sheet=2,na=c("","N/A"))
colnames(brazil_lhi_metadata) = brazil_lhi_metadata[1,]
colnames(brazil_lhi_metadata)[1] = "Marker"
brazil_lhi_metadata <- brazil_lhi_metadata[-1,] %>%
  mutate(`Scientific Name` = na.locf(`Scientific Name`),
         `Common Name` = na.locf(`Common Name`),
         `Species Identification Code` = na.locf(`Species Identification Code`)) %>%
  filter(Marker == "Reference(s)") %>%
  mutate(Country = "Brazil")

metadata <- bind_rows(phils_lhi_metadata,
                      indo_lhi_metadata,
                      brazil_lhi_metadata,
                      moz_lhi_metadata) %>%
  dplyr::select(Country,
                Species = `Scientific Name`,
                Common = `Common Name`,
                Code = `Species Identification Code`,
                L_inf = `L inf`,
                k = k,
                t0 = t0,
                M = M,
                Wa = Wa,
                Wb = Wb,
                m50 = m50,
                m95 = m95)

phils_lhi_database <- read_xlsx("data/Philippines Species Life History.xlsx",sheet=1,na=c("","N/A"))
colnames(phils_lhi_database) = phils_lhi_database[1,]
phils_lhi_database <- phils_lhi_database[-c(1,2,3),] %>%
  mutate(Country = "Philippines")

moz_lhi_database <- read_xlsx("data/Mozambique Life History Database.xlsx",sheet=1,na=c("","N/A"))
colnames(moz_lhi_database) = moz_lhi_database[1,]
moz_lhi_database <- moz_lhi_database[-c(1,2,3),] %>%
  mutate(Country = "Mozambique")

indo_lhi_database <- read_xlsx("data/Indonesia Life History Database.xlsx",sheet=1,na=c("","N/A"))
colnames(indo_lhi_database) = indo_lhi_database[1,]
indo_lhi_database <- indo_lhi_database[-c(1,2,3),] %>%
  mutate(Country = "Indonesia")

brazil_lhi_database <- read_xlsx("data/Brazil Life History Database.xlsx",sheet=1,na=c("","N/A"))
colnames(brazil_lhi_database) = brazil_lhi_database[1,]
brazil_lhi_database <- brazil_lhi_database[-c(1,2,3),] %>%
  mutate(Country = "Brazil")

lhi_database <- bind_rows(phils_lhi_database,
                          indo_lhi_database,
                          brazil_lhi_database,
                          moz_lhi_database) %>%
  dplyr::select(Country,
         Species = `Scientific Name`,
         Common = `Common Name`,
         Code = `Species Identification Code`,
         L_inf = `L inf`,
         k = k,
         t0 = t0,
         M = M,
         Wa = Wa,
         Wb = Wb,
         m50 = m50,
         m95 = m95) %>%
  rowwise() %>%
  mutate(L_inf = ifelse(is.na(as.numeric(L_inf)),str_extract_all(L_inf,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(L_inf)),
         k = ifelse(is.na(as.numeric(k)),str_extract_all(k,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(k)),
         t0 = ifelse(is.na(as.numeric(t0)),str_extract_all(t0,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(t0)),
         M = ifelse(is.na(as.numeric(M)),str_extract_all(M,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(M)),
         Wa = ifelse(is.na(as.numeric(Wa)),str_extract_all(Wa,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(Wa)),
         Wb = ifelse(is.na(as.numeric(Wb)),str_extract_all(Wb,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(Wb)),
         m50 = ifelse(is.na(as.numeric(m50)),str_extract_all(m50,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(m50)),
         m95 = ifelse(is.na(as.numeric(m95)),str_extract_all(m95,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(m95)))

has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

if (has_internet()) savedURL <- "https://sfg-ucsb.github.io/afamGuidanceDocument/" else savedURL <- "_book/"