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

df_length <- read_csv("data/data_length.csv",progress=TRUE)
df_catch <- read_csv("data/data_catch.csv",progress=TRUE)
indicatorTable <- read_csv("data/indicatorTable.csv")

phils_lhi_metadata <- read_xlsx("data/Philippines Species Life History.xlsx",sheet=2,na=c("","N/A"))
colnames(phils_lhi_metadata) = phils_lhi_metadata[1,]
colnames(phils_lhi_metadata)[1] = "Marker"
phils_lhi_metadata <- phils_lhi_metadata[-1,] %>%
  mutate(`Scientific Name` = na.locf(`Scientific Name`),
         `Common Name` = na.locf(`Common Name`),
         `Species Identification Code` = na.locf(`Species Identification Code`)) %>%
  filter(Marker == "Reference(s)") %>%
  mutate(Country = "Philippines")

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
                      brazil_lhi_metadata) %>%
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
                          brazil_lhi_database) %>%
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

