rm(list = ls())

library(tidyverse)
library(gridExtra)
library(pander)
library(shiny)
library(DT)
library(readxl)
library(stringr)

df_length <- read_csv("data/data_length.csv",progress=TRUE)
df_catch <- read_csv("data/data_catch.csv",progress=TRUE)
indicatorTable <- read_csv("data/indicatorTable.csv")

phils_lhi_database <- read_xlsx("data/Philippines Species Life History.xlsx",sheet=1,na=c("","N/A"))[-c(1,2,3),] %>%
  mutate(Country = "Philippines")
indo_lhi_database <- read_xlsx("data/Indonesia Life History Database.xlsx",sheet=1,na=c("","N/A"))[-c(1,2,3),] %>%
  mutate(Country = "Indonesia")
brazil_lhi_database <- read_xlsx("data/Brazil Life History Database.xlsx",sheet=1,na=c("","N/A"))[-c(1,2,3),] %>%
  mutate(Country = "Brazil")

lhi_database <- bind_rows(phils_lhi_database,
                          indo_lhi_database,
                          brazil_lhi_database) %>%
  select(Country,
         Species = X__3,
         Common = X__4,
         L_inf = `Needed for Adaptive Fisheries Assessment and Management`,
         k = X__11,
         t0 = X__12,
         M = X__13,
         Wa = X__14,
         Wb = X__15,
         m50 = X__16,
         m95 = X__17) %>%
  rowwise() %>%
  mutate(L_inf = ifelse(is.na(as.numeric(L_inf)),str_extract_all(L_inf,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(L_inf)),
         k = ifelse(is.na(as.numeric(k)),str_extract_all(k,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(k)),
         t0 = ifelse(is.na(as.numeric(t0)),str_extract_all(t0,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(t0)),
         M = ifelse(is.na(as.numeric(M)),str_extract_all(M,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(M)),
         Wa = ifelse(is.na(as.numeric(Wa)),str_extract_all(Wa,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(Wa)),
         Wb = ifelse(is.na(as.numeric(Wb)),str_extract_all(Wb,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(Wb)),
         m50 = ifelse(is.na(as.numeric(m50)),str_extract_all(m50,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(m50)),
         m95 = ifelse(is.na(as.numeric(m95)),str_extract_all(m95,"[-+.e0-9]*\\d")[[1]] %>% as.numeric() %>% mean(),as.numeric(m95)))

