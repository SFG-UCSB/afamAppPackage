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

df_length <- read_csv("data/data_length.csv")
df_catch <- read_csv("data/data_catch.csv")
df_biomass <- read_csv("data/data_biomass.csv")
df_density <- read_csv("data/data_density.csv")
indicatorTable <- read_csv("data/indicatorTable.csv")
lhi_database <- read_csv("data/LHI_Database.csv")
metadata <- read_csv("data/metadata.csv")

has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

if (has_internet()) savedURL <- "https://sfg-ucsb.github.io/afamGuidanceDocument/" else savedURL <- "_book/"