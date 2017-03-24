rm(list = ls())

library(tidyverse)
library(gridExtra)
library(pander)
library(shiny)
library(DT)

df_length <- read_csv("data/data_length.csv",progress=TRUE)
df_catch <- read_csv("data/data_catch.csv",progress=TRUE)
indicatorTable <- read_csv("data/indicatorTable.csv")