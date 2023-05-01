
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(skimr)
library(fpp3)
library(tseries)
library(tsibble)
library(fable)
library(tibbletime)
library(fpp2)


# Read Raw Data -----------------------------------------------------------

df_homicides <- read_excel("data/homicides_colombia_2010_2023.xlsx")
df_extorsion<- read_excel("data/extorsion_colombia_2010_2023.xlsx")
muggings_2010_2021 <- read_excel("data/muggings_colombia_2010_2023.xlsx", sheet=1)
muggings_2022_2023 <- read_excel("data/muggings_colombia_2010_2023.xlsx", sheet=2)
df_muggings <- rbind(muggings_2010_2021, muggings_2022_2023)
df_auto_theft <- read_excel("data/auto_theft_colombia_2010_2023.xlsx")
df_home_burglary <- read_excel("data/home_burglary_colombia_2010_2023.xlsx")


# Filter Data -------------------------------------------------------------
# Filter data to only include those from the city of Barranquilla
df_homicides <- df_homicides %>% 
  filter(CODIGO_DANE=="8001000")

df_extorsion <- df_extorsion %>% 
  filter(CODIGO_DANE=="08001000")

df_muggings <- df_muggings %>% 
  filter(CODIGO_DANE=="8001000")

df_auto_theft <- df_auto_theft %>% 
  filter(CODIGO_DANE=="8001000")

df_home_burglary <- df_home_burglary %>% 
  filter(CODIGO_DANE=="8001000")


# Create TS Object --------------------------------------------------------
daily_homicides <- df_homicides %>% 
  mutate(date = as.Date(FECHA, format = "%Y-%m-%d")) %>%
  group_by(date) %>% 
  summarise(value = sum(CANTIDAD))

daily_extorsion <- df_extorsion %>% 
  mutate(date = as.Date(FECHA, format = "%Y-%m-%d")) %>%
  group_by(date) %>% 
  summarise(value = sum(CANTIDAD))

daily_muggings <- df_muggings %>% 
  mutate(date = as.Date(FECHA, format = "%Y-%m-%d")) %>%
  group_by(date) %>% 
  summarise(value = sum(CANTIDAD))

daily_auto_theft <- df_auto_theft %>% 
  mutate(date = as.Date(FECHA, format = "%Y-%m-%d")) %>%
  group_by(date) %>% 
  summarise(value = sum(CANTIDAD))

daily_home_burglary <- df_home_burglary %>% 
  mutate(date = as.Date(FECHA, format = "%Y-%m-%d")) %>%
  group_by(date) %>% 
  summarise(value = sum(CANTIDAD))

tbl_homicides <- prep_tbl_time(daily_homicides, message = FALSE) %>%
  collapse_by("month") %>% 
  group_by(date) %>% 
  summarise(value = sum(value))

tbl_extorsion <- prep_tbl_time(daily_extorsion, message = FALSE) %>%
  collapse_by("month") %>% 
  group_by(date) %>% 
  summarise(value = sum(value))

tbl_muggings <- prep_tbl_time(daily_muggings, message = FALSE) %>%
  collapse_by("month") %>% 
  group_by(date) %>% 
  summarise(value = sum(value))

tbl_auto_theft <- prep_tbl_time(daily_auto_theft, message = FALSE) %>%
  collapse_by("month") %>% 
  group_by(date) %>% 
  summarise(value = sum(value))

tbl_home_burglary <- prep_tbl_time(daily_home_burglary, message = FALSE) %>%
  collapse_by("month") %>% 
  group_by(date) %>% 
  summarise(value = sum(value))

tbl_homicides$id <- "homicide"
tbl_auto_theft$id <- "auto_theft"
tbl_extorsion$id <- "extorsion"
tbl_home_burglary$id <- "home_burglary"
tbl_muggings$id <- "mugging"

crimes <-rbind(tbl_homicides, tbl_auto_theft, tbl_extorsion, tbl_home_burglary, tbl_muggings)

crimes <- as_tibble(crimes)

# Save
save(crimes, file = "crimes.RData")

# Create Tsibble Object ---------------------------------------------------
homicides <- df_homicides %>% 
  mutate(Month = yearmonth(as.Date(FECHA, format = "%Y-%m-%d"))) %>%
  group_by(Month) %>% 
  summarise(homicide = sum(CANTIDAD))

extorsion <- df_extorsion %>% 
  mutate(Month = yearmonth(as.Date(FECHA, format = "%Y-%m-%d"))) %>%
  group_by(Month) %>% 
  summarise(extorsion = sum(CANTIDAD))

muggings <- df_muggings %>% 
  mutate(Month = yearmonth(as.Date(FECHA, format = "%Y-%m-%d"))) %>%
  group_by(Month) %>% 
  summarise(mugging = sum(CANTIDAD))

auto_theft <- df_auto_theft %>% 
  mutate(Month = yearmonth(as.Date(FECHA, format = "%Y-%m-%d"))) %>%
  group_by(Month) %>% 
  summarise(auto_theft = sum(CANTIDAD))

home_burglary <- df_home_burglary %>% 
  mutate(Month = yearmonth(as.Date(FECHA, format = "%Y-%m-%d"))) %>%
  group_by(Month) %>% 
  summarise(home_burglary = sum(CANTIDAD)) 

tb_homicides <- homicides %>% as_tsibble(index = Month)
tb_extorsion <- extorsion %>% as_tsibble(index = Month)
tb_muggings <- muggings %>% as_tsibble(index = Month)
tb_auto_theft <- auto_theft %>% as_tsibble(index = Month)
tb_home_burglary <- home_burglary %>% as_tsibble(index = Month)


crimes_ts <-bind_cols(tb_homicides, tb_muggings$mugging, 
                   tb_auto_theft$auto_theft, tb_home_burglary$home_burglary, 
                   tb_extorsion$extorsion, .name_repair="universal")

crimes_ts <- crimes_ts %>% 
  rename(mugging = ...3, auto_theft = ...4, home_burglary = ...5, extorsion = ...6)


# Keep only final TS objects for App and save
rm(list=setdiff(ls(), c("crimes", "crimes_ts")))

# Save
save(crimes_ts, file = "crimes_ts.RData")
