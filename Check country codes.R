# Check country codes
# Alice Lepissier
# alice.lepissier@gmail.com

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Check UN regions
# Check World Bank regions
# Check IMF codes
# Import centroids



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(here)
setwd(here())
library(CoordinateCleaner)
library(openxlsx)
library(readxl)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# CHECK UN REGIONS          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes") %>%
  select(ISO3166.3, Country, 
         UN_Region, UN_Region_Code, 
         `UN_Sub-region`, `UN_Sub-region_Code`, 
         UN_Intermediate_Region, UN_Intermediate_Region_Code, 
         UN_M49_Code)
UNSD <- read_xlsx("2020_UNSD.xlsx", sheet = "Sheet1")

codes <- left_join(codes, UNSD %>% 
                     select(`ISO-alpha3 Code`, New_UN_Region = `Region Name`, New_UN_Region_Code = `Region Code`),
                   by = c("ISO3166.3" = "ISO-alpha3 Code"))

codes %>% filter(UN_Region != New_UN_Region)
codes %>% filter(UN_Region_Code != New_UN_Region_Code)

codes <- left_join(codes, UNSD %>% 
                     select(`ISO-alpha3 Code`, 
                            `New_UN_Sub-region` = `Sub-region Name`, 
                            `New_UN_Sub-region_Code` = `Sub-region Code`),
                   by = c("ISO3166.3" = "ISO-alpha3 Code"))

codes %>% filter(`UN_Sub-region` != `New_UN_Sub-region`)
codes %>% filter(`UN_Sub-region_Code` != `New_UN_Sub-region_Code`)

codes <- left_join(codes, UNSD %>% 
                     select(`ISO-alpha3 Code`, 
                            New_UN_Intermediate_Region = `Intermediate Region Name`, 
                            New_UN_Intermediate_Region_Code = `Intermediate Region Code`),
                   by = c("ISO3166.3" = "ISO-alpha3 Code"))

codes %>% filter(UN_Intermediate_Region != New_UN_Intermediate_Region)
codes %>% filter(UN_Intermediate_Region_Code != New_UN_Intermediate_Region_Code)

codes <- left_join(codes, UNSD %>% 
                     select(`ISO-alpha3 Code`, New_UN_M49_Code = `M49 Code`),
                   by = c("ISO3166.3" = "ISO-alpha3 Code"))

codes$New_UN_M49_Code <- str_remove(codes$New_UN_M49_Code, "^0+")
codes %>% filter(UN_M49_Code != New_UN_M49_Code)



## ## ## ## ## ## ## ## ## ## ##
# CHECK WORLD BANK REGIONS  ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes") %>% 
  select(ISO3166.3, Country, WB_Region)

WB <- read_xls("2020_World Bank.xls", skip = 4) %>% 
  select(Country = Economy, 
         ISO3166.3 = Code, 
         New_WB_Region = Region) %>%
  filter(Country != "x")

codes <- left_join(codes, WB %>% 
                     select(ISO3166.3, New_WB_Region),
                   by = c("ISO3166.3"))

codes %>% filter(WB_Region != New_WB_Region)



## ## ## ## ## ## ## ## ## ## ##
# CHECK IMF CODES           ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes") %>% 
  select(ISO3166.3, Country, IMF_Code)
IMF <- read_xlsx("IMF codes.xlsx", sheet = "Sheet1", skip = 1) %>%
  select(1:3)

codes <- left_join(codes, IMF %>% 
                     select(`ISO Code`, New_IMF_Code = `IMF Code`),
                   by = c("ISO3166.3" = "ISO Code"))

codes %>% filter(IMF_Code != New_IMF_Code)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT CENTROIDS          ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes")
regional <- read_xlsx("Codes_Masterlist.xlsx", 
                      sheet = "UN_Regional_Groupings")
wb <- read_xlsx("Codes_Masterlist.xlsx", 
                sheet = "World_Bank_Groupings")

data(countryref)

codes <- left_join(codes, countryref %>%
                     distinct(iso3, .keep_all = TRUE) %>%
                     select(iso3, Centroid_Longitude = centroid.lon, Centroid_Latitude = centroid.lat),
                   by = c("ISO3166.3" = "iso3"))

book <- createWorkbook()
hs <- createStyle(textDecoration = "Bold")

addWorksheet(book, "Codes")
writeData(book, sheet = "Codes", 
          x = codes, headerStyle = hs)

addWorksheet(book, "UN_Regional_Groupings")
writeData(book, sheet = "UN_Regional_Groupings",
          x = regional, headerStyle = hs)

addWorksheet(book, "World_Bank_Groupings")
writeData(book, sheet = "World_Bank_Groupings", 
          x = wb, headerStyle = hs)

saveWorkbook(book, "Codes_Masterlist.xlsx", overwrite = TRUE)
