# Import latest country classifications
# Alice Lepissier
# alice.lepissier@gmail.com
# Last update April 2023

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Import existing sheets
# Merge latest UNSD groups
# Merge latest HDI groups
# Merge latest World Bank groups
# Create workbook



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

library(here)
setwd(here())
library(openxlsx)
library(readxl)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# IMPORT EXISTING SHEETS    ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes")
regional <- read_xlsx("Codes_Masterlist.xlsx", 
                      sheet = "UN_Regional_Groupings")
wb <- read_xlsx("Codes_Masterlist.xlsx", 
                sheet = "World_Bank_Groupings")



## ## ## ## ## ## ## ## ## ## ##
# MERGE LATEST UNSD GROUPS  ####
## ## ## ## ## ## ## ## ## ## ##

# 2020 classification
UNSD <- read_xlsx("2020_UNSD.xlsx", sheet = "Sheet1") %>%
  select(Country = `Country or Area`,
         ISO3166.3 = `ISO-alpha3 Code`,
         New_UN_LDC = `Least Developed Countries (LDC)`,
         New_UN_LLDC = `Land Locked Developing Countries (LLDC)`,
         New_UN_SIDS = `Small Island Developing States (SIDS)`,
         `New_UN_Developing-Developed` = `Developed / Developing Countries`) %>%
  mutate_at(c("New_UN_LDC", "New_UN_LLDC", "New_UN_SIDS"),
            ~ifelse(. == "x", 1, .)) %>%
  mutate_at(c("New_UN_LDC", "New_UN_LLDC", "New_UN_SIDS"),
            ~ifelse(is.na(.), 0, .))

codes <- left_join(codes, UNSD %>% 
                     select(-Country),
                   by = c("ISO3166.3"))

codes %>% 
  select(Country,
         UN_LDC, New_UN_LDC) %>%
  filter(UN_LDC != New_UN_LDC) %>% 
  arrange(Country)

codes %>% 
  select(Country,
         UN_LLDC, New_UN_LLDC) %>%
  filter(UN_LLDC != New_UN_LLDC) %>% 
  arrange(Country)

codes %>% 
  select(Country,
         UN_SIDS, New_UN_SIDS) %>%
  filter(UN_SIDS != New_UN_SIDS) %>% 
  arrange(Country)

codes %>% 
  select(Country,
         `UN_Developing-Developed`, `New_UN_Developing-Developed`) %>%
  filter(`UN_Developing-Developed` != `New_UN_Developing-Developed`) %>% 
  arrange(Country)

codes <- codes %>%
  select(-c(UN_LDC, UN_LLDC, UN_SIDS, `UN_Developing-Developed`))
colnames(codes) <- gsub("New_", "", colnames(codes))



## ## ## ## ## ## ## ## ## ## ##
# MERGE LATEST HDI GROUPS   ####
## ## ## ## ## ## ## ## ## ## ##

UNDP <- read_xlsx("2020_UNDP.xlsx", sheet = "Table 1", skip = 6) %>%
  select(Country = ...2,
         HDI = `2019...3`) %>%
  mutate(HDI = as.numeric(HDI)) %>%
  filter(!is.na(HDI))

UNDP <- UNDP %>%
  mutate(New_UNDP_HDI_Group = ifelse(HDI < 0.550, "Lo HDI",
                                     ifelse(HDI >= 0.550 & HDI <= 0.699, "Med HDI",
                                            ifelse(HDI >= 0.700 & HDI <= 0.799, "Hi HDI",
                                                   ifelse(HDI >= 0.800, "VHi HDI", NA)))))

nogroup <- UNDP %>% filter(is.na(New_UNDP_HDI_Group))

UNDP <- left_join(UNDP, codes %>%
                    select(Country, ISO3166.3),
                  by = c("Country"))
UNDP %>% filter(is.na(ISO3166.3))

codes <- left_join(codes, UNDP %>%
                     select(ISO3166.3, New_UNDP_HDI_Group) %>%
                     filter(!is.na(ISO3166.3)),
                   by = c("ISO3166.3"))

changed <- codes %>% 
  select(Country, UNDP_HDI_Group, New_UNDP_HDI_Group) %>%
  filter(UNDP_HDI_Group != New_UNDP_HDI_Group) %>% 
  arrange(Country)

codes <- codes %>%
  select(-UNDP_HDI_Group) %>%
  rename(UNDP_HDI_Group = New_UNDP_HDI_Group)



## ## ## ## ## ## ## ## ## ## ## ## ##
# MERGE LATEST WORLD BANK GROUPS  ####
## ## ## ## ## ## ## ## ## ## ## ## ##

# Source: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

# 2022 classification
WB <- read_excel("2022_World Bank.xlsx") %>% 
  select(Country = Economy, 
         ISO3166.3 = Code, 
         New_WB_Income_Group = "Income group",
         New_WB_Lending_Category = "Lending category", 
         New_WB_Other = "Other (EMU or HIPC)",
         Region) %>%
  drop_na(Region)

WB <- WB %>%
  mutate(New_WB_Income_Group = case_when(New_WB_Income_Group == "Low income" ~ "LIC",
                                         New_WB_Income_Group == "Lower middle income" ~ "LMC",
                                         New_WB_Income_Group == "Upper middle income" ~ "UMC",
                                         New_WB_Income_Group == "High income" ~ "HIC"))

# Check countries with no group
WB %>% filter(is.na(New_WB_Income_Group))

# Venezuela has been temporarily unclassified by World Bank
# Assign latest known classification
WB <- WB %>% mutate(New_WB_Income_Group = ifelse(ISO3166.3 == "VEN", "UMC", New_WB_Income_Group))

codes <- left_join(codes, 
                   WB %>% 
                     select(ISO3166.3, New_WB_Income_Group, New_WB_Lending_Category, New_WB_Other),
                   by = c("ISO3166.3"))

# Countries that have changed income group
codes %>% 
  select(Country, WB_Income_Group, New_WB_Income_Group) %>%
  filter(WB_Income_Group != New_WB_Income_Group) %>% 
  arrange(Country) %>%
  distinct(Country, .keep_all = TRUE)

# Countries that have changed lending category
codes %>% 
  select(Country, WB_Lending_Category, New_WB_Lending_Category) %>%
  filter(WB_Lending_Category != New_WB_Lending_Category) %>% 
  arrange(Country) %>%
  distinct(Country, .keep_all = TRUE)

# Countries that changed other category
codes %>% 
  select(Country, WB_Other, New_WB_Other) %>%
  filter(WB_Other != New_WB_Other) %>% 
  arrange(Country) %>%
  distinct(Country, .keep_all = TRUE)

codes <- codes %>%
  select(-c(WB_Income_Group, WB_Lending_Category, WB_Other))
colnames(codes) <- gsub("New_", "", colnames(codes))



## ## ## ## ## ## ## ## ## ## ##
# CREATE WORKBOOK           ####
## ## ## ## ## ## ## ## ## ## ##

codes <- codes %>%
  select(ISO3166.3, ISO3166.2, Country,
         UN_Region, UN_Region_Code, `UN_Sub-region`, `UN_Sub-region_Code`, UN_Intermediate_Region, UN_Intermediate_Region_Code,
         UN_M49_Code, UN_LDC, UN_LLDC, UN_SIDS, `UN_Developing-Developed`, UNDP_HDI_Group,
         WB_Income_Group, WB_Region, WB_Lending_Category, WB_Other,
         IMF_Code, OECD, EU27, `Arab League`, Centroid_Longitude, Centroid_Latitude)
codes <- codes %>%
  arrange(ISO3166.3)

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
