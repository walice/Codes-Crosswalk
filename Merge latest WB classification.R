#setwd("C:/Users/Alice/Box Sync/PhD/Software/Codes-Masterlist") # Laptop
setwd("C:/boxsync/alepissier/PhD/Software/Codes-Masterlist") # Bren
library(tidyverse)
library(readxl)
library(openxlsx)

# Import three sheets
codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes") %>%
  select(-c(WB_Other, WB_Lending_Category))
regional <- read_xlsx("Codes_Masterlist.xlsx", 
                      sheet = "UN_Regional_Groupings")
wb <- read_xlsx("Codes_Masterlist.xlsx", 
                sheet = "World_Bank_Groupings")

# June 2018 classification
WB <- read_xls("CLASS.xls", skip = 4)
WB <- WB %>% 
  select(Economy, Code, Region, `Income group`, `Lending category`, Other) %>%
  rename(Country = Economy, ISO3166.3 = Code, WB_Income_Group = `Income group`,
         WB_Lending_Category = `Lending category`, WB_Other = Other) %>%
  filter(Country != "x")
WB$WB_Income_Group_Code2 <- NA
WB$WB_Income_Group_Code2 <- ifelse(WB$WB_Income_Group == "Low income", "LIC", WB$WB_Income_Group_Code2)
WB$WB_Income_Group_Code2 <- ifelse(WB$WB_Income_Group == "Lower middle income", "LMC", WB$WB_Income_Group_Code2)
WB$WB_Income_Group_Code2 <- ifelse(WB$WB_Income_Group == "Upper middle income", "UMC", WB$WB_Income_Group_Code2)
WB$WB_Income_Group_Code2 <- ifelse(WB$WB_Income_Group == "High income", "HIC", WB$WB_Income_Group_Code2)

no.group <- WB %>% filter(is.na(WB_Income_Group_Code2))

codes <- left_join(codes, WB %>% select(ISO3166.3, WB_Lending_Category, WB_Other, WB_Income_Group_Code2),
                   by = c("ISO3166.3"))
codes %>% filter(codes$WB_Income_Group_Code != WB_Income_Group_Code2) %>% nrow
codes$WB_Income_Group_Code2 <- NULL

codes <- codes %>%
  mutate_at(c("UN_Sub-region_Code", "UN_Intermediate_Region_Code", "UN_M49_Code", 
              "UN_LDC", "UN_LLDC", "UN_SIDS", "IMF_Code", 
              "OECD", "EU28", "Arab League"),
            as.numeric)
regional <- regional %>%
  mutate_at(c("UN_Region_Code", "UN_Sub-region_Code", "UN_Intermediate_Region_Code"),
            as.numeric)

codes <- codes %>%
  select(ISO3166.3, ISO3166.2, Country,
         UN_Region, UN_Region_Code, `UN_Sub-region`, `UN_Sub-region_Code`, UN_Intermediate_Region, UN_Intermediate_Region_Code,
         UN_M49_Code, UN_LDC, UN_LLDC, UN_SIDS, `UN_Developing-Developed`, IMF_Code,
         WB_Income_Group_Code, WB_Region, WB_Lending_Category, WB_Other,
         OECD, EU28, `Arab League`, Longitude, Latitude)
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
