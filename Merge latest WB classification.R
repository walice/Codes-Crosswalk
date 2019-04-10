#setwd("C:/Users/Alice/Box Sync/PhD/Software/Codes-Masterlist") # Laptop
setwd("C:/boxsync/alepissier/PhD/Software/Codes-Masterlist") # Bren
library(openxlsx)
library(tidyverse)
library(readxl)

# Import three sheets
codes <- read_xlsx("Codes_Masterlist.xlsx", sheet = "Codes")
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

book <- createWorkbook()
hs <- createStyle(textDecoration = "Bold")
addWorksheet(book, "Codes")
writeData(book, sheet = "Codes", x = codes, headerStyle = hs)
addWorksheet(book, "UN_Regional_Groupings")
writeData(book, sheet = "UN_Regional_Groupings", x = regional, headerStyle = hs)
addWorksheet(book, "World_Bank_Groupings")
writeData(book, sheet = "World_Bank_Groupings", x = wb, headerStyle = hs)
setColWidths(book, sheet = "Codes", cols = 1:ncol(codes), widths = "auto")
setColWidths(book, sheet = "UN_Regional_Groupings", cols = 1:ncol(regional), widths = "auto")
setColWidths(book, sheet = "World_Bank_Groupings", cols = 1:ncol(wb), widths = "auto")

saveWorkbook(book, "Codes_Masterlist.xlsx", overwrite = TRUE)
