### money FROM individuals TO anyone

library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

#### unitemized data
#### download from here:
#### https://classic.fec.gov/data/CampaignAndCommitteeSummary.do?format=html&election_yr=2018

c <- c("HOUSE_SENATE_CAMPAIGNS", "INDEPENDENT_EXPENDITURE", "PAC", "PARTY", "PRESIDENTIAL_CAMPAIGNS")

## start by reading in all the data from the FEC for 2016 and 2018, for each type
unitemized <- rbindlist(lapply(c, function(x){
  k <- rbindlist(lapply(c(2008, 2010, 2012, 2014, 2016, 2018), function(y){
    j <- fread(paste0("./raw/unitemized", "_", y, "/", y, "_", x, "_DOWNLOAD.csv"))
    colnames(j) <- make.unique(colnames(j))
    j <- j %>% 
      mutate(type = x,
             year = y)
  }))
  
}))

colnames(unitemized) <- tolower(colnames(unitemized))

## make these fields into actual numbers:
unitemized$ind_uni_con <- as.numeric(gsub("[$,]", "", unitemized$indv_unitem_contb))
unitemized$ind_con <- as.numeric(gsub("[$,]", "", unitemized$indv_contb))
unitemized$other_com_con <- as.numeric(gsub("[$,]", "", unitemized$oth_cmte_contb))
unitemized$end_date <- as.Date(unitemized$cvg_end_dt)

unitemized_ll <- unitemized %>% 
  filter(end_date <= "2018-12-20") %>% 
  mutate(id = cmte_id,
         name = ifelse(cand_id == "", cmte_nm, cand_name)) %>% 
  group_by(year, type, id, name) %>% 
  summarize(total_unitemized = sum(ind_uni_con, na.rm = T),
            total_individual = sum(indv_contb, na.rm = T),
            total_contributions = sum(ttl_contb, na.rm = T))

saveRDS(unitemized_ll, "./temp/collapsed_unitem.rds")

#### itemized
#### all from center for responsive politics
#### https://www.opensecrets.org/bulk-data
#### U: vyasn@brennan.law.nyu.edu
#### P: Brennan1
#### where certain codes are dropped or only included (ie, "DI == 'D'"), that info comes from website user guide


###### money from individuals

analysis_years <- c(2008, 2010, 2012, 2014, 2016, 2018)

for(year_x in analysis_years){
  short_year <- str_pad(as.character(year_x - 2000), 2, side = "left", pad = "0")
  individual_file <- fread(paste0("./raw/CampaignFin", short_year, "/indivs", short_year, ".txt"), sep = ",", quote = "|")
  cols <- fread("./raw/lookup/individual_contributor_fields.csv")
  colnames(individual_file) <- cols$Field
  individual_file$Type <- trimws(individual_file$Type)
  
  donor_cand <- individual_file %>% ## get rid of any contributions to a candidate less than 200 - already in the unitemized
    filter(trimws(ContribID) != "",
           tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
           !grepl("actblue", tolower(Contrib))) %>%  # no act blue donations
    group_by(hh = substring(ContribID, 1, 11), ContribID, RecipID) %>% 
    summarize(amount = sum(Amount, na.rm = T),
              Contrib = min(Contrib)) %>% 
    filter(amount >= 200)
  
  recip_level <- donor_cand %>% 
    group_by(RecipID) %>% 
    summarize(amount = sum(amount, na.rm = T)) %>% 
    mutate(type = "itemized")
  
  saveRDS(donor_cand, paste0("./temp/donor_cand_", year_x, ".rds"))
}