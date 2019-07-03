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
  filter(end_date <= "2018-12-20",
         type %in% c("PRESIDENTIAL_CAMPAIGNS", "HOUSE_SENATE_CAMPAIGNS")) %>% 
  group_by(year = cand_election_yr, type) %>% 
  summarize(total_unitemized = sum(ind_uni_con, na.rm = T))

### district-level

unitemized$district <- substring(unitemized$cand_id, 3, 6)

district <- unitemized %>% 
  group_by(year = cand_election_yr, district) %>% 
  summarize(total_unitemized = sum(ind_uni_con, na.rm = T))



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
  
  
  adelson <- filter(individual_file, substring(ContribID, 1, 11) == "U0000000310")
  sum(adelson$Amount, na.rm = T)
  
  
  adelson_ll <- adelson %>% 
    group_by(RecipID) %>% 
    summarize(amount = sum(Amount, na.rm = T))
  
  donor_cand <- individual_file %>% 
    filter(trimws(ContribID) != "",
           tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
           !grepl("actblue", tolower(Contrib))) %>%  # no act blue donations
    group_by(hh = substring(ContribID, 1, 11), ContribID, RecipID) %>% 
    summarize(amount = sum(Amount, na.rm = T),
              Contrib = min(Contrib)) %>% 
    filter(amount >= 200)
  
  donor_level <- donor_cand %>% 
    group_by(ContribID, hh) %>% 
    summarize(amount = sum(amount, na.rm = T),
              Contrib = min(Contrib)) %>% 
    arrange(-amount) %>% 
    mutate(type = "individual")
  
  #### combine individuals and pacs
  total_itemized <- donor_level
  
  
  ##
  total_given <- sum(total_itemized$amount, na.rm = T) + ## total from open secrets
    sum(filter(unitemized_ll, ## next couple lines keep unitemized totals from study year to any entity
               year == year_x)$total_unitemized)
  
  count_10tho <-  nrow(filter(total_itemized, amount >= 10000))
  count_huntho <- nrow(filter(total_itemized, amount >= 100000))
  count_mil <-    nrow(filter(total_itemized, amount >= 1000000))
  
  amounts_data <- data.frame("group" = c("Less than $200", "$350 or less", "$10,000 or More", "$100,000 or More", "$1,000,000 or More"), 
                             "amount" = c(sum(filter(unitemized_ll,
                                                     year == year_x)$total_unitemized),
                                          sum(filter(unitemized_ll,
                                                     year == year_x)$total_unitemized) + sum(filter(total_itemized, amount <= 350)$amount),
                                          sum(filter(total_itemized, amount >= 10000)$amount),
                                          sum(filter(total_itemized, amount >= 100000)$amount),
                                          sum(filter(total_itemized, amount >= 1000000)$amount)),
                             "count" = c(NA, NA, count_10tho, count_huntho, count_mil))
  
  amounts_data$total_given <- total_given
  amounts_data$share_given <- amounts_data$amount / total_given
  amounts_data$group <- factor(amounts_data$group, levels = c("Less than $200", "$350 or less", "$10,000 or More", "$100,000 or More", "$1,000,000 or More"))
  
  fwrite(amounts_data, paste0("./output/amounts_", year_x, ".csv"))
  
  if(year_x == 2018){
    ggplot(amounts_data, aes(x = group, y = share_given)) + geom_col(color = "black") + theme_bc() +
      scale_y_continuous(labels = percent) + ylab("Share of Money Given") + xlab("Money that came from donors who gave...") +
      ggtitle(paste0("Share of Money Given By Donor Size, ", year_x), subtitle = "Money from Individuals to Any Entity") + geom_text(aes(x = group, 
                                                                                                                       y = share_given + 0.01, label = percent(round(share_given, 3)),
                                                                                                                       family = "Roboto")) +
      labs(caption = "Sources: FEC, OpenSecrets\n\nNotes: Includes data through December 20, 2018")
    ggsave(paste0("./output/individuals_to_anywhere_", year_x, ".png"))
  }else{
    ggplot(amounts_data, aes(x = group, y = share_given)) + geom_col(color = "black") + theme_bc() +
      scale_y_continuous(labels = percent) + ylab("Share of Money Given") + xlab("Money that came from donors who gave...") +
      ggtitle(paste0("Share of Money Given By Donor Size, ", year_x), subtitle = "Money from Individuals to Any Entity") + geom_text(aes(x = group, 
                                                                                                                       y = share_given + 0.01, label = percent(round(share_given, 3)),
                                                                                                                       family = "Roboto")) +
      labs(caption = "Sources: FEC, OpenSecrets")
      ggsave(paste0("./output/individuals_to_anywhere_", year_x, ".png"))
  }

}

amounts <- rbindlist(lapply(analysis_years, function(year){
  j <- fread(paste0("./output/amounts_", year, ".csv")) %>% 
    mutate(year = year)
}))

fwrite(amounts, "./mip_2018/output/amounts.csv")