library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)


participation_rate <- 1

unitemized <- fread("./raw/new_unitemized/candidate_summary_2016.csv") %>% 
  #filter(Cand_Office == "H") %>% 
  mutate(year = 2016) %>% 
  select(Cand_Id, Total_Receipt, Individual_Itemized_Contribution, Individual_Unitemized_Contribution,
         Cand_Office_St, Cand_Office_Dist, Total_Receipt, year, Cand_Contribution) %>% 
  group_by(Cand_Id, year) %>% 
  summarize(small_donors = sum(Individual_Unitemized_Contribution),
            candidate_contribution = sum(Cand_Contribution)) %>% 
  mutate(type = "unitemized",
         match_gov = small_donors * 6,
         total_no_match = small_donors)


#### get total money
individual_file <- fread("./raw/CampaignFin16/indivs16.txt", sep = ",", quote = "|")
cols <- fread("./raw/lookup/individual_contributor_fields.csv")
colnames(individual_file) <- cols$Field

individual_file <- individual_file %>% 
  filter(trimws(ContribID) != "",
       tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
       !grepl("actblue", tolower(Contrib))) %>%  # no act blue donations
  group_by(hh = substring(ContribID, 1, 11), ContribID, RecipID) %>% 
  summarize(amount = sum(Amount, na.rm = T),
            Contrib = min(Contrib)) %>% 
  filter(amount >= 200)


small_no_match <- sum(unitemized$small_donors)
small_match <- (small_no_match * 7) - sum(unitemized$candidate_contribution)

  tot_no_match <- sum(individual_file$amount) + small_no_match
tot_match <- sum(individual_file$amount) + small_match
##### 2016, for instance

ids <- read_xlsx("./raw/test/CRP_IDs.xlsx", sheet = "Candidate IDs - 2016")

d16 <- left_join(readRDS("./temp/donor_cand_2016.rds"),
                 ids,
                 by = c("RecipID" = "CID")) %>% 
  filter(!is.na(FECCandID),
         substring(FECCandID, 1, 1) == "H")


small <- d16[d16$FECCandID %in% unitemized$Cand_Id, ]


candidate_amount <- small %>% 
  group_by(FECCandID) %>% 
  summarize(no_match = sum(amount),
            donors = n()) %>% 
  mutate(match_donors = donors * 200,
         match_gov = donors * 1200,
         match_total = match_donors + match_gov,
         type = "itemized") %>% 
  select(-donors) %>% 
  rename(Cand_Id = FECCandID)

ll <- bind_rows(candidate_amount, unitemized) %>% 
  group_by(Cand_Id) %>% 
  summarize(match = sum(match),
            no_match = sum(no_match),
            gov = sum(gov),
            candidate_contribution = sum(candidate_contribution, na.rm = T)) %>% 
  mutate(total_match = match + min(candidate_contribution, 50000),
    improvement = total_match / no_match) %>% 
  arrange(-improvement) %>% 
  mutate(participate = row_number() / nrow(ll) <= participation_rate,
         raised = ifelse(participate == T, match, no_match))