participation_rate <- 1
cap <- 

unitemized <- fread("./raw/test/candidate_summary_2016.csv") %>% 
  select(Cand_Id, Cand_Name, Individual_Unitemized_Contribution, Individual_Contribution,
         Cand_Contribution, Total_Contribution) %>% 
  filter(substring(Cand_Id, 1, 1) == "H") %>% 
  group_by(Cand_Id) %>% 
  summarize(no_match = sum(Individual_Unitemized_Contribution),
            candidate_contribution = sum(Cand_Contribution)) %>% 
  mutate(type = "unitemized",
         match_donors = no_match,
         match_gov = match_donors * 6,
         match_total = match_gov + match_donors)



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