### money FROM individuals TO anyone

participation_rate <- 1

library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

###### pac expenditures
exp_h <- fread("./raw/expenditures/pas2_header_file.csv")
exp <- rbindlist(lapply(seq(208, 218, 2), function(y){
  j <- fread(paste0("./raw/expenditures/pas", y, "/itpas2.txt"))
  colnames(j) <- colnames(exp_h)
  
  j <- j %>% 
    group_by(CMTE_ID) %>% 
    summarize(house_spending = sum(ifelse(substring(CAND_ID, 1, 1) == "H", TRANSACTION_AMT, 0)),
              total_spending = sum(TRANSACTION_AMT)) %>% 
    mutate(Cycle = y + 1800,
           share_house = house_spending / total_spending) %>% 
    select(-house_spending, -total_spending)
}))

### find out results from candidates file
results <- rbindlist(lapply(seq(2008, 2016, 2), function(year){
  j <- fread(paste0("./raw/results/results_clean_", year, ".csv")) %>% 
    group_by(V3, V4) %>% 
    filter(row_number() == 1) %>% ## top listed candidate in each district was winner
    ungroup() %>% 
    select(cand_id = V5) %>% 
    mutate(fec_election_yr = year) %>% 
    filter(substring(cand_id, 1, 1) == "H") %>% ## only care about the House
    mutate(general = "W")
}))


## read in candidate lookups from fec
## this lets us know where each individual ran

all_fec_lookup <- rbindlist(lapply(seq(8, 18, 2), function(year){
  header <- fread("./raw/lookup/fec_lookup/cn_header_file.csv")
  year <- str_pad(string = year, width = 2, side = "left", pad = "0")
  j <- fread(paste0("./raw/lookup/fec_lookup/cn", year, "/cn.txt"))
  colnames(j) <- colnames(header)
  j <- j %>% 
    mutate(filing_year = 2000 + as.integer(year)) %>% 
    select(cand_id = CAND_ID, CAND_ELECTION_YR, CAND_OFFICE_ST, CAND_OFFICE, CAND_OFFICE_DISTRICT, filing_year)
}))

all_fec_lookup <- unique(all_fec_lookup) %>% 
  filter(!is.na(CAND_OFFICE_DISTRICT))

#### unitemized data
#### download from here:
#### https://classic.fec.gov/data/CampaignAndCommitteeSummary.do?format=html&election_yr=2018

c <- c("HOUSE_SENATE_CAMPAIGNS", "INDEPENDENT_EXPENDITURE", "PAC", "PARTY", "PRESIDENTIAL_CAMPAIGNS")

unitemized <- rbindlist(lapply(c, function(x){
  k <- rbindlist(lapply(c(2008, 2010, 2012, 2014, 2016, 2018), function(y){
    j <- fread(paste0("./raw/unitemized", "_", y, "/", y, "_", x, "_DOWNLOAD.csv"))
    colnames(j) <- make.unique(colnames(j))
    j <- j %>% 
      mutate(type = x,
             filing_year = y)
  }))
  
}))

colnames(unitemized) <- tolower(colnames(unitemized))

## make these fields into actual numbers:
unitemized$indv_unitem_contb <- as.numeric(gsub("[$,]", "", unitemized$indv_unitem_contb))
unitemized$indv_contb <- as.numeric(gsub("[$,]", "", unitemized$indv_contb))
unitemized$oth_cmte_contb <- as.numeric(gsub("[$,]", "", unitemized$oth_cmte_contb))
unitemized$end_date <- as.Date(unitemized$cvg_end_dt)

## collapse down to candidate year level. already close, this cleans it up
unitemized_ll <- unitemized %>% 
  mutate(cand_id = ifelse(trimws(cand_id) == "", cmte_id, cand_id)) %>% 
  mutate_at(vars(indv_item_contb, indv_unitem_contb, cand_cntb), ~ ifelse(is.na(.), 0, .)) %>% 
  group_by(fec_election_yr, cand_id, filing_year, cand_election_yr) %>% 
  summarize(total_unitemized = sum(indv_unitem_contb, na.rm = T),
            candidate_contribution = sum(cand_cntb, na.rm = T),
            total_itemized = sum(indv_item_contb),
            ttl_receipts = sum(indv_item_contb + indv_unitem_contb + cand_cntb, na.rm = T))

unitemized_ll <- left_join(unitemized_ll,
                           all_fec_lookup,
                           by = c("cand_id" = "cand_id",
                                  "cand_election_yr" = "CAND_ELECTION_YR",
                                  "filing_year" = "filing_year")) %>% 
  group_by(cand_id, fec_election_yr, filing_year) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-cand_election_yr)

unitemized_ll <- left_join(unitemized_ll, results)

# cap is equal to half the amount spent in avg of the 20 most expensive winning campaigns

cap <- unitemized_ll %>% 
  filter(general == "W",
         CAND_OFFICE == "H") %>% 
  group_by(fec_election_yr) %>% 
  arrange(-ttl_receipts) %>% 
  filter(row_number() <= 20) %>% 
  group_by(fec_election_yr) %>% 
  summarize(cap = mean(ttl_receipts / 2))

unitemized_ll <- left_join(unitemized_ll, cap)

unitemized_ll <- left_join(unitemized_ll, exp, by = c("cand_id" = "CMTE_ID", "fec_election_yr" = "Cycle")) %>% 
  mutate(share_house = ifelse(is.na(share_house), 0, share_house),
         share_house = ifelse(substring(cand_id, 1, 1) == "H", 1, share_house))

#### itemized
#### all from center for responsive politics
#### https://www.opensecrets.org/bulk-data
#### U: vyasn@brennan.law.nyu.edu
#### P: Brennan1
#### where certain codes are dropped or only included (ie, "DI == 'D'"), that info comes from website user guide


###### money from individuals
# 
# itemized <- rbindlist(lapply(seq(2008, 2018, 2), function(year_x){
#   short_year <- str_pad(as.character(year_x - 2000), 2, side = "left", pad = "0")
#   individual_file <- fread(paste0("./raw/CampaignFin", short_year, "/indivs", short_year, ".txt"), sep = ",", quote = "|")
#   cols <- fread("./raw/lookup/individual_contributor_fields.csv")
#   colnames(individual_file) <- cols$Field
#   individual_file$Type <- trimws(individual_file$Type)
# 
#   donor_cand <- individual_file %>% 
#     filter(tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
#            !grepl("actblue", tolower(Contrib))) %>%  # no act blue donations
#     mutate(ContribID = ifelse(trimws(ContribID) == "", toupper(Contrib), ContribID)) %>% 
#     group_by(ContribID, RecipID, Cycle) %>% 
#     summarize(amount = sum(Amount, na.rm = T),
#               count = 1) %>% 
#     filter(amount >= 200)
#   
#   indiv_amounts <- donor_cand %>% 
#     group_by(ContribID) %>% 
#     summarize(total_given = sum(amount)) %>% 
#     mutate(gt_10k = total_given > 10000,
#            gt_100k = total_given > 100000,
#            gt_1m = total_given > 1000000,
#            lt_350 = total_given <= 350)
#   
#   cand_level <- left_join(donor_cand,
#                           indiv_amounts,
#                           by = "ContribID") %>% 
#     group_by(RecipID, Cycle) %>% 
#     mutate(amount_gt_10k = gt_10k * amount,
#            amount_gt_100k = gt_100k * amount,
#            amount_gt_1m = gt_1m * amount,
#            amount_lt_350 = lt_350 * amount) %>% 
#     summarize_at(vars(count, gt_10k, gt_100k, gt_1m, lt_350,
#                       starts_with("amount")), sum, na.rm = T) %>% 
#     mutate(type = "individual",
#            filing_year = year_x)
#   return(cand_level)
# }))
# 
#   
# 
# ## crp ids
# 
# ids <- unique(fread("./raw/lookup/CRPCandIDs.csv")) %>% ## CRP only uses 1 id per candidate
#   group_by(CID, Cycle) %>% #                         even if they run in multiple races
#   filter(row_number() == 1) %>% #                          thats dumb, but nothing to do
#   ungroup() %>% 
#   select(CID, Cycle, FECCandID)
# 
# ##
# 
# itemized <- left_join(itemized, ids, by = c("RecipID" = "CID", "Cycle")) %>% 
#   mutate(FECCandID = ifelse(is.na(FECCandID), RecipID, FECCandID))
# 
# saveRDS(itemized, "./temp/itemized_full.rds")

itemized <- readRDS("./temp/itemized_full.rds")

###
full_candidate_level <- left_join(unitemized_ll, itemized,
                                  by = c("cand_id"  = "FECCandID",
                                         "fec_election_yr" = "Cycle",
                                         "filing_year")) %>% 
  ungroup() %>% 
  rename(Cycle = fec_election_yr)

full_candidate_level <- full_candidate_level %>% 
  mutate_at(vars(ttl_receipts, total_itemized, total_unitemized, candidate_contribution, count),
            ~ ifelse(is.na(.), 0, .)) %>% 
  mutate(cap = ifelse(is.na(cap), Inf, cap),
         new_itemized = count * 7 * 200,
         new_unitemized = total_unitemized * 7,
         new_candidate_contrib = ifelse(candidate_contribution > 50000, 50000, candidate_contribution),
         old_total = ttl_receipts,
         new_total = ttl_receipts - total_itemized - total_unitemized - candidate_contribution +
           new_itemized + new_unitemized + new_candidate_contrib,
         change_perc = new_total / old_total,
         diff = new_total - old_total,
         eligible = CAND_OFFICE == "H" & !is.na(CAND_OFFICE) &
           ((total_unitemized + (count * 200)) >= 50000) & change_perc > 1) %>% 
  arrange(-change_perc) %>% 
  group_by(eligible, Cycle) %>% 
  ## eligible if in the house, top X% for participation rate,
  ## likely to have had $50,000 in eligible donations
  mutate(participate = eligible & (row_number() / n()) < participation_rate, ## find participants
         new_total = ifelse(participate, new_total, old_total), # total
         new_small = ifelse(participate, new_unitemized, total_unitemized), ## what old smalls look like now
         new_big = ifelse(participate, new_itemized, total_itemized), ## what old bigs look like now
         old_small = total_unitemized,
         old_big = total_itemized,
         share_small = new_unitemized / new_total,
         share_small = ifelse(is.na(share_small) | is.infinite(share_small), 0, share_small),
         public_money = ((count * 6 * 200) + old_small * 6) * participate,
         adj = ifelse((public_money > cap) & participate, public_money - cap, 0),
         public_money = public_money - adj,
         new_total = new_total - adj,
         new_small = new_small - (adj * share_small),
         new_big = new_big - (adj * (1 - share_small)),
         new_itemized_lt350 = ifelse(participate, lt_350 * 7 * 200, amount_lt_350),
         new_itemized_gt10k = ifelse(participate, gt_10k * 7 * 200, amount_gt_10k),
         new_itemized_gt100k = ifelse(participate, gt_100k * 7 * 200, amount_gt_100k),
         new_itemized_gt1m = ifelse(participate, gt_1m * 7 * 200, amount_gt_1m))

####
left_overs <- itemized %>% 
  filter(!(RecipID %in% full_candidate_level$RecipID)) %>% 
  select(Cycle, starts_with("amount")) %>% 
  group_by(Cycle) %>% 
  summarize_at(vars(starts_with("amount")), sum, na.rm = T) %>% 
  mutate(new_itemized_lt350 = amount_lt_350,
         new_itemized_gt10k = amount_gt_10k,
         new_itemized_gt100k = amount_gt_100k,
         new_itemized_gt1m = amount_gt_1m)


####
low_level <- bind_rows(full_candidate_level, left_overs) %>% 
  group_by(Cycle) %>% 
  summarize_at(vars(old_total, new_total,
                    old_small, new_small,
                    old_big,   new_big,
                    public_money,
                    new_itemized_gt10k, amount_gt_10k,
                    new_itemized_gt1m, amount_gt_1m), ~ sum(as.numeric(.), na.rm = T)) %>% 
  mutate(share_small_old = old_small / old_total,
         share_small_new = new_small / new_total,
         share_big_old = old_big / old_total,
         share_big_new = new_big / new_total,
         share_gt10k_old = amount_gt_10k / old_total,
         share_gt10k_new = new_itemized_gt10k / new_total,
         share_gt1m_old = amount_gt_1m / old_total,
         share_gt1m_new = new_itemized_gt1m / new_total)


fwrite(low_level, "./output/stats.csv")

############################# LIMIT TO ONLY HOUSE

j <- bind_rows(full_candidate_level, left_overs) %>% 
  filter(CAND_OFFICE != "H") %>% 
  group_by(Cycle) %>% 
  mutate_at(vars(old_total, new_total,
                 old_small, new_small,
                 old_big,   new_big,
                 public_money,
                 new_itemized_gt10k, amount_gt_10k,
                 new_itemized_gt1m, amount_gt_1m), ~ . * share_house) %>% 
  summarize_at(vars(old_total, new_total,
                    old_small, new_small,
                    old_big,   new_big,
                    public_money,
                    new_itemized_gt10k, amount_gt_10k,
                    new_itemized_gt1m, amount_gt_1m), ~ sum(as.numeric(.), na.rm = T)) %>% 
  mutate(share_small_old = old_small / old_total,
         share_small_new = new_small / new_total,
         share_big_old = old_big / old_total,
         share_big_new = new_big / new_total,
         share_gt10k_old = amount_gt_10k / old_total,
         share_gt10k_new = new_itemized_gt10k / new_total,
         share_gt1m_old = amount_gt_1m / old_total,
         share_gt1m_new = new_itemized_gt1m / new_total)


####
