library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

#### unitemized data
#### download from here:
#### https://classic.fec.gov/data/CampaignAndCommitteeSummary.do?format=html&election_yr=2018

c <- c("house_senate", "independent_exp", "pac", "party", "presidential")

## start by reading in all the data from the FEC for 2016 and 2018, for each type
unitemized <- rbindlist(lapply(c, function(x){
  k <- rbindlist(lapply(c(16, 18), function(y){
    j <- fread(paste0("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/raw/unitemized/", x, "_", y, ".csv")) %>% 
      mutate(type = x,
             year = 2000 + y)
  }))
  
}))

## make these fields into actual numbers:
unitemized$ind_uni_con <- as.numeric(gsub("[$,]", "", unitemized$ind_uni_con))
unitemized$ind_con <- as.numeric(gsub("[$,]", "", unitemized$ind_con))
unitemized$other_com_con <- as.numeric(gsub("[$,]", "", unitemized$other_com_con))
unitemized$end_date <- as.Date(unitemized$cov_end_dat)

unitemized_ll <- unitemized %>% 
  filter(end_date <= "2018-06-20") %>% 
  group_by(year, type) %>% 
  summarize(total_unitemized = sum(ind_uni_con, na.rm = T))


#### itemized
#### all from center for responsive politics
#### https://www.opensecrets.org/bulk-data
#### U: vyasn@brennan.law.nyu.edu
#### P: Brennan1
#### where certain codes are dropped or only included (ie, "DI == 'D'"), that info comes from website user guide


cids <- read_xls("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/raw/lookup/CRP_IDs.xls",
                 sheet = "Candidate Ids - 2018")

colnames(cids) <- cids[13,]
cids <- cids[14:nrow(cids),2:ncol(cids)]

###### read in pac data
p18 <- fread("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/raw/CampaignFin18/pacs18.txt", sep = ",", quote = "|")
cols <- fread("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/raw/lookup/pacs_to_individs_fields.csv")
colnames(p18) <- cols$Field

pac_cand <- p18 %>% 
  filter(tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
         DI == "D") %>% 
  group_by(PACID, CID) %>% 
  summarize(amount = sum(Amount, na.rm = T))

pac_level <- pac_cand %>% 
  group_by(PACID) %>% 
  summarize(amount = sum(amount, na.rm = T)) %>% 
  arrange(-amount) %>% 
  rename(ContribID = PACID) %>% 
  mutate(type = "PAC")



###### money from individuals


d18 <- fread("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/raw/CampaignFin18/indivs18.txt", sep = ",", quote = "|")
cols <- fread("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/raw/lookup/individual_contributor_fields.csv")
colnames(d18) <- cols$Field
d18$Type <- trimws(d18$Type)


donor_cand <- d18 %>% 
  filter(trimws(ContribID) != "",
         tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
         !grepl("actblue", tolower(Contrib)), # no act blue donations
         RecipID %in% cids$CID) %>% 
  group_by(Contrib, ContribID, RecipID) %>% 
  summarize(amount = sum(Amount, na.rm = T)) %>% 
  filter(amount >= 200)

donor_level <- donor_cand %>% 
  group_by(ContribID) %>% 
  summarize(amount = sum(amount, na.rm = T)) %>% 
  arrange(-amount) %>% 
  mutate(type = "individual")

#### combine individuals and pacs
total_itemized <- bind_rows(pac_level, donor_level) %>% 
  arrange(-amount)


##
total_given <- sum(total_itemized$amount, na.rm = T) + ## total from open secrets
  sum(filter(unitemized_ll, ## next couple lines keep unitemized totals from 2018 to candidates
             year == 2018,
             type %in% c("house_senate", "presidential"))$total_unitemized)

total_given_NO_PACS <- sum(donor_level$amount, na.rm = T) + ## total from open secrets
  sum(filter(unitemized_ll, ## next couple lines keep unitemized totals from 2018 to candidates
             year == 2018,
             type %in% c("house_senate", "presidential"))$total_unitemized)

share_small <- sum(filter(unitemized_ll,
                          year == 2018,
                          type %in% c("house_senate", "presidential"))$total_unitemized) /
  total_given

share_small_NO_PAC <- sum(filter(unitemized_ll,
                          year == 2018,
                          type %in% c("house_senate", "presidential"))$total_unitemized) /
  total_given_NO_PACS

##
share_top_100 <- sum(filter(total_itemized, row_number() <= 100)$amount) / total_given
share_top_1000 <- sum(filter(total_itemized, row_number() <= 1000)$amount) / total_given

share_huntho <- sum(filter(total_itemized, amount >= 100000)$amount) / total_given
count_huntho <- nrow(filter(total_itemized, amount >= 100000))

share_mil <- sum(filter(total_itemized, amount >= 1000000)$amount) / total_given
count_mil <- nrow(filter(total_itemized, amount >= 1000000))

share_tentho <- sum(filter(total_itemized, amount >= 10000)$amount) / total_given
count_10tho <- nrow(filter(total_itemized, amount >= 10000))

share_twotho <- sum(filter(total_itemized, amount >= 2000)$amount) / total_given
share_350 <- (sum(filter(total_itemized, amount <= 350)$amount) +
                sum(filter(unitemized_ll,
                           year == 2018,
                           type %in% c("house_senate", "presidential"))$total_unitemized)) /
  total_given

amounts_data <- data.frame("group" = c("Less than $200", "$350 or less", "$10,000 or More", "$100,000 or More", "$1,000,000 or More"), 
                           "amount_with_pac" = c(sum(filter(unitemized_ll,
                                                   year == 2018,
                                                   type %in% c("house_senate", "presidential"))$total_unitemized),
                                        sum(filter(unitemized_ll,
                                                   year == 2018,
                                                   type %in% c("house_senate", "presidential"))$total_unitemized) + sum(filter(total_itemized, amount <= 350)$amount),
                                        sum(filter(total_itemized, amount >= 10000)$amount),
                                        sum(filter(total_itemized, amount >= 100000)$amount),
                                        sum(filter(total_itemized, amount >= 1000000)$amount)),
                           "amount_no_pac" = c(sum(filter(unitemized_ll,
                                                          year == 2018,
                                                          type %in% c("house_senate", "presidential"))$total_unitemized),
                                               sum(filter(unitemized_ll,
                                                          year == 2018,
                                                          type %in% c("house_senate", "presidential"))$total_unitemized) + sum(filter(total_itemized, amount <= 350, type != "PAC")$amount),
                                               sum(filter(total_itemized, amount >= 10000, type != "PAC")$amount),
                                               sum(filter(total_itemized, amount >= 100000, type != "PAC")$amount),
                                               sum(filter(total_itemized, amount >= 1000000, type != "PAC")$amount)))

amounts_data$total_given <- total_given
amounts_data$total_given_no_pac <- total_given_NO_PACS
amounts_data$share_individuals <- amounts_data$amount_no_pac / total_given_NO_PACS
amounts_data$share_all_money <- amounts_data$amount_with_pac / total_given
amounts_data$group <- factor(amounts_data$group, levels = c("Less than $200", "$350 or less", "$10,000 or More", "$100,000 or More", "$1,000,000 or More"))

fwrite(amounts_data, "H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/output/amounts.csv")

ggplot(amounts_data, aes(x = group, y = share_all_money)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + ylab("Share of Money Given") + xlab("") +
  ggtitle("Share of Money Given by Individuals and PACs\nTo Candidates") + geom_text(aes(x = group, 
                                                                                y = share_all_money + 0.01, label = percent(round(share_all_money, 3)),
                                                                                family = "Roboto")) +
  labs(caption = "Sources: FEC, OpenSecrets\n\nNotes: Includes data through June 20, 2018")
ggsave("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/output/all_money_given.png")

ggplot(amounts_data, aes(x = group, y = share_individuals)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + ylab("Share of Money Given") + xlab("") +
  ggtitle("Share of Money Given by Individuals\nTo Candidates") + geom_text(aes(x = group, 
                                                                                         y = share_individuals + 0.015, label = percent(round(share_individuals, 3)),
                                                                                         family = "Roboto")) +
  labs(caption = "Sources: FEC, OpenSecrets\n\nNotes: Includes data through June 20, 2018")

ggsave("H:/Public/Democracy/Voting Rights & Elections/data/misc/mip_2018/output/money_given_by_individuals.png")













