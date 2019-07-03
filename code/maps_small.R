### district-level small donors
library(raster)
library(sp)
library(rgeos)
library(plotly)
library(rgdal)
library(tidycensus)
library(maptools)
library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

#### unitemized data
#### download from here:
#### https://www.fec.gov/data/browse-data/?tab=candidates

candidates <- rbindlist(lapply(seq(2008, 2018, 2), function(year){
  j <- fread(paste0("./raw/new_unitemized/candidate_summary_", year, ".csv")) %>% 
    filter(Cand_Office == "H") %>% 
    mutate(year = year) %>% 
    select(Cand_Id, Total_Receipt, Individual_Itemized_Contribution, Individual_Unitemized_Contribution,
           Cand_Office_St, Cand_Office_Dist, Total_Receipt, year)
}))

candidates <- left_join(candidates,
                      fips_codes %>% 
                        group_by(state) %>% 
                        filter(row_number() == 1) %>% 
                        select(state, state_code),
                      by = c("Cand_Office_St" = "state"))

district <- candidates %>% 
  group_by(dist = paste0(as.character(state_code), str_pad(as.character(Cand_Office_Dist), width = 2, side = "left", pad = "0")),
           year) %>% 
  summarize(total_individual = sum(Individual_Itemized_Contribution + Individual_Unitemized_Contribution),
            unitemized = sum(Individual_Unitemized_Contribution),
            total_receipt = sum(Total_Receipt)) %>% 
  mutate(share_small = unitemized / total_individual)


states_d <- district %>% 
   group_by(state = substring(dist, 1, 2),
            year) %>% 
   summarize(total_individual = sum(total_individual),
             unitemized = sum(unitemized),
             total_receipt = sum(total_receipt)) %>% 
   mutate(share_small = unitemized / total_individual)

########################## INDIVIDUAL LEVEL DATA FROM CRP

cand_ids <- fread("./raw/lookup/CRPCandIDs.csv")

individuals <- rbindlist(lapply(seq(08, 18, 2), function(year){
  year_s <- str_pad(as.character(year), pad = "0", width = 2, side = "left")
  individual_file <- fread(paste0("./raw/CampaignFin", year_s, "/indivs", year_s, ".txt"), sep = ",", quote = "|")
  cols <- fread("./raw/lookup/individual_contributor_fields.csv")
  colnames(individual_file) <- cols$Field
  individual_file$Type <- trimws(individual_file$Type)
  
  donor_cand <- individual_file %>% 
    filter(trimws(ContribID) != "",
           tolower(substring(RealCode, 1, 1)) != "z", #Z means bad things
           !grepl("actblue", tolower(Contrib))) %>%  # no act blue donations
    group_by(hh = substring(ContribID, 1, 11), ContribID, RecipID) %>% 
    summarize(amount = sum(Amount, na.rm = T),
              Contrib = min(Contrib)) %>% 
    filter(amount >= 200)
  
  candidate_level <- donor_cand %>% 
    group_by(RecipID) %>% 
    mutate(over_10k = (amount > 10000) * amount) %>% 
    summarize(amount = sum(amount, na.rm = T),
              over_10k = sum(over_10k, na.rm = T)) %>% 
    arrange(-amount) %>% 
    mutate(type = "individual")
  
  cs <- filter(cand_ids, year == year)
  
  candidate_level <- full_join(candidate_level, cs, by = c("RecipID" = "CID"))
  
  district_individ <- candidate_level %>% 
    filter(substring(DistIDRunFor, 3, 3) != "S",
           DistIDRunFor != "PRES") %>% 
    mutate(state = substring(DistIDRunFor, 1, 2)) %>% 
    group_by(district = DistIDRunFor, state) %>% 
    summarize(itemized = sum(amount, na.rm = T),
              over_10k = sum(over_10k, na.rm = T)) %>% 
    group_by(state) %>% 
    mutate(n = n(),
           district = ifelse(n == 1, paste0(state, "00"), district)) %>% 
    ungroup() %>% 
    mutate(year = year + 2000)
  return(district_individ)
}))



district_all <- full_join(district,
                          left_join(individuals %>% 
                                      mutate(state = substring(district, 1, 2)),
                                    fips_codes %>% 
                                      group_by(state) %>% 
                                      filter(row_number() == 1) %>% 
                                      select(state, state_code),
                                    by = "state") %>% 
                            mutate(district = paste0(state_code, substring(district, 3, 4))),
                          by = c("dist" = "district", "year")
                          )
district_all$share_over_10k <- district_all$over_10k / district_all$total_receipt


########### MAPS


####

cds <- readOGR("./raw/tl_2018_us_cd116",
                    "tl_2018_us_cd116")
cds <- spTransform(cds, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))


###
states <- readOGR("./raw/cb_2017_us_state_20m",
                  "cb_2017_us_state_20m")


states <- spTransform(states, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
####

cds <- intersect(states, cds)
cds@data$id <- rownames(cds@data)

alaska <- cds[cds$STATEFP.2=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(cds)

hawaii <- cds[cds$STATEFP.2=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(cds)

cds <- cds[(as.integer(as.character(cds$STATEFP.2)) <= 56) & (!cds$STATEFP.2 %in% c("02", "15")), ]

cds <- rbind(cds, hawaii, alaska)

cds_df <- fortify(cds)
cds <- left_join(cds_df, cds@data, by = c("id")) %>% 
  mutate(GEOID = as.character(GEOID.2))
rm(cds_df)

cds$CD116FP <- str_pad(ifelse(cds$CD116FP == "00", "01", as.character(cds$CD116FP)), width = 2, side = "left", pad = "0")

###
states@data$id <- rownames(states@data)

alaska <- states[states$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(states)

hawaii <- states[states$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(states)

states <- states[(as.integer(as.character(states$GEOID)) <= 56) & (!states$GEOID %in% c("02", "15")), ]

states <- rbind(states, hawaii, alaska)

states_df <- fortify(states)
states <- left_join(states_df, states@data, by = "id") %>% 
  mutate(GEOID = as.character(GEOID))

states <- left_join(states, states_d, by = c("STATEFP" = "state"))
###
cds <- left_join(cds, district_all, by = c("GEOID" = "dist"))
cds$share_small <- cds$unitemized / cds$total_receipt

for(y in seq(2008, 2010, 2)){
  plot <- ggplot() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)) +
    geom_polygon(data = filter(cds, year == y), aes(x = long, y = lat, group = group, fill = share_small)) +
    geom_path(data = states, aes(x = long, y = lat, group = group), color = "black") +
    coord_equal() +
    labs(x = NULL, y = NULL,
         fill = "Share of Money from Donors Giving < $200") +
    scale_fill_gradient(low = "#bfbfbf", high = "red", limits = c(0, 0.5),
                        oob = squish, labels = percent) +
    guides(fill = guide_legend(title.position = "top")) +
    ggtitle(paste0("Share of Money from Small Donors: ", y))
  ggsave(filename = paste0("./output/small_donor_map_", y, ".png"), plot = plot)
}
