library(tidyverse)
library(data.table)

names <- fread("./raw/colnames_fec_old.csv")

for(year in seq(08, 18, 2)){
  year <- str_pad(year, width = 2, pad = "0", side = "left")
  fec <- fread(paste0("./raw/old_data/weball", year, "/weball", year, ".txt"), sep = "|")
  colnames(fec) <- gsub("[.]", "_", make.unique(make.names(tolower(names$explanation))))
  fec <- filter(fec, total_receipts > 0)
  fwrite(select(fec, candidate_identification), paste0("./raw/old_data/weball", year, "/ids", year, ".csv"))
}


