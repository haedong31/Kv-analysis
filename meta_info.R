library(tidyverse)
library(readxl)

## utils #####
reorganize_meta <- function(meta_df) {
  for (i in 1:nrow(meta_df)) {
    r <- meta_df[i,]
    if (!is.na(r$weeks)) {
      temp_week <- r$weeks
    } else {
      meta_df[i, "weeks"] <- temp_week
    }
  }
 meta_df <- meta_df %>% 
   arrange(weeks)
 
 return(meta_df)
}

meta_info_ko <- read_excel("matching-table-ko.xlsx")
meta_info_wt <- read_excel("matching-table-wt.xlsx")

meta_info_ko_new <- reorganize_meta(meta_info_ko)
meta_info_wt_new <- reorganize_meta(meta_info_wt)

write_csv(meta_info_ko_new, "meta_info_ko.csv")
write_csv(meta_info_wt_new, "meta_info_wt.csv")
