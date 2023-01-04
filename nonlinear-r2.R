library(tidyverse)
library(readxl)
library(PAmeasures)

# g <- "wt"
gen_t2tbl <- function(exp_num, g) {
  calib_hat_dir <- file.path(str_c("calib_",exp_num),str_c(g,"_yhat"))
  idv_data_dir <- file.path("mgat1ko_data",str_c(g,"-preprocessed-25s"))
 
  # file names of idv-model data
  matching_tbl <- read_excel(
    file.path("mgat1ko_data",str_c("matching-table-",g,".xlsx")))
  file_names <- matching_tbl$trace_file_name_25 %>% na.omit()
  num_files <- length(file_names)
  
  # nonlinear-R2-data-frame properties
  col_names <- c("file_name","R2","L2")
  
  for (i in seq_along(file_names)) {
    y <- read_excel(
      file.path(idv_data_dir,file_names[i]),
      col_names = TRUE
    )
    
    yhat <- read_excel(
      file.path(calib_hat_dir,file_names[i]),
      col_names = TRUE
    )
    
    r <- vector('list', length = length(col_names))
    names(r) <- col_names
    
    r[[1]] <- file_names[i]
    pam <- pam.nlm(y[[2]], yhat[[2]])
    r[[2]] <- as.numeric(pam[[1]])
    r[[3]] <- as.numeric(pam[[2]])
    
    if (i == 1) {
      pam_df <- tibble(!!!r)
    } else {
      pam_df <- add_row(pam_df, !!!r)
    }
  }
  return(pam_df)
}
 
exp_num <- "exp1"
pam_wt <- gen_t2tbl(exp_num, "wt")
pam_wt <- pam_wt %>% 
  mutate(group = "wt")
pam_mgat1ko <- gen_t2tbl(exp_num, "mgat1ko")
pam_mgat1ko <- pam_mgat1ko %>% 
  mutate(group = "mgat1ko")

write_excel_csv(pam_wt, 
                file.path(str_c("calib_",exp_num),"r2l2_wt.csv"))
write_excel_csv(pam_mgat1ko, 
                file.path(str_c("calib_",exp_num),"r2l2_mgat1ko.csv"))
