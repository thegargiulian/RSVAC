# tidy SVAC data
# updated version of the daatabase original published by Cohen and Nordas (2014)
# http://www.sexualviolencedata.org/
# https://journals.sagepub.com/doi/10.1177/0022343314523028?etoc=

library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# SVAC version 3.2 (complete)
if (!file.exists(here::here("data-raw/SVAC_3.2_complete.xlsx"))) {
    download.file("http://www.sexualviolencedata.org/wp-content/uploads/2023/05/SVAC_3.2_complete.xlsx",
                  here::here("data-raw/SVAC_3.2_complete.xlsx"))
}

svac <- read_xlsx(here::here("data-raw/SVAC_3.2_complete.xlsx"))

svac <- read_xlsx(rsvac_path, sheet = "SVAC 1989-2015") %>%
    mutate(state_prev = na_if(state_prev, -99),
           ai_prev = na_if(ai_prev, -99),
           hrw_prev = na_if(hrw_prev, -99),
           form = na_if(form, "-99"))

write_csv(svac, here::here("data-raw/svac.csv"))
usethis::use_data(svac, overwrite = TRUE)

# done.
