# tidy version of data from the RSVAC data package
# Dumaine, Nordas, Gargiulo and Wood (2022)
# https://journals.sagepub.com/doi/abs/10.1177/00223433211044674

library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)

rsvac_path <- here::here("data-raw/RSVAC_1989_2015_03_29_20.xlsx")

rape <- read_xlsx(rsvac_path, sheet = "rape") %>%
    janitor::clean_names()  %>%
    # removing svac variables
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    rename_at(.vars = vars(ends_with("rape")),
              .funs = ~sub("[_]?rape$", "", .)) %>%
    mutate(form = "rape")

sexual_mutilation <- read_xlsx(rsvac_path, sheet = "sexual mutilation") %>%
    janitor::clean_names() %>%
    # removing svac variables
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    rename_at(.vars = vars(contains("sexual")),
              .funs = ~sub("sexualmut$|_sexual_mutilation", "", .)) %>%
    mutate(form = "sexual mutilation")

sexual_slavery <- read_xlsx(rsvac_path, sheet = "sexual slavery") %>%
    janitor::clean_names() %>%
    # removing svac variables
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    rename_at(.vars = vars(contains("sexual")),
              .funs = ~sub("sexualslav$|_sexual_slavery", "", .)) %>%
    mutate(form = "sexual slavery and forced marriage")

forced_prostitution <- read_xlsx(rsvac_path, sheet = "forced prostitution") %>%
    janitor::clean_names() %>%
    # removing svac variables
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    rename_at(.vars = vars(contains("forced")),
              .funs = ~sub("forcedpros$|_forced_prostitution", "", .)) %>%
    mutate(form = "forced prostitution")

forced_abortion <- read_xlsx(rsvac_path, sheet = "forced abortion") %>%
    janitor::clean_names()  %>%
    # removing svac variables
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    rename_at(.vars = vars(contains("forced")),
              .funs = ~sub("forcedabor$|_forced_sterilization_abortion", "", .)) %>%
    mutate(form = "forced abortion and forced sterilization")

sexual_torture <- read_xlsx(rsvac_path, sheet = "sexual torture") %>%
    janitor::clean_names() %>%
    # see note in Section 3.1.2 of coding manual
    mutate(state_prevsexualtor = if_else(str_detect(state_prevsexualtor, "See "),
                                         0,
                                         as.numeric(state_prevsexualtor)),
           ai_prevsexualtor = if_else(str_detect(state_prevsexualtor, "See "),
                                      0,
                                      as.numeric(state_prevsexualtor)),
           hrw_prevsexualtor = if_else(str_detect(state_prevsexualtor, "See "),
                                       0,
                                       as.numeric(state_prevsexualtor))) %>%
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    # removing svac variables
    rename_at(.vars = vars(contains("sexual")),
              .funs = ~sub("sexualtor$|_sexual_torture", "", .)) %>%
    mutate(form = "sexual torture")

sexual_abuse <- read_xlsx(rsvac_path, sheet = "sexual abuse") %>%
    janitor::clean_names() %>%
    select(-state_prev, -ai_prev, -hrw_prev, -form) %>%
    # removing svac variables
    rename_at(.vars = vars(contains("sexual")),
              .funs = ~sub("sexualabus$|_sexual_abuse", "", .)) %>%
    mutate(form = "sexual abuse")

rsvac <- bind_rows(rape,
                   sexual_mutilation ,
                   sexual_slavery,
                   forced_prostitution ,
                   forced_abortion,
                   sexual_torture,
                   sexual_abuse) %>%
    mutate(across(.cols = c("state_prev", "ai_prev", "hrw_prev"),
                  .fns = ~na_if(.x, -99)),
           mp = na_if(mp, -99),
           across(.cols = ends_with("_notes"),
                  .fns = ~na_if(.x, "-99"))) %>%
    relocate(form, .before = state_prev)

write_csv(rsvac, here::here("data-raw/rsvac.csv"))
usethis::use_data(rsvac, overwrite = TRUE)

# done.
