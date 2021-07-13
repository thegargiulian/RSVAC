#
# Authors:      MG
# Maintainers:  MG
# Last updated: FEBRUARY 2021
# =========================================
# scripts/00-reshape-functions.R

# this file contains helper functions used for the other `R` scripts in this
# repository: 01-repertoire-plots.R, 02-repertoire-width-depth.R, 03-maps.R.
# each of those scripts will source this file when run.

# --- setup

if (!(require(pacman))) {install.packages("pacman")}
pacman::p_load(readxl, dplyr, tidyr, assertr, tibble, glue, purrr)

# --- declare constants

fields <- c("actor", "actorid_new", "conflictid_new", "year", "location",
            "gwnoloc", "actor_type", "conflictyear", "interm", "postc")

rep_elements <- c("FA", "FP", "R", "SA", "SM", "SS", "ST")

conflict_actor_fields <- c("actorid_new", "conflictid_new", "actor_type", "gwnoloc")
conflict_actor_year_fields <- c("actorid_new", "conflictid_new", "actor_type", "year", "gwnoloc")
hp_indicators <- c("hp_FA", "hp_FP", "hp_R", "hp_SA", "hp_SM", "hp_SS", "hp_ST")

sheet_info <- tribble(~sheet_name, ~prev_suffix, ~viol_type,
                      "forced abortion", "forcedabor", "FA",
                      "forced prostitution", "forcedpros", "FP",
                      "rape", "rape", "R",
                      "sexual abuse", "sexualabus", "SA",
                      "sexual mutilation", "sexualmut", "SM",
                      "sexual slavery", "sexualslav", "SS",
                      "sexual torture", "sexualtor", "ST")

# --- functions


# input:    path to the RSVAC data excel file and a sheet name.
# output:   data from the sheet with the fields defined in the fields variable
#           above, prevalence data from the three sources, and a violation type
#           column; used as a helper function to `load_rsvac()`.
load_sheet <- function(rsvac_data, sheet_name) {

  prev_suffix <- sheet_info$prev_suffix[sheet_info$sheet_name == sheet_name]
  viol_type <- sheet_info$viol_type[sheet_info$sheet_name == sheet_name]

  sheet_data <- read_xlsx(rsvac_data, sheet = sheet_name) %>%
    select(!!fields,
           state_prev = !!glue("state_prev{prev_suffix}"),
           ai_prev = !!glue("ai_prev{prev_suffix}"),
           hrw_prev = !!glue("hrw_prev{prev_suffix}")) %>%
    mutate(viol_type = !!viol_type)

  if (sheet_name == "sexual torture") {

    sheet_data <- sheet_data %>%
      filter(!is.na(ai_prev) & !is.na(hrw_prev)) %>%
      mutate(state_prev = as.numeric(state_prev))

  }

  return(sheet_data)

}


# input:    path to the RSVAC data excel file and a prevalence threshold for
#           determining "high" prevalence; default threshold is 1
# output:   long data from all 7 RSVAC sheets combined into one dataframe with
#           an indicator for user-defined "high" prevalence and max prevalence.
#           NB: there is no data on forced pregnancy coded in the RSVAC dataset,
#           so that file is not loaded.
load_rsvac <- function(rsvac_data, high_prev_threshold = 1) {

  rsvac <- map_dfr(sheet_info$sheet_name, ~load_sheet(rsvac_data, .x)) %>%
    mutate(high_prev = (ai_prev > high_prev_threshold |
                          hrw_prev > high_prev_threshold |
                          state_prev > high_prev_threshold),
           max_prev = (ai_prev == 3 |
                         hrw_prev == 3 |
                         state_prev == 3))

  return(rsvac)

}


# input:    long RSVAC data created using the `load_RSVAC()` function. optionally
#           location_name = TRUE for returning the name of the location in
#           addition to the gwnoloc.
# output:   conflict-actor data for all conflicts with at least one year of
#           active conflict between 1985 and 2015 where at least one form of
#           sexual violence was perpetrated with prevalence 1, 2, or 3. Also
#           creates indicators for "high" prevalence and max prevalence.
make_conflict_actor <- function(rsvac_long, location_name = FALSE) {

  if (location_name) {
    conflict_actor_fields <- c(conflict_actor_fields, "location")
  }

  conflict_actor <- rsvac_long %>%
    filter(conflictyear == 1) %>%
    filter(!(state_prev + ai_prev + hrw_prev == 0)) %>%
    group_by(actorid_new, conflictid_new, actor_type) %>%
    mutate(FA = as.integer("FA" %in% viol_type),
           FP = as.integer("FP" %in% viol_type),
           R = as.integer("R" %in% viol_type),
           SA = as.integer("SA" %in% viol_type),
           SM = as.integer("SM" %in% viol_type),
           SS = as.integer("SS" %in% viol_type),
           ST = as.integer("ST" %in% viol_type),
           high_prev_tmp = any(high_prev),
           max_prev_tmp = any(max_prev)) %>%
    select(-high_prev, -max_prev) %>%
    rename(high_prev = high_prev_tmp,
           max_prev = max_prev_tmp) %>%
    select(!!conflict_actor_fields, !!rep_elements, high_prev, max_prev) %>%
    ungroup() %>%
    distinct()

  return(conflict_actor)

}


# input:    long RSVAC data created using the `load_RSVAC()` function.
# output:   conflict-actor-year data for all active conflict years between 1985
#           and 2015 where at least one form of sexual violence was perpetrated
#           with prevalence 1, 2, or 3.  Also creates indicators for "high"
#           prevalence and max prevalence.
make_conflict_actor_year <- function(rsvac_long, prev_threshold = 1) {

  conflict_actor_year <- rsvac_long %>%
    filter(conflictyear == 1) %>%
    filter(!(state_prev + ai_prev + hrw_prev == 0)) %>%
    mutate(hp_FA_tmp = case_when(viol_type == "FA" & high_prev ~ 1,
                                 TRUE ~ 0),
           hp_FP_tmp = case_when(viol_type == "FP" & high_prev ~ 1,
                                 TRUE ~ 0),
           hp_R_tmp = case_when(viol_type == "R" & high_prev ~ 1,
                                TRUE ~ 0),
           hp_SA_tmp = case_when(viol_type == "SA" & high_prev ~ 1,
                                 TRUE ~ 0),
           hp_SM_tmp = case_when(viol_type == "SM" & high_prev ~ 1,
                                 TRUE ~ 0),
           hp_SS_tmp = case_when(viol_type == "SS" & high_prev ~ 1,
                                 TRUE ~ 0),
           hp_ST_tmp = case_when(viol_type == "ST" & high_prev ~ 1,
                                 TRUE ~ 0)) %>%
    group_by(actorid_new, conflictid_new, actor_type, year) %>%
    mutate(FA = as.integer("FA" %in% viol_type),
           FP = as.integer("FP" %in% viol_type),
           R = as.integer("R" %in% viol_type),
           SA = as.integer("SA" %in% viol_type),
           SM = as.integer("SM" %in% viol_type),
           SS = as.integer("SS" %in% viol_type),
           ST = as.integer("ST" %in% viol_type),
           hp_FA = max(hp_FA_tmp),
           hp_FP = max(hp_FP_tmp),
           hp_R = max(hp_R_tmp),
           hp_SA = max(hp_SA_tmp),
           hp_SM = max(hp_SM_tmp),
           hp_SS = max(hp_SS_tmp),
           hp_ST = max(hp_ST_tmp),
           high_prev_tmp = any(high_prev),
           max_prev_tmp = any(max_prev)) %>%
    select(-high_prev, -max_prev, -hp_FA_tmp,
           -hp_FP_tmp, -hp_R_tmp, -hp_SA_tmp,
           -hp_SM_tmp, -hp_SS_tmp, -hp_ST_tmp) %>%
    rename(high_prev = high_prev_tmp,
           max_prev = max_prev_tmp) %>%
    select(!!conflict_actor_year_fields, !!rep_elements,
           !!hp_indicators, high_prev, max_prev) %>%
    arrange(year) %>%
    ungroup() %>%
    distinct()

  return(conflict_actor_year)

}


# done.
