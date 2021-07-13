#
# Authors:      MG
# Maintainers:  MG
# Last updated: FEBRUARY 2021
# =========================================
# scripts/02-repertoire-width-depth.R

# ----- setup

if (!(require(pacman))) {install.packages("pacman")}
pacman::p_load(here, dplyr, knitr)

# declare constants, load reshape functions

args <- list(rsvac = here::here("data/RSVAC_1989_2015_03_29_20.xlsx"),
             functions = here::here("scripts/00-reshape-functions.R"))

source(args$functions)

# ----- main

conflict_actor_year <- make_conflict_actor_year(load_rsvac(args$rsvac)) %>%
  filter(actor_type %in% c(1, 3, 5))

rsvac_actors <- unique(conflict_actor_year$actorid_new)
rsvac_conflicts <- unique(conflict_actor_year$conflictid_new)

# load all RSVAC obs for states + rebels, including active conflict years with 0
# prevalences; this is so we can calculate score changes below with all active
# conflict years for the conflict-actors coded in RSVAC
rsvac_all <- load_rsvac(args$rsvac) %>%
  filter(conflictyear == 1 & actor_type %in% c(1, 3, 5)) %>%
  select(actorid_new, conflictid_new, year, actor_type, gwnoloc) %>%
  distinct()

con_ac_yr <- merge(conflict_actor_year, rsvac_all, all = TRUE) %>%
  replace_na(list(FA = 0,
                  FP = 0,
                  R = 0,
                  SA = 0,
                  SM = 0,
                  SS = 0,
                  ST = 0,
                  hp_FA = 0,
                  hp_FP = 0,
                  hp_R = 0,
                  hp_SA = 0,
                  hp_SM = 0,
                  hp_SS = 0,
                  hp_ST = 0)) %>%
  mutate(FA_score = (FA + 2 * hp_FA),
         FP_score = (FP + 2 * hp_FP),
         R_score = (R + 2 * hp_R),
         SA_score = (SA + 2 * hp_SA),
         SM_score = (SM + 2* hp_SM),
         SS_score = (SS + 2 * hp_SS),
         ST_score = (ST + 2 * hp_ST),
         rep_size = (FA + FP + R + SA + SM + SS + ST)) %>%
  filter(actor_type %in% c(1, 3, 5)) %>%
  arrange(actorid_new, conflictid_new, year)

units_ <- con_ac_yr %>%
  select(actorid_new, conflictid_new) %>%
  distinct()

for(i in 1:nrow(units_)) {
  temp_ <- con_ac_yr[con_ac_yr$actorid_new == units_$actorid_new[i] &
                       con_ac_yr$conflictid_new == units_$conflictid_new[i],]

  con_ac_yr$FA_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$FA_score))
  con_ac_yr$FP_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$FP_score))
  con_ac_yr$R_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                             con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$R_score))
  con_ac_yr$SA_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$SA_score))
  con_ac_yr$SM_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$SM_score))
  con_ac_yr$SS_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$SS_score))
  con_ac_yr$ST_score_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$ST_score))

  con_ac_yr$rep_size_change[con_ac_yr$actorid_new == units_$actorid_new[i] &
                              con_ac_yr$conflictid_new == units_$conflictid_new[i]] <- c(-99, diff(temp_$rep_size))
}

con_ac_yr <- con_ac_yr %>%
  mutate(wide = rep_size > 2,
         any_hp = hp_FA | hp_FP | hp_R | hp_SA | hp_SM |hp_SS | hp_ST,
         any_hp2 = hp_FA + hp_FP + hp_R + hp_SA +  hp_SM + hp_SS + hp_ST >= 2,
         any_p = FA + FP + R + SA + SM + SS + ST >= 1)

# stability statistic used in text; 0 entry of table
prop.table(table(con_ac_yr$rep_size_change[con_ac_yr$rep_size_change != -99]))

# Table III
table_III_counts <- table(con_ac_yr$wide[con_ac_yr$any_p], con_ac_yr$any_hp[con_ac_yr$any_p])
table_III <- round(prop.table(table_III_counts, margin = 1), 3)
rownames(table_III) <- c("Narrow repertoire", "Wide repertoire")

kable(table_III,
      row.names = TRUE,
      col.names = c("Low prevalence", "High prevalence"),
      caption = "Violence and restraint at conflict-actor-year level (high prevalence in one or more forms)")

# Table A6
table_A6_counts <- table(con_ac_yr$wide[con_ac_yr$any_p], con_ac_yr$any_hp2[con_ac_yr$any_p])
table_A6 <- round(prop.table(table_A6_counts, margin = 1), 3)
rownames(table_A6) <- c("Narrow repertoire", "Wide repertoire")

kable(table_A6,
      row.names = TRUE,
      col.names = c("Low prevlaence", "High prevalence"),
      caption = "Violence and restraint at conflict-actor-year level (high prevalence in two or more forms)")

# Table A9
table_A9 <- con_ac_yr %>%
  select(matches("_score_change")) %>%
  pivot_longer(cols = everything(),
               names_to = "violation_type",
               values_to = "score_change") %>%
  filter(score_change != -99) %>%
  group_by(violation_type, score_change) %>%
  summarize(N = n()) %>%
  pivot_wider(names_from = violation_type, values_from = N) %>%
  arrange(score_change) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

kable(table_A9,
      col.names = c("Score change",
                    "Forced abortion and forced sterilization totals",
                    "Forced prostitution totals",
                    "Rape totals",
                    "Sexual abuse totals",
                    "Sexual mutilation totals",
                    "Sexual slavery and forced marriage totals",
                    "Sexual torture totals"),
      caption = "Score change frequency breakdown for sexual violence repertoire elements")

# repertoire stability statistics for R, ST, and SS/FM used in the text; 0
# entries of respective columns
table_A9 %>%
  mutate(across(ends_with("_score_change"), function(x) x / sum(x)))

# done.
