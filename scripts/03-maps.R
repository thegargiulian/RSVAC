#
# Authors:      MG
# Maintainers:  MG
# Last updated: FEBRUARY 2021
# =========================================
# scripts/03-maps.R

# ----- setup

if (!(require(pacman))) {install.packages("pacman")}
pacman::p_load(here, dplyr, stringr, ggplot2, tidyr)

# declare constants, load reshape functions

args <- list(rsvac = here::here("data/RSVAC_1989_2015_03_29_20.xlsx"),
             functions = here::here("scripts/00-reshape-functions.R"))

source(args$functions)

# ----- main

conflict_actor <- make_conflict_actor(load_rsvac(args$rsvac), location_name = TRUE) %>%
  # standardize location names with `map_data` data
  mutate(location = str_replace_all(location,
                                    pattern = c("Serbia \\(Yugoslavia\\)"="Serbia",
                                                "Bosnia-Herzegovina"="Bosnia and Herzegovina",
                                                "DR Congo \\(Zaire\\)"="Democratic Republic of the Congo",
                                                "Democratic Republic of Congo \\(Zaire\\)"="Democratic Republic of the Congo",
                                                "Sri Lanka \\(Ceylon\\)"="Sri Lanka",
                                                "Myanmar \\(Burma\\)"="Myanmar",
                                                "Russia \\(Soviet Union\\)"="Russia",
                                                "^Congo$"="Republic of Congo",
                                                "Yemen \\(North Yemen\\)"="Yemen",
                                                "Cote d'Ivoire"="Ivory Coast"))) %>%
  distinct()

# creating a flag for locations where SV occurred for the merge with the maps data
sv_locations <- conflict_actor %>%
  select(location) %>%
  distinct() %>%
  mutate(any_sv = TRUE)

# make rebel data
rebels <- conflict_actor %>%
  filter(actor_type == 3) %>%
  group_by(location) %>%
  mutate(rebel_R = case_when(any(R == 1) ~ 1,
                             all(R == 0) ~ 0),
         rebel_SS = case_when(any(SS == 1) ~ 1,
                              all(SS == 0) ~ 0),
         rebel_ST = case_when(any(ST == 1) ~ 1,
                              all(ST == 0) ~ 0)) %>%
  ungroup() %>%
  full_join(map_data("world"), by = c("location"="region")) %>%
  left_join(sv_locations, by = "location") %>%
  mutate(rebel_R = case_when(is.na(rebel_R) & is.na(any_sv) ~ -1,
                             is.na(rebel_R) & any_sv ~ 0,
                             TRUE ~ rebel_R),
         rebel_SS = case_when(is.na(rebel_SS) & is.na(any_sv) ~ -1,
                              is.na(rebel_SS) & any_sv ~ 0,
                              TRUE ~ rebel_SS),
         rebel_ST = case_when(is.na(rebel_ST) & is.na(any_sv) ~ -1,
                              is.na(rebel_ST) & any_sv ~ 0,
                              TRUE ~ rebel_ST))

# make state data
states <- conflict_actor %>%
  filter(actor_type %in% c(1, 5)) %>%
  group_by(location) %>%
  mutate(state_R=case_when(any(R == 1) ~ 1,
                           all(R == 0) ~ 0),
         state_SS=case_when(any(SS == 1) ~ 1,
                            all(SS == 0) ~ 0),
         state_ST=case_when(any(ST == 1) ~ 1,
                            all(ST == 0) ~ 0)) %>%
  ungroup() %>%
  full_join(map_data("world"), by = c("location"="region")) %>%
  left_join(sv_locations, by = "location") %>%
  mutate(state_R = case_when(is.na(state_R) & is.na(any_sv) ~ -1,
                             is.na(state_R) & !is.na(any_sv) ~ 0,
                             TRUE ~ state_R),
         state_SS = case_when(is.na(state_SS) & is.na(any_sv) ~ -1,
                              is.na(state_SS) & !is.na(any_sv) ~ 0,
                              TRUE ~ state_SS),
         state_ST = case_when(is.na(state_ST) & is.na(any_sv) ~ -1,
                              is.na(state_ST) & !is.na(any_sv) ~ 0,
                              TRUE ~ state_ST))

# Figure A1
ggplot() +
  geom_polygon(data = states,
               aes(x = long, y = lat, group = group, fill = factor(state_R))) +
  coord_fixed(1.15) +
  labs(title = "", x = "", y = "") +
  scale_fill_manual(values = c("#999999", "#545454", "#1874cd"),
                    name = "",
                    breaks = c("-1", "0", "1"),
                    labels = str_wrap(c("No conflict",
                                        "Rape not reported",
                                        "Rape reported"),
                                      15)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Figure A2
ggplot() +
  geom_polygon(data = rebels,
               aes(x = long, y = lat, group = group, fill = factor(rebel_R))) +
  coord_fixed(1.15) +
  labs(title = "", x = "", y = "") +
  scale_fill_manual(values = c("#999999", "#545454", "#8b1a1a"),
                    name = "",
                    breaks = c("-1", "0", "1"),
                    labels = str_wrap(c("No conflict",
                                        "Rape not reported",
                                        "Rape reported"),
                                      15)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Figure A3
ggplot() +
  geom_polygon(data = states,
               aes(x = long, y = lat, group = group, fill = factor(state_ST))) +
  coord_fixed(1.15) +
  labs(title = "", x = "", y = "") +
  scale_fill_manual(values = c("#999999", "#545454", "#1874cd"),
                    name = "",
                    breaks = c("-1", "0", "1"),
                    labels = str_wrap(c("No conflict",
                                        "Non-penetrative sexual torture not reported",
                                        "Non-penetrative sexual torture reported"),
                                      15)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Figure A4
ggplot() +
  geom_polygon(data = rebels,
               aes(x = long, y = lat, group = group, fill = factor(rebel_ST))) +
  coord_fixed(1.15) +
  labs(title = "", x = "", y = "") +
  scale_fill_manual(values = c("#999999", "#545454", "#8b1a1a"),
                    name = "",
                    breaks = c("-1", "0", "1"),
                    labels = str_wrap(c("No conflict",
                                        "Non-penetrative sexual torture not reported",
                                        "Non-penetrative sexual torture reported"),
                                      15)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Figure A5
ggplot() +
  geom_polygon(data = states,
               aes(x = long, y = lat, group = group, fill = factor(state_SS))) +
  coord_fixed(1.15) +
  labs(title = "", x = "", y = "") +
  scale_fill_manual(values = c("#999999", "#545454", "#1874cd"),
                    name = "",
                    breaks = c("-1", "0", "1"),
                    labels = str_wrap(c("No conflict",
                                        "Sexual slavery and forced marriage not reported",
                                        "Sexual slavery and forced marriage reported"),
                                      15)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# Figure A6
ggplot() +
  geom_polygon(data = rebels,
               aes(x = long, y = lat, group = group, fill = factor(rebel_SS))) +
  coord_fixed(1.15) +
  labs(title = "", x = "", y = "") +
  scale_fill_manual(values = c("#999999", "#545454", "#8b1a1a"),
                    name = "",
                    breaks = c("-1", "0", "1"),
                    labels = str_wrap(c("No conflict",
                                        "Sexual slavery and forced marriage not reported",
                                        "Sexual slavery and forced marriage reported"),
                                      15)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

# done.
