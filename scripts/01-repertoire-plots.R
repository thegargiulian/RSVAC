#
# Authors:      MG
# Maintainers:  MG
# Last updated: FEBRUARY 2021
# =========================================
# scripts/01-repertoire-plots.R

# ----- setup

if (!(require(pacman))) {install.packages("pacman")}
pacman::p_load(here, dplyr, tidyr, glue, ggplot2, ggupset, stringr)

# declare constants, load reshape functions

args <- list(rsvac = here::here("data/RSVAC_1989_2015_03_29_20.xlsx"),
             functions = here::here("scripts/00-reshape-functions.R"))

source(args$functions)

# for nicer plot labels
plot_rep_elements <- c("Rape",
                       "Sexual slavery and forced marriage",
                       "Sexual torture",
                       "Sexual abuse",
                       "Sexual mutilation",
                       "Forced abortion and forced sterilization",
                       "Forced prostitution")

# ----- functions


# NB: only plots repertoires reported more than once for the specified actor type
# Acceptable actor_type inputs are "STATE" or "REBEL"
repetoire_bar_chart <- function(conflict_actor_data, actor_type, fill_color) {

  conflict_actor_data %>%
    filter(high_prev & actor_type_lab == !!actor_type) %>%
    select(id, !!plot_rep_elements, actor_type_lab) %>%
    gather(element, presence, -id, -actor_type_lab) %>%
    filter(presence == 1) %>%
    select(-presence) %>%
    group_by(id) %>%
    summarize(Repertoire = paste(unique(element), collapse = " & ")) %>%
    group_by(Repertoire) %>%
    summarize(rep = n()) %>%
    filter(rep > 1) %>%
    ggplot(aes(x = reorder(Repertoire, -rep), y = rep)) +
    geom_col(fill = fill_color, width = 0.75) +
    scale_x_discrete(labels = function(x) str_wrap(str_to_sentence(x), width = 10)) +
    labs(x = "Repertoire", y = "Count") +
    theme_minimal()

}


repertoire_upset_plot <- function(conflict_actor_data, high_prev) {

  if (high_prev) {

    conflict_actor_data <- conflict_actor_data %>%
      filter(high_prev)

  }

  conflict_actor_data %>%
    select(id, !!plot_rep_elements, actor_type_lab) %>%
    gather(element, presence, -id, -actor_type_lab) %>%
    filter(presence == 1) %>%
    select(-presence) %>%
    group_by(id, actor_type_lab) %>%
    summarize(Repertoire = list(unique(element))) %>%
    ggplot(aes(x = Repertoire, fill=actor_type_lab)) +
    theme_minimal() +
    geom_bar(position = "stack") +
    scale_x_upset(sets = plot_rep_elements, order_by = "freq") +
    theme_combmatrix(combmatrix.panel.point.color.fill = "black",
                     combmatrix.panel.line.size = 0,
                     combmatrix.label.text = element_text(color = "black"),
                     combmatrix.label.make_space = TRUE) +
    labs(y = "Count", title = "") +
    scale_fill_manual(name = "Actor type",
                      labels = c("Rebel", "State"),
                      values = c("firebrick1", "dodgerblue1"))

}


# ----- main

conflict_actor <- make_conflict_actor(load_rsvac(args$rsvac)) %>%
  filter(actor_type %in% c(1, 3, 5)) %>%
  mutate(Rape = R,
         `Sexual slavery and forced marriage` = SS,
         `Sexual torture` = ST,
         `Sexual abuse` = SA,
         `Sexual mutilation` = SM,
         `Forced prostitution`= FP,
         `Forced abortion and forced sterilization` = FA,
         id = glue("{actorid_new}-{conflictid_new}-{actor_type}"),
         actor_type_lab = if_else(actor_type %in% c(1, 5), "STATE", "REBEL"))

# Figure 1a
repetoire_bar_chart(conflict_actor, actor_type = "STATE", "dodgerblue1")

# Figure 1b
repetoire_bar_chart(conflict_actor, actor_type = "REBEL", "firebrick1")

# Figure A7
repertoire_upset_plot(conflict_actor, high_prev = TRUE)

 # Fiugre A8
repertoire_upset_plot(conflict_actor, high_prev = FALSE)
# done.