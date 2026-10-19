library(tidyverse)
library(haven)
library(srvyr)
library(patchwork)

#setwd("/Users/elliothidalgo/Downloads/PAI 741/CCES 2006-2024 Cumulative")
fig_dir <- "/Users/elliothidalgo/Downloads/PAI 741/PAI741-Git-Repo/figures"

#Step 1. Manage Cumulative CES
ces <- read_dta("cumulative_2006-2024.dta")

ces

#0. Find Variables

unique(ces$pid7)               #7 point pid (8 = not sure, 9 = don't know)
unique(ces$year)               #year (odd years are smaller surveys)
unique(ces$voted_turnout_self) #self-reported turnout, even years only
summary(ces$weight)            #survey weight (too many values for unique)

#1. Filter and select
#Only even (election) years, to match the biennial NES Bartels used
ces <- ces |>
  select(year, pid7, voted_turnout_self, weight) |>
  filter(year %% 2 == 0)

#Midterm vs. presidential flag, for filtering later
ces <- ces |>
  mutate(presyear = if_else(year %% 4 == 0, 1, 0))

#2. Manipulate

#Drop the not sures and DKs (Bartels' proportions only use the 7 point scale)
ces <- ces |>
  mutate(pid = zap_labels(pid7)) |>
  filter(pid %in% 1:7)

#Dummies for each category, so the means are proportions
ces <- ces |>
  mutate(
    strong  = if_else(pid %in% c(1, 7), 1, 0),
    weak    = if_else(pid %in% c(2, 6), 1, 0),
    leaners = if_else(pid %in% c(3, 5), 1, 0),
    indeps  = if_else(pid == 4, 1, 0),

    #For figure 2: strong or weak identifiers, and turnout
    identifiers = strong + weak,
    #In the .dta this is a labelled number, so as_factor turns it back into Yes/No
    turnout = case_when(
      as_factor(voted_turnout_self) == "Yes" ~ 1,
      as_factor(voted_turnout_self) == "No"  ~ 0
    )
  )

#3. Declare the survey and collapse the data

ces_svy <- ces |>
  as_survey_design(weights = weight)

figure1_master <- ces_svy |>
  group_by(year, presyear) |>
  summarise(
    strong  = survey_mean(strong,  na.rm = TRUE, vartype = NULL),
    weak    = survey_mean(weak,    na.rm = TRUE, vartype = NULL),
    leaners = survey_mean(leaners, na.rm = TRUE, vartype = NULL),
    indeps  = survey_mean(indeps,  na.rm = TRUE, vartype = NULL)
  )

figure2_master <- ces_svy |>
  filter(!is.na(turnout)) |>
  group_by(year, presyear, turnout) |>
  summarise(identifiers = survey_mean(identifiers, na.rm = TRUE, vartype = NULL)) |>
  mutate(turnout = factor(turnout, levels = c(1, 0), labels = c("Voters", "Nonvoters")))

#Check things
figure1_master
figure2_master


draw_panel <- function(data, solid, dashed, solid_lab, dashed_lab) {
  ggplot(data, aes(x = year)) +
    scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, 0.1), name = NULL) +
    scale_x_continuous(breaks = unique(data$year), name = NULL) +

    #Solid line with filled points
    geom_line(aes(y = .data[[solid]], linetype = solid_lab), linewidth = 0.7) +
    geom_point(aes(y = .data[[solid]], shape = solid_lab), size = 2.2) +

    #Dashed line with hollow points
    geom_line(aes(y = .data[[dashed]], linetype = dashed_lab), linewidth = 0.7) +
    geom_point(aes(y = .data[[dashed]], shape = dashed_lab), size = 2.2, stroke = 0.9) +

    scale_linetype_manual(values = setNames(c("solid", "dashed"), c(solid_lab, dashed_lab)), name = NULL) +
    scale_shape_manual(values = setNames(c(16, 1), c(solid_lab, dashed_lab)), name = NULL) +

    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = c(0.75, 0.2),
      legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3)
    )
}

draw_figure1 <- function(data, caption_text) {
  bartels1A <- draw_panel(data, "strong", "weak",
                          "\"Strong\" Identifiers", "\"Weak\" Identifiers")
  bartels1B <- draw_panel(data, "indeps", "leaners",
                          "\"Pure\" Independents", "Independent \"Leaners\"")

  
  #Top layer: titles, but no x axis
  #Bottom layer: x axis and caption, but no titles
  (bartels1A +
     labs(title = paste0("The Distribution of Party Identification, ",
                         min(data$year), "-", max(data$year)),
          subtitle = "Proportions of CES Sample") +
     theme(
       axis.title.x = element_blank(),
       axis.text.x  = element_blank(),
       axis.ticks.x = element_blank(),
       plot.title   = element_text(face = "bold")
     )
  ) /
    (bartels1B +
       labs(caption = caption_text) +
       theme(axis.title.x = element_blank())
    )
}

#Step 3. Figure 2 plot function

draw_figure2 <- function(data, caption_text) {
  ggplot(data, aes(x = year, y = identifiers, linetype = turnout, shape = turnout)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name = NULL) +
    scale_x_continuous(breaks = unique(data$year), name = NULL) +

    geom_line(linewidth = 0.7) +
    geom_point(size = 2.2, stroke = 0.9) +

    scale_linetype_manual(values = c("Voters" = "solid", "Nonvoters" = "dashed"), name = NULL) +
    scale_shape_manual(values = c("Voters" = 16, "Nonvoters" = 1), name = NULL) +

    labs(
      title = "Party Identification Among Voters and Nonvoters",
      subtitle = "Proportions of (Strong or Weak) Identifiers in CES Sample",
      caption = caption_text
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = c(0.15, 0.15),
      legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3),
      plot.title = element_text(face = "bold")
    )
}

#Figure 1, Presidential Years Only
bartels1 <- draw_figure1(figure1_master |> filter(presyear == 1),
                         "Presidential Election Years Only")
bartels1
ggsave(file.path(fig_dir, "bartels1_1presidential.pdf"), bartels1, width = 7, height = 10)

#Figure 1, Midterms Included
bartels1 <- draw_figure1(figure1_master, "Midterms Included")
bartels1
ggsave(file.path(fig_dir, "bartels1_2midterms.pdf"), bartels1, width = 7, height = 10)

#Figure 2, Presidential Years Only (Bartels' version)
bartels2 <- draw_figure2(figure2_master |> filter(presyear == 1),
                         "Presidential Election Years Only. Turnout is self-reported.")
bartels2
ggsave(file.path(fig_dir, "bartels2_1presidential.pdf"), bartels2, width = 7, height = 7)

#Figure 2, Midterms Included
bartels2 <- draw_figure2(figure2_master, "Midterms Included. Turnout is self-reported.")
bartels2
ggsave(file.path(fig_dir, "bartels2_2midterms.pdf"), bartels2, width = 7, height = 7)