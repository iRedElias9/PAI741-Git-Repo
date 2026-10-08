#---- Load packages ----------------------------------------------
library(tidyverse)
library(haven)
library(srvyr)

#---- Load data --------------------------------------------------=
cdf_path  <- "~/Downloads/PAI 741/ANES 1948-2020 Cumulative/anes_timeseries_cdf_stata_20220916.dta"
ts24_path <- "~/Downloads/PAI 741/ANES 2024 Study/anes_timeseries_2024_stata_20250808.dta"

#---- Find the variables ---------------------------------------------
#Before picking columns, search the files for them, Then I make a table of every column's name and label and search the labels.

cdf_info <- read_dta(cdf_path, n_max = 0)
cdf_vars <- tibble(name = names(cdf_info), label = map_chr(cdf_info, ~ attr(.x, "label", exact = TRUE) %||% ""))
cdf_vars |> filter(str_detect(label, regex("year|party id|weight|thermometer", ignore_case = TRUE))) |> print(n = 100)

ts24_info <- read_dta(ts24_path, n_max = 0)
ts24_vars <- tibble(name = names(ts24_info), label = map_chr(ts24_info, ~ attr(.x, "label", exact = TRUE) %||% ""))
ts24_vars |> filter(str_detect(label, regex("party id|weight|importance", ignore_case = TRUE))) |> print(n = 100)

#What I picked from the search:
#VCF0301 is the 7-point party ID summary (V241227x in 2024)
#V240107a is the full 2024 sample including paper (PAPI) respondents.
#V240108a leaves out the 245 paper respondents, so it would drop them.

#The cumulative file is big, so I only read the columns I need.
#VCF0004  = year
#VCF0301  = party ID, 1 (strong Dem) to 7 (strong Rep)
#VCF0009z = weight
#VCF0218  = Democratic Party thermometer (univariate graph 1)
#VCF0224  = Republican Party thermometer (univariate graph 1)
cdf <- read_dta(cdf_path,
                col_select = c(VCF0004, VCF0301, VCF0009z, VCF0218, VCF0224))

#V241227x = party ID in 2024, same 1-7 scale as VCF0301
#V240107a = 2024 pre-election weight
#V241228  = how important party is to identity (univariate graph 2)
ts24 <- read_dta(ts24_path,
                 col_select = c(V241227x, V240107a, V241228))

#rows, columns, and types
dim(cdf)
dim(ts24)
glimpse(cdf)
glimpse(ts24)

#how many people per year
cdf |> count(VCF0004) |> print(n = 40)

#---- Check the codes before recoding --------------------------------

print_labels(cdf$VCF0301)
print_labels(ts24$V241227x)

#Codes:
#VCF0301:  1-7 = party ID, 0 = DK / NA / other / refused
#V241227x: 1-7 = party ID, -9 refused, -8 don't know, -4 error
#Bartels puts the 7 points into 4 groups (footnote 2):
#1 or 7 = strong
#2 or 6 = weak
#3 or 5 = leaners
#4 = pure independents

#Every code and how often it shows up
cdf |> count(VCF0301)
ts24 |> count(V241227x)

#Codes by year (each column is one code)
cdf |>
  count(VCF0004, VCF0301) |>
  pivot_wider(names_from = VCF0301, values_from = n) |>
  print(n = 40)

#Weights. No missing weights, and early years are all 1.

summary(cdf$VCF0009z)
summary(ts24$V240107a)
cdf |>
  group_by(VCF0004) |>
  summarise(missing_wt = sum(is.na(VCF0009z)),
            min_wt = min(VCF0009z, na.rm = TRUE),
            max_wt = max(VCF0009z, na.rm = TRUE)) |>
  print(n = 40)

#---- Stack the cumulative file and the 2024 file --------------------
#zap_labels() takes off the value labels so the two files stack.

pid_cdf <- cdf |>
  zap_labels() |>
  select(year = VCF0004, pid7 = VCF0301, wt = VCF0009z) |>
  filter(year >= 1952)            # 7-point party ID starts in 1952

pid_24 <- ts24 |>
  zap_labels() |>
  mutate(year = 2024) |>
  select(year, pid7 = V241227x, wt = V240107a)

pid <- bind_rows(pid_cdf, pid_24) |>
  mutate(
    # Bartels's four groups. Any other code (0, -9, -8, -4) becomes NA.
    strength = case_when(pid7 %in% c(1, 7) ~ "Strong identifiers",
                         pid7 %in% c(2, 6) ~ "Weak identifiers",
                         pid7 %in% c(3, 5) ~ "Independent leaners",
                         pid7 == 4         ~ "Pure independents"),
    # Top panel = identifiers, bottom panel = independents
    panel = if_else(strength %in% c("Strong identifiers", "Weak identifiers"),
                    "Party identifiers", "Independents"),
    panel = factor(panel, levels = c("Party identifiers", "Independents")),
    presidential = year %% 4 == 0
  ) |>
  filter(!is.na(strength))

#Old codes next to new groups. Each code should line up with one group.
pid |> count(pid7, strength)

#---- Share in each group, by year -----------------------------------
#Out of people with a party ID that year.
#Unweighted is closest to Bartels. Weighted is needed from 2012 on,
#when ANES added web samples.

shares_unw <- pid |>
  count(year, presidential, panel, strength) |>
  group_by(year) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

shares_wtd <- pid |>
  filter(!is.na(wt)) |>
  count(year, presidential, panel, strength, wt = wt) |>
  group_by(year) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

#Each year should add to 1
shares_wtd |> group_by(year) |> summarise(total = sum(prop)) |> print(n = 50)

#Check against the paper (p. 36):
#Strong identifiers 24% (1976), 31% (1996)
#Pure independents  16% (1976),  9% (1996)
shares_unw |>
  filter(year %in% c(1976, 1996)) |>
  select(year, strength, prop) |>
  pivot_wider(names_from = strength, values_from = prop)

#---- Graph style ---------------------------------------------------

bartels_style <- theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),            # no panel titles
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),         # panels touch
        legend.position = "none",                 # legend is drawn inside the panels
        plot.title = element_text(face = "bold", size = 11),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 7))

#Univariate graphs follow the lecture: box around the plot, no grid lines
clean <- theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot")


bartels_shapes <- c("Strong identifiers"  = 16, "Weak identifiers"  = 21,
                    "Independent leaners" = 21, "Pure independents" = 16)
bartels_lines  <- c("Strong identifiers"  = "solid", "Weak identifiers"  = "dashed",
                    "Independent leaners" = "dashed", "Pure independents" = "solid")

#Legend for each panel, in Bartels's wording and spot
#(top panel lower right, bottom panel upper left).
#y = height of the legend line
#pos = where it starts across the panel (0 = left, 1 = right)
key <- tibble(
  strength = c("Strong identifiers", "Weak identifiers",
               "Independent leaners", "Pure independents"),
  label    = c('"Strong" Identifiers', '"Weak" Identifiers',
               'Independent "Leaners"', '"Pure" Independents'),
  panel    = factor(c("Party identifiers", "Party identifiers",
                      "Independents", "Independents"),
                    levels = c("Party identifiers", "Independents")),
  y        = c(0.17, 0.12, 0.45, 0.40),
  pos      = c(0.55, 0.55, 0.03, 0.03)
)

#---- Part A. Replicate Figure 1 (presidential years, 1952-1996) ----
fig_a <- shares_unw |> filter(presidential, year <= 1996)
key_a <- key |>
  mutate(x0 = min(fig_a$year) + pos * (max(fig_a$year) - min(fig_a$year)),
         x1 = x0 + 0.07 * (max(fig_a$year) - min(fig_a$year)))

ggplot(fig_a, aes(x = year, y = prop, shape = strength, linetype = strength)) +
  geom_line() +
  geom_point(size = 2, fill = "white") +
  # legend: line, circle, then words
  geom_segment(data = key_a, aes(x = x0, xend = x1, y = y, yend = y)) +
  geom_point(data = key_a, aes(x = (x0 + x1) / 2, y = y), size = 2, fill = "white") +
  geom_text(data = key_a, aes(x = x1, y = y, label = label),
            hjust = 0, nudge_x = 0.01 * (max(fig_a$year) - min(fig_a$year)), size = 3) +
  facet_wrap(~ panel, ncol = 1) +
  scale_shape_manual(values = bartels_shapes) +
  scale_linetype_manual(values = bartels_lines) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0.1, 0.5, 0.1)) +
  scale_x_continuous(breaks = seq(1956, max(fig_a$year), 8)) +
  labs(title = "Figure 1. The Distribution of Party Identification, 1952-1996",
       subtitle = "Proportions of National Election Study Sample",
       x = NULL, y = NULL,
       caption = "Source: ANES Cumulative Data File (VCF0301). Unweighted.") +
  bartels_style
ggsave("~/Downloads/PAI 741/PAI741-Git-Repo/figures/Problem set 7/fig1_replication.png", width = 5.5, height = 6.5, dpi = 300)

#---- Part B. Extend through 2020 (cumulative file only) ----
fig_b <- shares_wtd |> filter(presidential, year <= 2020)
key_b <- key |>
  mutate(x0 = min(fig_b$year) + pos * (max(fig_b$year) - min(fig_b$year)),
         x1 = x0 + 0.07 * (max(fig_b$year) - min(fig_b$year)))

ggplot(fig_b, aes(x = year, y = prop, shape = strength, linetype = strength)) +
  geom_line() +
  geom_point(size = 2, fill = "white") +
  # legend: line, circle, then words
  geom_segment(data = key_b, aes(x = x0, xend = x1, y = y, yend = y)) +
  geom_point(data = key_b, aes(x = (x0 + x1) / 2, y = y), size = 2, fill = "white") +
  geom_text(data = key_b, aes(x = x1, y = y, label = label),
            hjust = 0, nudge_x = 0.01 * (max(fig_b$year) - min(fig_b$year)), size = 3) +
  facet_wrap(~ panel, ncol = 1) +
  scale_shape_manual(values = bartels_shapes) +
  scale_linetype_manual(values = bartels_lines) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0.1, 0.5, 0.1)) +
  scale_x_continuous(breaks = seq(1956, max(fig_b$year), 8)) +
  labs(title = "The Distribution of Party Identification, 1952-2020",
       subtitle = "Proportions of National Election Study Sample",
       x = NULL, y = NULL,
       caption = "Source: ANES Cumulative Data File (VCF0301). Weighted (VCF0009z).") +
  bartels_style
ggsave("~/Downloads/PAI 741/PAI741-Git-Repo/figures/Problem set 7/fig1_through_2020.png", width = 5.5, height = 6.5, dpi = 300)

#---- Part C. Extend to 2024 (cumulative file + 2024 file) ----
fig_c <- shares_wtd |> filter(presidential)
key_c <- key |>
  mutate(x0 = min(fig_c$year) + pos * (max(fig_c$year) - min(fig_c$year)),
         x1 = x0 + 0.07 * (max(fig_c$year) - min(fig_c$year)))

ggplot(fig_c, aes(x = year, y = prop, shape = strength, linetype = strength)) +
  geom_line() +
  geom_point(size = 2, fill = "white") +
  # legend: line, circle, then words
  geom_segment(data = key_c, aes(x = x0, xend = x1, y = y, yend = y)) +
  geom_point(data = key_c, aes(x = (x0 + x1) / 2, y = y), size = 2, fill = "white") +
  geom_text(data = key_c, aes(x = x1, y = y, label = label),
            hjust = 0, nudge_x = 0.01 * (max(fig_c$year) - min(fig_c$year)), size = 3) +
  facet_wrap(~ panel, ncol = 1) +
  scale_shape_manual(values = bartels_shapes) +
  scale_linetype_manual(values = bartels_lines) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0.1, 0.5, 0.1)) +
  scale_x_continuous(breaks = seq(1956, max(fig_c$year), 8)) +
  labs(title = "The Distribution of Party Identification, 1952-2024",
       subtitle = "Proportions of National Election Study Sample",
       x = NULL, y = NULL,
       caption = paste("Sources: ANES Cumulative Data File (VCF0301), 1952-2020;",
                     "\nANES 2024 Time Series (V241227x). Weighted (VCF0009z; V240107a).")) +
  bartels_style
ggsave("~/Downloads/PAI 741/PAI741-Git-Repo/figures/Problem set 7/fig1_through_2024.png", width = 5.5, height = 6.5, dpi = 300)

#---- Part D. Include midterm years ----
# All survey years, presidential and midterm.
fig_d <- shares_wtd
key_d <- key |>
  mutate(x0 = min(fig_d$year) + pos * (max(fig_d$year) - min(fig_d$year)),
         x1 = x0 + 0.07 * (max(fig_d$year) - min(fig_d$year)))

ggplot(fig_d, aes(x = year, y = prop, shape = strength, linetype = strength)) +
  geom_line() +
  geom_point(size = 2, fill = "white") +
  # legend: line, circle, then words
  geom_segment(data = key_d, aes(x = x0, xend = x1, y = y, yend = y)) +
  geom_point(data = key_d, aes(x = (x0 + x1) / 2, y = y), size = 2, fill = "white") +
  geom_text(data = key_d, aes(x = x1, y = y, label = label),
            hjust = 0, nudge_x = 0.01 * (max(fig_d$year) - min(fig_d$year)), size = 3) +
  facet_wrap(~ panel, ncol = 1) +
  scale_shape_manual(values = bartels_shapes) +
  scale_linetype_manual(values = bartels_lines) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0.1, 0.5, 0.1)) +
  scale_x_continuous(breaks = seq(1956, max(fig_d$year), 8)) +
  labs(title = "The Distribution of Party Identification,\nAll ANES Years, 1952-2024",
       subtitle = "Proportions of National Election Study Sample",
       x = NULL, y = NULL,
       caption = paste("Includes midterm years. ANES fielded no midterm time series after 2002.",
                     "\nSources: ANES Cumulative Data File and ANES 2024 Time Series. Weighted.")) +
  bartels_style
ggsave("~/Downloads/PAI 741/PAI741-Git-Repo/figures/Problem set 7/fig1_with_midterms.png", width = 5.5, height = 6.5, dpi = 300)

#---- Univariate graph 1: affective polarization, 2020 ---------------
#Own-party thermometer minus other-party thermometer, partisans and leaners.
#Above zero = warmer toward own party.

#Thermometer codes (0-96, 97 = 97-100, 98/99 = DK/NA)
print_labels(cdf$VCF0218)
summary(cdf$VCF0218)
summary(cdf$VCF0224)

affpol <- cdf |>
  zap_labels() |>
  filter(VCF0004 == 2020,
         VCF0301 %in% c(1, 2, 3, 5, 6, 7),           # partisans and leaners only
         VCF0218 <= 97, VCF0224 <= 97,               # drops 98/99 (DK/NA)
         !is.na(VCF0009z)) |>
  mutate(gap = if_else(VCF0301 <= 3,
                       VCF0218 - VCF0224,             # Democrats: Dem minus Rep
                       VCF0224 - VCF0218),            # Republicans: Rep minus Dem
         wt = VCF0009z)

#How many people, and the range (should be -97 to 97)
nrow(affpol)
summary(affpol$gap)

ap_stats <- affpol |>
  as_survey_design(weights = wt) |>
  summarise(mean = survey_mean(gap),
            q    = survey_quantile(gap, c(0.25, 0.5, 0.75)),
            var  = survey_var(gap)) |>
  mutate(sd  = sqrt(var),
         iqr = q_q75 - q_q25)
ap_stats   # q_q50 is the median; q_q25 to q_q75 is the middle 50%

#Whiskers go out to the furthest value within 1.5 x the IQR
whisker_lo <- min(affpol$gap[affpol$gap >= ap_stats$q_q25 - 1.5 * ap_stats$iqr])
whisker_hi <- max(affpol$gap[affpol$gap <= ap_stats$q_q75 + 1.5 * ap_stats$iqr])

#Bars are 5 points wide and centered on 0, 5, 10, because people tend to answer in round numbers
ap_bins <- affpol |>
  mutate(bin = round(gap / 5) * 5) |>
  count(bin, wt = wt) |>
  mutate(pct = n / sum(n))

#Pct should sum to 1
sum(ap_bins$pct)

#Box plot goes just under the bars, sized off the tallest bar
top   <- max(ap_bins$pct)
box_y <- -0.12 * top     # middle of the box
box_h <-  0.05 * top     # half the box height

ap_sub <- paste0("Mean = ", round(ap_stats$mean, 1),
                 "   Median = ", ap_stats$q_q50,
                 "   SD = ", round(ap_stats$sd, 1),
                 "   Middle 50% = ", ap_stats$q_q25, " to ", ap_stats$q_q75,
                 "   n = ", nrow(affpol))

ggplot(ap_bins, aes(x = bin, y = pct)) +
  geom_col(width = 5, fill = "grey75", colour = "white") +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dotted") +
  annotate("segment", x = ap_stats$mean,  xend = ap_stats$mean,  y = 0, yend = top * 1.05) +
  annotate("segment", x = ap_stats$q_q50, xend = ap_stats$q_q50, y = 0, yend = top * 1.05,
           linetype = "dashed") +
  annotate("text", x = ap_stats$mean,  y = top * 1.09, label = "Mean",   hjust = 1.1, size = 3.3) +
  annotate("text", x = ap_stats$q_q50, y = top * 1.09, label = "Median", hjust = -0.1, size = 3.3) +
  # Box plot: whiskers, box, median line
  annotate("segment", x = whisker_lo, xend = ap_stats$q_q25, y = box_y, yend = box_y) +
  annotate("segment", x = ap_stats$q_q75, xend = whisker_hi, y = box_y, yend = box_y) +
  annotate("rect", xmin = ap_stats$q_q25, xmax = ap_stats$q_q75,
           ymin = box_y - box_h, ymax = box_y + box_h, fill = "white", colour = "black") +
  annotate("segment", x = ap_stats$q_q50, xend = ap_stats$q_q50,
           y = box_y - box_h, yend = box_y + box_h, linewidth = 0.9) +
  scale_y_continuous(breaks = seq(0, 1, 0.02), labels = scales::percent) +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(-100, 100, 50)) +
  labs(title = "Affective Polarization Among Partisans, 2020",
       subtitle = ap_sub,
       x = "Feeling toward own party minus feeling toward other party (thermometer points)",
       y = "Share of partisans",
       caption = paste("Box plot below the bars: box = middle 50%, thick line = median,",
                       "whiskers = furthest values within 1.5 x the box width.",
                       "\nPartisans include leaners. Below zero = rated the other party higher.",
                       "\nSource: ANES Cumulative Data File (VCF0218, VCF0224, VCF0301). Weighted (VCF0009z).")) +
  clean
ggsave("~/Downloads/PAI 741/PAI741-Git-Repo/figures/Problem set 7/univariate_affective_polarization.png", width = 8, height = 5.5, dpi = 300)

#---- Univariate graph 2: importance of party identity, 2024 ---------
#Only asked of people who call themselves a Democrat, Republican, or Independent.

#Codes (-9 refused, -1 not asked, 1-5 answers)
print_labels(ts24$V241228)
ts24 |> count(V241228)

#as_factor() turns the codes into their text labels.
#fct_relabel() takes the "1. " off the front of each label.
pimp <- ts24 |>
  filter(V241228 > 0, !is.na(V240107a)) |>
  mutate(importance = as_factor(V241228),
         importance = droplevels(importance),
         importance = fct_relabel(importance, ~ str_remove(.x, "^[0-9]+\\. "))) |>
  count(importance, wt = V240107a) |>
  mutate(pct = n / sum(n),
         cum_pct = cumsum(pct))   # median = first category where cum_pct >= 0.5

#pct should sum to 1
pimp
sum(pimp$pct)

median_cat <- pimp |> filter(cum_pct >= 0.5) |> slice(1) |> pull(importance)
median_cat

pimp <- pimp |> mutate(is_median = importance == median_cat)

# Dot plot instead of bars (position is easier to compare than length)
ggplot(pimp, aes(x = pct, y = fct_rev(importance))) +
  geom_point(aes(shape = is_median), fill = "white", size = 3) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), nudge_x = 0.02, size = 3.3) +
  scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 16)) +
  scale_x_continuous(limits = c(0, NA), labels = scales::percent,
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "How Important Is Your Party to Your Identity? 2024",
       subtitle = paste("Median category (filled circle):", median_cat),
       x = "Share of respondents asked the question", y = NULL,
       caption = paste("Asked only of respondents who call themselves a Democrat, Republican, or Independent.",
                       "\nSource: ANES 2024 Time Series (V241228). Weighted (V240107a).")) +
  clean
ggsave("~/Downloads/PAI 741/PAI741-Git-Repo/figures/Problem set 7/univariate_party_importance.png", width = 8, height = 4, dpi = 300)

getwd()