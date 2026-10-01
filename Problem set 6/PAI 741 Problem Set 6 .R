# ---- Load the packages ----------------------------------------------
library(tidyverse)
library(haven)
library(srvyr)

# ---- Load the data --------------------------------------------------

gss_path <- "~/Downloads/PAI 741/GSS 1972-2024 Cumulative/gss7224_r1.dta"

gss <- read_dta(gss_path)

# Quick check: rows and columns, and how many people per year
dim(gss)
gss |> count(year) |> print(n = 50)

# ---- Filter to keep the data I need ---------------------------------
# Variables ending in 'y' are the gender-neutral wording versions asked to
# half the sample in 2021-2024. Only some items have one.

tol <- gss |>
  select(year, wtssps,
         spkath,  colath,  libath,  spkathy, libathy,
         spkrac,  colrac,  librac,  spkracy, libracy,
         spkcom,  colcom,  libcom,  spkcomy, colcomy, libcomy,
         spkmil,  colmil,  libmil,  spkmily, libmily,
         spkhomo, colhomo, libhomo, spkhomoy, libhomoy) |>
  mutate(year   = as.numeric(year),
         wtssps = as.numeric(wtssps))

# Quick check: 30 columns
summary(tol)

# ---- Recode each item: 1 = intolerant, 0 = tolerant -----------------
# Codes (check with print_labels):

print_labels(tol$spkath)
print_labels(tol$colath)
print_labels(tol$colcom)
print_labels(tol$colcomy)
print_labels(tol$libath)
print_labels(tol$spkmily)
print_labels(tol$libmily)
print_labels(tol$spkhomoy)
print_labels(tol$libhomoy)

#   SPK items: 1 = allowed to speak,  2 = not allowed   -> 2 is intolerant
#   COL items: 4 = allowed to teach,  5 = not allowed   -> 5 is intolerant
#   COLCOM:    4 = should be fired,   5 = not fired     -> 4 is intolerant
#   COLCOMY:   1 = should be fired,   2 = not fired     -> 1 is intolerant
#   LIB items: 1 = remove the book,   2 = do not remove -> 1 is intolerant
#   If the mily/homoy labels above differ from their originals, change the numbers in those lines below.
#
# "Don't know" counts as intolerant (Mondak and Sanders, footnote 3).
# is_tagged_na(x, "d") is TRUE when the answer was "don't know".
# Anything else (not asked, no answer) becomes NA automatically.
# Where a 'y' version exists, each line checks both versions with | ("or").

tol <- tol |>
  mutate(
    # Atheists
    i_spkath = case_when(spkath == 2 | spkathy == 2 ~ 1,
                         spkath == 1 | spkathy == 1 ~ 0,
                         is_tagged_na(spkath, "d") | is_tagged_na(spkathy, "d") ~ 1),
    i_colath = case_when(colath == 5 ~ 1,
                         colath == 4 ~ 0,
                         is_tagged_na(colath, "d") ~ 1),
    i_libath = case_when(libath == 1 | libathy == 1 ~ 1,
                         libath == 2 | libathy == 2 ~ 0,
                         is_tagged_na(libath, "d") | is_tagged_na(libathy, "d") ~ 1),
    # Racists
    i_spkrac = case_when(spkrac == 2 | spkracy == 2 ~ 1,
                         spkrac == 1 | spkracy == 1 ~ 0,
                         is_tagged_na(spkrac, "d") | is_tagged_na(spkracy, "d") ~ 1),
    i_colrac = case_when(colrac == 5 ~ 1,
                         colrac == 4 ~ 0,
                         is_tagged_na(colrac, "d") ~ 1),
    i_librac = case_when(librac == 1 | libracy == 1 ~ 1,
                         librac == 2 | libracy == 2 ~ 0,
                         is_tagged_na(librac, "d") | is_tagged_na(libracy, "d") ~ 1),
    # Communists (COLCOM 4 = fired; COLCOMY 1 = fired)
    i_spkcom = case_when(spkcom == 2 | spkcomy == 2 ~ 1,
                         spkcom == 1 | spkcomy == 1 ~ 0,
                         is_tagged_na(spkcom, "d") | is_tagged_na(spkcomy, "d") ~ 1),
    i_colcom = case_when(colcom == 4 | colcomy == 1 ~ 1,
                         colcom == 5 | colcomy == 2 ~ 0,
                         is_tagged_na(colcom, "d") | is_tagged_na(colcomy, "d") ~ 1),
    i_libcom = case_when(libcom == 1 | libcomy == 1 ~ 1,
                         libcom == 2 | libcomy == 2 ~ 0,
                         is_tagged_na(libcom, "d") | is_tagged_na(libcomy, "d") ~ 1),
    # Militarists
    i_spkmil = case_when(spkmil == 2 | spkmily == 2 ~ 1,
                         spkmil == 1 | spkmily == 1 ~ 0,
                         is_tagged_na(spkmil, "d") | is_tagged_na(spkmily, "d") ~ 1),
    i_colmil = case_when(colmil == 5 ~ 1,
                         colmil == 4 ~ 0,
                         is_tagged_na(colmil, "d") ~ 1),
    i_libmil = case_when(libmil == 1 | libmily == 1 ~ 1,
                         libmil == 2 | libmily == 2 ~ 0,
                         is_tagged_na(libmil, "d") | is_tagged_na(libmily, "d") ~ 1),
    # Homosexuals
    i_spkhomo = case_when(spkhomo == 2 | spkhomoy == 2 ~ 1,
                          spkhomo == 1 | spkhomoy == 1 ~ 0,
                          is_tagged_na(spkhomo, "d") | is_tagged_na(spkhomoy, "d") ~ 1),
    i_colhomo = case_when(colhomo == 5 ~ 1,
                          colhomo == 4 ~ 0,
                          is_tagged_na(colhomo, "d") ~ 1),
    i_libhomo = case_when(libhomo == 1 | libhomoy == 1 ~ 1,
                          libhomo == 2 | libhomoy == 2 ~ 0,
                          is_tagged_na(libhomo, "d") | is_tagged_na(libhomoy, "d") ~ 1)
  )

# Quick check: old codes next to new codes. Each original code should line
# up with one recoded value (e.g., colcom 4 or colcomy 1 -> i_colcom 1).
tol |> count(colcom, colcomy, i_colcom)
tol |> count(spkmil, spkmily, i_spkmil)
tol |> count(libhomo, libhomoy, i_libhomo)

# ---- Build the scales -----------------------------------------------
# Adding with + gives NA if any item is NA, so a person only gets a score
# when they answered every item in that scale.

tol <- tol |>
  mutate(
    racist  = i_spkrac + i_colrac + i_librac,                    # 0-3
    homo    = i_spkhomo + i_colhomo + i_libhomo,                 # 0-3
    intol9  = i_spkath + i_colath + i_libath +
              i_spkrac + i_colrac + i_librac +
              i_spkcom + i_colcom + i_libcom,                    # 0-9
    intol15 = intol9 +
              i_spkmil + i_colmil + i_libmil +
              i_spkhomo + i_colhomo + i_libhomo,                 # 0-15
    intolerant15 = if_else(intol15 > 0, 1, 0),                   # Figure 5
    intolerant9  = if_else(intol9 > 0, 1, 0)
  )

# Quick check: scores stay inside their ranges (intol15 0-15, intol9 0-9)
summary(tol$intol15)
summary(tol$intol9)

# Quick check: which years have usable scores?
# In 2021, n15 should be close to n9 (both wording versions counted).
tol |>
  group_by(year) |>
  summarise(n15 = sum(!is.na(intol15)),
            n9  = sum(!is.na(intol9)),
            .groups = "drop") |>
  filter(n9 > 0) |>
  print(n = 50)

# ---- Replication, 1976-1998 (unweighted) ----------------------------

rep <- tol |>
  filter(year >= 1976, year <= 1998, !is.na(intol15)) |>
  group_by(year) |>
  summarise(fig1   = mean(intol15),
            racist = mean(racist),
            homo   = mean(homo),
            fig5   = mean(intolerant15),
            n      = n(),
            .groups = "drop")

# Quick check against the paper:
#   Fig 1: 7.24 (1977), 5.28 (1998)
#   Fig 2: racists 1.43 (1985), 1.28 (1998); homosexuals 1.34 (1977), 0.75 (1998)
#   Fig 5: 0.85 (1988), 0.79 (1998)
rep |> filter(year %in% c(1977, 1985, 1988, 1998))

# Folder for saved figures (no warning if it already exists).
# Each ggsave() sits directly under its plot because it saves the last plot drawn.
dir.create("figures", showWarnings = FALSE)

# Figure 1
ggplot(rep, aes(x = year, y = fig1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 15)) +
  labs(title = "Figure 1. The 0-15 GSS Modified Stouffer Tolerance Battery, 1976-1998",
       subtitle = "High scores represent intolerance",
       x = NULL, y = "Mean intolerant responses") +
  theme_minimal()
ggsave("figures/fig1_replication.png", width = 8, height = 5, dpi = 300)

# Figure 2: pivot_longer() puts both groups in one column so ggplot can draw two lines
rep |>
  select(year, Racists = racist, Homosexuals = homo) |>
  pivot_longer(cols = c(Racists, Homosexuals),
               names_to = "group", values_to = "mean") |>
  ggplot(aes(x = year, y = mean, linetype = group)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 3)) +
  labs(title = "Figure 2. Intolerance Toward Racists and Homosexuals, 1976-1998",
       x = NULL, y = "Mean intolerant responses (0-3)", linetype = NULL) +
  theme_minimal()
ggsave("figures/fig2_replication.png", width = 8, height = 5, dpi = 300)

# Figure 5
ggplot(rep, aes(x = year, y = fig5)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Figure 5. A Dichotomous Measure of Tolerance and Intolerance, 1976-1998",
       x = NULL, y = "Proportion intolerant") +
  theme_minimal()
ggsave("figures/fig5_replication.png", width = 8, height = 5, dpi = 300)

# ---- Extension through 2024 (weighted) ------------------------------
# Each measure gets its own design object, built only from people who have
# a score on that measure, so years where it was not asked simply drop out.

ext15 <- tol |>
  filter(!is.na(intol15), !is.na(wtssps)) |>
  as_survey_design(weights = wtssps) |>
  group_by(year) |>
  summarise(mean       = survey_mean(intol15),
            prop_intol = survey_mean(intolerant15))

ext9 <- tol |>
  filter(!is.na(intol9), !is.na(wtssps)) |>
  as_survey_design(weights = wtssps) |>
  group_by(year) |>
  summarise(mean       = survey_mean(intol9),
            prop_intol = survey_mean(intolerant9))

ext_rac <- tol |>
  filter(!is.na(racist), !is.na(wtssps)) |>
  as_survey_design(weights = wtssps) |>
  group_by(year) |>
  summarise(mean = survey_mean(racist))

ext_homo <- tol |>
  filter(!is.na(homo), !is.na(wtssps)) |>
  as_survey_design(weights = wtssps) |>
  group_by(year) |>
  summarise(mean = survey_mean(homo))

# Quick check: last years in each series
# (ext15 and ext_homo should end in 2021; ext9 and ext_rac in 2024)
tail(ext15)
tail(ext9)

# Stack the results with bind_rows(), adding a label column first
ext_scales <- bind_rows(
  ext15 |> mutate(scale = "15 items (all five groups)"),
  ext9  |> mutate(scale = "9 items (atheists, racists, Communists)")
)
   ext_groups <- bind_rows(
     ext_rac  |> mutate(group = "Racists"),
     ext_homo |> mutate(group = "Homosexuals")
   ) |>
     filter(year >= 1976)

mode_note <- "Dashed line: 2021 web-only and 2022+ multimode designs. Post-2020 changes may reflect methodology."

# Extended Figure 1 (one panel per scale, since they have different ranges)
ggplot(ext_scales, aes(x = year, y = mean)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  facet_wrap(~ scale, ncol = 1, scales = "free_y") +
  labs(title = "Mean intolerance, 1976-2024",
       x = NULL, y = "Mean intolerant responses", caption = mode_note) +
  theme_minimal()
ggsave("figures/fig1_extension.png", width = 8, height = 7, dpi = 300)

# Extended Figure 2
ggplot(ext_groups, aes(x = year, y = mean, linetype = group)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 3)) +
  labs(title = "Intolerance toward racists and homosexuals, 1976-2024",
       x = NULL, y = "Mean intolerant responses (0-3)", linetype = NULL,
       caption = mode_note) +
  theme_minimal()
ggsave("figures/fig2_extension.png", width = 8, height = 5, dpi = 300)

# Extended Figure 5
ggplot(ext_scales, aes(x = year, y = prop_intol, colour = scale)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Proportion intolerant, 1976-2024",
       x = NULL, y = "Proportion intolerant", colour = NULL,
       caption = mode_note) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("figures/fig5_extension.png", width = 8, height = 5, dpi = 300)

# ---- Univariate graphs (swap in my own two variables) ---------------

gss24 <- gss |>
  filter(year == 2024) |>
  mutate(wtssps = as.numeric(wtssps))

# Quick check: what the variables mean before graphing them
print_labels(gss24$tvhours)
print_labels(gss24$polviews)

# Variable 1: a numeric variable (example: hours of TV per day)
tv <- gss24 |>
  select(tvhours, wtssps) |>
  mutate(tvhours = as.numeric(tvhours)) |>
  filter(!is.na(tvhours), !is.na(wtssps))

# Quick check: how many people answered, and the range
nrow(tv)
summary(tv$tvhours)

tv_stats <- tv |>
  as_survey_design(weights = wtssps) |>
  summarise(mean = survey_mean(tvhours),
            q    = survey_quantile(tvhours, c(0.25, 0.5, 0.75)),
            var  = survey_var(tvhours)) |>
  mutate(sd = sqrt(var))
tv_stats   # q_q50 is the median; q_q25 to q_q75 is the interquartile range

ggplot(tv, aes(x = tvhours, weight = wtssps)) +
  annotate("rect", xmin = tv_stats$q_q25, xmax = tv_stats$q_q75,
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.15) +
  geom_histogram(binwidth = 1, fill = "grey60", colour = "white") +
  geom_vline(xintercept = tv_stats$mean) +
  geom_vline(xintercept = tv_stats$q_q50, linetype = "dashed") +
  labs(title = "Hours of television per day, GSS 2024",
       subtitle = "Solid line = mean, dashed line = median, shaded band = middle 50%",
       x = "Hours per day", y = "Weighted count") +
  theme_minimal()
ggsave("figures/univariate_tvhours.png", width = 8, height = 5, dpi = 300)

# Variable 2: an ordinal variable (example: political views)
# as_factor() turns the number codes into their text labels
pv <- gss24 |>
  select(polviews, wtssps) |>
  mutate(polviews = as_factor(zap_missing(polviews))) |>
  filter(!is.na(polviews), !is.na(wtssps)) |>
  count(polviews, wt = wtssps) |>
  mutate(pct = n / sum(n),
         cum_pct = cumsum(pct))   # median = first category where cum_pct >= 0.5

# Quick check: pct should sum to 1
pv
sum(pv$pct)

ggplot(pv, aes(x = polviews, y = pct)) +
  geom_col(fill = "grey60") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.3) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Political views, GSS 2024",
       x = NULL, y = "Weighted share of respondents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("figures/univariate_polviews.png", width = 8, height = 5, dpi = 300)

getwd()