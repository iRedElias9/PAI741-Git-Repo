#Load libraries
library(tidyverse)
library(socviz)
library(sf)
library(maps)
library(ggthemes)

#My R is older than 4.5, so socviz (which has the 2024 data) will not install for me properly

#Load the 2024 election data and map shapes directly from Healy's GitHub instead
#The files are compressed, so each one is downloaded to a temporary file first and then loaded
socviz_data <- "https://raw.githubusercontent.com/kjhealy/socviz/main/data/"
for (f in c("election24", "election24_county_df", "states_sf", "counties_sf")) {
  tmp <- tempfile(fileext = ".rda")
  download.file(paste0(socviz_data, f, ".rda"), tmp, mode = "wb")
  load(tmp)
}

#Save every plot into my course repo folder
#setwd("/Users/elliothidalgo/Downloads/PAI 741/PAI741-Git-Repo/Problem Set 10")

#Party colors used on every election map so red and blue mean the same thing throughout
party_colors <- c(Democratic = "#2E74C0", Republican = "#CB454A")


#### Question 1: 2024 Election ####

#Load Healy's 2024 data and check what is in each object
#election24: one row per state. r_points = Trump percent minus Harris percent.
#election24_county_df: one row per county with the winning party.
#states_sf and counties_sf: the map shapes. Alaska and Hawaii are moved under the lower 48.
election24
election24_county_df
states_sf
counties_sf

#Join the results onto the state shapes. The merge identifier is fips (2 digits, character) in both.
states_24 <- states_sf |>
  left_join(election24 |> select(fips, party, r_points), by = "fips")

#1a. Winner by state
ggplot(states_24) +
  geom_sf(aes(fill = party), color = "white", linewidth = 0.2) +
  scale_fill_manual(values = party_colors) +
  labs(title = "Trump carried 31 states in 2024",
       subtitle = "Harris won 19 states and the District of Columbia",
       fill = NULL) +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("1a_winner_by_state.png", width = 10, height = 6.5, bg = "white")

#1a extra. Margin by state shows how close each state was, not just who won
#Blue = Harris margin, red = Trump margin. The scale runs -100 to 100 so white is a tie.
ggplot(states_24) +
  geom_sf(aes(fill = r_points), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2E74C0", mid = "white", high = "#CB454A",
                       midpoint = 0, limits = c(-100, 100)) +
  labs(title = "Most states were not close in 2024",
       subtitle = "Trump vote percent minus Harris vote percent",
       fill = "Margin (pts)") +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("1a_margin_by_state.png", width = 10, height = 6.5, bg = "white")

#Join the county results onto the county shapes. The merge identifier is fips (5 digits, character).
counties_24 <- counties_sf |>
  left_join(election24_county_df, by = "fips")

#1b. Winner by county
#State borders are drawn on top in white so the states are still readable
ggplot(counties_24) +
  geom_sf(aes(fill = winner), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = party_colors, na.value = "grey80") +
  labs(title = "Trump won 2,685 of 3,153 counties in 2024",
       subtitle = "Harris's 468 counties held about as many people. Grey: county lines do not match the results file.",
       fill = NULL) +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("1b_winner_by_county.png", width = 10, height = 6.5, bg = "white")

#1c. Population density by county
#pop_dens6 is already grouped into six bins in counties_sf
ggplot(counties_sf) +
  geom_sf(aes(fill = pop_dens6), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "grey40", linewidth = 0.1) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Half of Americans live on 4% of the land",
       subtitle = "268 counties with more than 500 people per square mile hold 53% of the population",
       fill = "People per\nsquare mile") +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("1c_pop_density_by_county.png", width = 10, height = 6.5, bg = "white")

#1d. Percent Black by county
#black_disc is already grouped into bins in counties_sf
ggplot(counties_sf) +
  geom_sf(aes(fill = black_disc), color = NA) +
  geom_sf(data = states_sf, fill = NA, color = "grey40", linewidth = 0.1) +
  scale_fill_brewer(palette = "Greens") +
  labs(title = "The Black population is concentrated in the South",
       subtitle = "371 of the 384 counties that are at least 25% Black are in Southern states",
       fill = "Percent Black") +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("1d_pct_black_by_county.png", width = 10, height = 6.5, bg = "white")


#### Question 2: 2026 Midterms ####

#Load the preliminary 2026 returns
#One row per candidate in each state's Senate or Governor race. vote_pct is the candidate's percent.
#Rows with no candidate are races with no results yet.
midterms_raw <- read_csv("~/Downloads/midterms_2026_preliminary.csv",
                         col_types = cols(state_fips = col_character()))
midterms_raw

#Collapse to one row per state per office
#Senate specials (Florida, Ohio) count as Senate
#Winner = candidate with the highest percent. A tie at the top (Mississippi Senate) is labeled "Tied".
#Margin = top Republican minus top Democrat. It is NA when a race has no Democrat or no Republican.
midterms <- midterms_raw |>
  filter(!is.na(candidate)) |>
  mutate(office = if_else(str_detect(office, "Senate"), "Senate", "Governor"),
         party = case_when(party == "D" ~ "Democratic",
                           party == "R" ~ "Republican",
                           TRUE ~ "Independent/Other")) |>
  group_by(st = state_abbr, office) |>
  summarize(
    top_pct  = max(vote_pct),
    winner   = party[which.max(vote_pct)],
    n_top    = sum(vote_pct == top_pct),
    dem_pct  = if (any(party == "Democratic")) max(vote_pct[party == "Democratic"]) else NA,
    rep_pct  = if (any(party == "Republican")) max(vote_pct[party == "Republican"]) else NA,
    .groups = "drop"
  ) |>
  mutate(party_26 = if_else(n_top > 1, "Tied", winner),
         r_points_26 = rep_pct - dem_pct)
midterms

#Add colors for independents and ties
colors_26 <- c(party_colors, "Independent/Other" = "#8E6BB0", "Tied" = "#F2C14E")

#Function to map the winner for one office
#The merge identifier is st. left_join keeps every state, so states with no race or no results are grey.
#breaks keeps "NA" out of the legend
map_winner_26 <- function(which_office, headline, numbers) {
  senate_or_gov <- states_sf |>
    left_join(midterms |> filter(office == which_office), by = "st")
  ggplot(senate_or_gov) +
    geom_sf(aes(fill = party_26), color = "white", linewidth = 0.2) +
    scale_fill_manual(values = colors_26, na.value = "grey85",
                      breaks = names(colors_26)) +
    labs(title = headline,
         subtitle = paste0(numbers, "\nLeader in preliminary returns. Grey: no race or no results yet."),
         fill = NULL,
         caption = "Source: 2026 midterms preliminary returns (course drive)") +
    theme_map() +
    theme(legend.position = "right", legend.justification = "center")
}

#2a. Senate winner
map_winner_26("Senate",
              "Democrats lead in 14 of 22 reported 2026 Senate races",
              "Republicans lead in 7. Mississippi is tied.")
ggsave("2a_senate_winner_2026.png", width = 10, height = 6.5, bg = "white")

#2b. Governor winner
map_winner_26("Governor",
              "Democrats lead in 17 of 20 reported 2026 governor races",
              "Republicans lead in Alabama, Arkansas, and Florida.")
ggsave("2b_governor_winner_2026.png", width = 10, height = 6.5, bg = "white")


#### Question 3: Swing from 2024 to 2026 ####

#Swing = 2026 margin minus 2024 Trump margin, both as R minus D in points
#Negative = the state moved toward Democrats. Positive = moved toward Republicans.
#Races with no Democrat or no Republican (Idaho, Montana, Nebraska Senate) have no swing and are grey.
#Both maps use the same -40 to 40 scale so the same color means the same swing on each.
map_swing <- function(which_office, headline, numbers) {
  swing_df <- states_sf |>
    left_join(election24 |> select(st, r_points), by = "st") |>
    left_join(midterms |> filter(office == which_office), by = "st") |>
    mutate(swing = r_points_26 - r_points)
  ggplot(swing_df) +
    geom_sf(aes(fill = swing), color = "white", linewidth = 0.2) +
    scale_fill_gradient2(low = "#2E74C0", mid = "white", high = "#CB454A",
                         midpoint = 0, limits = c(-40, 40), na.value = "grey85") +
    labs(title = headline,
         subtitle = paste0(numbers, "\nChange in R minus D margin. Grey: no race, no results, or no D vs. R contest."),
         fill = "Swing (pts)",
         caption = "Sources: socviz::election24; 2026 midterms preliminary returns") +
    theme_map() +
    theme(legend.position = "right", legend.justification = "center")
}

#3a. Senate swing
map_swing("Senate",
          "Every comparable Senate race swung toward Democrats",
          "19 of 19 states. Median swing: 16 points. Largest: Texas, 26 points.")
ggsave("3a_senate_swing.png", width = 10, height = 6.5, bg = "white")

#3b. Governor swing
map_swing("Governor",
          "Governor races swung toward Democrats in 19 of 20 states",
          "Median swing: 14 points. Oregon moved 8 points toward Republicans.")
ggsave("3b_governor_swing.png", width = 10, height = 6.5, bg = "white")


#### Question 4: Syracuse University ####

#Healy built states_sf in the USA Albers projection (ESRI:102003) and then labeled it EPSG:5070
#A normal st_transform() to EPSG:5070 puts points about 1,000 miles too far north
#This function projects points to ESRI:102003 and then copies the map's label over
#sf warns that relabeling does not reproject. That is intended here, so the warning is hidden.
to_map_crs <- function(points_sf) {
  points_sf <- st_transform(points_sf, "ESRI:102003")
  suppressWarnings(st_set_crs(points_sf, st_crs(states_sf)))
}

#Own data: Syracuse University's longitude and latitude
#crs = 4326 means plain longitude and latitude
su <- tibble(name = "Syracuse University", lon = -76.1351, lat = 43.0379) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  to_map_crs()
su

#Big orange star (shape 8) on a plain state map
ggplot() +
  geom_sf(data = states_sf, fill = "grey95", color = "grey60", linewidth = 0.2) +
  geom_sf(data = su, shape = 8, size = 8, stroke = 2.5, color = "#F76900") +
  labs(title = "Syracuse University",
       subtitle = "Syracuse, New York") +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("4_syracuse_university.png", width = 10, height = 6.5, bg = "white")


#### Question 5: Showcase, where large U.S. cities are ####

#Data: maps::us.cities. One row per city with name, state, population, latitude, and longitude.
#The populations are an older snapshot, so current Census estimates would be more precise.
head(us.cities)

#Keep cities with 100,000 or more people
#Alaska and Hawaii are dropped because states_sf moves them, so their points would land in the wrong place
cities <- us.cities |>
  filter(pop >= 100000, !country.etc %in% c("AK", "HI")) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  to_map_crs()

#5a. Points: every large city, sized by population
ggplot() +
  geom_sf(data = states_sf, fill = "grey95", color = "white", linewidth = 0.3) +
  geom_sf(data = cities, aes(size = pop), color = "#2E74C0", alpha = 0.5) +
  scale_size_area(max_size = 12, labels = scales::label_comma(),
                  breaks = c(250000, 1000000, 5000000)) +
  labs(title = "Large cities cluster on the coasts, the Great Lakes, and Texas",
       subtitle = "265 cities with 100,000 or more residents in the lower 48 states",
       size = "Population",
       caption = "Source: maps::us.cities") +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("5a_large_cities_points.png", width = 10, height = 6.5, bg = "white")

#5b. Areas: share of each state's population living in those large cities
#Add up large-city population by state. The merge identifier is st.
city_share <- us.cities |>
  filter(pop >= 100000) |>
  group_by(st = country.etc) |>
  summarize(big_city_pop = sum(pop))

#Add up county population to get state population. The first 2 digits of a county fips are the state fips.
state_pop <- counties_sf |>
  st_drop_geometry() |>
  mutate(st_fips = str_sub(fips, 1, 2)) |>
  group_by(st_fips) |>
  summarize(state_pop = sum(pop))

#Join both onto the state shapes
#States with no large city get 0
#Capped at 100 because DC's city population is older than its county population
states_cities <- states_sf |>
  left_join(state_pop, by = c("fips" = "st_fips")) |>
  left_join(city_share, by = "st") |>
  mutate(big_city_pop = replace_na(big_city_pop, 0),
         pct_big_city = pmin(100 * big_city_pop / state_pop, 100))
states_cities

ggplot(states_cities) +
  geom_sf(aes(fill = pct_big_city), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  labs(title = "The West is more urban than its empty map suggests",
       subtitle = "Share of each state's population living in cities of 100,000 or more. 6 states have none.",
       fill = "Percent",
       caption = "Source: maps::us.cities; socviz::counties_sf") +
  theme_map() +
  theme(legend.position = "right", legend.justification = "center")
ggsave("5b_big_city_share_by_state.png", width = 10, height = 6.5, bg = "white")

#5c. Supporting chart: the ten states with the highest big-city share
states_cities |>
  st_drop_geometry() |>
  slice_max(pct_big_city, n = 10) |>
  ggplot(aes(x = pct_big_city, y = reorder(state, pct_big_city))) +
  geom_col(fill = "#6A51A3") +
  labs(title = "Nevada and Arizona are more big-city than New York",
       subtitle = "Percent of state population living in cities of 100,000 or more",
       x = NULL, y = NULL) +
  theme_minimal()
ggsave("5c_top10_big_city_states.png", width = 8, height = 5, bg = "white")