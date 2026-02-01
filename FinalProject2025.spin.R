## Load required libraries

library(tidyverse)
library(ggplot2)
library(maps)
library(gganimate)


## Loading the dataset


shooters <- read.csv("shootings.csv")


## quick look at the data

nrow(shooters)
ncol(shooters)
head(shooters)


## Removing all NA values and creating new variable 

shooters <- shooters %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(TotalNumberofVictims),
    !is.na(RelationshiptoIncidentLocation),
    !is.na(year)
  ) %>%
  # new variable shooting at place of schooling
  mutate(
    placeofschooling = ifelse(
      RelationshiptoIncidentLocation == "Place of schooling", 1, 0
    ),
    placeofschooling_label = ifelse(
      placeofschooling == 1, "School-related", "Non-school"
    )
  )

# Check the difference in shooting school vs nonschool
table(shooters$placeofschooling_label)


## Summary of findings 

victim_summary <- shooters %>%
  group_by(placeofschooling_label) %>%
  summarize(
    incidents = n(),
    mean_victims = mean(TotalNumberofVictims, na.rm = TRUE),
    median_victims = median(TotalNumberofVictims, na.rm = TRUE),
    min_victims = min(TotalNumberofVictims, na.rm = TRUE),
    max_victims = max(TotalNumberofVictims, na.rm = TRUE)
  )

victim_summary


## VIZ 1: MAP OF SHOOTINGS 

usmap <- map_data("state")

map_plot <- ggplot() +
  geom_polygon(
    data = usmap,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "gray"
  ) +
  geom_point(
    data = shooters,
    aes(
      x = Longitude,
      y = Latitude,
      color = placeofschooling_label,
      size = TotalNumberofVictims
    ),
    alpha = 0.5
  ) +
  scale_size(name = "Total Victims", range = c(2, 8)) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Geographic Distribution of Mass Shootings in the United States",
    subtitle = "School-related vs non-school-related incidents",
    color = "Incident Type"
  )

map_plot



## VIZ 2: COMPARISON OF VICTIM COUNTS

box_plot <- ggplot(
  shooters,
  aes(x = placeofschooling_label, y = TotalNumberofVictims)
) +
  geom_boxplot() +
  labs(
    title = "Total Number of Victims by Incident Type",
    x = "Incident Type",
    y = "Total Number of Victims"
  ) +
  theme_minimal()

box_plot

## VIZ 1: Animated over the years 

animated_map <- ggplot() +
  geom_polygon(
    data = usmap,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "gray"
  ) +
  geom_point(
    data = shooters,
    aes(
      x = Longitude,
      y = Latitude,
      color = placeofschooling_label,
      size = TotalNumberofVictims
    ),
    alpha = 0.5
  ) +
  scale_size(range = c(2, 8)) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Mass Shootings in the United States Over Time",
    subtitle = "Year: {closest_state}",
    color = "Incident Type"
  ) +
  transition_states(year, state_length = 4)

animate(animated_map, fps = 8, width = 800, height = 600)

