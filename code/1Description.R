
library(jsonlite)
library(tidyverse)

# Load the JSON file into an R data frame
face_gender <- fromJSON("data/face_results_data.json", flatten = TRUE)

artwork_info = read.csv("data/artwork_data_cleaned.csv")

# Face Counts
## Counts Trend
face_counts <- face_gender %>%
  group_by(id) %>%
  summarise(face_count = ifelse(n()<3, as.character(n()), ">2"), .groups = "drop")

artwork_face <- artwork_info %>%
  left_join(face_counts, by = c("Artwork_ID" = "id"))

artwork_face <- artwork_face %>%
  mutate(face_count = ifelse(is.na(face_count), 0, face_count))

data_summary <- artwork_face %>%
  group_by(Year, face_count) %>%
  summarise(n_paintings = n(), .groups = "drop")

ggplot(data_summary, aes(x = Year, y = n_paintings, color = factor(face_count))) +
  geom_line(alpha = 0.4) +
  geom_smooth(linewidth = 0.5) +
  labs(
    x = "Year",
    y = "Number of Paintings",
    color = "Face Counts"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/1Trend_face.png", 
       device = "png",
       dpi = 300,
       height = 6,
       width = 10)

## gender trend
face_counts <- face_gender %>%
  group_by(id) %>%
  summarise(face_count = ifelse(n() < 3, as.character(n()), ">2"),
            .groups = "drop")

face_gender_with_counts <- face_gender %>%
  left_join(face_counts, by = "id")

face_year <- face_gender_with_counts %>%
  left_join(artwork_info, by = c("id" = "Artwork_ID"), relationship = "many-to-many")

gender_trend_faces <- face_year %>%
  group_by(Year, face_count) %>%
  summarise(
    total_faces = n(),
    female_faces = sum(gender == "Woman", na.rm = TRUE),
    female_proportion = female_faces / total_faces,
    .groups = "drop"
  )

ggplot(gender_trend_faces, aes(x = Year, y = female_proportion, color = face_count)) +
  geom_smooth(linewidth = 0.8)+
  labs(
    x = "Year",
    y = "Female Proportion",
    color = "Face Count"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/2Trend_gender_face.png", 
       device = "png",
       dpi = 300,
       height = 6,
       width = 10)

## Gender posture
face_gender_with_counts = rename(face_gender_with_counts,  Yaw = v_p)
face_gender_with_counts = rename(face_gender_with_counts, Pitch = h_p)
face_gender_with_counts = rename(face_gender_with_counts,  Roll = l_p)

posture_data <- face_gender_with_counts %>%
  select(gender, face_count, Yaw, Pitch, Roll) %>%
  pivot_longer(
    cols = c(Yaw, Pitch, Roll),
    names_to = "metric",
    values_to = "value"
  ) %>%
  # Use log(value + 1) to avoid issues with zero values.
  mutate(log_value = log(value + 1))

summary_stats <- posture_data %>%
  group_by(gender, metric, face_count) %>%
  summarise(
    mean_log = mean(log_value, na.rm = TRUE),
    sd_log   = sd(log_value, na.rm = TRUE),
    .groups  = "drop"
  )

ggplot(posture_data, aes(x = log_value, fill = gender)) +
  geom_density(alpha = 0.5) +
  # Facet by face_count (rows) and metric (columns); scales free for each facet
  facet_grid(face_count ~ metric, scales = "free") +
  labs(
    x = "Absolute Logged Value",
    y = "Density",
    fill = "Face Gender"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/3gender_posture_density.png",
       device = "png",
       dpi = 300,
       height = 6,
       width = 10)

## Gender Posture across time
face_time <- face_gender_with_counts %>%
  left_join(artwork_info %>% select(Artwork_ID, Year),
            by = c("id" = "Artwork_ID"))

posture_time <- face_time %>%
  select(Year, gender, face_count, Yaw, Pitch, Roll) %>%
  pivot_longer(
    cols = c(Yaw, Pitch, Roll),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(log_value = log(value + 1))

time_summary <- posture_time %>%
  group_by(Year, gender, face_count, metric) %>%
  summarise(
    mean_log = mean(log_value, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(time_summary, aes(x = Year, y = mean_log, color = gender)) +
  geom_smooth(linewidth = 0.5) +
  geom_line(alpha = 0.3, linewidth = 0.3) +
  facet_grid(face_count ~ metric, scales = "free_y") +
  labs(
    x = "Year",
    y = "Mean Absolute Logged Value",
    color = "Face Gender"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/4historical_mean_posture.png",
       device = "png",
       dpi = 300,
       height = 6,
       width = 10)
