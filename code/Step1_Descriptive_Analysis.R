# Gendered Paintings
# 1. Descriptive Analysis
# Author: Yangyu Wang
# Date: 04/29/2025

library(psych)
library(jsonlite)
library(tidyverse)

# Load Data
face_gender <- fromJSON("data/face_results_data.json", flatten = TRUE)
artwork_info = read.csv("data/artwork_data_cleaned.csv")

# Figure 1. Face Counts Number
face_counts <- face_gender %>%
  group_by(id) %>%
  summarise(face_count = ifelse(n()<3, as.character(n()), ">2"), .groups = "drop") %>%
  mutate(face_count = factor(face_count, levels = c("1", "2", ">2")))

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

ggsave("Img/1_FaceCountTrend.png", 
       device = "png",
       dpi = 300,
       height = 6,
       width = 10)

# Figure 2. Female Face Proportions
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
  ) %>%
  mutate(face_count = factor(face_count, levels = c("1", "2", ">2")))

gender_trend_20yr <- gender_trend_faces %>%
  mutate(
    time_20yr = cut(
      Year,
      breaks = seq(floor(min(Year, na.rm=TRUE) / 20) * 20,
                   ceiling(max(Year, na.rm=TRUE) / 20) * 20,
                   by = 20),
      right  = FALSE,
      labels = paste(
        seq(floor(min(Year, na.rm=TRUE) / 20) * 20,
            ceiling(max(Year, na.rm=TRUE) / 20) * 20 - 20,
            by = 20),
        seq(floor(min(Year, na.rm=TRUE) / 20) * 20 + 19,
            ceiling(max(Year, na.rm=TRUE) / 20) * 20 - 1,
            by = 20),
        sep = "–"
      )
    )
  ) %>%
  group_by(time_20yr, face_count) %>%
  summarise(
    total_faces      = sum(total_faces),
    female_faces     = sum(female_faces),
    female_proportion = female_faces / total_faces,
    .groups          = "drop"
  ) %>%
  mutate(
    start_year = as.numeric(sub("–.*", "", time_20yr)),
    year_mid   = start_year + 10
  )

ggplot(gender_trend_20yr, aes(x = year_mid, y = female_proportion, color = face_count)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_line(alpha = 0.5) +
  geom_smooth(alpha = 0.2) +
  scale_x_continuous(
    name   = "Year",
    breaks = seq(ceiling(min(gender_trend_20yr$start_year) / 100) * 100,
                 floor(max(gender_trend_20yr$start_year) / 100) * 100,
                 by = 100),
    labels = seq(ceiling(min(gender_trend_20yr$start_year) / 100) * 100,
                 floor(max(gender_trend_20yr$start_year) / 100) * 100,
                 by = 100)
  ) +
  labs(
    y     = "Female Proportion in All Faces",
    color = "Face Count"
  ) +
  theme_bw() + 
  theme(
    text        = element_text(size = 12),
    plot.title  = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/2_FemalePropTrend.png",
       dpi    = 300,
       height = 6,
       width  = 10)


# Figure 3. Face Feature by Face Gender
## 3.1 Yaw, Pitch, and Roll
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

ggsave("Img/3_1_GenderPosDens.png",
       device = "png",
       dpi = 300,
       height = 6,
       width = 10)

## 3.2 Obliqueness Factor
face_gender_with_counts <- face_gender_with_counts %>%
  mutate(
    log_Yaw = log(Yaw + 1),
    log_Pitch = log(Pitch + 1),
    log_Roll = log(Roll + 1)
  )

log_posture_matrix <- face_gender_with_counts %>%
  select(log_Yaw, log_Pitch, log_Roll) %>%
  drop_na()

fa_result <- psych::fa(log_posture_matrix, nfactors = 1, rotate = "none")

factor_scores <- as.data.frame(fa_result$scores)
colnames(factor_scores) <- "posture_factor_log"

face_gender_with_factors <- face_gender_with_counts %>%
  drop_na(log_Yaw, log_Pitch, log_Roll) %>%
  bind_cols(factor_scores) %>%
  mutate(face_count = factor(face_count, levels = c("1", "2", ">2")))

ggplot(face_gender_with_factors, aes(x = posture_factor_log, fill = gender)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ face_count, scales = "free_y") +
  labs(
    x = "Obliqueness",
    y = "Density",
    fill = "Face Gender"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/3_2_GenderPosFacDens.png",
       device = "png",
       dpi = 300,
       height = 3,
       width = 10)

## 3.3 Size
face_gender_with_counts <- face_gender_with_counts %>%
  mutate(log_size = log(sizes + 1e-05)) %>%
  mutate(face_count = factor(face_count, levels = c("1", "2", ">2")))

p_size_density <- ggplot(face_gender_with_counts, 
                         aes(x = log_size, fill = gender)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ face_count, scales = "free_y") +
  labs(
    x = "Log(Size)",
    y = "Density",
    fill = "Face Gender"
  ) +
  theme_bw() +
  theme(
    text        = element_text(size = 12),
    plot.title  = element_text(hjust = 0.5, face = "bold")
  )

print(p_size_density)

ggsave("Img/3_3_GenderSizeDens.png",
       plot = p_size_density,
       device = "png",
       dpi    = 300,
       height = 3,    # match your original height
       width  = 10)

## 3.4 Trend of size and obliqueness
trend_df <- face_gender_with_factors %>%
  mutate(log_size = log(sizes + 1e-5)) %>%
  left_join(artwork_info %>% select(Artwork_ID, Year),
            by = c("id" = "Artwork_ID")) %>%
  drop_na(Year)  

trend_20yr <- trend_df %>%
  mutate(
    time_20yr = cut(
      Year,
      breaks = seq(floor(min(Year)/20)*20,
                   ceiling(max(Year)/20)*20,
                   by = 20),
      right  = FALSE,
      labels = paste(
        seq(floor(min(Year)/20)*20,
            ceiling(max(Year)/20)*20 - 20,
            by = 20),
        seq(floor(min(Year)/20)*20 + 19,
            ceiling(max(Year)/20)*20 - 1,
            by = 20),
        sep = "–"
      )
    )
  ) %>%
  # pivot the two metrics into long form
  pivot_longer(
    cols      = c(posture_factor_log, log_size),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  group_by(time_20yr, gender, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    start_year = as.numeric(sub("–.*", "", time_20yr)),
    year_mid   = start_year + 10
  )

ggplot(trend_20yr, aes(x = year_mid, y = mean_value, color = gender)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ metric, 
             ncol  = 1,
             scales = "free_y",
             labeller = as_labeller(c(
               posture_factor_log = "Obliqueness",
               log_size           = "Log(Size)"
             ))) +
  scale_x_continuous(
    name   = "Year",
    breaks = seq(ceiling(min(trend_20yr$start_year)/100)*100,
                 floor(max(trend_20yr$start_year)/100)*100,
                 by = 100)
  ) +
  labs(
    y     = "Mean Value",
    color = "Face Gender"
  ) +
  theme_bw() +
  theme(
    legend.position  = "right",
    text             = element_text(size = 12),
    plot.title       = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Img/3_4_GenderPosSizeTrend.png",
       dpi    = 300,
       width  = 10,
       height = 6)
