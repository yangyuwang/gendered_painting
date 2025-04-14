library(broom.mixed)
library(jsonlite)
library(tidyverse)
library(lme4)
library(stargazer)
library(ggplot2)

# ----- Custom Plot Function -----
plot_coefs <- function(models, model_names = NULL, predictor_rename = NULL, predictor_order = NULL, 
                       save_path = NULL, ...) {
  # models: a list of fitted model objects.
  # model_names: vector of labels corresponding to each model.
  # predictor_rename: a named vector for recoding predictor names.
  # predictor_order: a character vector specifying the order of predictors.
  # save_path: if provided, save the plot to this file.
  
  if (is.null(model_names)) {
    model_names <- paste("Model", seq_along(models))
  }
  
  coef_list <- lapply(seq_along(models), function(i) {
    mod <- models[[i]]
    mod_name <- model_names[i]
    tidy_mod <- tidy(mod, effects = "fixed") %>%
      mutate(Model = mod_name) %>%       # add model label
      filter(term != "(Intercept)") %>%  # exclude intercept
      mutate(
        ci_lower = estimate - 1.96 * std.error,
        ci_upper = estimate + 1.96 * std.error
      )
    if (!is.null(predictor_rename)) {
      tidy_mod <- tidy_mod %>% mutate(term = recode(term, !!!predictor_rename))
    }
    return(tidy_mod)
  })
  
  all_coefs <- bind_rows(coef_list)
  
  # Order the models as provided.
  all_coefs <- all_coefs %>% mutate(Model = factor(Model, levels = model_names))
  
  # Order predictors if provided.
  if (!is.null(predictor_order)) {
    all_coefs <- all_coefs %>% mutate(term = factor(term, levels = predictor_order))
  }
  
  p <- ggplot(all_coefs, aes(x = term, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2,
                  position = position_dodge(width = 0.5)) +
    facet_wrap(~ Model, scales = "free_x") +
    coord_flip() +
    labs(x = "Independent Variables", y ="") +
    theme_bw() +
    theme(text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(panel.spacing = unit(0.5, "cm"))
  
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, ...)
  }
  
  return(p)
}

# ----- Data Loading and Preparation -----
# Load data files.
face_gender <- fromJSON("data/face_results_data.json", flatten = TRUE)
artwork_info <- read.csv("data/artwork_data_cleaned.csv")
demographic_info <- fromJSON("data/demographic_namemapped.json", flatten = TRUE)

# Prepare demographic data.
demographic_df <- enframe(demographic_info, name = "artist", value = "info") %>%
  unnest_wider(info) %>%
  select(artist, Gender, `Educational Level`) %>%
  mutate(Gender = ifelse(Gender %in% c("Female", "Male"), Gender, "Non-binary"))

# Compute face counts.
face_counts <- face_gender %>%
  group_by(id) %>%
  summarise(face_count = ifelse(n() < 3, as.character(n()), ">2"), .groups = "drop")

# Merge artwork info with face counts.
artwork_face <- artwork_info %>%
  left_join(face_counts, by = c("Artwork_ID" = "id")) %>%
  mutate(face_count = ifelse(is.na(face_count), 0, face_count))

# Combine all data.
face_info <- face_gender %>%
  left_join(artwork_face, by = c("id" = "Artwork_ID"), relationship = "many-to-many") %>%
  left_join(demographic_df, by = c("Artist_name" = "artist"))

# Basic variable renaming.
face_info <- face_info %>%
  rename(Yaw = v_p, Pitch = h_p, Roll = l_p) %>%
  rename(face_gender = gender, author_gender = Gender) %>%
  mutate(Year = (Year - 1400)/100) %>%
  mutate(
    log_yaw   = log(Yaw + 1),
    log_pitch = log(Pitch + 1),
    log_roll  = log(Roll + 1),
    log_sizes = log(sizes + 0.00001)
  )
face_info$author_gender <- relevel(as.factor(face_info$author_gender), ref = "Male")

# ----- Fit Models -----
# Basic mixed effects models (using lmer).
model_yaw   <- lmer(log_yaw   ~ face_gender + Year + author_gender + (1 | face_count), data = face_info)
model_pitch <- lmer(log_pitch ~ face_gender + Year + author_gender + (1 | face_count), data = face_info)
model_roll  <- lmer(log_roll  ~ face_gender + Year + author_gender + (1 | face_count), data = face_info)
model_sizes <- lmer(log_sizes ~ face_gender + Year + author_gender + (1 | face_count), data = face_info)

stargazer(model_yaw, model_pitch, model_roll, model_sizes,
          type = "latex",
          title = "Basic Models with Fixed-effect on Painting Face Counts",
          dep.var.labels = c("Log(Yaw)", "Log(Pitch)", "Log(Roll)", "Log(Sizes)"),
          model.numbers = FALSE,
          digits = 3,
          out = "model/model_results.tex")

# Author gender specific.
face_info_male   <- face_info %>% filter(author_gender == "Male")
face_info_female <- face_info %>% filter(author_gender == "Female")

model_yaw_male   <- lmer(log_yaw   ~ face_gender + Year + (1 | face_count), data = face_info_male)
model_pitch_male <- lmer(log_pitch ~ face_gender + Year + (1 | face_count), data = face_info_male)
model_roll_male  <- lmer(log_roll  ~ face_gender + Year + (1 | face_count), data = face_info_male)
model_sizes_male <- lmer(log_sizes ~ face_gender + Year + (1 | face_count), data = face_info_male)

model_yaw_female   <- lmer(log_yaw   ~ face_gender + Year + (1 | face_count), data = face_info_female)
model_pitch_female <- lmer(log_pitch ~ face_gender + Year + (1 | face_count), data = face_info_female)
model_roll_female  <- lmer(log_roll  ~ face_gender + Year + (1 | face_count), data = face_info_female)
model_sizes_female <- lmer(log_sizes ~ face_gender + Year + (1 | face_count), data = face_info_female)

# Robustness: Time Period Subset.
face_info_period1 <- face_info %>% filter(Year >= 0, Year <= 2)
face_info_period2 <- face_info %>% filter(Year > 2, Year <= 4)
face_info_period3 <- face_info %>% filter(Year > 4)

model_yaw_period1   <- lmer(log_yaw   ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period1)
model_pitch_period1 <- lmer(log_pitch ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period1)
model_roll_period1  <- lmer(log_roll  ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period1)
model_sizes_period1 <- lmer(log_sizes ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period1)

model_yaw_period2   <- lmer(log_yaw   ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period2)
model_pitch_period2 <- lmer(log_pitch ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period2)
model_roll_period2  <- lmer(log_roll  ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period2)
model_sizes_period2 <- lmer(log_sizes ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period2)

model_yaw_period3   <- lmer(log_yaw   ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period3)
model_pitch_period3 <- lmer(log_pitch ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period3)
model_roll_period3  <- lmer(log_roll  ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period3)
model_sizes_period3 <- lmer(log_sizes ~ face_gender + Year + author_gender + (1 | face_count), data = face_info_period3)

# Robustness: Face Count Models (using lm for simplicity).
face_info_1   <- face_info %>% filter(face_count == "1")
face_info_2   <- face_info %>% filter(face_count == "2")
face_info_gt2 <- face_info %>% filter(face_count == ">2")

model_yaw_1    <- lm(log_yaw   ~ face_gender + Year + author_gender, data = face_info_1)
model_pitch_1  <- lm(log_pitch ~ face_gender + Year + author_gender, data = face_info_1)
model_roll_1   <- lm(log_roll  ~ face_gender + Year + author_gender, data = face_info_1)
model_sizes_1  <- lm(log_sizes ~ face_gender + Year + author_gender, data = face_info_1)

model_yaw_2    <- lm(log_yaw   ~ face_gender + Year + author_gender, data = face_info_2)
model_pitch_2  <- lm(log_pitch ~ face_gender + Year + author_gender, data = face_info_2)
model_roll_2   <- lm(log_roll  ~ face_gender + Year + author_gender, data = face_info_2)
model_sizes_2  <- lm(log_sizes ~ face_gender + Year + author_gender, data = face_info_2)

model_yaw_gt2   <- lm(log_yaw   ~ face_gender + Year + author_gender, data = face_info_gt2)
model_pitch_gt2 <- lm(log_pitch ~ face_gender + Year + author_gender, data = face_info_gt2)
model_roll_gt2  <- lm(log_roll  ~ face_gender + Year + author_gender, data = face_info_gt2)
model_sizes_gt2 <- lm(log_sizes ~ face_gender + Year + author_gender, data = face_info_gt2)

# ----- Set Up Predictor Renaming and Order (Reversed) -----
predictor_rename <- c(
  "face_genderWoman" = "Face Gender: Female", 
  "face_genderMan"   = "Face Gender: Male",
  "Year"             = "Time",
  "author_genderFemale" = "Author Gender: Female",
  "author_genderMale"   = "Author Gender: Male",
  "author_genderNon-binary" = "Author Gender: Non-binary"
)

predictor_order <- c("Face Gender: Female", "Face Gender: Male", 
                     "Time", 
                     "Author Gender: Female", "Author Gender: Male", "Author Gender: Non-binary")
predictor_order <- rev(predictor_order)

# ----- Build Model Lists -----
## Yaw Models (model names without outcome suffix; outcome appears in overall title)
models_list_yaw <- list(
  "Baseline"                = model_yaw,
  "Author Gender: Male"    = model_yaw_male,
  "Author Gender: Female"  = model_yaw_female,
  "Time: 1400-1600"        = model_yaw_period1,
  "Time: 1600-1800"        = model_yaw_period2,
  "Time: 1800+"            = model_yaw_period3,
  "Face Count: 1"          = model_yaw_1,
  "Face Count: 2"          = model_yaw_2,
  "Face Count: >2"         = model_yaw_gt2
)

## Pitch Models
models_list_pitch <- list(
  "Baseline"                = model_pitch,
  "Author Gender: Male"    = model_pitch_male,
  "Author Gender: Female"  = model_pitch_female,
  "Time: 1400-1600"        = model_pitch_period1,
  "Time: 1600-1800"        = model_pitch_period2,
  "Time: 1800+"            = model_pitch_period3,
  "Face Count: 1"          = model_pitch_1,
  "Face Count: 2"          = model_pitch_2,
  "Face Count: >2"         = model_pitch_gt2
)

## Roll Models
models_list_roll <- list(
  "Baseline"                = model_roll,
  "Author Gender: Male"    = model_roll_male,
  "Author Gender: Female"  = model_roll_female,
  "Time: 1400-1600"        = model_roll_period1,
  "Time: 1600-1800"        = model_roll_period2,
  "Time: 1800+"            = model_roll_period3,
  "Face Count: 1"          = model_roll_1,
  "Face Count: 2"          = model_roll_2,
  "Face Count: >2"         = model_roll_gt2
)

## Sizes Models
models_list_sizes <- list(
  "Baseline"                = model_sizes,
  "Author Gender: Male"    = model_sizes_male,
  "Author Gender: Female"  = model_sizes_female,
  "Time: 1400-1600"        = model_sizes_period1,
  "Time: 1600-1800"        = model_sizes_period2,
  "Time: 1800+"            = model_sizes_period3,
  "Face Count: 1"          = model_sizes_1,
  "Face Count: 2"          = model_sizes_2,
  "Face Count: >2"         = model_sizes_gt2
)

# ----- Updated Plotting Function for Outcome-specific Titles -----
plot_outcome_coefs <- function(models_list, outcome_title, file_name) {
  model_names <- names(models_list)
  models <- unname(models_list)
  
  p <- plot_coefs(models, model_names, predictor_rename = predictor_rename, 
                  predictor_order = predictor_order,
                  save_path = file_name, dpi = 300, height = 6, width = 8)
  
  print(p)
  message(paste("Plot for", outcome_title, "saved as", file_name))
  
  return(p)
}

# ----- Produce Coefficient Plots for Each Outcome -----

library(cowplot)
# Remove legends from individual plots.
p_yaw   <- plot_outcome_coefs(models_list_yaw,   "Yaw",   "model/Yaw_combined_coeff_plot.png")
p_pitch <- plot_outcome_coefs(models_list_pitch, "Pitch", "model/Pitch_combined_coeff_plot.png")
p_roll  <- plot_outcome_coefs(models_list_roll,  "Roll",  "model/Roll_combined_coeff_plot.png")
p_sizes <- plot_outcome_coefs(models_list_sizes, "Sizes", "model/Sizes_combined_coeff_plot.png")

p_yaw_clean   <- p_yaw   + theme(legend.position = "none")
p_pitch_clean <- p_pitch + theme(legend.position = "none")
p_roll_clean  <- p_roll  + theme(legend.position = "none")
p_sizes_clean <- p_sizes + theme(legend.position = "none")

manual_labels <- c("A. Yaw", "B. Pitch", "C. Roll", "D. Size")
# Combine the cleaned plots into a 2x2 grid using cowplot.
combined_plot_cow <- plot_grid(p_yaw_clean, p_pitch_clean, 
                               p_roll_clean, p_sizes_clean, 
                               ncol = 2, labels = manual_labels,
                               label_size = 12)
print(combined_plot_cow)
ggsave("Img/5Combined_All_Coefficient_Plots.png", combined_plot_cow, dpi = 300, height = 10, width = 16)
