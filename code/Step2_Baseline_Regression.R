# Gendered Paintings
# 2. Basic Regression
# Author: Yangyu Wang
# Date: 04/29/2025

library(broom.mixed)
library(jsonlite)
library(tidyverse)
library(lme4)
library(stargazer)
library(ggplot2)
library(psych)
library(cowplot)
library(tableone)
library(labelled)
library(stringr)

# Load Data
face_gender      <- fromJSON("data/face_results_data.json", flatten = TRUE)
artwork_info     <- read.csv("data/artwork_data_cleaned.csv", stringsAsFactors = FALSE)
demographic_info <- fromJSON("data/demographic_namemapped.json", flatten = TRUE)

demographic_df <- enframe(demographic_info, name = "artist", value = "info") %>%
  unnest_wider(info) %>%
  select(artist, Gender, `Educational Level`) %>%
  mutate(Gender = ifelse(Gender %in% c("Female", "Male"), Gender, "Non-binary"))

face_counts <- face_gender %>%
  group_by(id) %>%
  summarise(face_count = ifelse(n() < 3, as.character(n()), ">2"), .groups = "drop")

artwork_face <- artwork_info %>%
  left_join(face_counts, by = c("Artwork_ID" = "id")) %>%
  mutate(face_count = ifelse(is.na(face_count), "0", face_count))

face_info <- face_gender %>%
  left_join(artwork_face, by = c("id" = "Artwork_ID"), relationship = "many-to-many") %>%
  left_join(demographic_df, by = c("Artist_name" = "artist")) %>%
  rename(
    Yaw = v_p, Pitch = h_p, Roll = l_p,
    face_gender   = gender,
    author_gender = Gender
  ) %>%
  mutate(
    Year      = (Year - 1400)/100,
    log_yaw   = log(Yaw + 1),
    log_pitch = log(Pitch + 1),
    log_roll  = log(Roll + 1),
    log_sizes = log(sizes + 1e-05)
  )

face_info$author_gender <- relevel(as.factor(face_info$author_gender), ref = "Male")
face_info <- face_info %>%
  mutate(edu_dummy = case_when(
    `Educational Level` %in% c("Self-taught","Self-educated","Self-trained","No formal education") ~ 0,
    !is.na(`Educational Level`)                                                              ~ 1,
    TRUE                                                                                     ~ NA_real_
  ))

# Factor analysis for obliqueness (same as in descriptive)
face_info_fa <- face_info %>%
  filter(!is.na(log_yaw), !is.na(log_pitch), !is.na(log_roll))

fa_result <- fa(face_info_fa %>% select(log_yaw, log_pitch, log_roll),
                nfactors = 1, rotate = "none")
face_info_fa$obliqueness <- as.numeric(fa_result$scores[,1])

face_info_fa <- face_info_fa %>%
  mutate(orig_year = Year * 100 + 1400) %>%
  mutate(
    period20 = cut(
      orig_year,
      breaks = seq(1400, ceiling(max(orig_year, na.rm=TRUE)/20)*20, by = 20),
      right = FALSE,
      labels = paste(
        seq(1400, ceiling(max(orig_year, na.rm=TRUE)/20)*20 - 20, by = 20),
        seq(1419, ceiling(max(orig_year, na.rm=TRUE)/20)*20 - 1, by = 20),
        sep = "–"
      )
    )
  )

# Descriptive Table
face_info_labeled <- face_info_fa %>%
  set_variable_labels(
    obliqueness    = "Obliqueness (Factor Score)",
    log_sizes      = "Face Size (log)",
    face_gender    = "Face Gender",
    period20       = "Period (20-year)",
    author_gender  = "Author Gender",
    edu_dummy      = "Author Education (1=Formal)",
    face_count     = "Number of Faces"
  )

vars       <- c("obliqueness","log_sizes","face_gender","period20",
                "author_gender","edu_dummy","face_count")
factorVars <- c("face_gender","period20","author_gender","edu_dummy","face_count")

desc_table <- CreateTableOne(vars = vars,
                             data = face_info_labeled,
                             factorVars = factorVars)

sink("model/descriptive_table.tex")
print(desc_table, showAllLevels = TRUE, format = "f", quote = FALSE, noSpaces = TRUE)
sink()

# Fixed-effect Linear Models
model_obliqueness              <- lmer(obliqueness ~ face_gender  + period20 + author_gender + edu_dummy + (1 | face_count),
                                       data = face_info_fa)
model_obliqueness_male         <- lmer(obliqueness ~ face_gender  + period20 + edu_dummy + (1 | face_count),
                                       data = filter(face_info_fa, author_gender == "Male"))
model_obliqueness_female       <- lmer(obliqueness ~ face_gender  + period20 + edu_dummy + (1 | face_count),
                                       data = filter(face_info_fa, author_gender == "Female"))
model_obliqueness_facecount1   <- lm(obliqueness  ~ face_gender  + period20 + author_gender + edu_dummy,
                                     data = filter(face_info_fa, face_count == "1"))
model_obliqueness_facecount2   <- lm(obliqueness  ~ face_gender  + period20 + author_gender + edu_dummy,
                                     data = filter(face_info_fa, face_count == "2"))
model_obliqueness_facecount_gt2<- lm(obliqueness  ~ face_gender  + period20 + author_gender + edu_dummy,
                                     data = filter(face_info_fa, face_count == ">2"))

model_sizes                   <- lmer(log_sizes  ~ face_gender  + period20 + author_gender + edu_dummy + (1 | face_count),
                                      data = face_info_fa)
model_sizes_male              <- lmer(log_sizes  ~ face_gender  + period20 + edu_dummy + (1 | face_count),
                                      data = filter(face_info_fa, author_gender == "Male"))
model_sizes_female            <- lmer(log_sizes  ~ face_gender  + period20 + edu_dummy + (1 | face_count),
                                      data = filter(face_info_fa, author_gender == "Female"))
model_sizes_facecount1        <- lm(log_sizes  ~ face_gender  + period20 + author_gender + edu_dummy,
                                    data = filter(face_info_fa, face_count == "1"))
model_sizes_facecount2        <- lm(log_sizes  ~ face_gender  + period20 + author_gender + edu_dummy,
                                    data = filter(face_info_fa, face_count == "2"))
model_sizes_facecount_gt2     <- lm(log_sizes  ~ face_gender  + period20 + author_gender + edu_dummy,
                                    data = filter(face_info_fa, face_count == ">2"))
model_obliqueness_edu_formal <- lmer(
  obliqueness ~ face_gender + period20 + author_gender + (1 | face_count),
  data = filter(face_info_fa, edu_dummy == 1)
)
model_obliqueness_edu_selftaught <- lmer(
  obliqueness ~ face_gender + period20 + author_gender + (1 | face_count),
  data = filter(face_info_fa, edu_dummy == 0)
)

model_sizes_edu_formal <- lmer(
  log_sizes ~ face_gender + period20 + author_gender + (1 | face_count),
  data = filter(face_info_fa, edu_dummy == 1)
)
model_sizes_edu_selftaught <- lmer(
  log_sizes ~ face_gender + period20 + author_gender + (1 | face_count),
  data = filter(face_info_fa, edu_dummy == 0)
)

# Coefficient Plot
plot_coefs <- function(models, model_names = NULL,
                       predictor_rename = NULL,
                       predictor_order  = NULL,
                       save_path        = NULL,
                       ...) {
  if (is.null(model_names)) {
    model_names <- paste("Model", seq_along(models))
  }
  coef_list <- lapply(seq_along(models), function(i) {
    mod      <- models[[i]]
    mod_name <- model_names[i]
    tidy_mod <- tidy(mod, effects = "fixed") %>%
      filter(term != "(Intercept)", !str_detect(term, "^period20")) %>%
      mutate(Model = mod_name,
             ci_lower = estimate - 1.96 * std.error,
             ci_upper = estimate + 1.96 * std.error)
    if (!is.null(predictor_rename)) {
      tidy_mod <- tidy_mod %>%
        mutate(term = recode(term, !!!predictor_rename))
    }
    tidy_mod
  })
  all_coefs <- bind_rows(coef_list) %>%
    mutate(Model = factor(Model, levels = model_names))
  if (!is.null(predictor_order)) {
    all_coefs <- all_coefs %>%
      mutate(term = factor(term, levels = predictor_order))
  }
  p <- ggplot(all_coefs, aes(x = term, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2, position = position_dodge(width = 0.5)) +
    facet_wrap(~ Model,
               ncol   = 4,
               nrow   = 2) +
    coord_flip() +
    labs(x = "Independent Variables", y = "Coefficients") +
    theme_bw() +
    theme(text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.spacing = unit(0.5, "cm"))
  if (!is.null(save_path)) {
    ggsave(filename = save_path, plot = p, ...)
  }
  return(p)
}

predictor_rename <- c(
  "face_genderWoman"        = "Face Gender: Female",
  "face_genderMan"          = "Face Gender: Male",
  "author_genderFemale"     = "Artist Gender: Female",
  "author_genderMale"       = "Artist Gender: Male",
  "author_genderNon-binary" = "Artist Gender: Non-binary",
  "edu_dummy"               = "Artist Education: Formal"
)
predictor_order <- rev(c(
  "Face Gender: Female", "Face Gender: Male", "Artist Education: Formal",
  "Artist Gender: Female", "Artist Gender: Male", "Artist Gender: Non-binary"
))

models_list_obliqueness <- list(
  "Baseline"               = model_obliqueness,
  "Face Count: 1"          = model_obliqueness_facecount1,
  "Face Count: 2"          = model_obliqueness_facecount2,
  "Face Count: >2"         = model_obliqueness_facecount_gt2,
  "Artist Gender: Male"    = model_obliqueness_male,
  "Artist Gender: Female"  = model_obliqueness_female,
  "Artist Education: Formal"      = model_obliqueness_edu_formal,
  "Artist Education: Self-taught" = model_obliqueness_edu_selftaught
)

models_list_sizes <- list(
  "Baseline"               = model_sizes,
  "Face Count: 1"          = model_sizes_facecount1,
  "Face Count: 2"          = model_sizes_facecount2,
  "Face Count: >2"         = model_sizes_facecount_gt2,
  "Artist Gender: Male"    = model_sizes_male,
  "Artist Gender: Female"  = model_sizes_female,
  "Artist Education: Formal"      = model_sizes_edu_formal,
  "Artist Education: Self-taught" = model_sizes_edu_selftaught
)

p_obliqueness <- plot_coefs(
  models_list_obliqueness,
  model_names      = names(models_list_obliqueness),
  predictor_rename = predictor_rename,
  predictor_order  = predictor_order,
  save_path        = "model/obliqueness_period20_coeffs.png",
  dpi = 300, height = 6, width = 8
)

p_sizes <- plot_coefs(
  models_list_sizes,
  model_names      = names(models_list_sizes),
  predictor_rename = predictor_rename,
  predictor_order  = predictor_order,
  save_path        = "model/sizes_period20_coeffs.png",
  dpi = 300, height = 6, width = 8
)

# Combine plot and save to file
p_obl_clean <- p_obliqueness + theme(legend.position = "none")
p_sz_clean  <- p_sizes      + theme(legend.position = "none")
library(ggpubr)
p1_tagged <- p_sz_clean +
  labs(tag = "A. Log(Size)") +
  theme(
    plot.tag.position = c(0.02, 0.98),  # x,y in [0,1]
    plot.tag           = element_text(size = 12, hjust = 0)
  )

p2_tagged <- p_obl_clean +
  labs(tag = "B. Obliqueness") +
  theme(
    plot.tag.position = c(0.02, 0.98),
    plot.tag           = element_text(size = 12, hjust = 0)
  )
combined <- ggarrange(
  p1_tagged,
  p2_tagged,
  ncol      = 1,
  nrow      = 2,
  align     = "v",
  heights   = c(1, 1)
)

print(combined)
ggsave("Img/5_1_CoefPlot.png",
       combined, width = 10, height = 8, dpi = 300)

# Robustness Check (yaw, pitch and roll)
make_subset_models <- function(varname) {
  f_lmer <- as.formula(paste0(varname,
                              " ~ face_gender + period20 + author_gender + edu_dummy + (1 | face_count)"))
  f_lm   <- as.formula(paste0(varname,
                              " ~ face_gender + period20 + author_gender + edu_dummy"))
  f_lmer_g <- as.formula(paste0(varname,
                              " ~ face_gender + period20 + edu_dummy + (1 | face_count)"))
  f_lmer_e <- as.formula(paste0(varname,
                              " ~ face_gender + period20 + author_gender + (1 | face_count)"))
  
  df <- face_info_fa
  
  list(
    "Baseline"               = lmer(f_lmer, data = df),
    "Face Count: 1"          = lm(f_lm,   data = filter(df, face_count == "1")),
    "Face Count: 2"          = lm(f_lm,   data = filter(df, face_count == "2")),
    "Face Count: >2"         = lm(f_lm,   data = filter(df, face_count == ">2")),
    "Artist Gender: Male"    = lmer(f_lmer_g, data = filter(df, author_gender == "Male")),
    "Artist Gender: Female"  = lmer(f_lmer_g, data = filter(df, author_gender == "Female")),
    "Edu: Formal"            = lmer(f_lmer_e, data = filter(df, edu_dummy == 1)),
    "Edu: Self-taught"       = lmer(f_lmer_e, data = filter(df, edu_dummy == 0))
  )
}

models_yaw   <- make_subset_models("log_yaw")
models_pitch <- make_subset_models("log_pitch")
models_roll  <- make_subset_models("log_roll")
models_size  <- make_subset_models("log_sizes")

p_yaw_sub   <- plot_coefs(models_yaw,   model_names = names(models_yaw),
                          predictor_rename = predictor_rename, 
                          predictor_order = predictor_order, 
                          save_path = "model/log_yaw_subsets_coeffs.png",
                          dpi = 300, height = 6, width = 8)
p_pitch_sub <- plot_coefs(models_pitch, model_names = names(models_pitch), 
                          predictor_rename = predictor_rename, 
                          predictor_order = predictor_order,
                          save_path = "model/log_pitch_subsets_coeffs.png",
                          dpi = 300, height = 6, width = 8)
p_roll_sub  <- plot_coefs(models_roll,  model_names = names(models_roll),
                          predictor_rename = predictor_rename,
                          predictor_order = predictor_order,
                          save_path = "model/log_roll_subsets_coeffs.png", 
                          dpi = 300, height = 6, width = 8)
p_size_sub  <- plot_coefs(models_size,  model_names = names(models_size), 
                          predictor_rename = predictor_rename, 
                          predictor_order = predictor_order, 
                          save_path = "model/log_sizes_subsets_coeffs.png", 
                          dpi = 300, height = 6, width = 8)


p_size_sub_t <- p_size_sub +
  labs(tag = "A. Log(Size)") +
  theme(
    plot.tag.position = c(0.02, 0.98),  
    plot.tag           = element_text(size = 12, hjust = 0)
  )

p_yaw_sub_t <- p_yaw_sub +
  labs(tag = "B. Log(Yaw)") +
  theme(
    plot.tag.position = c(0.02, 0.98),
    plot.tag           = element_text(size = 12, hjust = 0)
  )


p_pitch_sub_t <- p_pitch_sub +
  labs(tag = "C. Log(Pitch)") +
  theme(
    plot.tag.position = c(0.2, 0.98),
    plot.tag           = element_text(size = 12, hjust = 0)
  )

p_roll_sub_t <- p_roll_sub +
  labs(tag = "D. Log(Roll)") +
  theme(
    plot.tag.position = c(0.02, 0.98),
    plot.tag           = element_text(size = 12, hjust = 0)
  )

combined <- ggarrange(
  p_size_sub_t,
  p_yaw_sub_t,
  p_yaw_sub_t,
  p_roll_sub_t,
  ncol      = 2,
  nrow      = 2,
  align     = "v",
  heights   = c(1, 1)
)


print(combined)
ggsave("Img/5_2_CoefPlotSep.png",
       combined,
       width = 16, height = 7,
       dpi = 300)