# Gendered Paintings
# 3. Period-Interacted Regression
# Author: Yangyu Wang
# Date: 04/29/2025

library(broom.mixed)
library(jsonlite)
library(tidyverse)
library(lme4)
library(ggplot2)
library(cowplot)
library(psych)

# Load Data
face_gender      <- fromJSON("data/face_results_data.json", flatten = TRUE)
artwork_info     <- read.csv("data/artwork_data_cleaned.csv", stringsAsFactors = FALSE)
demographic_info <- fromJSON("data/demographic_namemapped.json", flatten = TRUE)

demographic_df <- enframe(demographic_info, name = "artist", value = "info") %>%
  unnest_wider(info) %>%
  select(artist, Gender, `Educational Level`) %>%
  mutate(Gender = ifelse(Gender %in% c("Female","Male"), Gender, "Non-binary"))

face_counts <- face_gender %>%
  group_by(id) %>%
  summarize(face_count = ifelse(n() < 3, as.character(n()), ">2"), .groups = "drop")

artwork_face <- artwork_info %>%
  left_join(face_counts, by = c("Artwork_ID" = "id")) %>%
  mutate(face_count = ifelse(is.na(face_count), "0", face_count))

face_info <- face_gender %>%
  left_join(artwork_face, by = c("id" = "Artwork_ID"), relationship = "many-to-many") %>%
  left_join(demographic_df, by = c("Artist_name" = "artist")) %>%
  rename(Yaw = v_p, Pitch = h_p, Roll = l_p,
         face_gender   = gender,
         author_gender = Gender) %>%
  mutate(
    Year      = (Year - 1400) / 100,
    log_yaw   = log(Yaw + 1),
    log_pitch = log(Pitch + 1),
    log_roll  = log(Roll + 1),
    log_sizes = log(sizes + 1e-05)
  )

face_info$author_gender <- relevel(as.factor(face_info$author_gender), ref = "Male")
face_info <- face_info %>%
  mutate(edu_dummy = case_when(
    `Educational Level` %in% c("Self-taught","Self-educated","Self-trained","No formal education") ~ 0,
    !is.na(`Educational Level`)                                                             ~ 1,
    TRUE                                                                                    ~ NA_real_
  ))

# Factor analysis for obliqueness
face_info_fa <- face_info %>% filter(!is.na(log_yaw) & !is.na(log_pitch) & !is.na(log_roll))
fa_res <- psych::fa(face_info_fa %>% select(log_yaw, log_pitch, log_roll), nfactors = 1, rotate = "none")
face_info_fa$obliqueness <- as.numeric(fa_res$scores[,1])

face_info_fa <- face_info_fa %>%
  mutate(orig_year = Year * 100 + 1400) %>%
  mutate(
    time_20yr = cut(
      orig_year,
      breaks = seq(1400, ceiling(max(orig_year,na.rm=TRUE)/20)*20, by=20),
      right = FALSE,
      labels = paste(
        seq(1400, ceiling(max(orig_year,na.rm=TRUE)/20)*20-20, by=20),
        seq(1419, ceiling(max(orig_year,na.rm=TRUE)/20)*20-1, by=20),
        sep = "–"
      )
    )
  )

# Period-interacted linear regression
model_obl_20 <- lmer(
  obliqueness ~ face_gender * time_20yr + author_gender + edu_dummy + (1 | face_count),
  data = face_info_fa
)
model_sz_20 <- lmer(
  log_sizes ~ face_gender * time_20yr + author_gender + edu_dummy + (1 | face_count),
  data = face_info_fa
)

coef_obl <- fixef(model_obl_20)
vcov_obl <- as.matrix(vcov(model_obl_20))
coef_sz  <- fixef(model_sz_20)
vcov_sz  <- as.matrix(vcov(model_sz_20))

## Function to compare gender across time
gender_contrast_manual <- function(coefs, vcov_mat, time_var_levels, time_var_name = "time_20yr") {
  gender_coef_names <- names(coefs)[grepl("^face_gender", names(coefs)) & !grepl(":", names(coefs))]
  if (length(gender_coef_names) != 1) stop("Unexpected number of gender coefficients")
  base_name <- gender_coef_names[1]
  beta_base <- coefs[base_name]
  v_base    <- vcov_mat[base_name, base_name]
  
  map_dfr(time_var_levels, function(lv) {
    int_name <- paste0(base_name, ":", time_var_name, lv)
    if (int_name %in% names(coefs)) {
      beta_int   <- coefs[int_name]
      v_int      <- vcov_mat[int_name, int_name]
      cov_base_i <- vcov_mat[base_name, int_name]
      est  <- beta_base + beta_int
      var_est <- v_base + v_int + 2 * cov_base_i
    } else {
      est     <- beta_base
      var_est <- v_base
    }
    se <- sqrt(var_est)
    tibble(
      period   = lv,
      estimate = est,
      lower    = est - 1.96 * se,
      upper    = est + 1.96 * se
    )
  })
}

periods_20     <- levels(face_info_fa$time_20yr)
obl_20_manual <- gender_contrast_manual(coef_obl, vcov_obl, periods_20)
sz_20_manual  <- gender_contrast_manual(coef_sz,  vcov_sz,  periods_20)

# Plot out the interaction coefficients
trend_manual_20 <- bind_rows(
  obl_20_manual %>% mutate(Outcome = "Obliqueness"),
  sz_20_manual  %>% mutate(Outcome = "Log(Size)")
)

trend20_mod <- trend_manual_20 %>%
  slice(-c(1, 32, 33, 64)) %>%                 
  mutate(
    period      = as.character(period), 
    start_year  = parse_number(period), 
    year_mid    = start_year + 10  
  ) %>%
  filter(!is.na(start_year))        

dodge20 <- position_dodge(width = 10)

p_trend_manual_20 <- ggplot(trend20_mod, 
                            aes(x = year_mid, 
                                y = estimate, 
                                color = Outcome,
                                group = Outcome)) + 
  geom_hline(yintercept = 0, linetype="dashed", color="grey50") +
  geom_point(position = dodge20, size = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 20,
                position = dodge20) +
  scale_x_continuous(
    name   = "Year",
    breaks = seq(1400, 2000, by = 100),
    labels = seq(1400, 2000, by = 100)
  ) +
  scale_y_continuous(
    name   = "Face Gender Coefficients (Female = 1)",
    breaks = seq(-0.6, 0.6, by = 0.3),
    labels = seq(-0.6, 0.6, by = 0.3)) + 
  labs(
    color = "Representation"
  ) +scale_color_manual(
    values = c("red", "blue")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(p_trend_manual_20)

ggsave("Img/6_GenderCoefTrend.png", p_trend_manual_20, 
       width = 8, height = 5, dpi = 300)

# Period-interacted models for subset datasets
face_info_fa <- subset(face_info_fa, orig_year>1740)
subsets_20 <- list(
  #"Baseline"                    = face_info_fa,
  "Artist Gender: Male"         = filter(face_info_fa, author_gender == "Male"),
  "Artist Gender: Female"       = filter(face_info_fa, author_gender == "Female"),
  "Artist Education: Formal"    = filter(face_info_fa, edu_dummy == 1),
  "Artist Education: Self-taught"= filter(face_info_fa, edu_dummy == 0)
)

fit_model_dynamic <- function(df, outcome_var) {
  fe <- "face_gender * time_20yr"
  if (length(unique(df$author_gender)) > 1) {
    fe <- paste(fe, "+ author_gender")
  }
  if (length(unique(df$edu_dummy[!is.na(df$edu_dummy)])) > 1) {
    fe <- paste(fe, "+ edu_dummy")
  }
  
  if (length(unique(df$face_count)) > 1) {
    fmla <- as.formula(paste0(outcome_var, " ~ ", fe, " + (1 | face_count)"))
    lmer(fmla, data = df)
  } else {
    fmla <- as.formula(paste0(outcome_var, " ~ ", fe))
    lm(fmla, data = df)
  }
}

models_obl_20 <- purrr::imap(subsets_20, ~ fit_model_dynamic(.x, "obliqueness"))
models_sz_20  <- purrr::imap(subsets_20, ~ fit_model_dynamic(.x, "log_sizes"))

face_info_fa$time_20yr <- droplevels(face_info_fa$time_20yr)
extract_trend <- function(mod, outcome_label) {
  coefs <- fixef(mod)
  vc    <- as.matrix(vcov(mod))
  tib   <- gender_contrast_manual(coefs, vc, levels(face_info_fa$time_20yr))
  tib %>% mutate(Outcome = outcome_label)
}

trend20_subsets <- purrr::imap_dfr(models_obl_20,
                                   ~ extract_trend(.x, "Obliqueness")  %>% mutate(Model = .y)
  ) %>%
  bind_rows(
    purrr::imap_dfr(models_sz_20,
                    ~ extract_trend(.x, "Log(Size)") %>% mutate(Model = .y)
    )
  ) %>%
  mutate(
    start_year = readr::parse_number(period),
    year_mid   = start_year + 10
  ) %>%
  filter(!period %in% c("1400–1419", "2020–2039"))
  desired_order <- c(
    "Artist Gender: Male",
    "Artist Education: Formal",
    "Artist Gender: Female",
    "Artist Education: Self-taught"
  )
  
trend20_subsets <- trend20_subsets %>%
  mutate(
    Model = factor(Model, levels = desired_order)
  )

lims <- data.frame(
  Model = rep(desired_order, each = 2),
  y     = c(
    -3.5, 2.5,
    -3.5, 2.5,
    -3.5, 2.5,
    -3.5, 2.5
  )
)

lims <- lims %>%
  mutate(
    Model = factor(Model, levels = desired_order)
  )

p_sub_trends_20 <- ggplot(trend20_subsets,
                          aes(x = year_mid, y = estimate, color = Outcome, group = Outcome)
) +
  geom_blank(data = lims, aes(y = y), inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width    = 10,               # controls the horizontal “cap” width
                position = dodge20) +
  geom_point(position = dodge20, size = 1.5) +
  facet_wrap(~ Model, ncol = 2, nrow = 2,
             scales = "free_y") +
  scale_x_continuous(
    name   = "Year",
    breaks = seq(1740, 2020, by = 40),
    labels = seq(1740, 2020, by = 40)
  ) +
  labs(
    y     = "Face Gender Coefficients (Female = 1)",
    color = "Representation"
  ) +
  scale_color_manual(
    values = c("red", "blue")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


print(p_sub_trends_20)
ggsave("Img/7_GenderCoefSubTrend.png",
       p_sub_trends_20, width = 10, height = 6, dpi = 300)
