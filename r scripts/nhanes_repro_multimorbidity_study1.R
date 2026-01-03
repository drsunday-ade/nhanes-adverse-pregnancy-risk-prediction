############################################################
# Study 1 – NHANES 2017–March 2020
# Adverse reproductive history, multimorbidity, and depression
# OUTPUT: 3 PNG figures + 3 HTML/PNG tables
############################################################

## 0. Packages ---------------------------------------------------------------

required_pkgs <- c(
  "tidyverse",   # data wrangling + ggplot2
  "haven",       # read_xpt
  "survey",      # complex survey design
  "srvyr",       # tidy survey interface
  "gtsummary",   # beautiful regression & descriptive tables
  "gt",          # saving tables as HTML/PNG
  "broom",       # tidy model output
  "scales",      # pretty axes for plots
  "patchwork"    # (optional) combining plots
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

options(survey.lonely.psu = "adjust")

## 1. File locations ---------------------------------------------------------

# Set this to the folder where your XPT files live
# (P_DEMO.xpt, P_RHQ.xpt, P_DPQ.xpt, P_BPQ.xpt, P_MCQ.xpt, P_KIQ_U.xpt)
data_dir <- "."   # if you run the script from that folder

# Output folder for figures and tables
out_dir <- file.path(data_dir, "nhanes_repro_multimorbidity_output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## 2. Load XPT files ---------------------------------------------------------

demo  <- haven::read_xpt(file.path(data_dir, "P_DEMO.xpt"))
rhq   <- haven::read_xpt(file.path(data_dir, "P_RHQ.xpt"))
dpq   <- haven::read_xpt(file.path(data_dir, "P_DPQ.xpt"))
bpq   <- haven::read_xpt(file.path(data_dir, "P_BPQ.xpt"))
mcq   <- haven::read_xpt(file.path(data_dir, "P_MCQ.xpt"))
kiq   <- haven::read_xpt(file.path(data_dir, "P_KIQ_U.xpt"))

## 3. Merge to analytic dataset ----------------------------------------------

analytic_raw <-
  demo %>%
  select(
    SEQN, RIAGENDR, RIDAGEYR,
    RIDRETH3, DMDBORN4, DMDYRUSZ,
    DMDEDUC2, DMDMARTZ, INDFMPIR, RIDEXPRG,
    SDMVPSU, SDMVSTRA, WTINTPRP
  ) %>%
  left_join(
    rhq %>%
      select(
        SEQN, RHQ131, RHQ160, RHD167, RHQ171, RHQ162,
        RHQ172, RHD180, RHD190
      ),
    by = "SEQN"
  ) %>%
  left_join(
    dpq %>%
      select(SEQN, DPQ010, DPQ020, DPQ030, DPQ040,
             DPQ050, DPQ060, DPQ070, DPQ080, DPQ090, DPQ100),
    by = "SEQN"
  ) %>%
  left_join(
    bpq %>% select(SEQN, BPQ020, BPQ050A, BPQ080, BPQ100D),
    by = "SEQN"
  ) %>%
  left_join(
    mcq %>% select(
      SEQN,
      MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F,
      MCQ300A, MCQ300B, MCQ300C
    ),
    by = "SEQN"
  ) %>%
  left_join(
    kiq %>% select(SEQN, KIQ022, KIQ025, KIQ026),
    by = "SEQN"
  )

## 4. Restrict to women 20–44 and ever-pregnant ------------------------------

analytic_base <-
  analytic_raw %>%
  filter(
    RIAGENDR == 2,
    RIDAGEYR >= 20, RIDAGEYR <= 44
  ) %>%
  mutate(
    # Ever pregnant
    ever_preg = dplyr::case_when(
      RHQ131 == 1 ~ 1L,
      RHQ131 == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(ever_preg == 1L)

## 5. Construct PHQ-9 and depression variable --------------------------------

dpq_items <- sprintf("DPQ%03d", seq(10, 90, by = 10))

analytic_base <-
  analytic_base %>%
  mutate(
    across(
      all_of(dpq_items),
      ~ dplyr::case_when(.x %in% 0:3 ~ as.numeric(.x),
                         TRUE ~ NA_real_),
      .names = "clean_{.col}"
    ),
    phq9 = rowSums(across(starts_with("clean_DPQ")), na.rm = FALSE),
    dep_modsev = dplyr::case_when(
      !is.na(phq9) & phq9 >= 10 ~ 1L,
      !is.na(phq9) & phq9 < 10 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(-starts_with("clean_DPQ"))

## 6. Construct multimorbidity outcome ---------------------------------------

analytic_base <-
  analytic_base %>%
  mutate(
    # Hypertension: diagnosis or on meds
    htn = dplyr::case_when(
      BPQ020 == 1 | BPQ050A == 1 ~ 1L,
      BPQ020 == 2 & (BPQ050A %in% c(2, NA)) ~ 0L,
      TRUE ~ NA_integer_
    ),
    # High cholesterol diagnosis or meds
    chol = dplyr::case_when(
      BPQ080 == 1 | BPQ100D == 1 ~ 1L,
      BPQ080 == 2 & (BPQ100D %in% c(2, NA)) ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Any diabetes / pre-diabetes diagnosis
    diab = dplyr::case_when(
      MCQ300A == 1 | MCQ300B == 1 | MCQ300C == 1 ~ 1L,
      MCQ300A == 2 & MCQ300B == 2 & MCQ300C == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # CVD composite: HF / CHD / angina / MI / stroke
    cvd = dplyr::case_when(
      MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 |
        MCQ160E == 1 | MCQ160F == 1 ~ 1L,
      MCQ160B == 2 & MCQ160C == 2 & MCQ160D == 2 &
        MCQ160E == 2 & MCQ160F == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Kidney disease / dialysis / stones
    kidney = dplyr::case_when(
      KIQ022 == 1 | KIQ025 == 1 | KIQ026 == 1 ~ 1L,
      KIQ022 == 2 & (KIQ025 %in% c(2, NA)) & KIQ026 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    cond_count = rowSums(
      dplyr::across(c(htn, chol, diab, cvd, kidney)),
      na.rm = FALSE
    ),
    multimorbidity = dplyr::case_when(
      !is.na(cond_count) & cond_count >= 2 ~ 1L,
      !is.na(cond_count) & cond_count < 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

## 7. Construct adverse reproductive history index ---------------------------

analytic_base <-
  analytic_base %>%
  mutate(
    n_preg   = if_else(RHQ160 >= 0, as.numeric(RHQ160), NA_real_),
    n_births = if_else(RHQ171 >= 0, as.numeric(RHQ171), NA_real_),
    
    preg_loss = dplyr::case_when(
      !is.na(n_preg) & !is.na(n_births) & (n_preg - n_births) >= 1 ~ 1L,
      !is.na(n_preg) & !is.na(n_births) & (n_preg - n_births) <= 0 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    gdm = dplyr::case_when(
      RHQ162 == 1 ~ 1L,
      RHQ162 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    macrosomia = dplyr::case_when(
      RHQ172 == 1 ~ 1L,
      RHQ172 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    high_parity = dplyr::case_when(
      !is.na(n_births) & n_births >= 3 ~ 1L,
      !is.na(n_births) & n_births < 3 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    early_first = dplyr::case_when(
      !is.na(RHD180) & RHD180 < 20 ~ 1L,
      !is.na(RHD180) & RHD180 >= 20 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    late_first = dplyr::case_when(
      !is.na(RHD180) & RHD180 >= 35 ~ 1L,
      !is.na(RHD180) & RHD180 < 35 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    adverse_score = rowSums(
      dplyr::across(
        c(preg_loss, gdm, macrosomia, high_parity, early_first, late_first)
      ),
      na.rm = FALSE
    ),
    adverse_any = dplyr::case_when(
      !is.na(adverse_score) & adverse_score > 0 ~ 1L,
      !is.na(adverse_score) & adverse_score == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

## 8. Confounders and derived factors ----------------------------------------

analytic <-
  analytic_base %>%
  mutate(
    age        = as.numeric(RIDAGEYR),
    age_group  = if_else(age < 35, "20–34", "35–44"),
    
    race4 = dplyr::case_when(
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 %in% c(1, 2) ~ "Hispanic",
      RIDRETH3 %in% c(6, 7) ~ "Other/Asian",
      TRUE ~ NA_character_
    ),
    
    educ_cat = dplyr::case_when(
      DMDEDUC2 %in% c(1, 2) ~ "< High school",
      DMDEDUC2 == 3 ~ "High school / GED",
      DMDEDUC2 == 4 ~ "Some college / AA",
      DMDEDUC2 == 5 ~ "College graduate+",
      TRUE ~ NA_character_
    ),
    
    marital_cat = dplyr::case_when(
      DMDMARTZ == 1 ~ "Married",
      DMDMARTZ == 6 ~ "Living with partner",
      DMDMARTZ == 5 ~ "Never married",
      DMDMARTZ %in% c(2, 3, 4) ~ "Widowed/divorced/separated",
      TRUE ~ NA_character_
    ),
    
    ses_cat = dplyr::case_when(
      INDFMPIR < 1.3 ~ "Low income (<1.3 PIR)",
      INDFMPIR >= 1.3 & INDFMPIR < 3.5 ~ "Middle income (1.3–<3.5)",
      INDFMPIR >= 3.5 ~ "High income (≥3.5)",
      TRUE ~ NA_character_
    ),
    
    current_preg = dplyr::case_when(
      RIDEXPRG == 1 ~ "Pregnant",
      RIDEXPRG == 2 ~ "Not pregnant",
      TRUE ~ NA_character_
    ),
    
    adverse_any_factor = factor(
      adverse_any,
      levels = c(0, 1),
      labels = c("No adverse history", "≥1 adverse feature")
    ),
    multimorbidity_factor = factor(
      multimorbidity,
      levels = c(0, 1),
      labels = c("<2 conditions", "≥2 conditions")
    ),
    dep_modsev_factor = factor(
      dep_modsev,
      levels = c(0, 1),
      labels = c("PHQ-9 <10", "PHQ-9 ≥10")
    ),
    
    race4 = factor(race4),
    age_group = factor(age_group),
    educ_cat = factor(educ_cat),
    marital_cat = factor(marital_cat),
    ses_cat = factor(ses_cat),
    current_preg = factor(current_preg)
  ) %>%
  filter(
    !is.na(WTINTPRP),
    !is.na(adverse_any),
    !is.na(multimorbidity),
    !is.na(dep_modsev)
  )

## 9. Survey design objects ---------------------------------------------------

nhanes_design <- survey::svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTINTPRP,
  nest = TRUE,
  data = analytic
)

nhanes_srvyr <- srvyr::as_survey(nhanes_design)

## 10. Descriptive Table 1 ----------------------------------------------------

tbl1 <-
  nhanes_srvyr %>%
  gtsummary::tbl_svysummary(
    by = adverse_any_factor,
    include = c(
      age, age_group, race4, educ_cat, marital_cat, ses_cat,
      multimorbidity_factor, dep_modsev_factor
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  gtsummary::add_p() %>%
  gtsummary::bold_labels()

tbl1_gt <- gtsummary::as_gt(tbl1)

gt::gtsave(
  tbl1_gt,
  filename = file.path(out_dir, "table1_characteristics.html")
)
gt::gtsave(
  tbl1_gt,
  filename = file.path(out_dir, "table1_characteristics.png")
)

## 11. Main survey-weighted logistic models ----------------------------------

# Multimorbidity
multi_model_adj <- survey::svyglm(
  multimorbidity ~ adverse_any + age + race4 + educ_cat +
    ses_cat + marital_cat + current_preg,
  design = nhanes_design,
  family = quasibinomial()
)

# Moderate-to-severe depression
dep_model_adj <- survey::svyglm(
  dep_modsev ~ adverse_any + age + race4 + educ_cat +
    ses_cat + marital_cat + current_preg,
  design = nhanes_design,
  family = quasibinomial()
)

## 12. Regression Tables 2 and 3 ---------------------------------------------

tbl2 <- gtsummary::tbl_regression(
  multi_model_adj,
  exponentiate = TRUE,
  label = list(
    adverse_any ~ "Adverse reproductive history (any)",
    age ~ "Age (per year)",
    race4 ~ "Race/ethnicity",
    educ_cat ~ "Education",
    ses_cat ~ "Household income (PIR)",
    marital_cat ~ "Marital status",
    current_preg ~ "Current pregnancy status"
  )
) %>%
  gtsummary::bold_labels()

tbl2_gt <- gtsummary::as_gt(tbl2)
gt::gtsave(tbl2_gt,
           filename = file.path(out_dir, "table2_multimorbidity_model.html"))
gt::gtsave(tbl2_gt,
           filename = file.path(out_dir, "table2_multimorbidity_model.png"))

tbl3 <- gtsummary::tbl_regression(
  dep_model_adj,
  exponentiate = TRUE,
  label = list(
    adverse_any ~ "Adverse reproductive history (any)",
    age ~ "Age (per year)",
    race4 ~ "Race/ethnicity",
    educ_cat ~ "Education",
    ses_cat ~ "Household income (PIR)",
    marital_cat ~ "Marital status",
    current_preg ~ "Current pregnancy status"
  )
) %>%
  gtsummary::bold_labels()

tbl3_gt <- gtsummary::as_gt(tbl3)
gt::gtsave(tbl3_gt,
           filename = file.path(out_dir, "table3_depression_model.html"))
gt::gtsave(tbl3_gt,
           filename = file.path(out_dir, "table3_depression_model.png"))

## 13. Figure 1 – Prevalence of multimorbidity by adverse history ------------

prev_multi <-
  nhanes_srvyr %>%
  group_by(adverse_any_factor) %>%
  summarise(
    prev = survey_mean(multimorbidity, vartype = "ci", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    prev_low = prev_low,
    prev_upp = prev_upp
  )

fig1_multi <-
  ggplot(prev_multi,
         aes(x = adverse_any_factor, y = prev, ymin = prev_low, ymax = prev_upp)) +
  geom_col(width = 0.6, fill = "#5B8FF9") +
  geom_errorbar(width = 0.15, linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Adverse reproductive history",
    y = "Weighted prevalence of multimorbidity (≥2 conditions)",
    title = "Multimorbidity by adverse reproductive history, U.S. women 20–44",
    caption = "NHANES 2017–March 2020 pre-pandemic; survey-weighted estimates."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

ggsave(
  filename = file.path(out_dir, "fig1_prevalence_multimorbidity.png"),
  plot = fig1_multi,
  width = 7, height = 5, dpi = 300
)
fig1_multi
## 14. Figure 2 – Prevalence of PHQ-9 ≥10 by adverse history -----------------

prev_dep <-
  nhanes_srvyr %>%
  group_by(adverse_any_factor) %>%
  summarise(
    prev = survey_mean(dep_modsev, vartype = "ci", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    prev_low = prev_low,
    prev_upp = prev_upp
  )

fig2_dep <-
  ggplot(prev_dep,
         aes(x = adverse_any_factor, y = prev, ymin = prev_low, ymax = prev_upp)) +
  geom_col(width = 0.6, fill = "#F6BD16") +
  geom_errorbar(width = 0.15, linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Adverse reproductive history",
    y = "Weighted prevalence of PHQ-9 ≥10",
    title = "Moderate-to-severe depressive symptoms by reproductive history",
    caption = "NHANES 2017–March 2020 pre-pandemic; survey-weighted estimates."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )
fig2_dep
ggsave(
  filename = file.path(out_dir, "fig2_prevalence_depression.png"),
  plot = fig2_dep,
  width = 7, height = 5, dpi = 300
)

## 15. Figure 3 – Forest plot of adjusted ORs for adverse history -------------

multi_or <-
  broom::tidy(multi_model_adj, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "adverse_any") %>%
  transmute(
    outcome = "Multimorbidity (≥2 conditions)",
    or = estimate,
    or_low = conf.low,
    or_high = conf.high
  )

dep_or <-
  broom::tidy(dep_model_adj, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "adverse_any") %>%
  transmute(
    outcome = "PHQ-9 ≥10 (moderate–severe depression)",
    or = estimate,
    or_low = conf.low,
    or_high = conf.high
  )

forest_df <- bind_rows(multi_or, dep_or) %>%
  mutate(outcome = factor(outcome, levels = rev(unique(outcome))))

fig3_forest <-
  ggplot(forest_df,
         aes(x = outcome, y = or, ymin = or_low, ymax = or_high)) +
  geom_pointrange(size = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10(
    name = "Adjusted odds ratio (log scale)",
    breaks = c(0.5, 1, 2, 3, 4),
    labels = c("0.5", "1", "2", "3", "4")
  ) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Adjusted odds ratios for adverse reproductive history",
    subtitle = "Survey-weighted logistic models, U.S. ever-pregnant women 20–44",
    caption = "Models adjusted for age, race/ethnicity, education,\nmarital status, income, and current pregnancy status."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank()
  )
fig3_forest
ggsave(
  filename = file.path(out_dir, "fig3_forest_or_adverse_history.png"),
  plot = fig3_forest,
  width = 7, height = 5, dpi = 300
)

############################################################
# END OF SCRIPT
############################################################
