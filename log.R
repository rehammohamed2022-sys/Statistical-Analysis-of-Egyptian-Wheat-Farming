# =============================================================================
# EGY_PostHarvest-Wheat: Full Pipeline
# Part 1 — Data Cleaning  (Winsorizing + per-hectare scaling + continuous costs)
# Part 2 — All 9 Research Question Models
#           (Robust regression, GLMs, Ordinal & Multinomial Logit)
#           Outputs: diagnostics, VIF, pseudo-R², odds ratios,
#                    stargazer tables, ggplot visualisations
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# PART 1 — DATA CLEANING
# ─────────────────────────────────────────────────────────────────────────────

# ── 1. Libraries ──────────────────────────────────────────────────────────────
packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "writexl",
  "MASS", "nnet", "AER", "car", "DescTools",
  "stargazer", "ggplot2", "broom", "sfsmisc",
  "lmtest", "sandwich", "conflicted"
)
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install)) install.packages(to_install)
lapply(packages, library, character.only = TRUE)

# Explicitly resolve conflicts so dplyr always wins
conflict_prefer("select",  "dplyr")
conflict_prefer("filter",  "dplyr")
conflict_prefer("rename",  "dplyr")
conflict_prefer("mutate",  "dplyr")
conflict_prefer("summarise","dplyr")

# ── 2. Read data ──────────────────────────────────────────────────────────────
df <- read_excel("EGY_PostHarvest-Wheat.xlsx")

# ── 3. NA percentages (diagnostic) ───────────────────────────────────────────
cat("── NA percentages ──\n")
print(round(colSums(is.na(df)) / nrow(df) * 100, 1))

# ── 4. Variable types ─────────────────────────────────────────────────────────
factor_vars <- c(
  "soil_test", "fertilizer", "crop_protection", "crop_injury",
  "herbicide_target", "postharvest_treatment", "yield_metric",
  "expectation", "yield_compare", "damaged_reason", "pest",
  "own_consumption", "keep_seed", "spray_guide", "input_from",
  "get_products", "get_products_why", "price_satisfied",
  "profitable", "adv_weather", "water_source", "agro_advice",
  "improve_with", "newly_seed", "spraying_advice", "pollinators",
  "season_accident", "mixture_protection", "spraying_protection",
  "empty_pesticides"
)
numeric_vars <- c(
  "weight_bag", "n_bags", "damaged_pct", "pct_own", "seed_pct",
  "ttl_land_perm", "ttl_land_temp", "ttl_fallow_land",
  "c_tree_or_seed", "c_ferti", "c_pesti", "c_labor", "c_machi",
  "c_water", "c_fuel", "c_electri", "c_gas", "c_rent",
  "ttl_rev", "ttl_own_consumption", "ttl_stored_sell",
  "ttl_feed", "ttl_byprod", "ttl_sharecropping", "est_rev_field"
)
df[factor_vars]  <- lapply(df[factor_vars], as.factor)
df[numeric_vars] <- lapply(df[numeric_vars], as.numeric)

# ── 6. Recode damaged_reason ──────────────────────────────────────────────────
# Convert to character first — str_detect fails silently on factors
df$damaged_reason <- as.character(df$damaged_reason)

df <- df %>%
  mutate(
    has_biotic  = str_detect(damaged_reason,
                             "Disease pressure|Insect pressure|Mites pressure|Weeds infestation"),
    has_abiotic = str_detect(damaged_reason, "Drought|Extreme temperatures"),
    damaged_reason = case_when(
      has_biotic & has_abiotic ~ "Mixed",
      has_biotic               ~ "Biotic",
      has_abiotic              ~ "Abiotic",
    ),
    damaged_reason = factor(damaged_reason,
                            levels = c("Abiotic", "Biotic", "Mixed"))
  ) %>%
  dplyr::select(-has_biotic, -has_abiotic)

# ── 7. Recode input_from ──────────────────────────────────────────────────────
# Convert to character first — str_detect fails silently on factors
df$input_from <- as.character(df$input_from)

df <- df %>%
  mutate(
    input_from = case_when(
      str_detect(input_from,
                 "Agronomic Advisor|Distributor|Spray Service provider") ~ "Formal",
      str_detect(input_from,
                 "Random Retailer|Nearest Retailer|Local Market")        ~ "Market",
      str_detect(input_from, "Other Farmers")                            ~ "Informal",
      TRUE                                                                ~ "Other"
    ),
    input_from = factor(input_from,
                        levels = c("Formal", "Informal", "Market", "Other"))
  )

# ── 8. adv_weather cleaning ───────────────────────────────────────────────────
df$adv_weather[df$adv_weather %in% c("Excessive cold", "Rain")] <- NA
df$adv_weather <- droplevels(as.factor(df$adv_weather))

# ── 9. Recode improve_with ────────────────────────────────────────────────────
# Convert to character first — str_detect fails silently on factors
df$improve_with <- as.character(df$improve_with)

df <- df %>%
  mutate(
    pest_input     = str_detect(improve_with,
                                "spraying pesticides|Integrated Pest Management|Spray equipment maintenance|decision support systems"),
    soil_fertility = str_detect(improve_with,
                                "Soil analysis|fertilizer|soil preservation"),
    water_seed     = str_detect(improve_with, "water use|seed selection"),
    safety         = str_detect(improve_with, "individual protection equipment"),
    improve_with   = case_when(
      (pest_input + soil_fertility + water_seed + safety) > 1 ~ "Multiple Improvements",
      pest_input                                               ~ "Pest & Input Management",
      soil_fertility                                           ~ "Soil & Fertility Management",
      water_seed                                               ~ "Water & Seed Management",
      safety                                                   ~ "Health & Safety",
      TRUE                                                     ~ "Other"
    ),
    improve_with = as.factor(improve_with)
  ) %>%
  dplyr::select(-pest_input, -soil_fertility, -water_seed, -safety)

# ── 10. mixture_protection & spraying_protection (ordered) ───────────────────
# Convert to character first — str_count fails silently on factors
df$mixture_protection   <- as.character(df$mixture_protection)
df$spraying_protection  <- as.character(df$spraying_protection)

df <- df %>%
  mutate(
    n_mix = str_count(mixture_protection, ",") + 1,
    mixture_protection = case_when(
      n_mix <= 2 ~ "Low protection",
      n_mix == 3 ~ "Medium protection",
      n_mix >= 4 ~ "High protection",
    ),
    mixture_protection = factor(mixture_protection,
                                levels  = c("Low protection", "Medium protection",
                                            "High protection"),
                                ordered = TRUE)
  ) %>% dplyr::select(-n_mix)


df <- df %>%
  mutate(
    n_spray = str_count(spraying_protection, ",") + 1,
    spraying_protection = case_when(
      n_spray <= 2 ~ "Low protection",
      n_spray == 3 ~ "Medium protection",
      n_spray >= 4 ~ "High protection",
    ),
    spraying_protection = factor(spraying_protection,
                                 levels  = c("Low protection", "Medium protection",
                                             "High protection"),
                                 ordered = TRUE)
  ) %>% dplyr::select(-n_spray)

# ── 11. Fix NA categories that are NOT real missings ─────────────────────────
df$get_products_why <- as.character(df$get_products_why)
df$get_products_why[is.na(df$get_products_why)] <- "Product obtained"
df$get_products_why <- as.factor(df$get_products_why)

df$newly_seed <- as.character(df$newly_seed)
df$newly_seed[is.na(df$newly_seed)]                                <- "Other"
df$newly_seed[df$newly_seed == "Seed treatment for Fall Armyworm"] <- "Other"
df$newly_seed <- as.factor(df$newly_seed)

# ── 12. Remove typo observation ───────────────────────────────────────────────
df <- df[-270, ]

# ── 13. Drop pct_own (all zeros) ──────────────────────────────────────────────
df$pct_own <- NULL

# ── 14. seed_pct: NA → -999 (not real missings) ───────────────────────────────
df$seed_pct[is.na(df$seed_pct)] <- -999

# ── 15. Remove Insecticide from crop_protection ───────────────────────────────
df <- df %>%
  filter(crop_protection != "Insecticide") %>%
  mutate(crop_protection = droplevels(crop_protection))

# ── 16. Remove constant / redundant variables ─────────────────────────────────
df <- df %>%
  dplyr::select(-ttl_byprod, -ttl_stored_sell, -ttl_feed,
                -ttl_own_consumption, -ttl_sharecropping, -c_labor)

# ── 17. Total land & per-hectare scaling ──────────────────────────────────────
df <- df %>% mutate(ttl_land = ttl_land_perm + ttl_land_temp)
df <- df %>% filter(ttl_land > 0)   # safety: remove zero-land rows

# Correct yield: raw first, then scale
df$yield    <- df$weight_bag * df$n_bags
df$yield_ha <- df$yield / df$ttl_land

df <- df %>%
  mutate(
    revenue_ha      = ttl_rev        / ttl_land,
    est_revenue_ha  = est_rev_field  / ttl_land,
    seed_cost_ha    = c_tree_or_seed / ttl_land,
    ferti_cost_ha   = c_ferti        / ttl_land,
    pesti_cost_ha   = c_pesti        / ttl_land,
    machi_cost_ha   = c_machi        / ttl_land,
    water_cost_ha   = c_water        / ttl_land,
    fuel_cost_ha    = c_fuel         / ttl_land,
    electri_cost_ha = c_electri      / ttl_land,
    n_bags_ha       = n_bags         / ttl_land,
    damaged_pct_ha  = damaged_pct    / ttl_land
  ) %>%
  dplyr::select(
    -ttl_land_perm, -ttl_land_temp,
    -ttl_rev, -est_rev_field,
    -c_tree_or_seed, -c_ferti, -c_pesti,
    -c_machi, -c_water, -c_fuel, -c_electri,
    -n_bags, -damaged_pct, -yield
  )

# ── 18. c_rent & c_gas → binary flags ────────────────────────────────────────
df$c_rent_bin <- factor(ifelse(df$c_rent == 0, "No Rent", "Has Rent"))
df$c_gas_bin  <- factor(ifelse(df$c_gas  == 0, "No Gas",  "Has Gas"))
df <- df %>% dplyr::select(-c_rent, -c_gas)

# ── 19. seed_pct tercile category ─────────────────────────────────────────────
df$seed_pct_cat <- ifelse(df$seed_pct == -999, "No Seed Kept", NA_character_)
qs_seed <- unique(quantile(df$seed_pct[df$seed_pct != -999],
                           probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
if (length(qs_seed) < 4)
  qs_seed[length(qs_seed)] <- qs_seed[length(qs_seed)] + 0.0001
df$seed_pct_cat[df$seed_pct != -999] <- as.character(cut(
  df$seed_pct[df$seed_pct != -999],
  breaks = qs_seed,
  labels = c("Low","Mid","High")[1:(length(qs_seed)-1)],
  include.lowest = TRUE
))
df$seed_pct_cat <- factor(df$seed_pct_cat,
                          levels = c("No Seed Kept","Low","Mid","High"))
df$seed_pct <- NULL

# ── 20. Ordered factors for ordinal models ────────────────────────────────────
df$price_satisfied <- factor(df$price_satisfied, ordered = TRUE)

# yield_compare: remap actual survey labels -> Lower / Same / Higher
df$yield_compare <- as.character(df$yield_compare)
cat("\nActual yield_compare values in data:\n")
print(unique(df$yield_compare))

df$yield_compare <- dplyr::case_when(
  stringr::str_detect(tolower(df$yield_compare), "low|less|declin|reduc|decreas|worse")  ~ "Lower",
  stringr::str_detect(tolower(df$yield_compare), "same|equal|similar|unchanged|stable") ~ "Same",
  stringr::str_detect(tolower(df$yield_compare), "high|more|increas|better|improv")     ~ "Higher",
  TRUE ~ NA_character_
)

cat("\nyield_compare after remapping:\n")
print(table(df$yield_compare, useNA = "ifany"))

df$yield_compare <- factor(df$yield_compare,
                           levels  = c("Lower", "Same", "Higher"),
                           ordered = TRUE)

# ── 21. Binary outcome columns ────────────────────────────────────────────────
df$profitable_bin   <- ifelse(df$profitable      == "Yes", 1, 0)
df$keep_seed_bin    <- ifelse(df$keep_seed        == "Yes", 1, 0)
df$accident_bin     <- ifelse(df$season_accident  == "Yes", 1, 0)
df$get_products_bin <- ifelse(df$get_products     == "Yes", 1, 0)

# ── 22. Final NA removal ──────────────────────────────────────────────────────
df <- df %>%
  mutate(across(
    c(accident_bin, get_products_bin, profitable_bin, keep_seed_bin),
    ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes"))
  ))

# Check the structure
str(df)
# Diagnose NAs before removing — so one bad column never silently wipes all rows
na_check <- data.frame(
  variable    = names(df),
  n_missing   = colSums(is.na(df)),
  pct_missing = round(colSums(is.na(df)) / nrow(df) * 100, 1)
)
cat("\nColumns with missing values:\n")
print(na_check[na_check$n_missing > 0, ])

# Only drop rows missing on variables actually used in models
essential_vars <- c(
  "yield_ha", "damaged_pct_ha", "revenue_ha",
  "profitable_bin", "keep_seed_bin", "accident_bin", "get_products_bin",
  "water_source", "adv_weather",
  "soil_test", "agro_advice", "damaged_reason", "pest",
  "postharvest_treatment", "input_from", "price_satisfied",
  "yield_compare", "expectation", "newly_seed", "season_accident",
  "spraying_protection", "mixture_protection", "herbicide_target",
  "crop_injury", "spraying_advice"
)
essential_exist <- intersect(essential_vars, names(df))
df <- df %>% dplyr::filter(dplyr::if_all(dplyr::all_of(essential_exist), ~ !is.na(.)))

cat("\n── Final dataset:", nrow(df), "rows ×", ncol(df), "cols ──\n")
summary(df)

df <- df  
cat("\n✓ Cleaning complete. Starting models...\n\n")


packages <- c(
  "dplyr",
  "ggplot2",
  "car",
  "lmtest",
  "sandwich",
  "pscl",
  "pROC",
  "caret",
  "ResourceSelection",
  "DescTools",
  "stargazer",
  "broom"
)

installed <- rownames(installed.packages())

to_install <- packages[!packages %in% installed]

if(length(to_install) > 0){
  install.packages(to_install)
}

lapply(packages, library, character.only = TRUE)
####################################################

## KERNAL DENSITY

plot(density(df$damaged_pct_ha, na.rm = TRUE),
     main = "Kernel Density of Wheat Damage (%)",
     xlab = "Damage Percentage",
     lwd = 2)
dens <- density(df$damaged_pct_ha, na.rm = TRUE)

plot(dens,
     main = "Kernel Density of Wheat Damage (%)",
     xlab = "Damage Percentage",
     lwd = 2,xlim = c(0, 12))

abline(v = median(df$damaged_pct_ha, na.rm = TRUE),
       col = "blue", lwd = 2, lty = 2)

abline(v = mean(df$damaged_pct_ha, na.rm = TRUE),
       col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("Median", "Mean"),
       col = c("blue", "red"),
       lty = 2,
       lwd = 2)

dens <- density(df$damaged_pct_ha, na.rm = TRUE)

plot(dens,
     main = "Kernel Density of Wheat Damage (%) with Threshold",
     xlab = "Damage Percentage",
     lwd = 2)

mode_value <- dens$x[which.max(dens$y)]

cutoff <- mode_value  # or your chosen threshold

abline(v = cutoff, col = "darkgreen", lwd = 2, lty = 2)

# Optional shading idea (visual separation)
polygon(c(dens$x[dens$x <= cutoff], cutoff),
        c(dens$y[dens$x <= cutoff], 0),
        col = rgb(0,1,0,0.2), border = NA)

df$loss_cat <- cut(
  df$damaged_pct_ha,
  breaks = c(-Inf, 0.5421714, Inf),
  labels = c("Low", "High"),
  right = TRUE
)

# Set reference category
df$loss_cat <- relevel(as.factor(df$loss_cat), ref = "Low")

table(df$loss_cat)

prop.table(table(df$loss_cat))
# =============================================================================
# 3. SELECT VARIABLES
# =============================================================================

logit_df <- df[, c(
  
  # Outcome
  "loss_cat",
  
  # Biological
  "pest","damaged_reason"
,
  
  # Environmental
  "adv_weather",
  "water_source",
  "season_accident",
  
  # Farm practices
  "spraying_protection",
  "mixture_protection",
  "spraying_advice",
  
  # Institutional / knowledge
  "agro_advice",
  "input_from",
  "spray_guide"
  
)]
# Binary outcome
logit_df$loss_bin <- ifelse(
  logit_df$loss_cat == "High",
  1,
  0
)
model1 <- glm(
  
  loss_bin ~
    
    # =====================================================
  # MAIN EFFECTS
  # =====================================================
  
  pest +
    damaged_reason +
    
    adv_weather +
    water_source +
    season_accident +
    
    spraying_protection +
    mixture_protection +
    spraying_advice +
    
    agro_advice +
    input_from +
    spray_guide,data = logit_df,
  
  family = binomial(link = "logit")
  
)

summary(model1)


model2<- step(model1,direction="both")
summary(model2)
######## model assumptions
### multicollinearity
library(car)
vif_values <- vif(model2)

vif_values
# Null model (intercept only)
null_model <- glm(loss_bin ~ 1,
                  data = logit_df,
                  family = binomial)

# Your fitted model (example: final model)
fit_model <- model2   # or your glm model

# Likelihood Ratio Test (G2)
anova(null_model, fit_model, test = "Chisq")
# predicted probabilities from your model
logit_df$prob <- predict(fit_model, type = "response")
library(pROC)

roc_obj <- roc(logit_df$loss_bin, logit_df$prob)

# optimal cutoff (maximizes sensitivity + specificity - 1)
opt_cutoff <- coords(roc_obj, "best", 
                     best.method = "youden",
                     ret = "threshold")

opt_cutoff
logit_df$pred_class <- ifelse(logit_df$prob >= 0.5993237, 1, 0)
table(Predicted = logit_df$pred_class,
      Actual = logit_df$loss_bin)
conf_mat <- table(logit_df$pred_class, logit_df$loss_bin)

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
sensitivity <- conf_mat[2,2] / sum(conf_mat[,2])   # recall for class 1
specificity <- conf_mat[1,1] / sum(conf_mat[,1])

accuracy
sensitivity
specificity
# predicted probabilities from your model
logit_df$prob <- predict(model2, type = "response")

# choose cutoff (replace with your optimal cutoff if needed)
cutoff <- 0.5993237   # or best_cutoff from your earlier step

# create predicted classes
logit_df$pred_class <- ifelse(logit_df$prob >= cutoff, 1, 0)
library(caret)

confusionMatrix(
  factor(logit_df$pred_class, levels = c(0, 1)),
  factor(logit_df$loss_bin, levels = c(0, 1)),
  positive = "1"
)
library(ggplot2)
library(pROC)

# ROC object
roc_obj <- roc(logit_df$loss_bin, logit_df$prob)

# AUC
auc_value <- auc(roc_obj)

# ROC dataframe
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

# Plot
ggplot(roc_df, aes(x = fpr, y = tpr)) +
  
  # Area under curve
  geom_area(fill = "#4C78A8", alpha = 0.25) +
  
  # ROC curve
  geom_line(color = "#1F4E79", linewidth = 1.4) +
  
  # Reference diagonal
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed",
              color = "gray60",
              linewidth = 0.9) +
  
  # AUC annotation
  annotate("text",
           x = 0.72,
           y = 0.15,
           label = paste("AUC =", round(auc_value, 3)),
           size = 5,
           fontface = "bold") +
  
  labs(
    title = " ROC Curve for Logistic Regression Model",
    
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  
  coord_equal() +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# Null model (intercept only)
null_model <- glm(loss_bin ~ 1,
                  data = logit_df,
                  family = binomial)

# Your fitted model
fit_model <- model2   # or your final glm model

# Log-likelihoods
ll_full <- logLik(fit_model)
ll_null <- logLik(null_model)

# McFadden's R²
mcfadden_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
mcfadden_r2
exp(coef(model2))
