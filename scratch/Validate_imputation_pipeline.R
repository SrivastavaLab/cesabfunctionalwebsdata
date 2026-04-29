## validate_imputation_pipeline.R
##
## Interactive step-by-step validation of the 03_imputed_data_new.R pipeline.
##
## HOW TO USE THIS SCRIPT
## ──────────────────────
## Work through each numbered section in order. Each section:
##   1. Builds a single target with tar_make()
##   2. Loads it into your session with tar_load()
##   3. Runs inspection checks with expected outputs noted
##   4. Provides a DOCUMENT block for you to fill in findings
##
## When a check passes, move on.
## When a check reveals a problem, stay in the section, fix the function, and
## re-run tar_make() for that target before continuing.
##
## The store path is set once at the top — adjust if needed.
## All tar_make() calls reference the imputed-data store explicitly so that
## this script works even when other stores exist in the project.

library(targets)
library(tarchetypes)
library(tidyverse)
library(bwgdata)

Sys.setenv(TAR_PROJECT = "project_imputed_data_new")

## ── Configuration ────────────────────────────────────────────────────────────

STORE      <- "store_imputed_data"   # adjust to your actual store name
OBS_STORE  <- "store_observed_data"  # upstream store, read-only here

## Quick helper: run a single target and report time taken
make_one <- function(target_name) {
  message("\n── Building: ", target_name, " ──")
  t0 <- proc.time()
  tar_make(names = target_name, script = "03_imputed_data_new.R", store = STORE)
  elapsed <- (proc.time() - t0)[["elapsed"]]
  message("   done in ", round(elapsed, 1), "s")
}

## ════════════════════════════════════════════════════════════════════════════
## SECTION 1 — File inputs
## Targets: fpom_fg_data, volume_estimated, aquilega_biog, aquilegaKT,
##          guzmania, mertensii, vriesea, vriesea_prod
## ════════════════════════════════════════════════════════════════════════════

## ── 1a. FPOM French Guiana conversion data ───────────────────────────────────

make_one("fpom_fg_data")
tar_load(fpom_fg_data, store = STORE)

## Expected: tibble with columns fpom_ml (double) and fpom_g (double).
## No NA in fpom_ml (filtered by read_fpom_fg). A handful of rows (~30).
glimpse(fpom_fg_data)
stopifnot(!any(is.na(fpom_fg_data$fpom_ml)))
cat("fpom_fg_data rows:", nrow(fpom_fg_data), "\n")

# fpom_fg_data rows: 22

## ── 1b. Hand-curated volume estimates ────────────────────────────────────────

make_one("volume_estimated")
tar_load(volume_estimated, store = STORE)

## Expected: tibble with bromeliad_id (character) and max_water (double).
## Some max_water will be NA — those are filtered inside add_24_volum_data().
glimpse(volume_estimated)
cat("volume_estimated rows:", nrow(volume_estimated),
    "  non-NA max_water:", sum(!is.na(volume_estimated$max_water)), "\n")

# volume_estimated rows: 1618   non-NA max_water: 894

## ── 1c. Supplementary morphometry files ──────────────────────────────────────

for (tgt in c("aquilega_biog", "aquilegaKT", "guzmania", "mertensii", "vriesea", "vriesea_prod")) {
  make_one(tgt)
}

tar_load(c(aquilega_biog, aquilegaKT, guzmania, mertensii, vriesea, vriesea_prod), store = STORE)

## Check each file loaded without error and has a plant_id column.
for (obj_name in c("aquilega_biog", "aquilegaKT", "guzmania", "mertensii", "vriesea", "vriesea_prod")) {
  obj <- get(obj_name)
  cat(obj_name, ": rows =", nrow(obj), "  cols =", ncol(obj), "\n")
  stopifnot("plant_id" %in% names(obj))
}

# aquilega_biog : rows = 27   cols = 4
# aquilegaKT : rows = 34   cols = 4
# guzmania : rows = 33   cols = 4
# mertensii : rows = 77   cols = 5
# vriesea : rows = 36   cols = 4
# vriesea_prod : rows = 87   cols = 4

for (obj_name in c("aquilega_biog", "aquilegaKT", "guzmania", "mertensii", "vriesea", "vriesea_prod")) {
  obj <- get(obj_name)
  str(obj)
}


## DOCUMENT — Section 1
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ fpom_fg_data:                                                           │
## │   Row count:   22                                                         │
## │   fpom_ml range:       4-38                                                 │
## │   fpom_g range:        0.001-2.96                                                 │
## │                                                                         │
## │ volume_estimated:                                                       │
## │   Row count:           1618                                                 │
## │   Non-NA max_water:     894                                                │
## │   Visit IDs covered:   unknown                                                 │
## │                                                                         │
## │ Supplementary files — any missing columns noted?                        │
## │   aquilega_biog:                                                        │
## │   aquilegaKT:                                                           │
## │   guzmania:                                                             │
## │   mertensii:                                                            │
## │   vriesea:                                                              │
## │   vriesea_prod:                                                         │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 2 — FPOM model (currently disabled)
## Target: model_fpom_g_ml
## ════════════════════════════════════════════════════════════════════════════

make_one("model_fpom_g_ml")
tar_load(model_fpom_g_ml, store = STORE)

## Expected: an lm object fit on fpom_g ~ I(fpom_ml^2) + fpom_ml.
## Prediction is currently bypassed; we just confirm the model fits cleanly.
stopifnot(inherits(model_fpom_g_ml, "lm"))
cat("FPOM model R²:", summary(model_fpom_g_ml)$r.squared, "\n")
cat("Coefficients:\n"); print(coef(model_fpom_g_ml))

## DOCUMENT — Section 2
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ model_fpom_g_ml:                                                        │
## │   Formula: fpom_g ~ I(fpom_ml^2) + fpom_ml                             │
## │   R²:   FPOM model R²: 0.8736784                                                                 │
## │   Intercept:   0.036871026                                                         │
## │   fpom_ml coeff:    0.024342626                                                    │
## │   I(fpom_ml^2) coeff:   0.001313873                                                │
## │   Notes on fit quality / outliers:  not a terribly good fit, with some outliers especially line 22                                    │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 3 — Wide phase: observed data + derived predictor columns
## Targets: bromeliads_wide, bromeliads_wide_pred_cols
## ════════════════════════════════════════════════════════════════════════════

make_one("bromeliads_wide")
tar_load(bromeliads_wide, store = STORE)

## Expected: same object as observed_data$bromeliads from the upstream store.
## Should have one column per original detritus size class (no derived columns).
original_det_cols <- names(bromeliads_wide)[startsWith(names(bromeliads_wide), "detritus")]
cat("Original detritus columns (", length(original_det_cols), "):\n")
print(sort(original_det_cols))

## Check no derived column names (should have no "_sum", "_plus", "_Japi" etc.)
derived_pattern <- "_sum$|_plus$|_Japi$|_na0$|_over_"
bad <- original_det_cols[grepl(derived_pattern, original_det_cols)]
if (length(bad) > 0) warning("Unexpected derived columns found: ", paste(bad, collapse = ", "))

## ── 3b. Derived predictor columns ────────────────────────────────────────────

make_one("bromeliads_wide_pred_cols")
tar_load(bromeliads_wide_pred_cols, store = STORE)

## Expected: same as bromeliads_wide plus the derived columns created by
## add_new_columns_for_prediction(). Original columns must be unchanged.
new_cols <- setdiff(names(bromeliads_wide_pred_cols), names(bromeliads_wide))
cat("Derived predictor columns added (", length(new_cols), "):\n")
print(sort(new_cols))

## Spot-check one derivation: detritus850_20000_sum should equal
## detritus1500_20000 + detritus850_1500 wherever detritus850_20000 is NA.
check_sum <- bromeliads_wide_pred_cols %>%
  filter(is.na(detritus850_20000)) %>%
  mutate(expected = detritus1500_20000 + detritus850_1500,
         diff     = abs(detritus850_20000_sum - expected)) %>%
  filter(!is.na(diff) & diff > 1e-9)
if (nrow(check_sum) > 0) warning(nrow(check_sum), " rows where detritus850_20000_sum != sum of parts")

## DOCUMENT — Section 3
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ bromeliads_wide:                                                        │
## │   Rows (bromeliads):                                                    │
## │   Original detritus columns:                                            │
## │   Any unexpected column names?                                          │
## │                                                                         │
## │ bromeliads_wide_pred_cols:                                              │
## │   New derived columns:                                                  │
## │   detritus20000_NA type after coercion (should be double):              │
## │   detritus850_20000_sum derivation check — rows with discrepancies:     │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 4 — Cross-site GLMs: model table → fits → predictions
## Targets: model_table, modelling_information, model_fits,
##          detritus_imputed_long, detritus_validation_long
## ════════════════════════════════════════════════════════════════════════════

## ── 4a. Model specification table ────────────────────────────────────────────

make_one("model_table")
tar_load(model_table, store = STORE)

## Expected: 12-row rowwise tibble. Columns include m_id, target_dat, src_dat,
## xvar (formula), yvar (formula), fml_string, yvar_min, yvar_max.
cat("model_table rows:", nrow(model_table), " (expected 12)\n")
print(model_table %>% select(m_id, fml_string, yvar_min, yvar_max))

## Verify every fml_string parses as a valid formula
bad_fml <- model_table %>%
  rowwise() %>%
  mutate(ok = tryCatch({ as.formula(fml_string); TRUE }, error = function(e) FALSE)) %>%
  filter(!ok)
if (nrow(bad_fml) > 0) stop("Invalid formula strings: ", paste(bad_fml$m_id, collapse = ", "))

## ── 4b. Modelling information (source data attached) ─────────────────────────

make_one("modelling_information")
tar_load(modelling_information, store = STORE)

## Expected: model_table enriched with src_df, x_vars, y_vars, x_funs, y_funs.
## Every src_df must be non-empty (enforced by derive_modelling_information()).
src_rows <- map_dbl(modelling_information$src_df, nrow)
cat("Source data row counts per model:\n")
print(data.frame(m_id = modelling_information$m_id, src_rows = src_rows))
stopifnot(all(src_rows > 0))

## Check extracted variable names look sensible
cat("\nExtracted y_vars:\n")
print(data.frame(m_id = modelling_information$m_id, y_vars = modelling_information$y_vars))

## ── 4c. GLM fits ─────────────────────────────────────────────────────────────

make_one("model_fits")
tar_load(model_fits, store = STORE)

## Expected: one row per model; predicting_model column holds fitted glm objects.
## safe_model$error should be NULL for every row — if not, print the error.
failed <- model_fits %>%
  rowwise() %>%
  filter(!is.null(safe_model$error))

if (nrow(failed) > 0) {
  message("⚠ Failed model fits:")
  walk2(failed$m_id, failed$safe_model, ~ message("  ", .x, ": ", .y$error$message))
} else {
  message("✓ All models fit without error")
}


## investigating the failures

failed$fml_string[[1]]
glimpse(failed$src_df[[1]])


## For each successful fit, print summary coefficients and R²
model_fits %>%
  rowwise() %>%
  filter(is.null(safe_model$error)) %>%
  mutate(
    r2     = summary(predicting_model)$r.squared,
    n_coef = length(coef(predicting_model))
  ) %>%
  select(m_id, fml_string, r2, n_coef) %>%
  print()

## DOCUMENT — Section 4a–4c
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ model_table: 12 rows? Y/N                                               │
## │   Any formula parse errors?                                             │
## │                                                                         │
## │ modelling_information:                                                  │
## │   Any models with 0-row src_df?                                         │
## │   y_vars look correct for each m_id?                                    │
## │                                                                         │
## │ model_fits:                                                             │
## │   Any failed fits?                                                      │
## │   Model R² values (fill in from print above):                           │
## │     m01:    m02:    m03:    m04:                                        │
## │     m05:    m06:    m07:    m08:                                        │
## │     m09:    m10:    m11:    m12:                                        │
## └─────────────────────────────────────────────────────────────────────────┘

## ── 4d. Imputed detritus (long) ───────────────────────────────────────────────

make_one("detritus_imputed_long")
tar_load(detritus_imputed_long, store = STORE)

## Expected columns: bromeliad_id, dataset_id, size_min, size_max,
##                   detritus_g, source (= "model_mXX")
cat("detritus_imputed_long columns:\n"); print(names(detritus_imputed_long))
cat("Row count:", nrow(detritus_imputed_long), "\n")
cat("Source values (should all start with 'model_'):\n")
print(table(detritus_imputed_long$source))
cat("size_min / size_max pairs present:\n")
print(detritus_imputed_long %>% count(size_min, size_max) %>% arrange(size_min))

## No imputed values should be negative
neg_rows <- detritus_imputed_long %>% filter(detritus_g < 0)
if (nrow(neg_rows) > 0) {
  warning(nrow(neg_rows), " rows with negative imputed detritus_g")
  print(neg_rows)
}

## ── 4e. Validation table ─────────────────────────────────────────────────────

make_one("detritus_validation_long")
tar_load(detritus_validation_long, store = STORE)

## Expected columns: bromeliad_id, dataset_id, detritus_observed,
##                   size_min, size_max, detritus_predicted, m_id
cat("detritus_validation_long columns:\n"); print(names(detritus_validation_long))

## Quick R² per model using the validation table
validation_r2 <- detritus_validation_long %>%
  filter(!is.na(detritus_observed) & !is.na(detritus_predicted)) %>%
  group_by(m_id) %>%
  summarise(
    n    = n(),
    r2   = cor(detritus_observed, detritus_predicted, use = "complete.obs")^2,
    rmse = sqrt(mean((detritus_observed - detritus_predicted)^2)),
    .groups = "drop"
  )
cat("\nValidation R² and RMSE per model:\n")
print(validation_r2)

## DOCUMENT — Section 4d–4e
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ detritus_imputed_long:                                                  │
## │   Row count:                                                            │
## │   Target visit IDs represented:                                         │
## │   Negative detritus_g values? (should be none)                         │
## │   Any size_min / size_max pairs unexpected?                             │
## │                                                                         │
## │ detritus_validation_long:                                               │
## │   Row count:                                                            │
## │   Validation R² per model (from table above):                          │
## │     m01:    m02:    m03:    m04:                                        │
## │     m05:    m06:    m07:    m08:                                        │
## │     m09:    m10:    m11:    m12:                                        │
## │   Models with poor fit (R² < 0.5)?                                     │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 5 — Single pivot + combine
## Targets: detritus_observed_long, detritus_long,
##          detritus_long_checked, detritus_summary
## ════════════════════════════════════════════════════════════════════════════

## ── 5a. Observed detritus pivoted to long ────────────────────────────────────

make_one("detritus_observed_long")
tar_load(detritus_observed_long, store = STORE)

## Expected: one row per bromeliad × original size class, NA rows dropped.
## All source values should be "observed". size_min and size_max are numeric.
cat("detritus_observed_long: rows =", nrow(detritus_observed_long), "\n")
stopifnot(all(detritus_observed_long$source == "observed"))
stopifnot(is.numeric(detritus_observed_long$size_min))
stopifnot(is.numeric(detritus_observed_long$size_max))
cat("Observed size_min / size_max pairs:\n")
print(detritus_observed_long %>% count(size_min, size_max) %>% arrange(size_min))

## ── 5b. Combined long table ───────────────────────────────────────────────────

make_one("detritus_long")
tar_load(detritus_long, store = STORE)

## Expected: observed + imputed rows, coalesced by bromeliad × size class.
## source column should be "observed", "model_mXX", or "observed+model_mXX".
## The last form is unexpected and should be investigated if it appears.
cat("detritus_long: rows =", nrow(detritus_long), "\n")
source_table <- table(detritus_long$source)
print(source_table)

mixed <- detritus_long %>% filter(grepl("observed.*model|model.*observed", source))
if (nrow(mixed) > 0) {
  warning(nrow(mixed), " rows have mixed observed+imputed source — investigate!")
  print(mixed)
}

## Confirm no bromeliad has duplicate size_min/size_max combinations
dup_ranges <- detritus_long %>%
  count(bromeliad_id, size_min, size_max) %>%
  filter(n > 1)
if (nrow(dup_ranges) > 0) {
  warning(nrow(dup_ranges), " duplicate size-class rows after combining")
  print(dup_ranges)
}

## ── 5c. Consecutiveness check ────────────────────────────────────────────────

make_one("detritus_long_checked")
tar_load(detritus_long_checked, store = STORE)

## Expected: detritus_long plus is_consecutive column.
## For bromeliads with a single size class, is_consecutive is always TRUE.
non_consec <- detritus_long_checked %>%
  group_by(bromeliad_id) %>%
  filter(any(!is_consecutive)) %>%
  ungroup()

cat("Bromeliads with non-consecutive size classes:",
    n_distinct(non_consec$bromeliad_id), "\n")
if (nrow(non_consec) > 0) {
  cat("Examples:\n")
  print(non_consec %>% arrange(bromeliad_id, size_min) %>% head(20))
}

## ── 5d. Detritus summary ─────────────────────────────────────────────────────

make_one("detritus_summary")
tar_load(detritus_summary, store = STORE)

## Expected: one row per bromeliad. Columns: bromeliad_id, total_detritus,
## size_range_min, size_range_max, source, all_consecutive.
cat("detritus_summary: rows =", nrow(detritus_summary), "\n")
cat("Bromeliads with all_consecutive == FALSE:",
    sum(!detritus_summary$all_consecutive), "\n")
cat("Source breakdown:\n"); print(table(detritus_summary$source))
cat("\ntotal_detritus summary:\n"); print(summary(detritus_summary$total_detritus))

## Flag extreme total_detritus values for inspection
high_det <- detritus_summary %>%
  filter(total_detritus > quantile(total_detritus, 0.99, na.rm = TRUE))
cat("Top 1% total_detritus (", nrow(high_det), " bromeliads):\n")
print(high_det %>% arrange(desc(total_detritus)))

## DOCUMENT — Section 5
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ detritus_observed_long:                                                 │
## │   Row count:                                                            │
## │   Size classes present (list min/max pairs):                           │
## │                                                                         │
## │ detritus_long (combined):                                               │
## │   Row count:                                                            │
## │   "observed+model_mXX" mixed rows? (should be 0)                      │
## │   Duplicate size-class rows? (should be 0)                             │
## │                                                                         │
## │ detritus_long_checked:                                                  │
## │   Bromeliads with non-consecutive size classes:                        │
## │   Visit IDs involved:                                                   │
## │   Cause identified?                                                     │
## │                                                                         │
## │ detritus_summary:                                                       │
## │   Row count (= n bromeliads):                                           │
## │   Bromeliads with all_consecutive == FALSE:                             │
## │   total_detritus range:                                                 │
## │   Any implausibly high values?                                          │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 6 — Final wide bromeliad table (detritus joined)
## Target: bromeliads_with_detritus
## ════════════════════════════════════════════════════════════════════════════

make_one("bromeliads_with_detritus")
tar_load(bromeliads_with_detritus, store = STORE)

## Expected: bromeliads_wide minus raw detritus columns, plus the four summary
## columns from detritus_summary (total_detritus, size_range_min,
## size_range_max, source, all_consecutive).
cat("bromeliads_with_detritus: rows =", nrow(bromeliads_with_detritus), "\n")
cat("Columns added from detritus_summary:\n")
print(names(bromeliads_with_detritus)[names(bromeliads_with_detritus) %in%
      c("total_detritus", "size_range_min", "size_range_max", "source", "all_consecutive")])

## Confirm no raw detritus columns leaked through
leaked <- names(bromeliads_with_detritus)[
  startsWith(names(bromeliads_with_detritus), "detritus")
]
if (length(leaked) > 0) warning("Raw detritus columns still present: ", paste(leaked, collapse = ", "))

## Row count should be unchanged from bromeliads_wide
stopifnot(nrow(bromeliads_with_detritus) == nrow(bromeliads_wide))

## How many bromeliads have NA total_detritus?
na_det <- sum(is.na(bromeliads_with_detritus$total_detritus))
cat("Bromeliads with NA total_detritus:", na_det, "\n")

## DOCUMENT — Section 6
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ bromeliads_with_detritus:                                               │
## │   Row count (= bromeliads_wide rows?):                                 │
## │   Raw detritus columns leaked? (should be none)                        │
## │   NA total_detritus count:                                              │
## │   Notes on unexpected NAs or row count mismatch:                       │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 7 — Volume imputation
## Targets: supplementary_size_data, supp_data_additional, supp_data_renamed,
##          supp_size_models, supp_size_model_info, supp_size_model_data,
##          supp_size_model_fits, bromeliads_vol_imputed, bromeliads_vol_final
## ════════════════════════════════════════════════════════════════════════════

## ── 7a. Supplementary size data assembly ─────────────────────────────────────

make_one("supplementary_size_data")
tar_load(supplementary_size_data, store = STORE)
cat("supplementary_size_data: rows =", nrow(supplementary_size_data), "\n")
cat("filename values:\n"); print(table(supplementary_size_data$filename))

make_one("supp_data_additional")
tar_load(supp_data_additional, store = STORE)
## Check that the species join worked — no NAs in species column
na_species <- sum(is.na(supp_data_additional$species))
if (na_species > 0) warning(na_species, " rows with NA species after add_more_info_to_supp()")
cat("species values in supp_data_additional:\n")
print(table(supp_data_additional$species))

## FIX 7 verification: confirm "Vriesea_splendens" (not "Vrisea_splendens")
stopifnot(!"Vrisea_splendens" %in% supp_data_additional$species)

make_one("supp_data_renamed")
tar_load(supp_data_renamed, store = STORE)
## Expected after FIX 8: diameter, num_leaf, max_water columns present.
## Diam2 absent. No error even if a file lacked one of the renamed columns.
cat("supp_data_renamed columns:\n"); print(names(supp_data_renamed))
for (col in c("diameter", "num_leaf", "max_water")) {
  if (!col %in% names(supp_data_renamed)) warning("Missing expected column: ", col)
}
stopifnot(!"Diam2" %in% names(supp_data_renamed))

## ── 7b. Volume model specification and fits ──────────────────────────────────

make_one("supp_size_models")
tar_load(supp_size_models, store = STORE)
cat("supp_size_models:\n"); print(supp_size_models %>% select(m_id, formula_text))

make_one("supp_size_model_info")
tar_load(supp_size_model_info, store = STORE)
## Each src_df must be non-empty
src_rows_vol <- map_dbl(supp_size_model_info$src_df, nrow)
cat("Volume model source data rows:\n")
print(data.frame(m_id = supp_size_model_info$m_id, src_rows = src_rows_vol))
stopifnot(all(src_rows_vol > 0))

make_one("supp_size_model_data")
tar_load(supp_size_model_data, store = STORE)
cat("supp_size_model_data (target visits):\n"); print(supp_size_model_data)

make_one("supp_size_model_fits")
tar_load(supp_size_model_fits, store = STORE)
## Check models fitted, print summary
cat("Volume model fits:\n")
supp_size_model_fits %>%
  rowwise() %>%
  mutate(r2 = summary(model)$r.squared) %>%
  select(m_id, formula_text, r2) %>%
  print()

## ── 7c. Volume predictions applied to bromeliads ─────────────────────────────

make_one("bromeliads_vol_imputed")
tar_load(bromeliads_vol_imputed, store = STORE)

## Expected: bromeliads_with_detritus plus max_water_combined and
## max_water_estimated columns.
stopifnot("max_water_combined"  %in% names(bromeliads_vol_imputed))
stopifnot("max_water_estimated" %in% names(bromeliads_vol_imputed))

## FIX 1 verification: predicted volumes should be on a plausible water-volume
## scale (mL), NOT log-scale. A log-scale value for max_water would be ~0–10;
## a real water volume would be ~10–5000 mL depending on species.
imputed_rows <- bromeliads_vol_imputed %>%
  filter(max_water_estimated == "estimated")
cat("Bromeliads with imputed volume:", nrow(imputed_rows), "\n")
cat("Imputed max_water_combined range (should be plausible mL, not log-scale):\n")
print(summary(imputed_rows$max_water_combined))
## If you see values like 3.5, 4.2, 6.1 — the exp() back-transform is missing.
## Plausible values are typically 50–3000 mL for tank bromeliads.

cat("Observed max_water_combined range:\n")
print(summary(bromeliads_vol_imputed %>%
              filter(max_water_estimated == "observed") %>%
              pull(max_water_combined)))

## ── 7d. 24_volume hand-curated values merged ─────────────────────────────────

make_one("bromeliads_vol_final")
tar_load(bromeliads_vol_final, store = STORE)

cat("bromeliads_vol_final: rows =", nrow(bromeliads_vol_final), "\n")
cat("max_water_estimated breakdown:\n")
print(table(bromeliads_vol_final$max_water_estimated, useNA = "always"))

## Row count should equal bromeliads_with_detritus
stopifnot(nrow(bromeliads_vol_final) == nrow(bromeliads_with_detritus))

## How many bromeliads still have NA max_water_combined after all imputation?
na_vol <- sum(is.na(bromeliads_vol_final$max_water_combined))
cat("Bromeliads with NA max_water_combined after imputation:", na_vol, "\n")

## DOCUMENT — Section 7
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ supp_data_renamed:                                                      │
## │   Columns present (especially diameter, num_leaf, max_water):          │
## │   Vriesea species spelling confirmed correct? Y/N                      │
## │                                                                         │
## │ Volume model fits:                                                      │
## │   v1 (Aechmaea_mertensii): R² =          n_src =                       │
## │   v2 (Aechmaea_aquilega):  R² =          n_src =                       │
## │                                                                         │
## │ bromeliads_vol_imputed:                                                 │
## │   Imputed bromeliads:                                                   │
## │   Imputed max_water_combined range (plausible mL scale? Y/N):          │
## │   If N: exp() back-transform may still be missing — re-check FIX 1    │
## │                                                                         │
## │ bromeliads_vol_final:                                                   │
## │   Rows (= bromeliads_with_detritus rows?):                             │
## │   Bromeliads still with NA max_water_combined:                         │
## │   max_water_estimated breakdown:                                        │
## └─────────────────────────────────────────────────────────────────────────┘


## ════════════════════════════════════════════════════════════════════════════
## SECTION 8 — Final output list
## Target: imputed_data
## ════════════════════════════════════════════════════════════════════════════

make_one("imputed_data")
tar_load(imputed_data, store = STORE)

## Expected: named list with elements datasets, visits, traits, bromeliads,
## abundance, synonymous_names, abundance_matrix, detritus_validation.
cat("imputed_data elements:\n"); print(names(imputed_data))
stopifnot(all(c("datasets", "visits", "traits", "bromeliads",
                "abundance", "synonymous_names", "abundance_matrix",
                "detritus_validation") %in% names(imputed_data)))

## Check bromeliads is the final processed table
cat("bromeliads: rows =", nrow(imputed_data$bromeliads),
    "  cols =", ncol(imputed_data$bromeliads), "\n")

## Check detritus_validation is the long validation table
cat("detritus_validation: rows =", nrow(imputed_data$detritus_validation),
    "  cols:", paste(names(imputed_data$detritus_validation), collapse = ", "), "\n")

## Final cross-check: every bromeliad_id in detritus_summary should appear
## in the final bromeliads table
missing_ids <- setdiff(detritus_summary$bromeliad_id,
                       imputed_data$bromeliads$bromeliad_id)
if (length(missing_ids) > 0) {
  warning(length(missing_ids), " bromeliad IDs in detritus_summary missing from final table")
}

## DOCUMENT — Section 8
## ┌─────────────────────────────────────────────────────────────────────────┐
## │ imputed_data:                                                           │
## │   All expected list elements present? Y/N                              │
## │   bromeliads rows:                  cols:                               │
## │   detritus_validation rows:                                             │
## │   bromeliad_id cross-check passed? Y/N                                 │
## │                                                                         │
## │ OVERALL PIPELINE NOTES:                                                 │
## │   Date validated:                                                       │
## │   Validated by:                                                         │
## │   Outstanding issues:                                                   │
## └─────────────────────────────────────────────────────────────────────────┘
