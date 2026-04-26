## 03_imputed_data_new.R
## Imputation pipeline: fills in missing detritus size-class masses and water
## volumes using cross-site GLMs and supplementary field data.
##
## Design principle — three phases:
##
##   WIDE PHASE   (Sections 1–4)
##     Data stays wide (one column per detritus size class) because the GLMs
##     require both predictor and response to be columns on the same row.
##     Derived combination columns are added here as model predictors.
##
##   SINGLE PIVOT (Section 5)
##     Observed detritus pivots to long format once.
##     Model predictions are already returned in long format.
##     The two are bound together; coalesce() resolves any overlap.
##     String parsing of column names happens here and nowhere else.
##
##   LONG PHASE   (Sections 5–6)
##     All downstream work (consecutiveness check, total detritus summary)
##     operates on tidy long data with explicit size_min / size_max columns.
##
##   VOLUME IMPUTATION (Section 7)
##     Handled separately: max_water is a scalar per bromeliad, not a
##     size-class variable, so it does not enter the long detritus table.
##     It is joined onto the final bromeliad table independently.
##
## Reads from:  store_process_data  (observed_data from 02_observed_data.R)
## Terminal output: imputed_data list

library(targets)
library(tarchetypes)
library(bwgdata)
tar_option_set(
  packages = c(
    "bwgdata", "dplyr", "purrr", "readr",
    "tidyr", "stringr", "assertr", "assertthat",
    "broom", "modelr"
  )
)

tar_source(files = "Rfunctions/03_imputed_data_functions_new.R")

## Read terminal outputs from the observed-data pipeline
observed_data <- tar_read(observed_data, store = "store_observed_data")

list(

  ## ===========================================================================
  ## SECTION 1: File inputs
  ## ===========================================================================

  tar_target(
    name = fpom_fg_csv,
    command = "data_files_additional/FPOMdecanted_dryweight.csv",
    format = "file"
  ),
  tar_target(
    name = fpom_fg_data,
    command = read_fpom_fg(fpom_fg_csv)
  ),

  tar_target(
    name = volume_estimated_csv,
    command = "data_files_additional/24_volume.csv",
    format = "file"
  ),
  tar_target(
    name = volume_estimated,
    command = read_volume_estimated(volume_estimated_csv)
  ),

  tar_target(name = aquilega_biog,  command = read_size_aquilega("data-intermediate/size_aquilega_Biog.csv")),
  tar_target(name = aquilegaKT,     command = read_size_aquilega("data-intermediate/size_aquilegaKT.csv")),
  tar_target(name = guzmania,       command = read_size_Guzmania_mertensii("data-intermediate/size_Guzmania_PR.csv")),
  tar_target(name = mertensii,      command = read_size_Guzmania_mertensii("data-intermediate/size_mertensii.csv")),
  tar_target(name = vriesea,        command = read_size_vriesea("data-intermediate/size_vriesea.csv")),
  tar_target(name = vriesea_prod,   command = read_size_vriesea("data-intermediate/size_vriesea_productive.csv")),


  ## ===========================================================================
  ## SECTION 2: FPOM mass-from-volume model (currently disabled)
  ## ===========================================================================
  ## The French Guiana sites recorded fine detritus as a volume (ml decanted).
  ## The intention is to predict dry mass (g) from this volume using a model
  ## fit to an external dataset where both were measured.
  ## The prediction step is currently bypassed (add_predictions_to_data returns
  ## the input unchanged). Kept here so the model object is available when
  ## the prediction is re-enabled.

  tar_target(
    name = model_fpom_g_ml,
    command = fit_fpom_g_ml(fpom_fg_data)
  ),

  ## ===========================================================================
  ## SECTION 3: Wide phase — add derived predictor columns
  ## ===========================================================================
  ## The observed bromeliads table from 02_observed_data.R is the entry point.
  ## It contains one column per original detritus size class, and is already
  ## fully cleaned and corrected.
  ##
  ## add_new_columns_for_prediction() creates composite columns (e.g.
  ## detritus850_20000_sum = detritus1500_20000 + detritus850_1500) that serve
  ## as predictors in the cross-site GLMs. These columns are NOT measurements —
  ## they should never appear in the final long data.

  tar_target(
    name = bromeliads_wide,
    command = observed_data$bromeliads
  ),

  tar_target(
    name = bromeliads_wide_pred_cols,
    command = add_new_columns_for_prediction(bromeliads_wide)
  ),


  ## ===========================================================================
  ## SECTION 4: Detritus size-class imputation via cross-site GLMs
  ## ===========================================================================
  ## Each row of model_table specifies:
  ##   src_dat   — dataset(s) where both predictor and response are observed;
  ##               used to fit the GLM
  ##   target_dat — dataset(s) where the response is missing; the fitted model
  ##               is applied here to produce imputed values
  ##   xvar / yvar — GLM formula terms (log-log, gaussian)
  ##   yvar_min / yvar_max — the size range (µm) of the predicted variable;
  ##               used to label long-format output without parsing column names

  tar_target(
    name = model_table,
    command = create_model_table()
  ),

  ## Attach source data and parse formula metadata for each model row
  tar_target(
    name = modelling_information,
    command = derive_modelling_information(model_table, bromeliads_wide_pred_cols)
  ),

  ## Fit one GLM per model row against its source data
  tar_target(
    name = model_fits,
    command = do_fit_predictive_model(modelling_information)
  ),

  ## Apply each fitted model to its target sites.
  ## Returns long-format predictions: bromeliad_id | size_min | size_max |
  ##   detritus_g | source (= model m_id)
  tar_target(
    name = detritus_imputed_long,
    command = predict_detritus_long(
      model_fits,
      modelling_information,
      bromeliads_wide_pred_cols
    )
  ),

  ## Apply each fitted model to its SOURCE sites (where the true value is
  ## known). Returns a table suitable for computing residuals and R².
  ## This is a byproduct of the same prediction step at no extra modelling cost.
  tar_target(
    name = detritus_validation_long,
    command = validate_detritus_long(
      model_fits,
      modelling_information,
      bromeliads_wide_pred_cols
    )
  ),


  ## ===========================================================================
  ## SECTION 5: Single pivot — combine observed and imputed in long format
  ## ===========================================================================
  ## This is the only place where detritus column names are parsed into
  ## size_min / size_max. Everything downstream works with explicit numeric
  ## columns and never inspects column names.

  ## Pivot the original (pre-predictor-column) bromeliads to long format.
  ## Excludes derived combination columns by pivoting before they are added.
  tar_target(
    name = detritus_observed_long,
    command = pivot_observed_detritus_long(bromeliads_wide)
  ),

  ## Bind observed and imputed rows.
  ## coalesce(observed, imputed) picks observed where present, imputed where not.
  ## In practice observed and imputed rows come from different sites by design,
  ## so coalesce is a safety net rather than an active resolver.
  tar_target(
    name = detritus_long,
    command = combine_observed_imputed(detritus_observed_long, detritus_imputed_long)
  ),

  ## Within each bromeliad, check that the size classes reported cover a
  ## consecutive range (no gaps, no overlaps). Flag rather than error so that
  ## problematic bromeliads can be inspected.
  tar_target(
    name = detritus_long_checked,
    command = check_detritus_consecutive(detritus_long)
  ),

  ## Summarise to one row per bromeliad: total detritus mass, covered range,
  ## and provenance (observed / imputed / mixed).
  tar_target(
    name = detritus_summary,
    command = summarise_detritus(detritus_long_checked)
  ),


  ## ===========================================================================
  ## SECTION 6: Final bromeliad table — join detritus summary
  ## ===========================================================================
  ## Attach total_detritus and provenance metadata to the wide bromeliad table.
  ## The wide table is still needed for the final output (users expect columns,
  ## not rows, for detritus fractions).

  tar_target(
    name = bromeliads_with_detritus,
    command = left_join(
      bromeliads_wide |>
        select(-starts_with("detritus")),   # drop raw size-class columns
      detritus_summary,
      by = "bromeliad_id"
    )
  ),


  ## ===========================================================================
  ## SECTION 7: Water volume imputation
  ## ===========================================================================
  ## Some visits have no max_water measurements. Two complementary strategies:
  ##
  ##   (a) GLMs fit to supplementary external datasets of bromeliad morphometry:
  ##       diameter (and leaf number) → max_water. Species-specific models.
  ##
  ##   (b) A hand-curated CSV (24_volume.csv) of volume estimates that predate
  ##       or fall outside this pipeline; merged in after (a).
  ##
  ## max_water is a scalar per bromeliad, not a size-class variable, so it is
  ## handled entirely in wide format and joined onto the bromeliad table.

  tar_target(
    name = supplementary_size_data,
    command = bind_rows(
      aquilega_biog = aquilega_biog,
      aquilegaKT    = aquilegaKT,
      guzmania      = guzmania,
      mertensii     = mertensii,
      vriesea       = vriesea,
      vriesea_prod  = vriesea_prod,
      .id = I("filename")
    )
  ),

  tar_target(
    name = supp_data_additional,
    command = add_more_info_to_supp(supplementary_size_data)
  ),

  tar_target(
    name = supp_data_renamed,
    command = supp_data_rename(supp_data_additional)
  ),

  tar_target(
    name = supp_size_models,
    command = make_model_data()
  ),

  tar_target(
    name = supp_size_model_info,
    command = derive_modelling_information_simpler(supp_size_models, supp_data_renamed)
  ),

  tar_target(
    name = supp_size_model_data,
    command = make_model_target_data()
  ),

  tar_target(
    name = supp_size_model_fits,
    command = fit_size_models_to_data(supp_size_model_info, supp_size_model_data)
  ),

  ## Apply volume models to target visits (those with missing max_water)
  tar_target(
    name = bromeliads_vol_imputed,
    command = predict_add_imputed(supp_size_model_fits, bromeliads_with_detritus)
  ),

  ## Merge in hand-curated volume estimates for any remaining gaps
  tar_target(
    name = bromeliads_vol_final,
    command = add_24_volum_data(volume_estimated, bromeliads_vol_imputed)
  ),


  ## ===========================================================================
  ## SECTION 8: Final output
  ## ===========================================================================

  tar_target(
    name = imputed_data,
    command = list(
      datasets         = observed_data$datasets,
      visits           = observed_data$visits,
      traits           = observed_data$traits,
      bromeliads       = bromeliads_vol_final,
      abundance        = observed_data$abundance,
      synonymous_names = observed_data$synonymous_names,
      abundance_matrix = observed_data$abundance_matrix,
      ## validation table kept as a named element so authors can inspect model
      ## performance without re-running the pipeline
      detritus_validation = detritus_validation_long
    )
  )

)
