## 03_imputed_data_functions_new.R
## Functions for the imputation pipeline (03_imputed_data_new.R).
## Sections mirror the section structure of that file.
##
## Key changes from the previous version:
##
##   create_model_table()         — adds yvar_min / yvar_max columns so
##                                  predictions carry their size range
##                                  explicitly, without parsing column names.
##
##   predict_detritus_long()      — replaces estimate_missing_detritus_new_site()
##                                  Returns long-format imputed values directly.
##
##   validate_detritus_long()     — new. Applies models to source sites for
##                                  validation; returns long format with both
##                                  observed and predicted columns side-by-side.
##
##   pivot_observed_detritus_long() — new. The single location where detritus
##                                  column names are parsed into numeric
##                                  size_min / size_max. Called once.
##
##   combine_observed_imputed()   — new. Binds the two long tables and uses
##                                  coalesce() to resolve any overlap.
##
##   check_detritus_consecutive() — replaces add_consecutive_detritus_col() +
##                                  extract_numeric_min_max(). Simplified now
##                                  that size_min / size_max are already numeric.
##
##   summarise_detritus()         — replaces create_detritus_summary(). Same
##                                  logic, updated column names.
##
## Removed (no longer needed):
##   combine_detritus_predictions(), combine_all_detritus_values(),
##   filter_just_orig_fitted(), split_detritus_categories(),
##   extract_numeric_min_max(), add_consecutive_detritus_col(),
##   add_detritus_summary()
##
## ── Bug fixes applied (2025-04) ──────────────────────────────────────────────
##
##   FIX 1  predict_add_imputed()        — added exp() back-transform on
##                                         log-scale GLM volume predictions
##                                         before coalescing with observed values.
##
##   FIX 2  do_fit_predictive_model()    — replaced dplyr::if_else() (which
##                                         enforces strict type matching) with
##                                         base-R if/else for list-column model
##                                         objects.
##
##   FIX 3  do_fit_predictive_model()    — changed purrr:::safely (internal API)
##                                         to purrr::safely (exported function).
##
##   FIX 4  add_24_volum_data()          — corrected coalesce() argument order:
##                                         hand-curated 24_volume values now take
##                                         priority over GLM estimates, consistent
##                                         with the old version's if_else logic.
##
##   FIX 5  predict_detritus_long(),
##          validate_detritus_long()     — changed y_funs == "log" to
##                                         "log" %in% y_funs to guard against
##                                         multi-element y_funs vectors.
##
##   FIX 6  predict_detritus_long()      — added explicit NA guard so a failed
##                                         model fit produces a clear error
##                                         message rather than a cryptic one from
##                                         predict().
##
##   FIX 7  more_information()           — corrected "Vrisea_splendens" to
##                                         "Vriesea_splendens". NOTE: verify that
##                                         this spelling matches the species
##                                         column in supp_data_renamed.
##
##   FIX 8  supp_data_rename()           — replaced unconditional select(-Diam2)
##                                         with select(-any_of("Diam2")) and
##                                         used rename(any_of(...)) so that
##                                         datasets missing those columns do not
##                                         error.


## SECTION 1: File inputs ----------------------------------------------------

check_brom_data <- function(d) {
  return(d)
}

read_size_aquilega <- function(filenm) {
  read_delim(
    filenm, delim = ";",
    show_col_types = FALSE,
    col_types = cols(
      ID    = col_character(),
      Diam1 = col_double(),
      NL    = col_double(),
      Vmax  = col_double()
    )
  ) %>%
    rename(plant_id = ID) %>%
    mutate(plant_id = str_trim(plant_id))  # aquilegaKT has trailing spaces
}

read_size_Guzmania_mertensii <- function(filenm) {
  read_delim(
    filenm, delim = ";",
    show_col_types = FALSE,
    col_types = cols(
      plante = col_character(),  # double in guzmania, chr in mertensii — read as chr
      Diam1  = col_double(),
      Diam2  = col_double(),     # absent in guzmania; readr silently skips missing cols
      NL     = col_double(),
      Vmax   = col_double()
    )
  ) %>%
    rename(plant_id = plante)
}

read_size_vriesea <- function(filenm) {
  read_delim(
    filenm, delim = ";",
    show_col_types = FALSE,
    col_types = cols(
      BromeliadID = col_character(),  # double in vriesea, chr in vriesea_prod
      Diam1       = col_double(),
      NL          = col_double(),
      Vmax        = col_double()
    )
  ) %>%
    rename(plant_id = BromeliadID)
}

#' Read the external French Guiana FPOM dataset (ml decanted vs dry weight g).
#' Used by fit_fpom_g_ml() to build the mass-from-volume prediction model.
read_fpom_fg <- function(path) {
  path %>%
    read_csv(col_types = cols(
      `FPOM (ml decanted)` = col_double(),
      `FPOM (mm3)`         = col_integer(),
      `dry weight (g)`     = col_double(),
      `dry weight (mg)`    = col_integer(),
      .default             = col_skip()    # skip everything else
    )) %>%
    rename(fpom_ml = `FPOM (ml decanted)`,
           fpom_g  = `dry weight (g)`) %>%
    filter(!is.na(fpom_ml))
}

#' Read the hand-curated volume estimates CSV (24_volume.csv).
#' These are merged in after the GLM-based volume imputation as a final gap-fill.
read_volume_estimated <- function(fname) {
  read_csv(fname, col_types = cols(
    bromeliad_id = col_character(),
    max_water    = col_double(),
    .default     = col_skip
  ))
}


## SECTION 2: FPOM mass-from-volume model (currently disabled) ---------------

#' Fit a quadratic model to predict FPOM dry mass (g) from decanted volume (ml).
#' The model form matches the original Excel analysis by Regis et al.
#' Currently the prediction step is bypassed; this function is retained so the
#' model object is available when the step is re-enabled.
fit_fpom_g_ml <- function(fpom_fg_data) {
  lm(fpom_g ~ I(fpom_ml^2) + fpom_ml, data = fpom_fg_data)
}

#' Extract the name of the response variable from a fitted model object.
get_response_from_model <- function(mod) {
  mod$model %>%
    attr("terms") %>%
    formula() %>%
    all.vars(.) %>%
    .[1]
}

#' Add model predictions to a data frame.
#' Currently returns .df unchanged (prediction bypassed via if(TRUE) guard).
#' To re-enable: remove the if(TRUE) block and uncomment the prediction code.
add_predictions_to_data <- function(.df, .model) {
  if (TRUE) {
    warning("skipping FPOM detritus prediction — returning input unchanged")
    return(.df)
  }
  respname <- get_response_from_model(.model)
  model_predictions <- broom::augment(x = .model)
  list(
    model_predictions    = model_predictions,
    data_with_prediction = left_join(.df, model_predictions, by = join_by(bromeliad_id))
  )
}


## SECTION 3: Wide phase — add derived predictor columns ---------------------

#' Add composite detritus columns used as GLM predictors.
#'
#' Some sites measured detritus in non-standard size fractions that do not
#' align with the canonical bins used in most sites. These derived columns
#' combine adjacent bins so that a single predictor spans the right range for
#' a given cross-site model. They are not measurements and must not appear in
#' the final long data.
add_new_columns_for_prediction <- function(.detritus_data) {

  message("changing detritus20000_NA column type to double")
  if (is.character(.detritus_data$detritus20000_NA)) {
    .detritus_data$detritus20000_NA <- readr::parse_double(.detritus_data$detritus20000_NA)
  }

  .detritus_data %>%
    mutate(detritus10_1500_2000_NA = detritus10_1500 + detritus1500_20000 + detritus20000_NA) %>%
    mutate(detritus_over_150       = detritus0_150   + detritus150_20000  + detritus20000_NA) %>%
    mutate(detritus850_20000_sum   = if_else(is.na(detritus850_20000),
                                             true  = detritus1500_20000 + detritus850_1500,
                                             false = detritus850_20000)) %>%
    mutate(detritus0_NA_sum    = detritus0_150 + detritus150_850 + detritus850_20000_sum + detritus20000_NA) %>%
    mutate(detritus150_NA_sum  = detritus150_850 + detritus850_20000_sum + detritus20000_NA) %>%
    mutate(detritus0_20000_sum = detritus0_150 + detritus150_850 + detritus850_20000_sum,
           detritus20000_NA_na0 = if_else(detritus20000_NA < 0.001,
                                          true  = NA_real_,
                                          false = detritus20000_NA)) %>%
    mutate(detritus150_1500 = detritus150_850 + detritus850_1500,
           detritus150_1500_plus = if_else(is.na(detritus150_1500),
                                           true  = detritus150_NA,
                                           false = detritus150_1500)) %>%
    mutate(detritus150_NA_sum_Japi = detritus150_850 + detritus850_1500 + detritus1500_20000 + detritus20000_NA,
           detritus150_NA_sum_Japi = if_else(is.na(detritus150_NA_sum_Japi),
                                             true  = detritus125_NA,
                                             false = detritus150_NA_sum_Japi))
}


## SECTION 4: Detritus size-class imputation via cross-site GLMs -------------

#' Find variable names and any transformation functions used in a formula.
#' Used by derive_modelling_information() to record axis back-transforms
#' for plotting.
find_symbols <- function(expr) {
  functions <- variables <- character(0)
  f <- function(e) {
    if (!is.recursive(e)) {
      if (!is.symbol(e)) return()
      variables <<- c(variables, deparse(e))
    } else {
      functions <<- c(functions, deparse(e[[1]]))
      for (a in as.list(e[-1])) {
        if (!missing(a)) f(a)
      }
    }
  }
  if (inherits(expr, "formula")) expr <- expr[[length(expr)]]
  f(expr)
  list(functions = unique(functions),
       variables = unique(variables))
}

#' Specification table for the cross-site detritus imputation GLMs.
#'
#' Each row describes one model:
#'   m_id       — unique model identifier
#'   target_dat — dataset IDs where the response variable is missing; the
#'                fitted model is applied here to produce imputed values
#'   src_dat    — dataset IDs where both predictor and response are observed;
#'                used to fit the model
#'   xvar / yvar — one-sided formula strings for the GLM
#'   yvar_min / yvar_max — size range (µm) of the predicted variable.
#'                These are stored explicitly so that predicted values can be
#'                labelled with their size range in the long output without
#'                parsing the column name string.
#'   .f / family — model function and family (all currently glm / gaussian)
#'
#' NOTE on yvar_min / yvar_max for derived composite variables:
#'   Some response variables (e.g. detritus10_1500_2000_NA) are sums of
#'   adjacent bins. yvar_min and yvar_max record the outer bounds of that
#'   composite range (e.g. 10, Inf), not the individual bin boundaries.
create_model_table <- function(){
  tribble(
    ~m_id,  ~target_dat,           ~src_dat,                       ~xvar,                           ~yvar,                            ~yvar_min, ~yvar_max, ~.f, ~family,
    "m01",  "116",                 c("131","126","121","221"),      "~log(diameter)",                "~log(detritus10_1500_2000_NA)",   10,        Inf,       glm, "gaussian",
    "m02",  c("186","216"),        c("211"),                        "~log(detritus0_150)",           "~log(detritus150_20000)",         150,       20000,     glm, "gaussian",
    # "m03",  c("186","216"),        c("211"),                        "~log(detritus0_150)",           "~log(detritus20000_NA)",          20000,     Inf,       glm, "gaussian",
    "m04",  c("201"),              c("211"),                        "~log(detritus0_150)",           "~log(detritus_over_150)",         0,         Inf,       glm, "gaussian",
    "m05",  c("71","51","61"),     c("56"),                         "~log(detritus850_20000_sum)",   "~log(detritus0_150)",             0,         150,       glm, "gaussian",
    "m06",  c("71","51"),          c("61"),                         "~log(detritus850_20000_sum)",   "~log(detritus20000_NA)",          20000,     Inf,       glm, "gaussian",
    "m07",  c("66"),               c("56"),                         "~diameter",                     "~detritus0_NA_sum",               0,         Inf,       glm, "gaussian",
    "m08",  c("76","81","91"),     c("56"),                         "~detritus150_NA_sum",           "~detritus0_150",                  0,         150,       glm, "gaussian",
    "m09",  c("86"),               c("91"),                         "~num_leaf",                     "~detritus150_NA",                 150,       Inf,       glm, "gaussian",
    "m10",  c("101","106"),        c("56"),                         "~log(detritus0_20000_sum)",     "~log(detritus20000_NA_na0)",      20000,     Inf,       glm, "gaussian",
    "m11",  c("146"),              c("56"),                         "~log(detritus150_1500_plus)",   "~log(detritus0_150)",             0,         150,       glm, "gaussian",
    "m12",  c("161"),              c("56"),                         "~log(detritus150_NA_sum_Japi)", "~log(detritus0_150)",             0,         150,       glm, "gaussian"
  ) %>%
    rowwise() |>
    mutate(
      fml_string = paste0(
        stringr::str_replace(yvar, "~", ""),
        xvar),
      xvar = list(as.formula(xvar)),
      yvar = list(as.formula(yvar)),
      fml  = list(as.formula(fml_string))
    )
}

#' Attach source data and formula metadata to each model row.
#'
#' Enriches the model table with:
#'   src_df  — the rows of .detritus_data whose dataset_id is in src_dat
#'   x_vars / y_vars — variable names extracted from the formula
#'   x_funs / y_funs — transformation functions (e.g. "log") used in formula;
#'                     needed for back-transformation when plotting
derive_modelling_information <- function(.model_table, .detritus_data){
  model_info <- .model_table %>%
    rowwise() |>
    mutate(
      src_df = list(
        .detritus_data %>%
          filter(dataset_id %in% src_dat)
      )
    ) |>
    mutate(x_symb = list(find_symbols(xvar)),
           y_symb = list(find_symbols(yvar)),
           x_funs = list(x_symb$functions),
           x_vars = x_symb$variables,
           y_funs = list(y_symb$functions),
           y_vars = y_symb$variables)

  nrows_src_df <- map_dbl(model_info$src_df, nrow)
  if (any(nrows_src_df == 0)) stop("there are 0 rows in some input data for the imputation models")

  return(model_info)
}

#' Fit one GLM per row of the modelling information table.
#'
#' Uses purrr::safely() so that a model failure on one row does not abort the
#' entire pipeline. Failed fits produce NA in predicting_model and can be
#' inspected in the returned table.
#'
#' FIX 2: replaced dplyr::if_else() with base-R if/else. dplyr::if_else()
#'   enforces strict type equality on both branches; list(NA) and list(<glm>)
#'   are incompatible types and will error. In a rowwise() context, base-R
#'   if/else is the correct tool.
#'
#' FIX 3: changed purrr:::safely (internal, unexported) to purrr::safely
#'   (exported, stable public API).
do_fit_predictive_model <- function(.modelling_information){
  .modelling_information %>%
    select(m_id, src_df, fml_string, family, target_dat, yvar_min, yvar_max) |>
    mutate(
      safe_model = list(
        purrr::safely(glm)(as.formula(fml_string), family = family, data = src_df)  # FIX 3
      ),
      ## FIX 2: base-R if/else — dplyr::if_else() would error on type mismatch
      ## between list(NA) and list(<glm object>).
      predicting_model = if (is.null(safe_model$result)) list(NA) else list(safe_model$result)
    )
}

#' Apply each fitted model to its TARGET sites and return long-format predictions.
#'
#' For each model row, filters .detritus_data to target_dat sites, generates
#' predictions, back-transforms where necessary, and immediately pivots to long
#' format. yvar_min and yvar_max (from the model table) label the predicted
#' size class explicitly — no column-name parsing required.
#'
#' Returns a tidy tibble with one row per bromeliad × imputed size class:
#'   bromeliad_id | dataset_id | size_min | size_max | detritus_g | source
#' where source is the m_id string (e.g. "model_m05").
#'
#' FIX 5: changed y_funs == "log" to "log" %in% y_funs. If find_symbols()
#'   ever returns a multi-element y_funs vector, == would produce a vector
#'   condition and error; %in% always returns a scalar logical.
#'
#' FIX 6: added explicit guard when predicting_model is NA (i.e., the GLM
#'   fit failed). Without this guard, predict(NA, ...) produces a cryptic
#'   error that does not identify which model failed.
predict_detritus_long <- function(.model_fits, .modelling_information, .detritus_data) {

  model_info <- .modelling_information %>%
    select(m_id, y_vars, y_funs, yvar_min, yvar_max)

  .model_fits %>%
    left_join(model_info, by = "m_id") %>%
    rowwise() %>%
    mutate(
      ## FIX 6: fail clearly if the model did not fit
      pred_raw = list({
        if (is.na(predicting_model[[1]])) {
          stop("Model fit failed for m_id = ", m_id,
               " — inspect model_fits$safe_model for the error message.")
        }
        target_df <- .detritus_data %>% filter(dataset_id %in% target_dat)
        predict(predicting_model, newdata = target_df)
      }),
      target_df = list(
        .detritus_data %>%
          filter(dataset_id %in% target_dat)
      ),
      ## FIX 5: "log" %in% y_funs instead of y_funs == "log"
      pred_bt  = list(
        if ("log" %in% y_funs) exp(pred_raw) else pred_raw
      ),
      ## assemble one long row per bromeliad with size range and provenance
      pred_long = list(
        target_df %>%
          select(bromeliad_id, dataset_id) %>%
          mutate(
            size_min    = yvar_min,
            size_max    = yvar_max,
            detritus_g  = as.numeric(pred_bt),
            source      = paste0("model_", m_id)
          )
      )
    ) %>%
    pull(pred_long) %>%
    bind_rows()
}

#' Apply each fitted model to its SOURCE sites and return long-format results
#' for model validation.
#'
#' Source sites have known observed values, so the returned table contains both
#' detritus_observed and detritus_predicted columns, enabling direct comparison.
#' This table is stored as imputed_data$detritus_validation and is not used in
#' building the final bromeliads table.
#'
#' FIX 5: changed y_funs == "log" to "log" %in% y_funs (same rationale as
#'   predict_detritus_long() above).
validate_detritus_long <- function(.model_fits, .modelling_information, .detritus_data) {

  model_info <- .modelling_information %>%
    select(m_id, src_df, y_vars, y_funs, yvar_min, yvar_max)

  .model_fits %>%
    left_join(model_info, by = "m_id") %>%
    rowwise() %>%
    mutate(
      pred_raw = list(
        predict(predicting_model, newdata = src_df)
      ),
      ## FIX 5: "log" %in% y_funs instead of y_funs == "log"
      pred_bt = list(
        if ("log" %in% y_funs) exp(pred_raw) else pred_raw
      ),
      val_long = list(
        src_df %>%
          select(bromeliad_id, dataset_id, detritus_observed = !!sym(y_vars)) %>%
          mutate(
            size_min            = yvar_min,
            size_max            = yvar_max,
            detritus_predicted  = as.numeric(pred_bt),
            m_id                = m_id
          )
      )
    ) %>%
    pull(val_long) %>%
    bind_rows()
}


## SECTION 5: Single pivot — observed detritus to long format ----------------

#' Parse a single detritus column name into its lower and upper size bounds.
#'
#' Column names follow the pattern "detritusLOW_HIGH" where HIGH may be "NA"
#' to indicate an open upper bound (coded as Inf). For example:
#'   "detritus0_150"     → size_min = 0,     size_max = 150
#'   "detritus20000_NA"  → size_min = 20000, size_max = Inf
#'
#' This function is called once, inside pivot_observed_detritus_long(). It is
#' the only place in the pipeline where column names are parsed as strings.
parse_detritus_colname <- function(col_name) {
  suffix <- stringr::str_remove(col_name, "^detritus")
  parts  <- stringr::str_split(suffix, "_")[[1]]
  ## guard: only handle two-part names (original measurements)
  ## derived columns like detritus850_20000_sum have 3+ parts and are excluded
  ## by the caller
  size_min <- as.numeric(parts[1])
  size_max <- if (parts[2] == "NA") Inf else as.numeric(parts[2])
  list(size_min = size_min, size_max = size_max)
}

#' Pivot the wide bromeliads table to long-format observed detritus.
#'
#' This is the SINGLE PIVOT POINT in the pipeline. Only original measurement
#' columns (those whose name has exactly two numeric-or-NA components after the
#' "detritus" prefix) are pivoted. Derived combination columns created by
#' add_new_columns_for_prediction() are excluded because this function is called
#' on bromeliads_wide, which is the observed data BEFORE those columns are added.
#'
#' Returns one row per bromeliad × size class, with NA rows dropped:
#'   bromeliad_id | dataset_id | size_min | size_max | detritus_g | source
pivot_observed_detritus_long <- function(.bromeliads_wide) {

  ## identify original detritus columns (two-part names only)
  is_original_detritus <- function(nm) {
    if (!stringr::str_starts(nm, "detritus")) return(FALSE)
    suffix <- stringr::str_remove(nm, "^detritus")
    parts  <- stringr::str_split(suffix, "_")[[1]]
    length(parts) == 2
  }

  det_cols <- names(.bromeliads_wide)[map_lgl(names(.bromeliads_wide), is_original_detritus)]

  .bromeliads_wide %>%
    select(bromeliad_id, dataset_id, all_of(det_cols)) %>%
    pivot_longer(
      cols      = all_of(det_cols),
      names_to  = "detritus_col",
      values_to = "detritus_g"
    ) %>%
    filter(!is.na(detritus_g)) %>%
    ## parse size range from column name — this is the only call site
    mutate(
      bounds   = map(detritus_col, parse_detritus_colname),
      size_min = map_dbl(bounds, "size_min"),
      size_max = map_dbl(bounds, "size_max"),
      source   = "observed"
    ) %>%
    select(bromeliad_id, dataset_id, size_min, size_max, detritus_g, source)
}

#' Combine observed and imputed detritus into a single long table.
#'
#' Binds the two tables and then applies coalesce() within each
#' bromeliad × size_min × size_max group: observed takes priority; imputed
#' fills gaps. In a well-formed pipeline the two sources should cover disjoint
#' sets of bromeliads, so coalesce is a safety net rather than an active
#' resolver. The source column records provenance: "observed", "model_mXX",
#' or "observed+model_mXX" where both exist (the last case signals something
#' unexpected and worth investigating).
combine_observed_imputed <- function(.observed_long, .imputed_long) {

  bound <- bind_rows(.observed_long, .imputed_long)

  bound %>%
    group_by(bromeliad_id, dataset_id, size_min, size_max) %>%
    summarise(
      detritus_g = coalesce(
        ## first non-NA wins; observed rows sort before imputed rows
        detritus_g[source == "observed"][1],
        detritus_g[source != "observed"][1]
      ),
      source = paste0(unique(source), collapse = "+"),
      .groups = "drop"
    )
}

#' Check that size classes within each bromeliad form a consecutive range.
#'
#' Within a bromeliad, after sorting by size_min, each interval should begin
#' where the previous one ended: size_min[i+1] == size_max[i]. The function
#' adds an is_consecutive flag per bromeliad rather than erroring, so
#' problematic bromeliads can be inspected without aborting the pipeline.
#'
#' Note: this is a much simpler implementation than the previous version
#' because size_min and size_max are already numeric columns — no string
#' parsing is required.
check_detritus_consecutive <- function(.detritus_long) {
  .detritus_long %>%
    group_by(bromeliad_id) %>%
    arrange(size_min, .by_group = TRUE) %>%
    mutate(
      is_consecutive = size_min == lag(size_max, default = first(size_min))
    ) %>%
    ungroup()
}

#' Summarise long detritus to one row per bromeliad.
#'
#' Computes total detritus mass, the lower and upper bounds of the covered
#' size range, and a provenance string (e.g. "observed", "model_m05",
#' "observed+model_m05") that records whether imputation was used.
#'
#' The is_consecutive flag from check_detritus_consecutive() is summarised as
#' all_consecutive: TRUE only if every interval within the bromeliad is
#' consecutive. Bromeliads where this is FALSE may have gaps or overlaps in
#' their size-class coverage and warrant inspection.
summarise_detritus <- function(.detritus_long_checked) {
  .detritus_long_checked %>%
    group_by(bromeliad_id) %>%
    summarise(
      total_detritus   = sum(detritus_g, na.rm = TRUE),
      size_range_min   = min(size_min),
      size_range_max   = max(size_max),
      source           = paste0(unique(source), collapse = "+"),
      all_consecutive  = all(is_consecutive),
      .groups          = "drop"
    )
}


## SECTION 7: Water volume imputation ----------------------------------------
## These functions are unchanged from the previous version, except where noted.

#' Label each supplementary size dataset with its bromeliad species name.
#'
#' FIX 7: corrected "Vrisea_splendens" → "Vriesea_splendens".
#' NOTE: verify that this spelling matches the species column in the data
#' returned by supp_data_rename(), especially if the source CSV files use
#' the misspelled form. Run: unique(supp_data_renamed$species) to confirm.
more_information <- function() {
  tribble(
    ~ filename,       ~species,
    "aquilega_biog",  "Aechmaea_aquilega",
    "aquilegaKT",     "Aechmaea_aquilega",
    "guzmania",       "Guzmania_sp",
    "mertensii",      "Aechmaea_mertensii",
    "vriesea_prod",   "Vriesea_splendens",   # FIX 7: was "Vrisea_splendens"
    "vriesea",        "Vriesea_splendens"    # FIX 7: was "Vrisea_splendens"
  )
}

add_more_info_to_supp <- function(.all_size_data){
  .all_size_data %>%
    left_join(more_information())
}

#' Rename supplementary data columns to match the main bromeliads table.
#'
#' FIX 8: replaced rename(diameter = Diam1, ...) with rename(any_of(...)) and
#'   select(-Diam2) with select(-any_of("Diam2")). The three read_size_*
#'   functions use plain read_delim() with no column selection, so column names
#'   vary across files. The original code would error if any file lacked Diam2
#'   (or Diam1 / NL / Vmax). any_of() silently skips absent columns.
#'   NOTE: if a required column (diameter, num_leaf, max_water) ends up missing
#'   after the rename, the downstream volume models will fail with an
#'   uninformative error. Add an explicit check here if that happens.
supp_data_rename <- function(.supp_data_additional){
  .supp_data_additional %>%
    rename(any_of(c(           # FIX 8: any_of() — don't error on absent columns
      diameter  = "Diam1",
      num_leaf  = "NL",
      max_water = "Vmax"
    ))) %>%
    select(-any_of("Diam2"))   # FIX 8: any_of() — don't error if Diam2 absent
}

#' Specification table for the two volume-imputation models.
#' src_species: which species the model is trained on (from supplementary data)
#' target_dat:  visit IDs where max_water is missing and should be predicted
make_model_data <- function(){
  tribble(
    ~m_id, ~src_species,          ~formula_text,                                  ~.f, ~family,
    "v1",  "Aechmaea_mertensii",  "log(max_water)~log(diameter)",                  glm, "gaussian",
    "v2",  "Aechmaea_aquilega",   "log(max_water)~log(diameter) + log(num_leaf)",  glm, "gaussian"
  ) %>%
    rowwise() |>
    mutate(model_formula = list(as.formula(formula_text)))
}

#' Attach source data rows to each volume model.
derive_modelling_information_simpler <- function(.model_table, .obs_data){
  .model_table %>%
    rowwise() |>
    mutate(
      src_df = list(.obs_data %>%
                      filter(species %in% src_species))
    )
}

#' Table of target visit IDs for each volume model.
make_model_target_data <- function(){
  tribble(
    ~m_id, ~target_dat,
    "v1",  c("286"),
    "v2",  c("301", "296")
  )
}

#' Fit volume GLMs to their respective source data.
fit_size_models_to_data <- function(.mod_info, .supp_size_model_data){
  .mod_info %>%
    left_join(.supp_size_model_data) %>%
    rowwise() |>   # explicit rowwise() after left_join(), which may drop grouping
    mutate(model = list(.f(model_formula, family = family, data = src_df)))
}

#' Apply volume models to target visits and fill max_water for missing rows.
#'
#' Only bromeliads in target visits (those in make_model_target_data()) receive
#' predicted values. All others keep their observed max_water unchanged.
#' A stopifnot() guards against accidentally overwriting an observed value.
#'
#' FIX 1: added exp() back-transform on predicted_water.
#'   The volume models (v1, v2) are Gaussian GLMs fit on log(max_water).
#'   modelr::add_predictions() returns predictions on the response scale, which
#'   here is log(max_water). Without exp(), predicted_water is in log-units and
#'   coalesce(max_water, predicted_water) would silently mix incompatible scales.
predict_add_imputed <- function(.supp_size_model_fits, .bromeliad_detritus) {

  supp_size_model_ready_to_apply <- .supp_size_model_fits %>%
    unnest(target_dat) |>
    mutate(target_dat = readr::parse_integer(target_dat))

  size_data_to_impute <- .bromeliad_detritus %>%
    semi_join(supp_size_model_ready_to_apply, by = c("visit_id" = "target_dat")) %>%
    nest_by(visit_id)

  predicted_max_volume <- size_data_to_impute %>%
    left_join(supp_size_model_ready_to_apply, by = c("visit_id" = "target_dat")) %>%
    mutate(predicted_size = list(
      modelr::add_predictions(data, model, var = "predicted_water")
    ))

  visit_and_predictions <- predicted_max_volume %>%
    unnest(predicted_size) %>%
    ## FIX 1: back-transform from log scale to water-volume scale.
    ## Models are fit on log(max_water); predictions are therefore log-units.
    mutate(predicted_water = exp(predicted_water)) %>%
    select(visit_id, bromeliad_id, predicted_water)

  observed_and_guesses <- .bromeliad_detritus %>%
    select(visit_id, bromeliad_id, max_water) %>%
    anti_join(visit_and_predictions, by = "visit_id") %>%
    bind_rows(visit_and_predictions)

  stopifnot(nrow(observed_and_guesses) == nrow(.bromeliad_detritus))

  ## guard: imputation should never overwrite an observed value
  observed_and_guesses %>%
    filter(!is.na(max_water) & !is.na(predicted_water)) %>%
    { stopifnot(nrow(.) == 0) }

  new_water <- observed_and_guesses %>%
    mutate(max_water_combined  = coalesce(max_water, predicted_water),
           max_water_estimated = if_else(!is.na(predicted_water), "estimated", "observed")) %>%
    select(-max_water)

  .bromeliad_detritus %>%
    left_join(new_water, by = c("visit_id", "bromeliad_id"))
}

#' Merge hand-curated volume estimates (24_volume.csv) into the bromeliad table.
#'
#' These estimates were produced outside this pipeline and fill in any visits
#' not covered by predict_add_imputed(). The same coalesce logic applies:
#' existing max_water_combined values are never overwritten.
#'
#' FIX 4: corrected coalesce() argument order. The new version had:
#'   coalesce(max_water_combined, max_water_from_24_volume)
#' which silently prioritises GLM estimates over hand-curated values — the
#' opposite of the design intent. The correct form gives 24_volume priority:
#'   coalesce(max_water_from_24_volume, max_water_combined)
#' In practice the two sources cover disjoint bromeliads (guarded by the
#' stopifnot() below), so the order only matters if that assumption is violated.
add_24_volum_data <- function(.volume_estimated, .bromeliad_detritus_vol_imputed) {

  volume_estimated_values <- .volume_estimated %>%
    filter(!is.na(max_water)) %>%
    rename(max_water_from_24_volume = max_water)

  observed_and_guesses <- .bromeliad_detritus_vol_imputed %>%
    select(bromeliad_id, max_water_combined, max_water_estimated) %>%
    anti_join(volume_estimated_values, by = "bromeliad_id") %>%
    bind_rows(volume_estimated_values)

  stopifnot(nrow(observed_and_guesses) == nrow(.bromeliad_detritus_vol_imputed))

  observed_and_guesses %>%
    filter(!is.na(max_water_combined) & !is.na(max_water_from_24_volume)) %>%
    { stopifnot(nrow(.) == 0) }

  new_values <- observed_and_guesses %>%
    mutate(
      ## FIX 4: 24_volume takes priority — was coalesce(max_water_combined, ...)
      max_water_combined  = coalesce(max_water_from_24_volume, max_water_combined),
      max_water_estimated = if_else(!is.na(max_water_from_24_volume),
                                    "estimated", max_water_estimated)
    )

  .bromeliad_detritus_vol_imputed %>%
    select(-max_water_combined, -max_water_estimated) %>%
    left_join(new_values, by = "bromeliad_id")
}
