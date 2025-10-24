## updating the workflow to targets
library(targets)
library(tarchetypes)
library(bwgdata)
tar_option_set(
  packages = c(
    "bwgdata",
    "dplyr",
    "bwgdata",
    "purrr",
    "readr")
  )

# read in functions -- only needed for `get_all_abundance`s
tar_source(files = "Rfunctions/")

## read in data downloaded in 01_download_data.R

dats <- tar_read(dats, store = "store_download_data")
visits <- tar_read(visits, store = "store_download_data")
broms <- tar_read(broms, store = "store_download_data")
abds <- tar_read(abds, store = "store_download_data")
trts_all <- tar_read(trts_all, store = "store_download_data")

list(
  tar_target(
    name = broms_rename_unnest,
    command = no_attrib_unnest_det(broms),
  ),

  tar_target(
    name = trts_all_filtered,
    command = combine_multi_names(trts_all),
  ),

  tar_target(
    name = trts_parsed_cols,
    command = parse_column_types_reader(trts_all_filtered),
  ),

  tar_target(
    name = pres_abds,
    command = discard(abds, I(is.na)),
  ),

  tar_target(
    name = abundance,
    command = tidy_dataset_list(pres_abds),
  ),

  tar_target(
    name = fpom_fg_data,
    command = read_fpom_fg("data-raw/FPOMdecanted_dryweight.csv"),
  ),

  tar_target(
    name = visits_date,
    command = parse_column_types_reader(visits),
  ),

  tar_target(
    name = dats_date,
    command = parse_column_types_reader(dats),
  ),

  tar_target(
    name = brom_clean_name,
    command = drop_bad_name(broms_rename_unnest),
  ),

  tar_target(
    name = broms_date,
    command = parse_column_types_reader(brom_clean_name),
  ),

  tar_target(
    name = visitnames,
    command = make_visitnames(visits_date, dats_date),
  ),

  tar_target(
    name = datasetnames,
    command = make_datasetnames(visitnames),
  ),

  tar_target(
    name = diam_brom,
    command = make_diam_brom(broms_date),
  ),

  tar_target(
    name = fpom_brom,
    command = make_fpom_brom(broms_date),
  ),

  tar_target(
    name = detritus_wide,
    command = make_detritus_wide(broms_date),
  ),

  tar_target(
    name = detritus_wider,
    command = make_detritus_wider(broms_date, detritus_wide, visitnames, diam_brom, fpom_brom),
  ),

  tar_target(
    name = bromeliad_names,
    command = import_BromeliadSpecies("data-intermediate/bromeliad_names.csv"),
  ),

  tar_target(
    name = detritus_wider_bromeliad_names,
    command = fix_whitespace_bromeliad_names(detritus_wider),
  ),

  tar_target(
    name = detritus_wider_cardoso_corrected,
    command = correct_cardoso_detritus_wider(detritus_wider_bromeliad_names),
  ),

  tar_target(
    name = detritus_wider_correct_frenchguiana,
    command = correct_frenchguiana_detritus(detritus_wider_cardoso_corrected),
  ),

  tar_target(
    name = detritus_wider_correct_brazil,
    command = correct_picin_juraea(detritus_wider_correct_frenchguiana),
  ),

  tar_target(
    name = model_fpom_g_ml,
    command = fit_fpom_g_ml(fpom_fg_data),
  ),

  tar_target(
    name = detritus_wider_fpom_g_pred,
    command = add_predictions_to_data(detritus_wider_correct_brazil, model_fpom_g_ml),
  ),

  tar_target(
    name = detritus_wider_0_150_added,
    command = combine_observed_predicted_0_150_det(detritus_wider_fpom_g_pred),
  ),

  tar_target(
    name = detritus_wider_150_name_changed,
    command = change_name_150_combo(detritus_wider_0_150_added),
  ),

  # tar_target(
  #   name = "outputs/bromeliad_observations.csv",
  #   command = write_csv(detritus_wider_150_name_changed, path = target_name),
  # ),
  #
  # tar_target(
  #   name = "outputs/dataset_information.csv",
  #   command = write_csv(dats_date, path = target_name),
  # ),
  #
  # tar_target(
  #   name = "outputs/visit_information.csv",
  #   command = write_csv(visits_date, path = target_name),
  # ),
  #
  # tar_target(
  #   name = "outputs/detritus.csv",
  #   command = write_csv(detritus_wider_new_variables, path = target_name),
  # ),
  #
  # tar_target(
  #   name = "outputs/fpom_fg_data.csv",
  #   command = write_csv(fpom_fg_data, path = target_name),
  # ),

  tar_target(
    name = spp_dictonary,
    command = output_spp_dictionary(abundance, broms_date, visits_date, trts_all_filtered),
  ),

  # tar_target(
  #   name = "outputs/spp_dictionary.csv",
  #   command = write_csv(spp_dictonary, path = target_name),
  # ),
  #
  # tar_target(
  #   name = "outputs/detritus_models.rds",
  #   command = write_rds(detritus_estimated_with_model, path = target_name),
  # ),
  #
  # tar_target(
  #   name = "outputs/detritus_plots.rds",
  #   command = write_rds(detritus_model_plots, path = target_name),
  # ),

  tar_target(
    name = equation_table,
    command = create_equation_table(),
  ),

  tar_target(
    name = detritus_equation_plots,
    command = plot_data_with_equation_table(equation_table, .detritus_data = detritus_wider_150_name_changed),
  ),

  tar_target(
    name = detritus_estimate_equation_filt,
    command = do_filter_dataset_id(equation_table, .detritus_data = detritus_wider_150_name_changed),
  ),

  tar_target(
    name = detritus_estimated_with_equation,
    command = do_mutate_new_col(detritus_estimate_equation_filt),
  ),

  tar_target(
    name = detritus_wider_new_variables,
    command = add_new_columns_for_prediction(detritus_wider_150_name_changed),
  ),

  tar_target(
    name = model_table,
    command = create_model_table(),
  ),

  tar_target(
    name = modelling_information,
    command = derive_modelling_information(model_table, detritus_wider_new_variables),
  ),

  tar_target(
    name = observed_model_fit,
    command = do_fit_predictive_model(modelling_information),
  ),

  tar_target(
    name = plotting_information,
    command = construct_plotting_information(.observed_model_fit = observed_model_fit,
                                             .modelling_information = modelling_information),
  ),

  tar_target(
    name = detritus_model_plots,
    command = plot_model_and_supporting_data(.plotting_information = plotting_information,
                                             .modelling_information = modelling_information),
  ),

  tar_target(
    name = detritus_estimated_with_model,
    command = estimate_missing_detritus_new_site(.observed_model_fit = observed_model_fit,
                                                 .modelling_information = modelling_information,
                                                 .detritus_data = detritus_wider_new_variables),
  ),

  tar_target(
    name = detritus_all_preds,
    command = combine_detritus_predictions(detritus_estimated_with_model),
  ),

  tar_target(
    name = detritus_long_categories,
    command = combine_all_detritus_values(detritus_wider_new_variables, detritus_all_preds, broms_date),
  ),

  tar_target(
    name = detritus_long_filtered,
    command = filter_just_orig_fitted(detritus_wider_150_name_changed, detritus_long_categories),
  ),

  tar_target(
    name = det_long_broken_up,
    command = split_detritus_categories(detritus_long_filtered),
  ),

  tar_target(
    name = det_long_min_max,
    command = extract_numeric_min_max(det_long_broken_up),
  ),

  tar_target(
    name = det_long_check_consec,
    command = add_consecutive_detritus_col(det_long_min_max),
  ),

  tar_target(
    name = detritus_summary,
    command = create_detritus_summary(det_long_check_consec),
  ),

  tar_target(
    name = bromeliad_detritus,
    command = add_detritus_summary(detritus_wider_150_name_changed, detritus_summary),
  ),

  tar_target(
    name = taxonomy_cols,
    command = make_taxonomy_cols(trts_parsed_cols),
  ),

  tar_target(
    name = lowest_taxonomic,
    command = get_lowest_taxonomic(taxonomy_cols),
  ),

  tar_target(
    name = canonical_traits,
    command = get_canonical_traits(trts_parsed_cols),
  ),

  tar_target(
    name = taxon_lowest_names,
    command = lowest_name_and_subspecies(taxonomy_cols, lowest_taxonomic),
  ),

  tar_target(
    name = traits_from_tax,
    command = merge_trait_by_taxonomy(trait_spreadsheet, taxon_lowest_names),
  ),

  tar_target(
    name = traits,
    command = left_join(canonical_traits, traits_from_tax, by = I("species_id")),
  ),

  tar_target(
    name = aquilega_biog,
    command = read_size_aquilega("data-intermediate/size_aquilega_Biog.csv"),
  ),

  tar_target(
    name = aquilegaKT,
    command = read_size_aquilega("data-intermediate/size_aquilegaKT.csv"),
  ),

  tar_target(
    name = guzmania,
    command = read_size_Guzmania_mertensii("data-intermediate/size_Guzmania_PR.csv"),
  ),

  tar_target(
    name = mertensii,
    command = read_size_Guzmania_mertensii("data-intermediate/size_mertensii.csv"),
  ),

  tar_target(
    name = vriesea,
    command = read_size_vriesea("data-intermediate/size_vriesea.csv"),
  ),

  tar_target(
    name = vriesea_prod,
    command = read_size_vriesea("data-intermediate/size_vriesea_prod.csv"),
  ),

  tar_target(
    name = supplementary_size_data,
    command = bind_rows(aquilega_biog = aquilega_biog, aquilegaKT = aquilegaKT,
                        guzmania = guzmania, mertensii = mertensii, vriesea = vriesea,
                        vriesea_prod = vriesea_prod, .id = I("filename")),
  ),

  tar_target(
    name = supp_data_additional,
    command = add_more_info_to_supp(supplementary_size_data),
  ),

  tar_target(
    name = supp_data_renamed,
    command = supp_data_rename(supp_data_additional),
  ),

  tar_target(
    name = supp_size_models,
    command = make_model_data(),
  ),

  tar_target(
    name = supp_size_model_info,
    command = derive_modelling_information_simpler(supp_size_models, supp_data_renamed),
  ),

  tar_target(
    name = supp_size_model_data,
    command = make_model_target_data(),
  ),

  tar_target(
    name = supp_size_model_fits,
    command = fit_size_models_to_data(supp_size_model_info, supp_size_model_data),
  ),

  tar_target(
    name = bromeliad_detritus_vol_imputed,
    command = predict_add_imputed(supp_size_model_fits, bromeliad_detritus),
  ),

  tar_target(
    name = volume_estimated,
    command = read_volume_estimated("data-raw/24_volume.csv"),
  ),

  tar_target(
    name = bromeliad_detritus_vol_24_added,
    command = add_24_volum_data(volume_estimated, bromeliad_detritus_vol_imputed),
  ),

  tar_target(
    name = bromeliad_detritus_extra_cols,
    command = add_in_extra_columns(detritus_wide, detritus_wider, bromeliad_detritus_vol_24_added),
  ),

  tar_target(
    name = bromeliad_detritus_incidentrad,
    command = correct_incident_rad_percent(bromeliad_detritus_extra_cols),
  ),

  tar_target(
    name = openness_conversion_table,
    command = create_openness_conversion_table(),
  ),

  tar_target(
    name = bromeliad_detritus_open,
    command = left_join(bromeliad_detritus_incidentrad, openness_conversion_table),
  ),

  tar_target(
    name = bromeliad_detritus_open_converted,
    command = convert_incident_to_openness(bromeliad_detritus_open),
  ),

  tar_target(
    name = bromeliad_elevation,
    command = add_elevation(bromeliad_detritus_open_converted),
  ),

  tar_target(
    name = genus_spp_corrected,
    command = extract_bromeliad_species_names(bromeliad_elevation),
  ),

  tar_target(
    name = correct_name_pairing,
    command = join_old_new_bromeliad_names(genus_spp_corrected, bromeliad_names),
  ),

  tar_target(
    name = bromeliad_correctnames,
    command = correct_bromelaid_species_names(bromeliad_elevation, correct_name_pairing),
  ),

  tar_target(
    name = summed_abundance_spp,
    command = sum_species_abundances(abundance),
  ),

  tar_target(
    name = summed_abundance_lasgamas_dyst_correct,
    command = correct_lasgamas_dytiscid(summed_abundance_spp),
  ),

  tar_target(
    name = abundance_no_zero,
    command = filter_zero_abd(summed_abundance_lasgamas_dyst_correct),
  ),

  tar_target(
    name = synonymous_names,
    command = identify_merge_duplicates(traits, summed_abundance_lasgamas_dyst_correct),
  ),

  tar_target(
    name = spp_abundances_wide,
    command = spread_present_species(summed_abundance_lasgamas_dyst_correct),
  ),

  tar_target(
    name = visit_no_81,
    command = filter_visit_81(visits_date),
  ),

  tar_target(
    name = bromeliads_visit_no_81,
    command = filter_bromeliads_visit81(bromeliad_correctnames, visit_no_81),
  ),

  tar_target(
    name = abundance_no_81,
    command = filter_abundance_81(abundance_no_zero, bromeliads_visit_no_81),
  )

)
