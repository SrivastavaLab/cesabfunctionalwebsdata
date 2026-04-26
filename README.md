Data preparation for Bromeliad Working Group datasets
===================================

This repository contains code that downloads and organizes all the data from the Bromeliad Working Group. 
This work was begun during the CESAB working group FunctionalWebs, which is what gives this repository its name. 

The updated workflow uses [targets](https://docs.ropensci.org/targets/). 
Targets is a package for creating and managing "make-like" workflows in R.
Workflows like this track how the various steps of the process are related, 
d also tracks which of these have become out of date.
A step might become out of date if the input has changed (for example, 
new data is added to the database) or if the process has changed (for example,
the function that corrects the name of a morphospecies is updated to fix a spelling error).

Targets is well-established, stable, and has excellent documentation. 
Be sure to [read the manual](https://books.ropensci.org/targets/) while maintaining and editing this repository!

Important files to note:

* `00_run_scripts.R` -- the "main" document, and the one you should open if you are working -- for example, if you are making a new data release. 
By default it does NOT download new data, but this is easy to toggle on and off. 
The assumption here is that the database itself changes slowly, but the code to clean those data and add new information to them changes more rapidly.
* `_targets.yaml` -- contains info that makes `00_run_scripts.R` work as it should. 
To understand the process here (and see the example code on which this is all based)
read [the chapter on Projects](https://books.ropensci.org/targets/projects.html) in the user manual.
* `01_download_data.R` -- downloads the BWG working group data. 
This requires the user's BWG password. Previously, `bwgdata` (the R package that accesses the database API)
prompted users to enter a username and password interactively. This won't work with the `targets` workflow, which is not interactive.
This is why bwgdata 0.4.0 uses environment variables to store usernames and passwords. 
Look at the [bwgdata Readme](https://github.com/SrivastavaLab/bwgdata) to know more about that! 
This workflow will **NOT WORK** unless `bwgdata` is at version 0.4.0, which dates from October 2025.
* `02_process_data` UNDER CONSTRUCTION. corrects, combines, and adds additional information to the dataset.

## Authenticating with the OSF

some data files associated with this project are now hosted at the OSF.
To download them you need an osf PAT.
See this [documentation](https://docs.ropensci.org/osfr/articles/auth)
which also links to [this book chapter](https://rstats.wtf/r-startup.html)


# 02_observed_data — Pipeline README

This pipeline takes raw data downloaded by `01_download_data.R` and produces a
fully cleaned, corrected dataset with **no imputation**. The output is a single
list object (`observed_data`) consumed by `03_imputed_data.R`.

All steps are defined as `targets` targets in `02_observed_data.R`. The
functions that implement each step are in
`Rfunctions/02_observed_data_functions.R`. Section numbers are consistent
across both files.

---

## Section 1 — File inputs (local)

These targets track local files that are needed for cleaning and correction but
are not downloaded from the BWG database.

- `fuzzy_traits_csv` — registers `data_files_additional/fuzzy_traits.csv` as a
  tracked file target, so the pipeline reruns if the file changes on disk.
- `trait_spreadsheet` — reads that CSV, drops all columns whose names start with
  "reference", and checks that no rows are exact duplicates. Stops with an error
  if duplicates are found.
- `bromeliad_names` — reads a semicolon-delimited table of canonical bromeliad
  species names from `data-intermediate/bromeliad_names.csv`. Used later in
  Section 9 to standardise species names.

> `get_osf_spreadsheet()` is preserved in the functions file for reference. It
> was used when the trait spreadsheet was hosted on OSF and is no longer called
> by the pipeline.

---

## Section 2 — Bromeliads: reshaping and validating

Takes the raw `broms` list from the download store and produces a flat,
type-validated data frame.

- `schema` — builds a named list that declares the expected column names and
  types (integer, double, character) for the bromeliad data frame. Columns that
  are suspected duplicates or have unclear provenance are annotated with
  comments flagging them for follow-up with the API developers.
- `broms_unnested_attrib` — unnests the nested `attributes` column so that each
  attribute becomes its own column.
- `brom_unnested_detritus` — unnests the nested `detritus` column into three
  flat columns: `detritus_min_size`, `detritus_max_size`, and `detritus_mass_g`.
  Bromeliads with no detritus data get `NA` rows rather than being dropped.
- `brom_validated` — compares the data frame against the schema. Warns about
  any columns that are missing from or unexpected in the schema. Flattens any
  remaining list columns to atomic vectors and coerces each column to its
  declared type, messaging when a coercion is applied.

---

## Section 3 — Visits and datasets

Builds clean lookup tables for visits and datasets, and removes the one
incomplete visit from the data.

- `visitnames` — filters the raw visits table down to only those visits that
  correspond to known datasets (semi-join on `dataset_id`), then retains
  `visit_id`, `dataset_id`, and `dataset_name`.
- `datasetnames` — derives a deduplicated table of dataset IDs and names from
  `visitnames`.
- `visit_no_81` — removes visit 81 from the visits table. Visit 81 is not a
  complete sample and must be excluded from the release.

---

## Section 4 — Traits

Builds the species trait table from the raw `trts_all` download, resolving
taxonomy down to the finest available level and merging in external trait data.

- `trts_all_filtered` — flattens the `names` list column (which can hold
  multiple names per species) into a single semicolon-separated string per
  species.
- `trts_parsed_cols` — converts any column that contains the literal text "NA"
  into a proper `NA` value across all character columns.
- `taxonomy_cols` — selects the full taxonomic hierarchy columns. Fuses `genus`
  and `species` into a single `species_name` column, and validates that no row
  has a species epithet without a genus, and that no concatenated name begins
  with "NA" followed by letters (a sign of malformed data).
- `lowest_taxonomic` — pivots the taxonomy columns long and, for each
  morphospecies, identifies the finest taxonomic rank to which it has been
  identified. Returns one row per species at its lowest resolved level.
- `canonical_traits` — selects the BWG functional trait columns
  (functional group, predation, realm, etc.) from the parsed traits table for
  use in the final join.
- `taxon_lowest_names` — re-runs `get_lowest_taxonomic` after dropping the
  subspecies column, then adds subspecies back onto the result. This handles the
  case where subspecies records a life-history stage rather than a true
  taxonomic rank. Also asserts that the only species with no taxonomic
  information at all are three known "Unknown animal" morphospecies (IDs 6506,
  6511, 6516).
- `traits_from_tax` — joins the external trait spreadsheet to the species list
  by matching on `taxon_name`, `taxon_level`, and `taxon_number`. Filters out
  two known problem IDs (8036, 8171) before the join. Stops if any species
  cannot be matched and verifies that no rows are duplicated.
- `traits` — left-joins `canonical_traits` with `traits_from_tax` on
  `species_id` (coerced to character in both tables to avoid type mismatches).

> The functions `add_MD_trait`, `rename_check_traits`, `join_check_by_tax`,
> `read_trait_data`, `put_traits_together`, `select_new_traits_sp_id`, and
> `combine_bwg_new_traits` are preserved in the functions file for reference.
> They encode domain knowledge about morphological defence traits from an older
> pipeline and are not currently called.

---

## Section 5 — Abundance

Tidies the raw abundance data, applies one known dataset-specific correction,
and produces both a long-format abundance table and a wide species-by-bromeliad
matrix.

- `pres_abds` — filters the raw `abds` download to keep only list elements that
  are themselves lists and that have more than one species record. This drops
  empty or degenerate datasets before further processing.
- `abundance` — calls `tidy_dataset_list()` to convert the nested list
  structure into a tidy data frame with one row per species-by-bromeliad
  observation, with measurements still nested.
- `summed_abundance_spp` — unnests the measurements column, flattens the `abd`
  list column to a character vector and parses it as a double, then sums
  abundances across size classes within each dataset-species-bromeliad
  combination. This collapses records where the same species was recorded in
  multiple size bins.
- `summed_abundance_lasgamas_dyst_correct` — corrects a known misidentification
  in three Las Gamas datasets (IDs 166, 171, 181): a dytiscid beetle was
  recorded under the wrong species ID (4516) and bwg_name (Coleoptera.52) and
  is reassigned to the correct ID (5496) and name (Coleoptera.64).
- `abundance_no_zero` — asserts that no `abd` values are `NA`, then removes all
  zero-abundance records and renames the column `abundance`.
- `spp_abundances_wide` — produces a wide species-by-bromeliad matrix from the
  corrected abundance data. Columns are named by combining `dataset_id` and
  `species_id`; absent species are filled with 0.
- `synonymous_names` — identifies morphospecies that have been recorded under
  multiple `species_id` values but resolve to the same `taxon_name`. Also checks
  whether any synonymous species co-occur in the same bromeliad (which would
  require merging). Returns the table of synonymous name pairings.
- `abundance_no_81` — removes abundance records for visit 81 by semi-joining
  against the validated bromeliad table with that visit filtered out. ID columns
  are coerced to integer before the join.

---

## Section 6 — Bromeliad morphology derived variables

Reshapes the validated bromeliad data into the working format used by the
correction and imputation steps.

- `diam_brom` — selects the bromeliad morphology columns (water volume, leaf
  dimensions, diameter, height, etc.) from the validated bromeliads table, after
  removing the long-format detritus columns. Verifies that the row count is
  unchanged by the selection.
- `fpom_brom` — selects the fine particulate organic matter columns
  (`fpom_ml`, `fpom_mg`, `fpom_g`, `cpom_g`) using `any_of()` because these
  columns are not guaranteed to be present.
- `detritus_wide` — pivots the three long-format detritus columns
  (`detritus_min_size`, `detritus_max_size`, `detritus_mass_g`) into wide
  format, creating one column per unique size-range pair (e.g.
  `detritus0_150`, `detritus150_20000`). Joins back onto the non-detritus
  bromeliad columns and verifies row count.
- `detritus_wider` — assembles the main working data frame for the detritus
  pipeline. Starts from a distinct bromeliad-visit ID table and left-joins in
  the wide detritus columns, visit names, morphology columns, and FPOM columns.
  Verifies at each join that no rows have been duplicated.

---

## Section 7 — Detritus cleaning and corrections

Applies a sequence of site-specific data corrections to the detritus data.
Each correction targets a known data-entry error in a specific visit or dataset.

- `detritus_wider_bromeliad_names` — strips leading and trailing whitespace
  from the `species` column. Verifies that every `bromeliad_id` appears exactly
  once (no duplicates introduced by the whitespace fix).
- `detritus_wider_cardoso_corrected` — for visit 21 (Cardoso), moves values
  from `detritus150_20000` into `detritus150_NA`, where they should have been
  entered. Before applying the correction, checks whether the destination column
  already has values and the source is already NA; if so, the database error has
  been fixed upstream and the step is skipped with a message.
- `detritus_wider_correct_frenchguiana` — corrects two French Guiana issues:
  (1) for dataset 211, moves values from the named columns `fpom_g`, `cpom_g`,
  and `dead_leaves` into the correct quantitative detritus columns; (2) for
  datasets 206 and 216, converts an FPOM value recorded in milligrams back to
  grams. Validates that no source-column values were lost in the move before
  dropping the now-redundant source columns, and zeroes out particle-count data
  erroneously recorded as mass in dataset 201 (Nouragues 2006).
- `detritus_wider_correct_brazil` — for a single bromeliad (ID 7646) in
  Picinguaba (visit 241), moves a value from `detritus125_NA` into `detritus0_NA`
  where it was entered in the wrong column. Asserts afterward that all
  `detritus0_NA` values in visit 241 are filled and all `detritus125_NA` values
  are NA.

---

## Section 8 — Bromeliad environmental variables

Adds and corrects environmental variables: canopy openness and elevation.
No imputation occurs here; all values are either directly measured, looked up
from a table, or derived from a recorded measurement by a fixed rule.

- `bromeliad_detritus_extra_cols` — identifies columns present in
  `detritus_wide` but absent from `detritus_wider`, and joins them back onto the
  corrected detritus table. This recovers optional environmental columns that
  were present in the wide bromeliad data but not carried through to the working
  detritus frame.
- `bromeliad_detritus_incidentrad` — corrects a single known data-entry error:
  bromeliad 3846 has an `incident_radiation_percentage` value greater than 100,
  which is divided by 10 to recover the correct value (~14.59).
- `openness_conversion_table` — builds a lookup table that assigns a binary
  `open.canopy` value (1 = open, 0 = closed) to each visit. Three sources are
  combined: a mapping from text values ("open", "closed", "edge") to binary; an
  explicit per-visit assignment for all visits with known canopy status; and a
  cross-join for the Las Gamas visits (281, 266, 271) which are assigned
  openness based on their recorded `canopy_openess_chr` value.
- `bromeliad_detritus_open` — left-joins the openness conversion table onto the
  corrected bromeliad data by `visit_id`. The `visit_id` in the conversion table
  is coerced to integer before the join.
- `bromeliad_detritus_open_converted` — for two groups of visits that recorded
  canopy openness indirectly via light measurements rather than text, derives
  `open.canopy` from `incident_radiation_above_ground_percentage` (visits 296,
  301) or `incident_radiation_percentage` (visit 331): values ≥ 50% → 1 (open),
  < 50% → 0 (closed).
- `bromeliad_elevation` — assigns `elevation_m` for all visits where it was not
  recorded directly in the database, using a lookup of known site elevations.
  Sites covered include Puerto Rico (dwarf forest, Palo colorado, tabunocco,
  Sonadora elevational gradient), Saba, and Dominica.

---

## Section 9 — Bromeliad species names

Standardises bromeliad species names in the bromeliad data frame and removes
the incomplete visit.

- `genus_spp_corrected` — extracts the distinct species names from the
  bromeliad table and splits each into genus and trivial (epithet) parts.
  Corrects a small number of known misspellings (e.g. "gladifolia" →
  "gladioliflora", "desantsii" → "desautelsii") and standardises the
  Vriesea/Guzmania unknown-species label. Returns a two-column table of old
  names to corrected names.
- `correct_name_pairing` — fuzzy-joins the corrected species names against the
  canonical `bromeliad_names` table (from Section 1) using string distance ≤ 2,
  to catch any remaining near-matches. Returns the paired old-to-canonical name
  table.
- `bromeliad_correctnames` — applies the name corrections to the full bromeliad
  data frame using the lookup from `correct_name_pairing`.
- `bromeliads_visit_no_81` — removes all bromeliad records belonging to visit
  81 by semi-joining against `visit_no_81`.

> Note: the function is named `correct_bromelaid_species_names` (with a typo —
> "bromelaid"). This is harmless but could be corrected in a future refactor.

---

## Section 10 — Species dictionary

Produces a reference table linking each species to the visits and datasets where
it was observed, together with its taxonomy.

- `spp_dictionary` — takes the unnested abundance data and traces each
  species back through bromeliads to visits, then joins on visit-level metadata
  (date, habitat, dataset name) and species-level taxonomy (order, family, genus,
  species epithet) from `trts_all_filtered`. Returns one row per unique
  species-by-visit combination, deduplicated.

> Note: the target is named `spp_dictonary` (with a typo — "dictonary") in the
> pipeline. This could be corrected in a future refactor.

---

## Final output — `observed_data`

A named list assembled from the cleaned targets above. Consumed by
`03_imputed_data.R`.

| Element              | Contents                                                  |
|----------------------|-----------------------------------------------------------|
| `datasets`           | Raw dataset metadata from the download store              |
| `visits`             | Visit table with visit 81 removed                         |
| `traits`             | Species trait table with taxonomy and functional traits   |
| `bromeliads`         | Corrected bromeliad morphology and environment data       |
| `abundance`          | Long-format abundance, visit 81 removed, zeros removed    |
| `synonymous_names`   | Table of morphospecies that resolve to the same taxon     |
| `abundance_matrix`   | Wide species-by-bromeliad presence matrix                 |
| `brom_validated`     | Type-validated bromeliads table (needed by imputation)    |
| `detritus_corrected` | Fully corrected detritus table (needed by imputation)     |

---

## Known issues and technical debt

- Several columns in the schema are flagged as probable duplicates (e.g.
  `plant_area` / `plant_area_m2`, `elevation` / `elevation_m`). These should
  be investigated with the BWG database developers.
