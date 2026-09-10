# 2Reg ETL 01c: build the adult control-group dataset from registry3 JSON
#
# The control form uses the same four registry timepoints as registry2, but it
# has no hemoperfusion procedures. This script keeps the documented timepoints
# and creates a separate control_tidy dataset; patients_tidy is not modified.

# 1. Packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(lubridate)
  library(jsonlite)
  library(openxlsx)
})

# 2. Paths ----------------------------------------------------------------

DATA_DIR <- "data"
REPORT_TZ <- "UTC"

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

source(file.path("R", "etl_helpers.R"))

control_registry_file <- pick_path(c(
  file.path(DATA_DIR, "registry3-accepted-records.json"),
  file.path("/mnt/data", DATA_DIR, "registry3-accepted-records.json"),
  "registry3-accepted-records.json"
))

control_out_rds <- file.path(DATA_DIR, "control_tidy.rds")
control_out_xlsx <- file.path(DATA_DIR, "2Reg_control_tidy.xlsx")

save_rds <- TRUE
save_xlsx <- TRUE

# 3. Small schema helpers -------------------------------------------------

yes_no_to_int <- function(x) {
  x <- clean_na(str_to_lower(str_squish(as.character(x))))
  case_when(
    x %in% c("1", "да", "true", "t", "yes", "y") ~ 1L,
    x %in% c("0", "нет", "false", "f", "no", "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

outcome_key <- function(x) {
  x <- str_to_lower(str_squish(coalesce(as.character(x), "")))
  case_when(
    str_detect(x, "умер|death|dead") ~ "dead",
    str_detect(x, "выпис|discharg") ~ "out",
    str_detect(x, "перевед|transfer") ~ "transferred",
    str_detect(x, "наход|орит|icu") ~ "in",
    TRUE ~ "unknown"
  )
}

extract_lab <- function(block, key, impute_ineq = FALSE) {
  obj <- pluck0(block, key, .default = NULL)
  list(
    value = lab_num(obj, impute_ineq = impute_ineq),
    unit = lab_unit(obj)
  )
}

# 4. Extract one registry3 record ----------------------------------------

extract_control_rows <- function(record) {
  d <- pluck0(record, "data", .default = list())
  outcome_block <- pluck0(d, "ИсходНаблюдения", .default = list())
  organ_block <- pluck0(d, "ПоддержкаОрганнойДисфункции", .default = list())
  vpr_block <- pluck0(organ_block, "Вазопрессоры", .default = list())
  mv_block <- pluck0(organ_block, "ИВЛ", .default = list())
  rrt_block <- pluck0(organ_block, "ЗПТ", .default = list())
  ecmo_block <- pluck0(organ_block, "ЭКМО", .default = list())

  static <- tibble(
    pat_record_id = chr0(pluck0(record, "id", .default = NA_character_)),
    registry_source = "registry3_control",
    comparison_group = "Control",
    record_author = dplyr::coalesce(
      chr0(pluck0(d, "Автор", .default = NA_character_)),
      chr0(pluck0(record, "createdBy", .default = NA_character_))
    ),
    organization = NA_character_,
    record_date_created_raw = chr0(pluck0(record, "createdAt", .default = NA_character_)),
    record_date_updated_raw = chr0(pluck0(record, "updatedAt", .default = NA_character_)),
    admission_date = chr0(pluck0(d, "ДатаГоспитализации", .default = NA_character_)),
    diagnoses = chr0(pluck0(d, "Диагнозы", .default = NA_character_)),
    HA_indications = chr0(pluck0(d, "ПоказанияГС", .default = NA_character_)),
    ICU_6_month_history = chr0(pluck0(d, "ПребываниеВОРИТВПоследние6Мес", .default = NA_character_)),
    from_other_ICU_transfer = chr0(pluck0(d, "ПереводИзДругогоОРИТ", .default = NA_character_)),
    age_yr = num0(pluck0(d, "ВозрастЛет", .default = NA)),
    age_m = num0(pluck0(d, "ВозрастМесяцев", .default = NA)),
    sex = chr0(pluck0(d, "Пол", .default = NA_character_)),
    BMI = num0(pluck0(d, "ИМТ", .default = NA)),
    charlson_index = num0(pluck0(d, "ИндексЧарлсон", .default = NA)),
    pat_gram_positive = chr0(norm_culture(pluck0(d, "Грамположительные", .default = NA_character_))),
    pat_gram_negative = chr0(norm_culture(pluck0(d, "Грамотрицательные", .default = NA_character_))),
    pat_bacteremia = chr0(norm_culture(pluck0(d, "Бактериемия", .default = NA_character_))),
    pat_fungal = chr0(norm_culture(pluck0(d, "Грибы", .default = NA_character_))),
    outcome = chr0(pluck0(outcome_block, "Исход", .default = NA_character_)),
    HA_efficency_CGI_E = chr0(pluck0(outcome_block, "ШкалаЭффективностиГемосорбции", .default = NA_character_)),
    ICU_in_datetime = chr0(pluck0(d, "ДатаВремяПоступленияОРИТ", .default = NA_character_)),
    ICU_out_or_death_date = chr0(pluck0(outcome_block, "ДатаВремяВыпискиОРИТСмерти", .default = NA_character_)),
    antibiotics_start_datetime = chr0(pluck0(d, "ДатаВремяНачалаАнтибиотикотерапии", .default = NA_character_)),
    vasopressors_if_used = yn_ru(pluck0(vpr_block, "применялась", .default = NA)),
    MV_if_used = yn_ru(pluck0(mv_block, "применялась", .default = NA)),
    RRT_if_used = yn_ru(pluck0(rrt_block, "применялась", .default = NA)),
    ECMO_if_used = yn_ru(pluck0(ecmo_block, "применялась", .default = NA)),
    first_sorption_type = NA_character_
  )

  tp_names <- unique(c(
    names(pluck0(d, "КлиническаяОценка", .default = list())),
    names(pluck0(d, "КлеткиКрови", .default = list())),
    names(pluck0(d, "БиохимияКрови", .default = list()))
  ))
  tp_names <- tp_names[str_detect(tp_names, "^Точка")]

  if (!length(tp_names)) {
    return(static %>% mutate(timepoint = NA_character_))
  }

  dynamic <- map_dfr(tp_names, function(tp_ru) {
    cl <- pluck0(d, "КлиническаяОценка", tp_ru, .default = list())
    cells <- pluck0(d, "КлеткиКрови", tp_ru, .default = list())
    bio <- pluck0(d, "БиохимияКрови", tp_ru, .default = list())

    # Suffix `_lab` prevents a tibble data-mask collision: after the numeric
    # column `lactate` is created, `lactate$unit` would otherwise refer to that
    # new atomic column instead of the list returned by extract_lab().
    lactate_lab <- extract_lab(bio, "лактат")
    creatinine_lab <- extract_lab(bio, "креатинин")
    albumin_lab <- extract_lab(bio, "альбумин")
    procalcitonin_lab <- extract_lab(bio, "прокальцитонин", impute_ineq = TRUE)
    crp_lab <- extract_lab(bio, "С_реактивный_белок")
    fibrinogen_lab <- extract_lab(bio, "Фибриноген")
    bilirubin_lab <- extract_lab(bio, "Общий_билирубин")

    FiO2 <- dplyr::coalesce(
      num0(pluck0(cl, "oiParams", "FiO2", .default = NA)),
      num0(pluck0(cl, "OxygenationIndex", "FiO2", .default = NA))
    )
    PaO2 <- dplyr::coalesce(
      num0(pluck0(cl, "oiParams", "PaO2", .default = NA)),
      num0(pluck0(cl, "OxygenationIndex", "PaO2", .default = NA))
    )
    SpO2 <- dplyr::coalesce(
      num0(pluck0(cl, "oiParams", "SpO2", .default = NA)),
      num0(pluck0(cl, "OxygenationIndex", "SpO2", .default = NA))
    )
    pafi_from_raw <- if_else(
      !is.na(PaO2) & !is.na(FiO2) & FiO2 > 0,
      PaO2 / (FiO2 / 100),
      NA_real_
    )

    tibble(
      timepoint = tp_code(tp_ru),
      clin_assess_date = chr0(pluck0(cl, "датаПроведения", .default = NA_character_)),
      blood_cells_date = chr0(pluck0(cells, "датаПробы", .default = NA_character_)),
      blood_bio_date = chr0(pluck0(bio, "датаПробы", .default = NA_character_)),
      SOFA = num0(pluck0(cl, "балSOFA", .default = NA)),
      VIS2020 = num0(pluck0(cl, "индексVIS2020", .default = NA)),
      avg_BP = num0(pluck0(cl, "среднееАД", .default = NA)),
      INR = num0(pluck0(cl, "МНО", .default = NA)),
      FiO2 = FiO2,
      PaO2 = PaO2,
      SpO2 = SpO2,
      PaFiO2 = dplyr::coalesce(
        num0(pluck0(cl, "OxygenationIndex", "OI", .default = NA)),
        num0(pluck0(cl, "PaO2_FiO2", .default = NA)),
        pafi_from_raw
      ),
      SpFiO2 = NA_real_,
      leucocytes = num0(pluck0(cells, "лейкоциты", .default = NA)),
      neutrophils = num0(pluck0(cells, "нейтрофилы", .default = NA)),
      lymphocytes = num0(pluck0(cells, "лимфоциты", .default = NA)),
      thrombocytes = num0(pluck0(cells, "тромбоциты", .default = NA)),
      lactate = lactate_lab$value,
      lactate_unit = lactate_lab$unit,
      creatinine = creatinine_lab$value,
      creatinine_unit = creatinine_lab$unit,
      albumin = albumin_lab$value,
      albumin_unit = albumin_lab$unit,
      procalcitonin = procalcitonin_lab$value,
      procalcitonin_unit = procalcitonin_lab$unit,
      C_react_protein = crp_lab$value,
      C_react_protein_unit = crp_lab$unit,
      D_dimer = NA_real_,
      D_dimer_unit = NA_character_,
      fibrinogen = fibrinogen_lab$value,
      fibrinogen_unit = fibrinogen_lab$unit,
      bilirubin_total = bilirubin_lab$value,
      bilirubin_total_unit = bilirubin_lab$unit
    )
  })

  bind_cols(dynamic, static[rep(1, nrow(dynamic)), , drop = FALSE])
}

# 5. Read and extract -----------------------------------------------------

control_raw <- read_json_list(control_registry_file)

control_records <- keep(
  control_raw,
  ~ identical(chr0(pluck0(.x, "registryId", .default = NA_character_)), "registry3") &&
    identical(chr0(pluck0(.x, "status", .default = NA_character_)), "accepted")
)

control_tidy <- map_dfr(control_records, extract_control_rows) %>%
  mutate(across(where(is.character), clean_na))

tp_levels <- c("m12h_0h", "48h_72h", "4d_6d", "7d_10d")
control_tidy <- control_tidy %>%
  mutate(timepoint = factor(timepoint, levels = tp_levels, ordered = TRUE))

# 6. Dates and patient-level outcomes ------------------------------------

dt_map <- c(
  ICU_in_datetime = "ICU_in_dt",
  ICU_out_or_death_date = "ICU_out_or_death_dt",
  antibiotics_start_datetime = "antibiotics_start_dt",
  clin_assess_date = "clin_assess_dt",
  blood_cells_date = "blood_cells_dt",
  blood_bio_date = "blood_bio_dt"
)

control_tidy <- control_tidy %>%
  mutate(across(any_of(names(dt_map)), ~ .x, .names = "{.col}_raw"))

control_tidy <- reduce(
  names(dt_map),
  .init = control_tidy,
  .f = ~ add_dt(.x, .y, dt_map[[.y]], tz = REPORT_TZ)
) %>%
  select(-any_of(names(dt_map))) %>%
  mutate(
    record_date_created_dt = parse_dt(record_date_created_raw, tz = REPORT_TZ),
    record_date_updated_dt = parse_dt(record_date_updated_raw, tz = REPORT_TZ),
    first_sorption_dt = as.POSIXct(NA, tz = REPORT_TZ),
    icu_to_HA1_hours = NA_real_,
    ICU_to_first_LPS = NA_real_,
    ICU_to_first_CT = NA_real_,
    NLR = if_else(
      !is.na(neutrophils) & !is.na(lymphocytes) & lymphocytes > 0,
      neutrophils / lymphocytes,
      NA_real_
    ),
    PaFiO2_calc = round(PaFiO2, 2),
    PaFiO2_calc_source = if_else(!is.na(PaFiO2), "observed", "missing"),
    sex_male_bin = case_when(
      str_detect(str_to_lower(coalesce(sex, "")), "муж|male|^м$|^m$") ~ 1L,
      str_detect(str_to_lower(coalesce(sex, "")), "жен|female|^ж$|^f$") ~ 0L,
      TRUE ~ NA_integer_
    ),
    vasopressors_if_used_bin = yes_no_to_int(vasopressors_if_used),
    RRT_if_used_bin = yes_no_to_int(RRT_if_used),
    MV_if_used_bin = yes_no_to_int(MV_if_used),
    ECMO_if_used_bin = yes_no_to_int(ECMO_if_used),
    from_other_ICU_transfer_bin = yes_no_to_int(from_other_ICU_transfer),
    ICU_6_month_history_bin = yes_no_to_int(ICU_6_month_history),
    ICU_to_antibiotics_dt = round(
      as.numeric(difftime(antibiotics_start_dt, ICU_in_dt, units = "hours")),
      1
    ),
    .outcome_key = outcome_key(outcome),
    outcome_status = factor(
      .outcome_key,
      levels = c("in", "out", "dead", "transferred", "unknown")
    ),
    outcome_dead_bin = case_when(
      .outcome_key == "dead" ~ 1L,
      .outcome_key == "out" ~ 0L,
      TRUE ~ NA_integer_
    ),
    outcome_days = as.numeric(
      difftime(ICU_out_or_death_dt, ICU_in_dt, units = "days")
    )
  )

# The control index is the documented baseline clinical assessment. It is the
# registry's available proxy for the moment when sorption became indicated.
control_index <- control_tidy %>%
  filter(timepoint == "m12h_0h") %>%
  transmute(
    pat_record_id,
    index_dt = clin_assess_dt,
    index_source = "documented_m12h_0h"
  )

control_tidy <- control_tidy %>%
  left_join(control_index, by = "pat_record_id") %>%
  mutate(
    ICU_to_index_hours = round(
      as.numeric(difftime(index_dt, ICU_in_dt, units = "hours")),
      1
    ),
    antibiotics_to_index_hours = round(
      as.numeric(difftime(index_dt, antibiotics_start_dt, units = "hours")),
      1
    ),
    index_to_ICU_event_days = as.numeric(
      difftime(ICU_out_or_death_dt, index_dt, units = "days")
    )
  )

# 7. Status and informed zeros -------------------------------------------

tp_windows <- tibble(
  timepoint = factor(tp_levels, levels = tp_levels, ordered = TRUE),
  end_h = c(0, 72, 144, 240)
)

control_tidy <- control_tidy %>%
  left_join(tp_windows, by = "timepoint") %>%
  mutate(
    .hours_to_event = as.numeric(
      difftime(ICU_out_or_death_dt, index_dt, units = "hours")
    ),
    status = case_when(
      is.na(index_dt) | is.na(end_h) ~ "n/a",
      is.na(ICU_out_or_death_dt) ~ "in",
      .hours_to_event <= end_h & .outcome_key == "out" ~ "out",
      .hours_to_event <= end_h & .outcome_key == "dead" ~ "dead",
      .hours_to_event <= end_h & .outcome_key == "transferred" ~ "transferred",
      .hours_to_event > end_h ~ "in",
      TRUE ~ "n/a"
    ),
    SOFA_raw = SOFA,
    SOFA_fill_rule = case_when(
      !is.na(SOFA) ~ "observed",
      status == "out" ~ "zero_after_ICU_discharge",
      TRUE ~ "missing"
    ),
    SOFA = if_else(is.na(SOFA) & status == "out", 0, SOFA),
    VIS2020_raw = VIS2020,
    VIS2020_fill_rule = case_when(
      !is.na(VIS2020) ~ "observed",
      status == "out" ~ "zero_after_ICU_discharge",
      vasopressors_if_used_bin == 0L ~ "zero_no_vasopressors",
      TRUE ~ "missing_unknown"
    ),
    # MAP > 65 alone is not used: MAP can be normal while vasopressors are running.
    VIS2020 = if_else(
      is.na(VIS2020) &
        (status == "out" | vasopressors_if_used_bin == 0L),
      0,
      VIS2020
    )
  ) %>%
  group_by(pat_record_id) %>%
  mutate(
    .vis_baseline_positive = any(
      timepoint == "m12h_0h" & !is.na(VIS2020) & VIS2020 > 0,
      na.rm = TRUE
    ),
    VIS2020_0_excl = if_else(.vis_baseline_positive, VIS2020, NA_real_)
  ) %>%
  ungroup() %>%
  select(-end_h, -.hours_to_event, -.outcome_key, -.vis_baseline_positive)

# 8. QC and outputs -------------------------------------------------------

dup_control <- control_tidy %>%
  count(pat_record_id, timepoint) %>%
  filter(n > 1)

cat("control_tidy rows:", nrow(control_tidy), "\n")
cat("control_tidy ids:", n_distinct(control_tidy$pat_record_id), "\n")
cat("duplicate pat_record_id + timepoint:", nrow(dup_control), "\n")
cat("missing control index dates:", sum(is.na(control_index$index_dt)), "\n")
cat("outcomes:\n")
print(
  control_tidy %>%
    distinct(pat_record_id, outcome_status) %>%
    count(outcome_status, name = "n_patients", .drop = FALSE)
)
cat("VIS fill rules:\n")
print(control_tidy %>% count(VIS2020_fill_rule, name = "n_rows"))

if (isTRUE(save_rds)) {
  saveRDS(control_tidy, control_out_rds)
}

if (isTRUE(save_xlsx)) {
  openxlsx::write.xlsx(
    control_tidy,
    control_out_xlsx,
    asTable = TRUE,
    overwrite = TRUE
  )
}
