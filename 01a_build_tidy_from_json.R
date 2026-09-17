# 2Reg ETL 01a: extract structured tables from JSON
# Цель:
# - прочитать accepted registry JSON + справочники doctors/organizations;
# - извлечь patient/timepoint и hemoperfusion rows;
# - сохранить промежуточные RDS для отдельного postprocessing-этапа.

# 1. Packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(lubridate)
  library(jsonlite)
})

# 2. Paths ----------------------------------------------------------------

DATA_DIR <- "data"
INTERMEDIATE_DIR <- file.path(DATA_DIR, "intermediate")
REPORT_TZ <- "UTC"

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(INTERMEDIATE_DIR, showWarnings = FALSE, recursive = TRUE)

source(file.path("R", "etl_helpers.R"))

registry_file <- pick_path(c(
  file.path(DATA_DIR, "registry2-accepted-records.json"),
  file.path("/mnt/data", DATA_DIR, "registry2-accepted-records.json"),
  "registry2-accepted-records.json"
))

doctors_file <- pick_path(c(
  file.path(DATA_DIR, "doctors.json"),
  file.path("/mnt/data", DATA_DIR, "doctors.json"),
  "doctors.json"
))

orgs_file <- pick_path(c(
  file.path(DATA_DIR, "organizations.json"),
  file.path("/mnt/data", DATA_DIR, "organizations.json"),
  "organizations.json"
))

patients_extracted_rds <- file.path(INTERMEDIATE_DIR, "patients_extracted.rds")
ha_extracted_rds <- file.path(INTERMEDIATE_DIR, "HA_extracted.rds")
registry_ids_rds <- file.path(INTERMEDIATE_DIR, "accepted_registry_ids.rds")

# 3. Checkbox mappings for current JSON schema ----------------------------

diag_checkbox_map <- c(
  "Злокачественное новообразование" = "diag_bin_malignancy",
  "Инфекция мочевыводящих путей" = "diag_bin_urinary_tract_infection",
  "Инфекция мягких тканей" = "diag_bin_soft_tissue_infection",
  "Менингит" = "diag_bin_meningitis",
  "Ожоги/термоингаляционная травма" = "diag_bin_burn_inhalation_injury",
  "Остеомиелит" = "diag_bin_osteomyelitis",
  "Острый панкреатит" = "diag_bin_acute_pancreatitis",
  "Перитонит" = "diag_bin_peritonitis",
  "Пиелонефрит" = "diag_bin_pyelonephritis",
  "Пневмония" = "diag_bin_pneumonia",
  "Состояние после массивного хирургического вмешательства" = "diag_bin_post_major_surgery",
  "Травма" = "diag_bin_trauma",
  "Холангит и инфекция желчевыводящих путей" = "diag_bin_cholangitis_biliary_infection",
  "Экзогенная интоксикация" = "diag_bin_exogenous_intoxication",
  "Эндокардит" = "diag_bin_endocarditis",
  "Другое (указать)" = "diag_bin_other"
)

diag_checkbox_aliases <- c(
  "Другое" = "Другое (указать)",
  "После массивного хирургического вмешательства" = "Состояние после массивного хирургического вмешательства"
)

# "ПоказанияГС" (новая форма с selected/other)
ind_checkbox_map <- c(
  "Бактериемия (подтвержденная или подозреваемая)" = "ind_bin_bacteremia",
  "Рабдомиолиз" = "ind_bin_rhabdomyolysis",
  "Сепсис (подтвержденный или подозреваемый)" = "ind_bin_sepsis",
  "Ишемия-реперфузионное повреждение" = "ind_bin_ischemia_reperfusion_injury",
  "Септический шок" = "ind_bin_septic_shock",
  "Массивный некроз тканей" = "ind_bin_massive_tissue_necrosis",
  "Синдром капиллярной утечки" = "ind_bin_capillary_leak_syndrome",
  "Острая печёночная недостаточность" = "ind_bin_acute_liver_failure",
  "Синдром полиорганной недостаточности (СПОН)" = "ind_bin_multiorgan_failure",
  "Острая почечная недостаточность" = "ind_bin_acute_kidney_injury",
  "Повышенный уровень лактата" = "ind_bin_elevated_lactate",
  "Синдром системного воспалительного ответа (ССВР)" = "ind_bin_systemic_inflammatory_response",
  "Повышенный уровень маркеров воспаления" = "ind_bin_elevated_inflammatory_markers",
  "Эндогенная интоксикация" = "ind_bin_endogenous_intoxication",
  "Другое" = "ind_bin_other"
)

ind_checkbox_aliases <- c(
  "Другое (указать)" = "Другое",
  "Ишемия–реперфузионное повреждение" = "Ишемия-реперфузионное повреждение",
  "Синдром полиорганной недостаточности" = "Синдром полиорганной недостаточности (СПОН)",
  "Синдром системного воспалительного ответа" = "Синдром системного воспалительного ответа (ССВР)"
)

# Исторический переход формы:
# раньше часть checkbox-вариантов, которые сейчас относятся к основному диагнозу,
# могла приходить внутри блока "ПоказанияГС". Не создаём для них ind_bin_*;
# при checkbox-разборе переносим их в диагнозные признаки / raw selected.
legacy_indication_as_diag <- c(
  "Изменения показателей биохимического анализа крови" = "Другое (указать)",
  "После массивного хирургического вмешательства" = "Состояние после массивного хирургического вмешательства",
  "Экзогенная интоксикация" = "Экзогенная интоксикация"
)


# 4. Read inputs ----------------------------------------------------------

doctors <- read_json_list(doctors_file)
orgs    <- read_json_list(orgs_file)
reg_raw <- read_json_list(registry_file)

# 5. Lookup tables for author and organization ----------------------------

doctors_lu <- purrr::map_dfr(doctors, function(x) {
  tibble::tibble(
    createdBy = chr1(x$id),
    organizationId = chr1(x$organizationId),
    record_author = stringr::str_squish(paste(chr1(x$lastName), chr1(x$firstName), chr1(x$middleName)))
  )
})

orgs_lu <- purrr::map_dfr(orgs, function(x) {
  tibble::tibble(
    organizationId = chr1(x$id),
    organization   = chr1(x$shortName)
  )
})

# 6. Registry base table --------------------------------------------------

reg_tbl <- purrr::map_dfr(reg_raw, function(r) {
  tibble::tibble(
    pat_record_id = chr1(r$id),
    registryId    = chr1(r$registryId),
    status        = chr1(r$status),
    createdBy     = chr1(r$createdBy),
    createdAt     = chr1(r$createdAt),
    updatedAt     = chr1(r$updatedAt),
    data          = list(r$data)
  )
}) %>%
  dplyr::filter(registryId == "registry2", status == "accepted") %>%
  dplyr::left_join(doctors_lu, by = "createdBy") %>%
  dplyr::left_join(orgs_lu, by = "organizationId")


# Универсальное извлечение date/time из полей JSON ------------------------
#
# Поддерживает:
# - scalar datetime;
# - вложенный объект с date/time;
# - объект с value/значение;
# - поиск полей по смысловому шаблону имени.

datetime_value <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)

  if (!is.list(x)) {
    return(clean_na(as.character(x)[1]))
  }

  nms <- names(x)
  if (is.null(nms)) return(NA_character_)

  nms_lower <- str_to_lower(nms)

  date_nm <- nms[
    str_detect(nms_lower, "дата|date") &
      !str_detect(nms_lower, "время|time")
  ][1]

  time_nm <- nms[
    str_detect(nms_lower, "время|time") &
      !str_detect(nms_lower, "дата|date")
  ][1]

  if (!is.na(date_nm)) {
    date_value <- chr0(pluck0(x, date_nm, .default = NA_character_))
    time_value <- if (!is.na(time_nm)) {
      chr0(pluck0(x, time_nm, .default = NA_character_))
    } else {
      NA_character_
    }

    combined <- combine_date_time(date_value, time_value)
    if (!is.na(combined)) return(combined)
  }

  value_nm <- nms[str_detect(nms_lower, "^value$|значение")][1]
  if (!is.na(value_nm)) {
    return(chr0(pluck0(x, value_nm, .default = NA_character_)))
  }

  NA_character_
}

extract_datetime_named <- function(
  x,
  direct_keys,
  scope_regex
) {
  if (is.null(x) || length(x) == 0) return(NA_character_)

  direct_values <- purrr::map_chr(
    direct_keys,
    ~ datetime_value(pluck0(x, .x, .default = NULL))
  )

  direct_values <- direct_values[
    !is.na(direct_values) &
      purrr::map_lgl(direct_values, ~ !is.na(parse_dt(.x, tz = "UTC")))
  ]

  if (length(direct_values)) return(direct_values[1])

  nms <- names(x)
  if (is.null(nms)) return(NA_character_)

  nms_lower <- str_to_lower(nms)
  scope_idx <- str_detect(nms_lower, scope_regex)

  scope_names <- nms[scope_idx]
  scope_names_lower <- nms_lower[scope_idx]

  if (!length(scope_names)) return(NA_character_)

  date_idx <- str_detect(
    scope_names_lower,
    "(_|^)(дата|date)$|дата$|date$"
  )
  time_idx <- str_detect(
    scope_names_lower,
    "(_|^)(время|time)$|время$|time$"
  )

  if (any(date_idx)) {
    date_value <- datetime_value(
      pluck0(x, scope_names[which(date_idx)[1]], .default = NULL)
    )
    time_value <- if (any(time_idx)) {
      datetime_value(
        pluck0(x, scope_names[which(time_idx)[1]], .default = NULL)
      )
    } else {
      NA_character_
    }

    combined <- combine_date_time(date_value, time_value)
    if (!is.na(combined) && !is.na(parse_dt(combined, tz = "UTC"))) {
      return(combined)
    }
  }

  candidate_idx <- str_detect(
    scope_names_lower,
    "дат|врем|date|time|datetime|timestamp|at$"
  )

  candidate_names <- scope_names[candidate_idx]
  if (!length(candidate_names)) return(NA_character_)

  candidate_values <- purrr::map_chr(
    candidate_names,
    ~ datetime_value(pluck0(x, .x, .default = NULL))
  )

  parseable <- purrr::map_lgl(
    candidate_values,
    ~ !is.na(.x) && !is.na(parse_dt(.x, tz = "UTC"))
  )

  hit <- candidate_values[parseable][1]
  if (is.na(hit)) NA_character_ else hit
}


# 7. Build patients_tidy --------------------------------------------------

# Главная функция извлечения данных пациента.
# На входе: одна запись реестра (включая вложенный блок data).
# На выходе: tibble с одной или несколькими строками пациента по временным точкам.
extract_patient_rows <- function(pat_record_id, record_author, organization, createdAt, updatedAt, data) {
  d <- data
  
  # static / patient-level
  # сведения, которые относятся к пациенту в целом и не меняются от таймпойнта к таймпойнту.
  outcome_block <- pluck0(d, "ИсходНаблюдения", .default = list())
  organ_block   <- pluck0(d, "ПоддержкаОрганнойДисфункции", .default = list())
  vpr_block   <- pluck0(organ_block, "Вазопрессоры", .default = list())
  mv_block    <- pluck0(organ_block, "ИВЛ", .default = list())
  rrt_block   <- pluck0(organ_block, "ЗПТ", .default = list())
  ecmo_block  <- pluck0(organ_block, "ЭКМО", .default = list())
  
  diag_block <- pluck0(d, "ОсновнойДиагноз", .default = NULL)
  ind_block  <- pluck0(d, "ПоказанияГС", .default = NULL)

  legacy_diag_from_ind <- checkbox_selected(ind_block)
  legacy_diag_from_ind <- legacy_diag_from_ind[legacy_diag_from_ind %in% names(legacy_indication_as_diag)]
  legacy_diag_selected <- unname(legacy_indication_as_diag[legacy_diag_from_ind])
  legacy_diag_other <- legacy_diag_from_ind[legacy_diag_selected == "Другое (указать)"]

  diag_block_for_checkbox <- checkbox_with_extra_selected(
    diag_block,
    extra_selected = legacy_diag_selected,
    extra_other    = legacy_diag_other
  )
  ind_block_for_checkbox <- checkbox_drop_selected(ind_block, names(legacy_indication_as_diag))

  diag_cols <- checkbox_cols(
    x = diag_block_for_checkbox,
    mapping = diag_checkbox_map,
    aliases = diag_checkbox_aliases,
    other_col = "diag_other_text",
    raw_col   = "diag_selected_raw"
  )

  ind_cols <- checkbox_cols(
    x = ind_block_for_checkbox,
    mapping = ind_checkbox_map,
    aliases = ind_checkbox_aliases,
    other_col = "ind_other_text",
    raw_col   = "ind_selected_raw"
  )

  static <- tibble::tibble(
    pat_record_id = chr0(pat_record_id),
    record_author = chr0(record_author),
    organization  = chr0(organization),
    
    # Даты записи
    record_date_created_raw = createdAt,
    record_date_created_dt  = parse_iso_utc(createdAt),
    record_date_created     = fmt_any_dmy_hm(createdAt),
    
    record_date_updated_raw = updatedAt,
    record_date_updated_dt  = parse_iso_utc(updatedAt),
    record_date_updated     = fmt_any_dmy_hm(updatedAt),
    
    # Базовые сведения / анамнез
    admission_date          = chr0(pluck0(d, "ДатаГоспитализации", .default = NA_character_)),
    diagnoses               = dplyr::coalesce(
      chr_scalar0(pluck0(d, "Диагнозы", .default = NA_character_)),
      checkbox_collapse(diag_block_for_checkbox, aliases = diag_checkbox_aliases)
    ),
    HA_indications          = dplyr::coalesce(
      chr_scalar0(pluck0(d, "ПоказанияГС", .default = NA_character_)),
      checkbox_collapse(ind_block_for_checkbox, aliases = ind_checkbox_aliases)
    ),
    !!!diag_cols,
    !!!ind_cols,
    ICU_6_month_history     = chr0(pluck0(d, "ПребываниеВОРИТВПоследние6Мес", .default = NA_character_)),
    from_other_ICU_transfer = chr0(pluck0(d, "ПереводИзДругогоОРИТ", .default = NA_character_)),
    
    age_yr = num0(pluck0(d, "ВозрастЛет", .default = NA)),
    age_m  = num0(pluck0(d, "ВозрастМесяцев", .default = NA)),
    sex    = chr0(pluck0(d, "Пол", .default = NA_character_)),
    BMI    = num0(pluck0(d, "ИМТ", .default = NA)),
    
    charlson_index = num0(pluck0(d, "ИндексЧарлсон", .default = NA)),
    
    # Исходные поля посевов по грам-окраске
    pat_gram_positive = chr0(norm_culture(pluck0(d, "Грамположительные", .default = NA_character_))),
    pat_gram_negative = chr0(norm_culture(pluck0(d, "Грамотрицательные", .default = NA_character_))),
    pat_bacteremia    = chr0(norm_culture(pluck0(d, "Бактериемия", .default = NA_character_))),
    pat_fungal        = chr0(norm_culture(pluck0(d, "Грибы", .default = NA_character_))),
    
    # Исходы
    outcome = chr0(pluck0(outcome_block, "Исход", .default = NA_character_)),
    HA_efficency_CGI_E = chr0(pluck0(outcome_block, "ШкалаЭффективностиГемосорбции", .default = NA_character_)),
    
    # Даты ICU / антибиотики
    # ICU date/time поддерживает старую split-схему и новые единые/nested поля.
    ICU_in_datetime = extract_datetime_named(
      d,
      direct_keys = c(
        "ДатаВремяПоступленияОРИТ",
        "датаВремяПоступленияОРИТ",
        "ДатаВремяПоступленияВОРИТ",
        "датаВремяПоступленияВОРИТ",
        "ICUAdmissionDateTime",
        "icuAdmissionDateTime"
      ),
      scope_regex = "поступ.*орит|поступ.*ворит|орит.*поступ|ворит.*поступ|icu.*admi|admi.*icu"
    ),
    ICU_out_or_death_date = extract_datetime_named(
      outcome_block,
      direct_keys = c(
        "ДатаВремяВыпискиОРИТСмерти",
        "датаВремяВыпискиОРИТСмерти",
        "ДатаВремяВыпискиОРИТ_Смерти",
        "датаВремяВыпискиОРИТ_Смерти",
        "ICUDischargeOrDeathDateTime",
        "icuDischargeOrDeathDateTime"
      ),
      scope_regex = "выписк.*орит|орит.*выписк|смерт|icu.*discharg|discharg.*icu|death"
    ),
    antibiotics_start_datetime = fmt_any_dmy_hm(pluck0(d, "ДатаВремяНачалаАнтибиотикотерапии", .default = NA_character_)),
    
    # Поддержка органной дисфункции (Да/Нет + детализация)
    vasopressors_if_used    = yn_ru(pluck0(vpr_block, "применялась", .default = NA)),
    vasopressors_start      = fmt_any_dmy_hm(pluck0(vpr_block, "датаВремяНачала", .default = NA_character_)),
    vasopressors_resolution = fmt_any_dmy_hm(pluck0(vpr_block, "ДатаОкончательногоРазрешения", .default = NA_character_)),
    vasopressors_breaks     = yn_ru(pluck0(vpr_block, "наличиеПерерыва", .default = NA)),
    vasopressors_dur_days   = dur_days(vpr_block),
    vasopressors_dur_hours  = dur_hours(vpr_block),
    
    MV_if_used    = yn_ru(pluck0(mv_block, "применялась", .default = NA)),
    MV_start      = fmt_any_dmy_hm(pluck0(mv_block, "датаВремяНачала", .default = NA_character_)),
    MV_resolution = fmt_any_dmy_hm(pluck0(mv_block, "ДатаОкончательногоРазрешения", .default = NA_character_)),
    MV_breaks     = yn_ru(pluck0(mv_block, "наличиеПерерыва", .default = NA)),
    MV_dur_days   = dur_days(mv_block),
    MV_dur_hours  = dur_hours(mv_block),
    
    RRT_if_used    = yn_ru(pluck0(rrt_block, "применялась", .default = NA)),
    RRT_start      = fmt_any_dmy_hm(pluck0(rrt_block, "датаВремяНачала", .default = NA_character_)),
    RRT_resolution = fmt_any_dmy_hm(pluck0(rrt_block, "ДатаОкончательногоРазрешения", .default = NA_character_)),
    RRT_breaks     = yn_ru(pluck0(rrt_block, "наличиеПерерыва", .default = NA)),
    RRT_dur_days   = dur_days(rrt_block),
    RRT_dur_hours  = dur_hours(rrt_block),
    
    ECMO_if_used    = yn_ru(pluck0(ecmo_block, "применялась", .default = NA)),
    ECMO_start      = fmt_any_dmy_hm(pluck0(ecmo_block, "датаВремяНачала", .default = NA_character_)),
    ECMO_resolution = fmt_any_dmy_hm(pluck0(ecmo_block, "ДатаОкончательногоРазрешения", .default = NA_character_)),
    ECMO_breaks     = yn_ru(pluck0(ecmo_block, "наличиеПерерыва", .default = NA)),
    ECMO_dur_days   = dur_days(ecmo_block),
    ECMO_dur_hours  = dur_hours(ecmo_block)
  )
  
  # собираем общий список всех точек, которые есть у пациента.
  tp_names <- unique(c(
    names(pluck0(d, "КлиническаяОценка", .default = list())),
    names(pluck0(d, "КлеткиКрови", .default = list())),
    names(pluck0(d, "БиохимияКрови", .default = list()))
  ))
  tp_names <- tp_names[grepl("^Точка", tp_names)]
  if (!length(tp_names)) {
    return(static %>% mutate(timepoint = NA_character_))
  }
  
  # Для каждой найденной временной точки достаём клинические показатели,
  # клетки крови и биохимию, после чего собираем одну строку
  dyn <- purrr::map_dfr(tp_names, function(tp_ru) {
    tp <- tp_code(tp_ru)
    
    cl <- pluck0(d, "КлиническаяОценка", tp_ru, .default = list())
    kc <- pluck0(d, "КлеткиКрови", tp_ru, .default = list())
    bi <- pluck0(d, "БиохимияКрови", tp_ru, .default = list())
    
    # биохимия: значение (numeric) + единицы (character) отдельными колонками
    lactate_obj <- pluck0(bi, "лактат", .default = NULL)
    creat_obj   <- pluck0(bi, "креатинин", .default = NULL)
    alb_obj     <- pluck0(bi, "альбумин", .default = NULL)
    pct_obj     <- pluck0(bi, "прокальцитонин", .default = NULL)
    crp_obj     <- pluck0(bi, "С_реактивный_белок", .default = pluck0(bi, "С_реактивный белок", .default = NULL))
    dd_obj      <- pluck0(bi, "Д_Димер", .default = NULL)
    fib_obj     <- pluck0(bi, "Фибриноген", .default = NULL)
    bili_obj    <- pluck0(bi, "Общий_билирубин", .default = NULL)
    
    # Оксигенация:
    # старая структура: pao2FiO2Params$fiO2 / paO2;
    # новая структура: oiParams$FiO2 / PaO2 / SpO2
    # и дублирующий блок OxygenationIndex.
    
    FiO2_raw <- dplyr::coalesce(
      num0(pluck0(cl, "oiParams", "FiO2", .default = NA)),
      num0(pluck0(cl, "OxygenationIndex", "FiO2", .default = NA)),
      num0(pluck0(cl, "pao2FiO2Params", "fiO2", .default = NA))
    )
    
    PaO2_raw <- dplyr::coalesce(
      num0(pluck0(cl, "oiParams", "PaO2", .default = NA)),
      num0(pluck0(cl, "OxygenationIndex", "PaO2", .default = NA)),
      num0(pluck0(cl, "pao2FiO2Params", "paO2", .default = NA))
    )
    
    SpO2_raw <- dplyr::coalesce(
      num0(pluck0(cl, "oiParams", "SpO2", .default = NA)),
      num0(pluck0(cl, "OxygenationIndex", "SpO2", .default = NA))
    )
    
    PaFiO2_from_raw <- dplyr::if_else(
      !is.na(PaO2_raw) & !is.na(FiO2_raw) & FiO2_raw > 0,
      PaO2_raw / (FiO2_raw / 100),
      NA_real_
    )
    
    SpFiO2_from_raw <- dplyr::if_else(
      !is.na(SpO2_raw) & !is.na(FiO2_raw) & FiO2_raw > 0,
      SpO2_raw / (FiO2_raw / 100),
      NA_real_
    )
    
    tibble::tibble(
      timepoint = tp,
      
      clin_assess_date = fmt_any_dmy_hm(pluck0(cl, "датаПроведения", .default = NA_character_)),
      blood_cells_date = fmt_any_dmy_hm(pluck0(kc, "датаПробы", .default = NA_character_)),
      blood_bio_date   = fmt_any_dmy_hm(pluck0(bi, "датаПробы", .default = NA_character_)),
      
      # клиническая оценка
      SOFA    = num0(pluck0(cl, "балSOFA", .default = NA)),
      VIS2020 = num0(pluck0(cl, "индексVIS2020", .default = NA)),
      avg_BP  = num0(pluck0(cl, "среднееАД", .default = NA)),
      INR     = num0(pluck0(cl, "МНО", .default = NA)),
      FiO2 = FiO2_raw,
      PaO2 = PaO2_raw,
      SpO2 = SpO2_raw,
      
      PaFiO2 = dplyr::coalesce(
        num0(pluck0(cl, "PaO2_FiO2", .default = NA)),
        PaFiO2_from_raw
      ),
      
      SpFiO2 = dplyr::coalesce(
        num0(pluck0(cl, "SpO2_FiO2", .default = NA)),
        SpFiO2_from_raw
      ),
      
      # клетки крови
      leucocytes   = num0(pluck0(kc, "лейкоциты", .default = NA)),
      neutrophils  = num0(pluck0(kc, "нейтрофилы", .default = NA)),
      lymphocytes  = num0(pluck0(kc, "лимфоциты", .default = NA)),
      thrombocytes = num0(pluck0(kc, "тромбоциты", .default = NA)),
      
      # биохимия
      lactate         = lab_num(lactate_obj),
      lactate_unit    = lab_unit(lactate_obj),
      
      creatinine      = lab_num(creat_obj),
      creatinine_unit = lab_unit(creat_obj),
      
      albumin         = lab_num(alb_obj),
      albumin_unit    = lab_unit(alb_obj),
      
      procalcitonin = lab_num(pct_obj, impute_ineq = TRUE),
      procalcitonin_unit = lab_unit(pct_obj),
      
      C_react_protein      = lab_num(crp_obj),
      C_react_protein_unit = lab_unit(crp_obj),
      
      D_dimer      = lab_num(dd_obj),
      D_dimer_unit = lab_unit(dd_obj),
      
      fibrinogen      = lab_num(fib_obj),
      fibrinogen_unit = lab_unit(fib_obj),
      
      bilirubin_total      = lab_num(bili_obj),
      bilirubin_total_unit = lab_unit(bili_obj)
    )
  })
  
  # Повторяем статические данные рядом с каждой динамической строкой.
  out <- dplyr::bind_cols(
    dyn,
    static[rep(1, nrow(dyn)), , drop = FALSE]
  )
  
  out
}

# Применяем extract_patient_rows() ко всем accepted-записям реестра
# и склеиваем результат в одну большую таблицу patients_tidy.
patients_tidy <- purrr::pmap_dfr(
  reg_tbl %>% dplyr::select(pat_record_id, record_author, organization, createdAt, updatedAt, data),
  extract_patient_rows
) %>%
  dplyr::mutate(across(where(is.character), clean_na))


# Жёсткий QC дат ICU: эти поля нужны для интервалов, статуса и survival.
if (nrow(patients_tidy) > 0 && all(is.na(patients_tidy$ICU_in_datetime))) {
  first_icu_fields <- reg_tbl %>%
    mutate(
      .candidate_fields = purrr::map(
        data,
        ~ {
          nms <- names(.x)
          if (is.null(nms)) return(character(0))
          nms[str_detect(str_to_lower(nms), "орит|ворит|icu")]
        }
      )
    ) %>%
    filter(lengths(.candidate_fields) > 0) %>%
    slice(1) %>%
    pull(.candidate_fields)

  first_icu_fields_text <- if (length(first_icu_fields)) {
    paste(first_icu_fields[[1]], collapse = ", ")
  } else {
    "<поля-кандидаты не найдены>"
  }

  stop(
    "Не удалось извлечь дату/время поступления в ОРИТ ни для одной записи. ",
    "Поля-кандидаты первой подходящей записи: ",
    first_icu_fields_text
  )
}

terminal_outcomes <- c("Умер", "Выписан из ОРИТ")

if (
  any(patients_tidy$outcome %in% terminal_outcomes, na.rm = TRUE) &&
    all(is.na(patients_tidy$ICU_out_or_death_date))
) {
  first_outcome_fields <- reg_tbl %>%
    mutate(
      .outcome = purrr::map(
        data,
        ~ pluck0(.x, "ИсходНаблюдения", .default = list())
      ),
      .candidate_fields = purrr::map(
        .outcome,
        ~ {
          nms <- names(.x)
          if (is.null(nms)) return(character(0))
          nms[str_detect(str_to_lower(nms), "выписк|смерт|death|discharg")]
        }
      )
    ) %>%
    filter(lengths(.candidate_fields) > 0) %>%
    slice(1) %>%
    pull(.candidate_fields)

  first_outcome_fields_text <- if (length(first_outcome_fields)) {
    paste(first_outcome_fields[[1]], collapse = ", ")
  } else {
    "<поля-кандидаты не найдены>"
  }

  stop(
    "Не удалось извлечь дату/время выписки из ОРИТ/смерти ни для одного пациента ",
    "с терминальным исходом. Поля-кандидаты первой подходящей записи: ",
    first_outcome_fields_text
  )
}


# фиксируем порядок таймпойнтов 
tp_levels_core <- c("m12h_0h", "48h_72h", "4d_6d", "7d_10d")
tp_levels <- c(tp_levels_core, setdiff(sort(unique(patients_tidy$timepoint)), tp_levels_core))

patients_tidy <- patients_tidy %>%
  mutate(timepoint = factor(timepoint, levels = tp_levels, ordered = TRUE))
  
# 8. Build HA_tidy --------------------------------------------------------

# Извлечение даты/времени начала ГС с поддержкой нескольких схем JSON.
# Приоритет: старые split-поля, единое datetime-поле, вложенный date/time,
# затем осторожный поиск start/datetime-поля внутри самой процедуры.
extract_ha_start_datetime <- function(p) {
  old_split <- combine_date_time(
    pluck0(p, "ДатаВремяНачалаГС_Дата",  .default = NA_character_),
    pluck0(p, "ДатаВремяНачалаГС_Время", .default = NA_character_)
  )
  if (!is.na(old_split)) return(old_split)

  direct_keys <- c(
    "ДатаВремяНачалаГС",
    "датаВремяНачалаГС",
    "ДатаВремяНачала",
    "датаВремяНачала",
    "StartDateTime",
    "startDateTime",
    "startedAt"
  )

  direct_values <- purrr::map_chr(
    direct_keys,
    ~ datetime_value(pluck0(p, .x, .default = NULL))
  )

  direct_hit <- direct_values[!is.na(direct_values)][1]
  if (!is.na(direct_hit)) return(direct_hit)

  proc_names <- names(p)
  if (is.null(proc_names)) return(NA_character_)

  proc_names_lower <- str_to_lower(proc_names)
  start_scope <- str_detect(proc_names_lower, "начал|start")

  date_names <- proc_names[
    start_scope &
      str_detect(proc_names_lower, "дат|date") &
      !str_detect(proc_names_lower, "врем|time")
  ]

  time_names <- proc_names[
    start_scope &
      str_detect(proc_names_lower, "врем|time") &
      !str_detect(proc_names_lower, "дат|date")
  ]

  if (length(date_names)) {
    date_value <- datetime_value(pluck0(p, date_names[1], .default = NULL))
    time_value <- if (length(time_names)) {
      datetime_value(pluck0(p, time_names[1], .default = NULL))
    } else {
      NA_character_
    }

    split_hit <- combine_date_time(date_value, time_value)
    if (!is.na(split_hit)) return(split_hit)
  }

  candidate_names <- proc_names[
    start_scope &
      str_detect(proc_names_lower, "дат|врем|date|time|at$")
  ]

  if (!length(candidate_names)) return(NA_character_)

  candidate_values <- purrr::map_chr(
    candidate_names,
    ~ datetime_value(pluck0(p, .x, .default = NULL))
  )

  is_parseable <- purrr::map_lgl(
    candidate_values,
    ~ !is.na(.x) && !is.na(parse_dt(.x, tz = "UTC"))
  )

  hit <- candidate_values[is_parseable][1]
  if (is.na(hit)) NA_character_ else hit
}

# Извлекает все процедуры гемосорбции одного пациента из блока data.
extract_ha_rows <- function(pat_record_id, data) {
  procs <- pluck0(data, "ПроцедурыГемосорбции", .default = list())
  if (is.null(procs) || !length(procs)) return(tibble::tibble())

  ha0 <- purrr::map_dfr(procs, function(p) {
    tibble::tibble(
      pat_record_id = pat_record_id,
      HA_cartrige_type = pluck0(p, "DeviceType", .default = NA_character_),
      HA_serial_number = pluck0(p, "СерийныйНомерУстройства", .default = NA_character_),
      HA_start_datetime = extract_ha_start_datetime(p),
      HA_duration = pluck0(p, "ДлительностьГС", .default = NA_character_),
      HA_avg_blood_flow = pluck0(p, "СредняяСкоростьПотока", .default = NA),
      HA_anticoagulation = pluck0(p, "Антикоагуляция", .default = NA_character_),
      HA_anticoagulation_other = pluck0(p, "АнтикоагуляцияДругое", .default = NA_character_),
      HA_other_methods = pluck0(p, "КомбинацияСДругимиЭкстракорпоральнымиМетодами", .default = NA_character_),
      HA_adverse_effects = pluck0(p, "НежелательныеЯвления", .default = NA_character_)
    )
  })

  ha0 %>%
    mutate(.start_dt_tmp = parse_dt(HA_start_datetime, tz = "UTC")) %>%
    arrange(.start_dt_tmp) %>%
    mutate(HA_parameter = paste0("Гемоперфузия ", dplyr::row_number())) %>%
    select(-.start_dt_tmp)
}

HA_tidy <- purrr::pmap_dfr(
  reg_tbl %>% dplyr::select(pat_record_id, data),
  extract_ha_rows
) %>%
  mutate(
    HA_avg_blood_flow = to_num(HA_avg_blood_flow),
    # Сохраняем строку JSON (ЧЧ:ММ) до перевода в десятичные часы.
    HA_duration_raw = as.character(HA_duration),
    HA_duration = round(dur_to_hours(HA_duration_raw), 2)
  )

# Жёсткий QC: если процедуры есть, но дата старта не извлечена ни разу,
# прекращаем ETL вместо создания пустых first_sorption_dt/clinical windows.
if (nrow(HA_tidy) > 0 && all(is.na(HA_tidy$HA_start_datetime))) {
  first_proc_fields <- reg_tbl %>%
    mutate(
      .procs = purrr::map(
        data,
        ~ pluck0(.x, "ПроцедурыГемосорбции", .default = list())
      )
    ) %>%
    filter(lengths(.procs) > 0) %>%
    slice(1) %>%
    pull(.procs) %>%
    purrr::pluck(1, 1) %>%
    names()

  stop(
    "Не удалось извлечь дату/время начала ни для одной процедуры ГС. ",
    "Поля первой процедуры: ",
    paste(first_proc_fields, collapse = ", ")
  )
}

# Добавляем количество процедур с разными картриджами по каждому пациенту
HA_counts <- HA_tidy %>%
  mutate(.ct = str_to_upper(norm_txt(HA_cartrige_type))) %>%
  group_by(pat_record_id) %>%
  summarise(
    CT_count = sum(.ct == "CT", na.rm = TRUE),
    LPS_count = sum(.ct == "LPS", na.rm = TRUE),
    .groups = "drop"
  )

HA_tidy <- HA_tidy %>%
  left_join(HA_counts, by = "pat_record_id") %>%
  mutate(
    CT_count = if_else(is.na(CT_count), 0L, as.integer(CT_count)),
    LPS_count = if_else(is.na(LPS_count), 0L, as.integer(LPS_count))
  )


# 9. Save intermediate outputs -------------------------------------------

accepted_registry_ids <- reg_tbl %>%
  transmute(pat_record_id = clean_na(pat_record_id)) %>%
  filter(!is.na(pat_record_id), pat_record_id != "") %>%
  distinct(pat_record_id) %>%
  pull(pat_record_id)

saveRDS(patients_tidy, patients_extracted_rds)
saveRDS(HA_tidy, ha_extracted_rds)
saveRDS(accepted_registry_ids, registry_ids_rds)

cat("patients_extracted rows:", nrow(patients_tidy), "\n")
cat("patients_extracted ids:", n_distinct(patients_tidy$pat_record_id), "\n")
cat("HA_extracted rows:", nrow(HA_tidy), "\n")
cat("HA_extracted ids:", n_distinct(HA_tidy$pat_record_id), "\n")
