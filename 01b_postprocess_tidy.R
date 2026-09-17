# 2Reg ETL 01b: postprocess extracted tidy tables
# Цель:
# - прочитать промежуточные patients_extracted / HA_extracted;
# - нормализовать даты и лабораторные единицы;
# - добавить производные клинические и HA-признаки;
# - выполнить ETL-QC и сохранить финальные RDS/XLSX.
#
# Baseline severity ranks и composite severity index намеренно НЕ входят в ETL.

# 1. Packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(openxlsx)
  library(lubridate)
})

# 2. Paths and flags ------------------------------------------------------

DATA_DIR <- "data"
INTERMEDIATE_DIR <- file.path(DATA_DIR, "intermediate")
REPORT_TZ <- "UTC"

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

source(file.path("R", "etl_helpers.R"))

patients_extracted_rds <- pick_path(c(
  file.path(INTERMEDIATE_DIR, "patients_extracted.rds"),
  file.path("/mnt/data", INTERMEDIATE_DIR, "patients_extracted.rds")
))

ha_extracted_rds <- pick_path(c(
  file.path(INTERMEDIATE_DIR, "HA_extracted.rds"),
  file.path("/mnt/data", INTERMEDIATE_DIR, "HA_extracted.rds")
))

registry_ids_file <- pick_path(c(
  file.path(INTERMEDIATE_DIR, "accepted_registry_ids.rds"),
  file.path("/mnt/data", INTERMEDIATE_DIR, "accepted_registry_ids.rds")
))

patients_indications_file <- optional_path(c(
  file.path(DATA_DIR, "2Reg_patients_indications.xlsx"),
  file.path("/mnt/data", DATA_DIR, "2Reg_patients_indications.xlsx"),
  "2Reg_patients_indications.xlsx"
))

patients_out_xlsx <- file.path(DATA_DIR, "2Reg_patients_tidy.xlsx")
ha_out_xlsx <- file.path(DATA_DIR, "2Reg_HA_tidy.xlsx")

patients_out_rds <- file.path(DATA_DIR, "patients_tidy.rds")
ha_out_rds <- file.path(DATA_DIR, "HA_tidy.rds")

save_xlsx <- TRUE
save_rds <- TRUE

# patients_clinical_window больше не является выходом ETL.
# Аналитика использует исходные timepoint из patients_tidy. Старые файлы,
# оставшиеся от предыдущих прогонов, не обновляются и не должны использоваться.
legacy_clinical_window_files <- c(
  file.path(DATA_DIR, "patients_clinical_window.rds"),
  file.path(DATA_DIR, "2Reg_patients_clinical_window.xlsx")
)

if (any(file.exists(legacy_clinical_window_files))) {
  warning(
    "Найдены устаревшие clinical-window файлы от прежнего ETL: ",
    paste(legacy_clinical_window_files[file.exists(legacy_clinical_window_files)], collapse = ", "),
    ". Они больше не создаются и не используются актуальными аналитическими скриптами."
  )
}

# 3. Read intermediate tables --------------------------------------------

patients_tidy <- readRDS(patients_extracted_rds)
HA_tidy <- readRDS(ha_extracted_rds)
accepted_registry_ids <- readRDS(registry_ids_file)

# 3a. Organ support duration: total hours and raw components --------------
# В JSON общаяДлительность содержит компоненты, а не две альтернативные меры:
# ОбщаяДлительностьСуток + ОбщаяДлительностьЧасов.
# 01a извлекает их в *_dur_days и *_dur_hours. До пересчёта сохраняем *_raw.
# Итог: *_dur_hours = 24 * days_raw + hours_raw; *_dur_days = hours / 24.
# Не используем разность дат начала/окончания: поддержка могла прерываться.
# Пропуск компонента не заменяем нулём. Отсутствие обеих частей также даёт NA.
# При days_raw = 0 и hours_raw >= 24 полное время однозначно (например, 32 ч).
# При days_raw > 0 и hours_raw >= 24 смысл часов неоднозначен: итог NA до QC.
# HA_duration — отдельное поле ЧЧ:ММ, к этой коррекции не относится.
support_duration_total <- function(days_raw, hours_raw) {
  duration_qc <- dplyr::case_when(
    is.na(days_raw) & is.na(hours_raw) ~ "missing",
    is.na(days_raw) | is.na(hours_raw) ~ "incomplete",
    !is.finite(days_raw) | !is.finite(hours_raw) |
      days_raw < 0 | hours_raw < 0 | days_raw != floor(days_raw) ~ "invalid",
    days_raw > 0 & hours_raw >= 24 ~ "ambiguous_hours_ge24",
    days_raw == 0 & hours_raw >= 24 ~ "hours_ge24_days_zero",
    TRUE ~ "ok"
  )
  total_hours <- dplyr::if_else(
    duration_qc %in% c("ok", "hours_ge24_days_zero"),
    24 * days_raw + hours_raw,
    NA_real_
  )
  tibble::tibble(
    total_days = total_hours / 24,
    total_hours = total_hours,
    duration_qc = duration_qc
  )
}

support_duration_qc <- list()
for (support in c("vasopressors", "MV", "RRT", "ECMO")) {
  days_col <- paste0(support, "_dur_days")
  hours_col <- paste0(support, "_dur_hours")
  days_raw_col <- paste0(days_col, "_raw")
  hours_raw_col <- paste0(hours_col, "_raw")
  # Источник — patients_extracted, а не финальный patients_tidy.
  # Уже существующие raw не перезаписываем рассчитанными итогами.
  if (!days_raw_col %in% names(patients_tidy)) {
    if (!days_col %in% names(patients_tidy)) stop("Нет компонента длительности: ", days_col)
    patients_tidy[[days_raw_col]] <- patients_tidy[[days_col]]
  }
  if (!hours_raw_col %in% names(patients_tidy)) {
    if (!hours_col %in% names(patients_tidy)) stop("Нет компонента длительности: ", hours_col)
    patients_tidy[[hours_raw_col]] <- patients_tidy[[hours_col]]
  }
  duration_result <- support_duration_total(
    patients_tidy[[days_raw_col]], patients_tidy[[hours_raw_col]]
  )
  patients_tidy[[days_col]] <- duration_result$total_days
  patients_tidy[[hours_col]] <- duration_result$total_hours
  qc_col <- paste0(support, "_dur_qc")
  patients_tidy[[qc_col]] <- duration_result$duration_qc
  support_duration_qc[[support]] <- patients_tidy %>%
    transmute(pat_record_id, support = support, duration_qc = .data[[qc_col]]) %>%
    distinct() %>%
    count(support, duration_qc, name = "n_patients")
}
support_duration_qc <- bind_rows(support_duration_qc)
message("Длительность поддержки: полные часы и сутки рассчитаны из raw-компонентов.")
print(support_duration_qc)

# 4. Manual diagnosis/indication helper ----------------------------------

manual_bin_to_int <- function(x) {
  x <- clean_na(as.character(x))
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, ",", ".")
  dplyr::case_when(
    x %in% c("1", "1.0", "да", "true", "yes", "y") ~ 1L,
    x %in% c("0", "0.0", "нет", "false", "no", "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

# Универсальная перекодировка полей Да/Нет в настоящий integer 1/0.
# Неизвестные, пустые и нераспознанные значения остаются NA.
yes_no_to_int <- function(x) {
  x <- clean_na(as.character(x))
  x <- stringr::str_to_lower(stringr::str_squish(x))

  dplyr::case_when(
    x %in% c("1", "1.0", "да", "true", "t", "yes", "y") ~ 1L,
    x %in% c("0", "0.0", "нет", "false", "f", "no", "n") ~ 0L,
    TRUE ~ NA_integer_
  )
}

# Наличие содержательного текста: 1 = поле содержит событие/метод,
# 0 = явно указано отсутствие, NA = поле не заполнено или не распознано.
present_text_to_int <- function(x) {
  x <- clean_na(as.character(x))
  x <- stringr::str_to_lower(stringr::str_squish(x))

  dplyr::case_when(
    is.na(x) ~ NA_integer_,
    x %in% c("нет", "no", "false", "0", "-", "—") ~ 0L,
    stringr::str_detect(x, "не было|не примен|отсутств|без особенностей") ~ 0L,
    TRUE ~ 1L
  )
}

# Добавляет ручную разметку старых free-text записей из data/2Reg_patients_indications.xlsx.
# Логика:
# - join только по pat_record_id;
# - diagnoses и HA_indications из JSON не перезаписываем;
# - обновляем только существующие в patients_tidy diag_bin_*/ind_bin_* и *_other_text;
# - если в manual Excel bin-колонки целиком пустые, ничего не затираем нулями.
apply_manual_indications <- function(patients_tidy, path, blank_binary_as_zero = TRUE) {
  if (is.na(path) || !file.exists(path)) {
    message("Manual indications file not found; skip manual diagnosis/indication merge.")
    return(patients_tidy)
  }

  manual_raw <- openxlsx::read.xlsx(path, sheet = 1, detectDates = FALSE)
  manual_raw <- tibble::as_tibble(manual_raw)

  if (!"pat_record_id" %in% names(manual_raw)) {
    stop("В файле ручной разметки нет обязательной колонки pat_record_id: ", path)
  }

  manual_raw <- manual_raw %>%
    dplyr::mutate(pat_record_id = clean_na(.data$pat_record_id)) %>%
    dplyr::filter(!is.na(.data$pat_record_id), .data$pat_record_id != "")

  dup_ids <- manual_raw %>%
    dplyr::count(pat_record_id, name = "n") %>%
    dplyr::filter(n > 1)

  if (nrow(dup_ids) > 0) {
    warning(
      "В ручной разметке есть повторяющиеся pat_record_id; оставлена первая строка для каждого id. Примеры: ",
      paste(utils::head(dup_ids$pat_record_id, 10), collapse = ", ")
    )
    manual_raw <- manual_raw %>% dplyr::distinct(pat_record_id, .keep_all = TRUE)
  }

  manual_bin_cols <- names(manual_raw)[stringr::str_detect(names(manual_raw), "^(diag|ind)_bin_")]
  manual_bin_cols <- intersect(manual_bin_cols, names(patients_tidy))

  manual_text_cols <- intersect(c("diag_other_text", "ind_other_text"), names(manual_raw))
  manual_text_cols <- intersect(manual_text_cols, names(patients_tidy))

  if (length(c(manual_bin_cols, manual_text_cols)) == 0) {
    message("Manual indications file has no columns matching patients_tidy; skip merge.")
    return(patients_tidy)
  }

  manual_tbl <- manual_raw %>%
    dplyr::select(
      dplyr::all_of("pat_record_id"),
      dplyr::all_of(manual_bin_cols),
      dplyr::all_of(manual_text_cols)
    )

  manual_tbl <- manual_tbl %>%
    mutate(across(all_of(manual_bin_cols), manual_bin_to_int))

  has_any_manual_bin <- length(manual_bin_cols) > 0 &&
    any(!is.na(unlist(manual_tbl[manual_bin_cols], use.names = FALSE)))

  if (length(manual_bin_cols) > 0 && isTRUE(blank_binary_as_zero) && has_any_manual_bin) {
    # После ручной проверки пустая bin-ячейка трактуется как 0,
    # но только если в файле вообще есть хотя бы одно заполненное bin-значение.
    manual_tbl <- manual_tbl %>%
      mutate(across(all_of(manual_bin_cols), ~ coalesce(.x, 0L)))
  }

  if (length(manual_text_cols) > 0) {
    manual_tbl <- manual_tbl %>%
      mutate(across(all_of(manual_text_cols), clean_na))
  }

  update_cols <- c(
    if (has_any_manual_bin) manual_bin_cols else character(0),
    manual_text_cols
  )

  if (length(update_cols) == 0) {
    warning(
      "Файл ручной разметки найден, но в bin-колонках нет ни одного 0/1-значения; ",
      "patients_tidy не изменён по ручным диагнозам/показаниям."
    )
    return(patients_tidy)
  }

  manual_tbl <- manual_tbl %>%
    dplyr::select(dplyr::all_of("pat_record_id"), dplyr::all_of(update_cols)) %>%
    dplyr::rename_with(~ paste0(.x, "__manual"), .cols = -dplyr::all_of("pat_record_id"))

  out <- patients_tidy %>%
    left_join(manual_tbl, by = "pat_record_id")

  out <- purrr::reduce(
    update_cols,
    .init = out,
    .f = function(df, nm) {
      df %>%
        mutate(
          !!nm := coalesce(
            .data[[paste0(nm, "__manual")]],
            .data[[nm]]
          )
        )
    }
  ) %>%
    select(-ends_with("__manual"))

  message(
    "Manual indications merged: ",
    dplyr::n_distinct(manual_raw$pat_record_id), " ids in file; ",
    sum(patients_tidy$pat_record_id %in% manual_raw$pat_record_id), " patients_tidy rows matched; ",
    length(update_cols), " columns updated."
  )

  out
}


# 9. Postprocess: datetime columns ----------------------------------------

# parse raw datetime strings into *_dt and keep *_raw

# В dt_map перечислены текстовые колонки с датой/временем и имена,
# под которыми будут храниться их распарсенные версии.
dt_map <- c(
  ICU_in_datetime            = "ICU_in_dt",
  ICU_out_or_death_date      = "ICU_out_or_death_dt",
  antibiotics_start_datetime = "antibiotics_start_dt",
  RRT_start                  = "RRT_start_dt",
  RRT_resolution             = "RRT_resolution_dt",
  MV_start                   = "MV_start_dt",
  MV_resolution              = "MV_resolution_dt",
  clin_assess_date           = "clin_assess_dt",
  blood_cells_date           = "blood_cells_dt",
  blood_bio_date             = "blood_bio_dt"
)

# 1) сохраняем исходные значения в *_raw (только для тех колонок, которые реально есть)
patients_tidy <- patients_tidy %>%
  mutate(across(any_of(names(dt_map)), ~ .x, .names = "{.col}_raw"))

# 2) добавляем распарсенные *_dt
patients_tidy <- purrr::reduce(
  names(dt_map),
  .init = patients_tidy,
  .f = ~ add_dt(.x, .y, dt_map[[.y]], tz = "UTC")
)

# 3) удаляем "оригинальные" колонки, чтобы остались только *_raw и *_dt
patients_tidy <- patients_tidy %>%
  select(-any_of(names(dt_map)))

# HA: datetime + нормализация типа картриджа
HA_tidy <- HA_tidy %>%
  mutate(HA_start_datetime_raw = HA_start_datetime) %>%
  add_dt("HA_start_datetime", "HA_start_dt") %>%
  mutate(
    HA_cartrige_type_std = str_to_upper(str_squish(as.character(HA_cartrige_type))),
    .ha_anticoag = str_to_lower(str_squish(as.character(HA_anticoagulation))),
    .ha_anticoag_other = str_to_lower(str_squish(as.character(HA_anticoagulation_other))),
    HA_anticoagulation_cat = dplyr::case_when(
      str_detect(coalesce(.ha_anticoag_other, ""), "без антикоаг") ~ "Без антикоагуляции",
      str_detect(coalesce(.ha_anticoag, ""), "без антикоаг") ~ "Без антикоагуляции",
      str_detect(coalesce(.ha_anticoag, ""), "цитрат") ~ "Цитрат",
      str_detect(coalesce(.ha_anticoag, ""), "гепарин") ~ "Гепарин",
      str_detect(coalesce(.ha_anticoag, ""), "друг") ~ "Другой",
      !is.na(clean_na(as.character(HA_anticoagulation))) ~ "Другой",
      TRUE ~ NA_character_
    ),
    HA_anticoag_heparin = dplyr::case_when(
      HA_anticoagulation_cat == "Гепарин" ~ 1L,
      !is.na(HA_anticoagulation_cat) ~ 0L,
      TRUE ~ NA_integer_
    ),
    HA_cartridge_LPS_bin = dplyr::case_when(
      HA_cartrige_type_std == "LPS" ~ 1L,
      HA_cartrige_type_std == "CT" ~ 0L,
      TRUE ~ NA_integer_
    ),
    HA_with_RRT_bin = as.integer(str_detect(
      str_to_lower(coalesce(as.character(HA_other_methods), "")),
      "гемодиафильтрац|гемодиализ|гемофильтрац"
    )),
    HA_other_methods_bin = present_text_to_int(HA_other_methods),
    HA_adverse_effects_bin = present_text_to_int(HA_adverse_effects),
    # Для описательной таблицы отдельно фиксируем сам факт записи НЯ:
    # пропуск и явное отрицание трактуются как 0, содержательная запись — как 1.
    HA_adverse_effects_recorded_bin = coalesce(HA_adverse_effects_bin, 0L)
  ) %>%
  # Аналогично: оставляем только parsed-вариант даты старта (HA_start_dt)
  select(-any_of("HA_start_datetime"), -.ha_anticoag, -.ha_anticoag_other)

if (nrow(HA_tidy) > 0 && all(is.na(HA_tidy$HA_start_dt))) {
  stop(
    "В HA_tidy есть процедуры, но HA_start_dt полностью пуст. ",
    "Сначала проверьте извлечение HA_start_datetime в 01a_build_tidy_from_json.R."
  )
}

if (nrow(patients_tidy) > 0 && all(is.na(patients_tidy$ICU_in_dt))) {
  stop(
    "ICU_in_dt полностью пуст после datetime parsing. ",
    "Сначала проверьте извлечение ICU_in_datetime в 01a_build_tidy_from_json.R."
  )
}

terminal_outcomes <- c("Умер", "Выписан из ОРИТ")

if (
  any(patients_tidy$outcome %in% terminal_outcomes, na.rm = TRUE) &&
    all(is.na(patients_tidy$ICU_out_or_death_dt))
) {
  stop(
    "ICU_out_or_death_dt полностью пуст у датасета с терминальными исходами. ",
    "Сначала проверьте извлечение ICU_out_or_death_date в 01a_build_tidy_from_json.R."
  )
}

# Время от поступления в ОРИТ до начала антибиотикотерапии, часы.
# Отрицательные значения допустимы: антибиотики могли начаться до поступления в ОРИТ.
if (all(c("ICU_in_dt", "antibiotics_start_dt") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      ICU_to_antibiotics_dt = round(
        as.numeric(difftime(antibiotics_start_dt, ICU_in_dt, units = "hours")),
        1
      )
    )
}

# 10. Postprocess: simple derived variables -------------------------------

# Основной расчёт VIS2020_0_excl (если VIS2020 есть)
# Для анализа VIS исключаем ВСЕХ пациентов, у кого на baseline (m12h_0h)
# VIS2020 отсутствует или равен 0.
if ("VIS2020" %in% names(patients_tidy)) {
  patients_tidy <- patients_tidy %>%
    group_by(pat_record_id) %>%
    mutate(
      .vis_baseline_bad = any(timepoint == "m12h_0h" & (is.na(VIS2020) | VIS2020 == 0)),
      VIS2020_0_excl = if_else(.vis_baseline_bad, NA_real_, VIS2020)
    ) %>%
    ungroup() %>%
    select(-.vis_baseline_bad)
}

# NLR: один раз на всех timepoint
if (all(c("neutrophils","lymphocytes") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      NLR = if_else(
        !is.na(neutrophils) & !is.na(lymphocytes) & lymphocytes > 0,
        neutrophils / lymphocytes,
        NA_real_
      )
    )
}

# PaFiO2_calc: если PaFiO2 отсутствует, но есть SpFiO2,
# восстанавливаем PaFiO2 по линейной модели PaFiO2 ~ SpFiO2.
# Если PaFiO2 уже есть, сохраняем его как итоговое значение.

pafi_fit <- NULL

if (all(c("PaFiO2", "SpFiO2") %in% names(patients_tidy))) {
  fit_dat <- patients_tidy %>%
    filter(!is.na(PaFiO2), !is.na(SpFiO2))
  
  if (nrow(fit_dat) >= 3 && dplyr::n_distinct(fit_dat$SpFiO2) >= 2) {
    pafi_fit <- lm(PaFiO2 ~ SpFiO2, data = fit_dat)
    
    pred_idx <- is.na(patients_tidy$PaFiO2) & !is.na(patients_tidy$SpFiO2)
    pred_val <- rep(NA_real_, nrow(patients_tidy))
    
    if (any(pred_idx)) {
      pred_val[pred_idx] <- suppressWarnings(
        as.numeric(predict(pafi_fit, newdata = patients_tidy[pred_idx, , drop = FALSE]))
      )
    }
    
    # отрицательные прогнозы физически неинтерпретируемы -> NA
    pred_val[!is.na(pred_val) & pred_val < 0] <- NA_real_
    
    patients_tidy <- patients_tidy %>%
      mutate(
        PaFiO2_calc = case_when(
          !is.na(PaFiO2) ~ round(PaFiO2, 2),
          is.na(PaFiO2) & !is.na(pred_val) ~ round(pred_val, 2),
          TRUE ~ NA_real_
        ),
        PaFiO2_calc_source = case_when(
          !is.na(PaFiO2) ~ "observed",
          is.na(PaFiO2) & !is.na(pred_val) ~ "imputed_from_SpFiO2",
          TRUE ~ "missing"
        )
      )
  } else {
    patients_tidy <- patients_tidy %>%
      mutate(
        PaFiO2_calc = round(PaFiO2, 2),
        PaFiO2_calc_source = case_when(
          !is.na(PaFiO2) ~ "observed",
          TRUE ~ "missing"
        )
      )
  }
}

# 11. Postprocess: lab unit harmonization ---------------------------------

# Фибриноген -> мг/дл
if (all(c("fibrinogen", "fibrinogen_unit") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      fibrinogen_raw = fibrinogen,
      fibrinogen_unit_raw = fibrinogen_unit,
      .fib_unit = unit_norm(fibrinogen_unit),
      .fib_is_gdl = !is.na(.fib_unit) & str_detect(.fib_unit, "^(г|g)/(дл|dl)\\.?$"),
      .fib_is_gl = !is.na(.fib_unit) & str_detect(.fib_unit, "^(г|g)/(л|l)\\.?$"),
      .fib_is_mgl = !is.na(.fib_unit) & str_detect(.fib_unit, "^(мг|mg)/(л|l)\\.?$"),
      .fib_is_mgdl = !is.na(.fib_unit) & str_detect(.fib_unit, "^(мг|mg)/(дл|dl)\\.?$"),

      # В JSON для единицы "г/дл" видны три раздельных масштаба:
      #   < 1     — значения действительно похожи на г/дл;
      #   1..<100 — значения похожи на г/л, но единица выбрана ошибочно;
      #   >= 100  — значения уже похожи на мг/дл.
      # Поэтому приводим все три группы к мг/дл по масштабу значения.
      .fib_gdl_true = .fib_is_gdl & !is.na(fibrinogen) & fibrinogen < 1,
      .fib_gdl_probably_gl = .fib_is_gdl & !is.na(fibrinogen) &
        fibrinogen >= 1 & fibrinogen < 100,
      .fib_gdl_probably_mgdl = .fib_is_gdl & !is.na(fibrinogen) &
        fibrinogen >= 100,

      fibrinogen = case_when(
        .fib_gdl_true ~ fibrinogen * 1000,
        .fib_gdl_probably_gl ~ fibrinogen * 100,
        .fib_gdl_probably_mgdl ~ fibrinogen,
        .fib_is_gl ~ fibrinogen * 100,
        .fib_is_mgl ~ fibrinogen / 10,
        .fib_is_mgdl ~ fibrinogen,
        TRUE ~ NA_real_
      ),
      fibrinogen_unit = case_when(
        .fib_is_gdl | .fib_is_gl | .fib_is_mgl | .fib_is_mgdl ~ "мг/дл",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-starts_with(".fib_"))

  fibrinogen_harmonization_qc <- patients_tidy %>%
    filter(!is.na(fibrinogen_raw)) %>%
    mutate(.unit = unit_norm(fibrinogen_unit_raw)) %>%
    summarise(
      n_total = n(),
      n_gdl_lt1 = sum(
        str_detect(coalesce(.unit, ""), "^(г|g)/(дл|dl)\\.?$") &
          fibrinogen_raw < 1,
        na.rm = TRUE
      ),
      n_gdl_1_to_100 = sum(
        str_detect(coalesce(.unit, ""), "^(г|g)/(дл|dl)\\.?$") &
          fibrinogen_raw >= 1 & fibrinogen_raw < 100,
        na.rm = TRUE
      ),
      n_gdl_ge100 = sum(
        str_detect(coalesce(.unit, ""), "^(г|g)/(дл|dl)\\.?$") &
          fibrinogen_raw >= 100,
        na.rm = TRUE
      ),
      n_unrecognized_unit = sum(is.na(fibrinogen)),
      min_harmonized = min(fibrinogen, na.rm = TRUE),
      max_harmonized = max(fibrinogen, na.rm = TRUE)
    )

  message("Фибриноген: гармонизация единиц выполнена")
  print(fibrinogen_harmonization_qc)
}

# Общий билирубин -> мкмоль/л
if (all(c("bilirubin_total", "bilirubin_total_unit") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      bilirubin_total_raw = bilirubin_total,
      bilirubin_total_unit_raw = bilirubin_total_unit,
      .bili_unit = unit_norm(bilirubin_total_unit),
      .bili_is_mmol = !is.na(.bili_unit) & str_detect(.bili_unit, "^(ммоль|mmol)/(л|l)\\.?$"),
      .bili_is_umol = !is.na(.bili_unit) & str_detect(.bili_unit, "^(мкмоль|umol|µmol)/(л|l)\\.?$"),
      .bili_is_mgdl = !is.na(.bili_unit) & str_detect(.bili_unit, "^(мг|mg)/(дл|dl)\\.?$"),
      .bili_is_mgl = !is.na(.bili_unit) & str_detect(.bili_unit, "^(мг|mg)/(л|l)\\.?$"),

      # В текущем JSON все записи с выбранной единицей "ммоль/л"
      # имеют значения 6–515, то есть по масштабу соответствуют мкмоль/л.
      # Для защиты от будущих корректных значений в ммоль/л:
      #   >= 1  — считаем ошибочно подписанными мкмоль/л;
      #   < 1   — конвертируем как настоящие ммоль/л.
      .bili_mmol_probably_umol = .bili_is_mmol & !is.na(bilirubin_total) &
        bilirubin_total >= 1,
      .bili_mmol_true = .bili_is_mmol & !is.na(bilirubin_total) &
        bilirubin_total < 1,

      bilirubin_total = case_when(
        .bili_mmol_probably_umol ~ bilirubin_total,
        .bili_mmol_true ~ bilirubin_total * 1000,
        .bili_is_umol ~ bilirubin_total,
        .bili_is_mgdl ~ bilirubin_total * 17.104,
        .bili_is_mgl ~ bilirubin_total * 1.7104,
        TRUE ~ NA_real_
      ),
      bilirubin_total_unit = case_when(
        .bili_is_mmol | .bili_is_umol | .bili_is_mgdl | .bili_is_mgl ~ "мкмоль/л",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-starts_with(".bili_"))

  bilirubin_harmonization_qc <- patients_tidy %>%
    filter(!is.na(bilirubin_total_raw)) %>%
    mutate(.unit = unit_norm(bilirubin_total_unit_raw)) %>%
    summarise(
      n_total = n(),
      n_mmol_lt1 = sum(
        str_detect(coalesce(.unit, ""), "^(ммоль|mmol)/(л|l)\\.?$") &
          bilirubin_total_raw < 1,
        na.rm = TRUE
      ),
      n_mmol_ge1_reinterpreted_as_umol = sum(
        str_detect(coalesce(.unit, ""), "^(ммоль|mmol)/(л|l)\\.?$") &
          bilirubin_total_raw >= 1,
        na.rm = TRUE
      ),
      n_unrecognized_unit = sum(is.na(bilirubin_total)),
      min_harmonized = min(bilirubin_total, na.rm = TRUE),
      max_harmonized = max(bilirubin_total, na.rm = TRUE)
    )

  message("Билирубин: гармонизация единиц выполнена")
  print(bilirubin_harmonization_qc)
}

# D-димер -> мг/л FEU
if (all(c("D_dimer", "D_dimer_unit") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      D_dimer_raw = D_dimer,
      D_dimer_unit_raw = D_dimer_unit,
      .dd_unit = unit_norm(D_dimer_unit),
      .dd_is_mgl = !is.na(.dd_unit) & str_detect(.dd_unit, "^(мг|mg)/(л|l)"),
      .dd_is_mgdl = !is.na(.dd_unit) & str_detect(.dd_unit, "^(мг|mg)/(дл|dl)"),
      .dd_is_ugml = !is.na(.dd_unit) & str_detect(.dd_unit, "^(мкг|µg|ug)/(мл|ml)"),
      .dd_is_ugl = !is.na(.dd_unit) & str_detect(.dd_unit, "^(мкг|µg|ug)/(л|l)"),
      .dd_is_ngml = !is.na(.dd_unit) & str_detect(.dd_unit, "^(нг|ng)/(мл|ml)"),
      .dd_is_ddu = !is.na(.dd_unit) & str_detect(.dd_unit, "ddu"),
      .dd_unit_known = .dd_is_mgl | .dd_is_mgdl | .dd_is_ugml | .dd_is_ugl | .dd_is_ngml,
      .dd_mgl = case_when(
        .dd_is_mgdl ~ D_dimer * 10,
        .dd_is_mgl ~ D_dimer,
        .dd_is_ugml ~ D_dimer,
        .dd_is_ugl ~ D_dimer / 1000,
        .dd_is_ngml ~ D_dimer / 1000,
        TRUE ~ NA_real_
      ),
      D_dimer = case_when(
        .dd_unit_known & .dd_is_ddu ~ .dd_mgl * 2,
        .dd_unit_known ~ .dd_mgl,
        TRUE ~ NA_real_
      ),
      D_dimer_unit = case_when(
        .dd_unit_known ~ "мг/л FEU",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-starts_with(".dd_"))
}

# CLI: индекс капиллярной утечки для каждой исходной временной точки.
# Формула: СРБ (мг/дл) / альбумин (г/л) * 100.
# Конвертация только для расчёта CLI: исходные показатели не изменяем.
# Неизвестные единицы/пропуски, СРБ < 0 и альбумин <= 0 дают NA.
patients_tidy$CLI <- NA_real_
if (all(c("C_react_protein", "C_react_protein_unit", "albumin", "albumin_unit") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      .cli_crp_unit = unit_norm(C_react_protein_unit),
      .cli_alb_unit = unit_norm(albumin_unit),
      .cli_crp_mgdl = case_when(
        str_detect(.cli_crp_unit, "^(мг|mg)/(дл|dl)\\.?$") ~ C_react_protein,
        str_detect(.cli_crp_unit, "^(мг|mg)/(л|l)\\.?$") ~ C_react_protein / 10,
        TRUE ~ NA_real_
      ),
      .cli_albumin_gl = case_when(
        str_detect(.cli_alb_unit, "^(г|g)/(л|l)\\.?$") ~ albumin,
        str_detect(.cli_alb_unit, "^(г|g)/(дл|dl)\\.?$") ~ albumin * 10,
        TRUE ~ NA_real_
      ),
      CLI = if_else(
        is.finite(.cli_crp_mgdl) & .cli_crp_mgdl >= 0 &
          is.finite(.cli_albumin_gl) & .cli_albumin_gl > 0,
        .cli_crp_mgdl / .cli_albumin_gl * 100,
        NA_real_
      )
    ) %>%
    select(-starts_with(".cli_")) %>%
    relocate(CLI, .after = C_react_protein_unit)
}

# HA_sorption_dose: скорость * длительность (часы, как в HA_duration) / BMI.
# Это буквальная формула пользователя, без перевода часов в минуты (* 60).
# BMI — признак уровня пациента; повторение по timepoint не размножает HA.
# При противоречивых BMI расчёт останавливаем, не выбираем случайную строку.
HA_tidy$HA_sorption_dose <- NA_real_
if (all(c("pat_record_id", "BMI") %in% names(patients_tidy)) &&
    all(c("pat_record_id", "HA_avg_blood_flow", "HA_duration") %in% names(HA_tidy))) {
  bmi_by_id <- patients_tidy %>%
    group_by(pat_record_id) %>%
    summarise(
      .bmi_n = n_distinct(BMI[!is.na(BMI)]),
      .dose_bmi = first(BMI[!is.na(BMI)], default = NA_real_),
      .groups = "drop"
    )
  if (any(bmi_by_id$.bmi_n > 1L)) {
    stop("Разные BMI для одного pat_record_id: ",
         paste(bmi_by_id$pat_record_id[bmi_by_id$.bmi_n > 1L], collapse = ", "))
  }
  HA_tidy <- HA_tidy %>%
    left_join(select(bmi_by_id, pat_record_id, .dose_bmi),
              by = "pat_record_id", na_matches = "never") %>%
    mutate(
      HA_sorption_dose = if_else(
        is.finite(HA_avg_blood_flow) & HA_avg_blood_flow >= 0 &
          is.finite(HA_duration) & HA_duration >= 0 &
          is.finite(.dose_bmi) & .dose_bmi > 0,
        HA_avg_blood_flow * HA_duration / .dose_bmi,
        NA_real_
      )
    ) %>%
    select(-.dose_bmi) %>%
    relocate(HA_sorption_dose, .after = HA_duration)
}

# 12. HA -> patient-level features ----------------------------------------

# Из таблицы процедур получаем признаки уровня пациента:
# (1) is_lps_72h: был ли хотя бы один LPS-картридж в первые 72 часа от поступления в ОРИТ
# (2) ICU_to_first_LPS / ICU_to_first_CT:
#     время (в часах) от ICU_in_dt до первого по времени HA_start_dt среди LPS / CT соответственно
# (3) first_sorption_dt / first_sorption_type:
#     дата/время и тип картриджа процедуры с параметром HA_parameter == "Гемоперфузия 1"
# - если по пациенту нет ни одной записи HA, то is_lps_72h = 0, остальные переменные = NA

icu_by_id <- patients_tidy %>%
  group_by(pat_record_id) %>%
  summarise(
    ICU_in_dt = first(na.omit(ICU_in_dt)),
    .groups = "drop"
  ) %>%
  mutate(ICU_in_plus72 = ICU_in_dt + hours(72))

ha_by_id <- HA_tidy %>%
  left_join(icu_by_id, by = "pat_record_id") %>%
  mutate(
    .cart = HA_cartrige_type_std,
    .is_hp1 = str_detect(norm_txt(HA_parameter), regex("^Гемоперфузия\\s*1$", ignore_case = TRUE))
  ) %>%
  group_by(pat_record_id) %>%
  summarise(
    # Количество процедур по типу картриджа.
    CT_count = sum(.cart == "CT", na.rm = TRUE),
    LPS_count = sum(.cart == "LPS", na.rm = TRUE),

    # 1) LPS в первые 72 часа
    is_lps_72h = as.integer(any(
      .cart == "LPS" &
        !is.na(HA_start_dt) & !is.na(ICU_in_dt) &
        HA_start_dt >= ICU_in_dt & HA_start_dt <= ICU_in_plus72
    )),
    
    # 2) первые даты LPS/CT
    .first_LPS_start_dt = {
      x <- HA_start_dt[.cart == "LPS"]
      if (any(!is.na(x))) min(x, na.rm = TRUE) else as.POSIXct(NA, tz = "UTC")
    },
    .first_CT_start_dt = {
      x <- HA_start_dt[.cart == "CT"]
      if (any(!is.na(x))) min(x, na.rm = TRUE) else as.POSIXct(NA, tz = "UTC")
    },
    
    # 3) «Гемоперфузия 1»: берём самую раннюю по времени запись
    first_sorption_dt = {
      x <- HA_start_dt[.is_hp1]
      if (any(!is.na(x))) min(x, na.rm = TRUE) else as.POSIXct(NA, tz = "UTC")
    },
    first_sorption_type = {
      idx <- which(.is_hp1 & !is.na(HA_start_dt))
      if (length(idx) == 0) NA_character_
      else .cart[idx[which.min(HA_start_dt[idx])]]
    },
    
    ICU_in_dt = first(na.omit(ICU_in_dt)),
    .groups = "drop"
  ) %>%
  mutate(
    # время (часы) от ICU до первых LPS/CT и до первой «Гемоперфузия 1»
    ICU_to_first_LPS = round(as.numeric(difftime(.first_LPS_start_dt, ICU_in_dt, units = "hours")), 1),
    ICU_to_first_CT  = round(as.numeric(difftime(.first_CT_start_dt,  ICU_in_dt, units = "hours")), 1),
    icu_to_HA1_hours = round(as.numeric(difftime(first_sorption_dt,   ICU_in_dt, units = "hours")), 1),
    devices_total = CT_count + LPS_count,
    first_sorption_LPS = dplyr::case_when(
      first_sorption_type == "LPS" ~ 1L,
      first_sorption_type == "CT" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(
    pat_record_id,
    CT_count,
    LPS_count,
    devices_total,
    is_lps_72h,
    ICU_to_first_LPS,
    ICU_to_first_CT,
    icu_to_HA1_hours,
    first_sorption_dt,
    first_sorption_type,
    first_sorption_LPS
  )

patients_tidy <- patients_tidy %>%
  left_join(ha_by_id, by = "pat_record_id") %>%
  mutate(
    CT_count = coalesce(as.integer(CT_count), 0L),
    LPS_count = coalesce(as.integer(LPS_count), 0L),
    devices_total = coalesce(as.integer(devices_total), 0L),
    is_lps_72h = coalesce(as.integer(is_lps_72h), 0L)
  )

# Время от начала антибиотикотерапии до первой «Гемоперфузии 1», часы.
# Отрицательные значения допустимы: первая сорбция могла начаться до антибиотиков.
if (all(c("antibiotics_start_dt", "first_sorption_dt") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    mutate(
      antibiotics_to_HA1_hours = round(
        as.numeric(difftime(first_sorption_dt, antibiotics_start_dt, units = "hours")),
        1
      )
    )
} else {
  patients_tidy <- patients_tidy %>%
    mutate(antibiotics_to_HA1_hours = NA_real_)
}

# 12b. Manual diagnosis/indication review --------------------------------

# Для записей из старой формы без checkbox-структуры подставляем ручную разметку
# из data/2Reg_patients_indications.xlsx. Разметка применяется ко всем строкам
# patients_tidy с pat_record_id, который есть в Excel-файле.
patients_tidy <- apply_manual_indications(
  patients_tidy,
  patients_indications_file,
  blank_binary_as_zero = TRUE
)

# 13. Postprocess: status / outcomes / pathogen recodes ------------------

# Устойчивые patient-level варианты исхода --------------------------------
#
# Все варианты сохраняются рядом, чтобы аналитические файлы могли выбирать
# подходящее определение, не перекодируя outcome каждый раз по-разному.
#
# outcome_status:
# - dead = умер;
# - out  = выписан из ОРИТ;
# - in   = продолжает находиться в ОРИТ или переведён в другой ОРИТ;
# - n/a  = исход неизвестен / не распознан.
#
# outcome_dead_bin: 1 = умер, 0 = выписан; продолжающие лечение и переведённые = NA.
# outcome_survival_bin: обратное кодирование того же терминального сравнения.
# outcome_dead_all_bin: 1 = умер, 0 = любой другой известный исход.
# outcome_logit: legacy-кодирование из прежних моделей:
#   1 = умер; 0 = выписан или продолжает находиться в ОРИТ; перевод/неизвестно = NA.
# outcome_28d_status: единый статус на 28-й день для графиков и 28-дневных
#   сравнений: alive / death / unknown.
# outcome_death_28d_bin: 1 = умер к 28-му дню, 0 = известен живым на 28-й день,
#   NA = 28-дневный статус определить нельзя.
if ("outcome" %in% names(patients_tidy)) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      .outcome_std = stringr::str_to_lower(
        stringr::str_squish(as.character(outcome))
      ),
      .outcome_key = dplyr::case_when(
        is.na(.outcome_std) | .outcome_std == "" ~ NA_character_,
        stringr::str_detect(.outcome_std, "умер|смерт") ~ "dead",
        stringr::str_detect(.outcome_std, "выпис") ~ "out",
        stringr::str_detect(.outcome_std, "перевед") ~ "transferred",
        stringr::str_detect(.outcome_std, "продолж|наход") ~ "in",
        TRUE ~ NA_character_
      ),
      outcome_status = dplyr::case_when(
        .outcome_key == "dead" ~ "dead",
        .outcome_key == "out" ~ "out",
        .outcome_key %in% c("in", "transferred") ~ "in",
        TRUE ~ "n/a"
      ),
      outcome_status = factor(
        outcome_status,
        levels = c("in", "out", "dead", "n/a")
      ),
      outcome_dead_bin = dplyr::case_when(
        .outcome_key == "dead" ~ 1L,
        .outcome_key == "out" ~ 0L,
        TRUE ~ NA_integer_
      ),
      outcome_survival_bin = dplyr::case_when(
        .outcome_key == "out" ~ 1L,
        .outcome_key == "dead" ~ 0L,
        TRUE ~ NA_integer_
      ),
      outcome_dead_all_bin = dplyr::case_when(
        .outcome_key == "dead" ~ 1L,
        .outcome_key %in% c("out", "in", "transferred") ~ 0L,
        TRUE ~ NA_integer_
      ),
      outcome_logit = dplyr::case_when(
        .outcome_key == "dead" ~ 1L,
        .outcome_key %in% c("out", "in") ~ 0L,
        TRUE ~ NA_integer_
      ),
      outcome_days = as.numeric(
        difftime(ICU_out_or_death_dt, ICU_in_dt, units = "days")
      ),
      .last_registry_dt = dplyr::coalesce(
        record_date_updated_dt,
        record_date_created_dt
      ),
      .last_registry_followup_days = as.numeric(
        difftime(.last_registry_dt, ICU_in_dt, units = "days")
      ),
      .outcome_28d_status_chr = dplyr::case_when(
        .outcome_key == "dead" &
          !is.na(outcome_days) & outcome_days >= 0 & outcome_days <= 28 ~ "death",
        .outcome_key == "dead" &
          !is.na(outcome_days) & outcome_days > 28 ~ "alive",
        .outcome_key == "out" ~ "alive",
        .outcome_key == "transferred" &
          !is.na(outcome_days) & outcome_days >= 28 ~ "alive",
        .outcome_key == "in" &
          !is.na(.last_registry_followup_days) &
          .last_registry_followup_days >= 28 ~ "alive",
        TRUE ~ "unknown"
      ),
      outcome_28d_status = factor(
        .outcome_28d_status_chr,
        levels = c("alive", "death", "unknown")
      ),
      outcome_death_28d_bin = dplyr::case_when(
        .outcome_28d_status_chr == "death" ~ 1L,
        .outcome_28d_status_chr == "alive" ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::select(
      -.outcome_std,
      -.outcome_key,
      -.last_registry_dt,
      -.last_registry_followup_days,
      -.outcome_28d_status_chr
    )
}

# Базовые бинарные признаки, однозначно выводимые из patients_tidy.
# Они нужны в нескольких аналитических файлах и поэтому рассчитываются один раз в ETL.
patients_tidy <- patients_tidy %>%
  dplyr::mutate(
    sex_male_bin = dplyr::case_when(
      stringr::str_detect(
        stringr::str_to_lower(stringr::str_squish(as.character(sex))),
        "муж|male|^м$|^m$"
      ) ~ 1L,
      stringr::str_detect(
        stringr::str_to_lower(stringr::str_squish(as.character(sex))),
        "жен|female|^ж$|^f$"
      ) ~ 0L,
      TRUE ~ NA_integer_
    ),
    vasopressors_if_used_bin = yes_no_to_int(vasopressors_if_used),
    RRT_if_used_bin = yes_no_to_int(RRT_if_used),
    MV_if_used_bin = yes_no_to_int(MV_if_used),
    ECMO_if_used_bin = yes_no_to_int(ECMO_if_used),
    from_other_ICU_transfer_bin = yes_no_to_int(from_other_ICU_transfer),
    ICU_6_month_history_bin = yes_no_to_int(ICU_6_month_history),
    antibiotics_started_bin = dplyr::if_else(
      is.na(antibiotics_start_dt),
      0L,
      1L
    )
  )

# Потребность в ЗПТ/ИВЛ в baseline-окне относительно первой сорбции.
# Отдельные имена не перезаписывают исходные RRT_if_used / MV_if_used из JSON.
if (all(c(
  "first_sorption_dt", "RRT_start_dt", "RRT_resolution_dt",
  "MV_start_dt", "MV_resolution_dt"
) %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      RRT_required_baseline = dplyr::case_when(
        is.na(first_sorption_dt) ~ NA_integer_,
        is.na(RRT_start_dt) ~ 0L,
        first_sorption_dt >= (RRT_start_dt - lubridate::hours(12)) &
          (is.na(RRT_resolution_dt) | first_sorption_dt <= RRT_resolution_dt) ~ 1L,
        TRUE ~ 0L
      ),
      MV_required_baseline = dplyr::case_when(
        is.na(first_sorption_dt) ~ NA_integer_,
        is.na(MV_start_dt) ~ 0L,
        first_sorption_dt >= (MV_start_dt - lubridate::hours(12)) &
          (is.na(MV_resolution_dt) | first_sorption_dt <= MV_resolution_dt) ~ 1L,
        TRUE ~ 0L
      )
    )
} else {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      RRT_required_baseline = NA_integer_,
      MV_required_baseline = NA_integer_
    )
}

# patient status по таймпойнтам (in / out / dead / n/a)
# Здесь для каждой строки пациента определяем, находился ли он в ОРИТ на этот момент.
# - "in"   = к концу окна пациент ещё в ОРИТ;
# - "out"  = выписан из ОРИТ;
# - "dead" = умер;
# - "n/a"  = не хватает информации, чтобы надёжно решить.
# Правило:
# - если событие (ICU_out_or_death_dt) произошло ДО начала окна -> уже out/dead (по outcome)
# - если событие ВНУТРИ окна -> out/dead (по outcome)
# - если событие ПОСЛЕ окна -> in
# - если даты/исходы недостаточны -> n/a
if (all(c("timepoint", "first_sorption_dt", "ICU_out_or_death_dt", "outcome") %in% names(patients_tidy))) {
  
  tp_windows <- tibble::tibble(
    timepoint = c("m12h_0h", "48h_72h", "4d_6d", "7d_10d"),
    start_h   = c(-12, 48, 96, 168),
    end_h     = c(0, 72, 144, 240)
  )
  
  patients_tidy <- patients_tidy %>%
    dplyr::left_join(tp_windows, by = "timepoint") %>%
    dplyr::mutate(
      .hours_to_event = as.numeric(difftime(ICU_out_or_death_dt, first_sorption_dt, units = "hours")),
      .event_status = as.character(outcome_status),
      status = dplyr::case_when(
        # неизвестен таймпойнт или нет даты включения
        is.na(start_h) | is.na(first_sorption_dt) ~ "n/a",
        
        # нет ICU_out_or_death_dt: если outcome говорит, что пациент в ОРИТ -> in, иначе n/a
        is.na(ICU_out_or_death_dt) ~ dplyr::if_else(.event_status == "in", "in", "n/a"),
        
        # событие до начала окна
        .hours_to_event < start_h ~ .event_status,
        
        # событие внутри окна
        .hours_to_event >= start_h & .hours_to_event <= end_h ~ .event_status,
        
        # событие после окна -> пациент ещё in на этой точке
        .hours_to_event > end_h ~ "in",
        
        TRUE ~ "n/a"
      ),
      status = factor(status, levels = c("in", "out", "dead", "n/a"), ordered = TRUE)
    ) %>%
    dplyr::select(-start_h, -end_h, -.hours_to_event, -.event_status)
}

# SOFA: сохраняем исходные значения и заполняем 0 при status == out
# Логика: если SOFA на точке неизвестна (NA) и пациент к этой точке уже OUT, то SOFA = 0.
if (all(c("SOFA", "status") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      SOFA_raw = SOFA,
      SOFA = dplyr::if_else(is.na(SOFA) & status == "out", 0, SOFA)
    )
}

# VIS2020: информированный ноль ставим только при явном отсутствии вазопрессоров
# или после выписки. Одного MAP > 65 для восстановления VIS = 0 недостаточно.
# Уже зарегистрированное значение VIS не перезаписываем.
if (all(c("VIS2020", "status", "vasopressors_if_used_bin") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      VIS2020_raw = VIS2020,
      VIS2020_fill_rule = dplyr::case_when(
        !is.na(VIS2020) ~ "observed",
        status == "out" ~ "zero_after_ICU_discharge",
        vasopressors_if_used_bin == 0L ~ "zero_no_vasopressors",
        TRUE ~ "missing_unknown"
      ),
      VIS2020 = dplyr::if_else(
        is.na(VIS2020) &
          (status == "out" | vasopressors_if_used_bin == 0L),
        0,
        VIS2020
      )
    ) %>%
    dplyr::group_by(pat_record_id) %>%
    dplyr::mutate(
      .vis_baseline_positive = any(
        timepoint == "m12h_0h" & !is.na(VIS2020) & VIS2020 > 0,
        na.rm = TRUE
      ),
      # Для пациентов с исходным VIS > 0 сохраняем последующие нули после выписки.
      VIS2020_0_excl = dplyr::if_else(
        .vis_baseline_positive,
        VIS2020,
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.vis_baseline_positive)
}

# VDI (Vasopressor Dependency Index): VIS2020 / avg_BP для каждой timepoint.
# avg_BP — среднее артериальное давление (MAP), мм рт. ст.
# Вариант VIS / MAP использован, например, в EUPHAS2:
# https://doi.org/10.1111/aor.13900
# В реестре VIS2020 хранится готовым числом; ETL не рассчитывает его из доз.
# До проверки формулы реестра результат следует описывать как VDI на основе
# VIS2020: эквивалентность VDI конкретной публикации зависит от числителя.
# VIS и MAP берём из одной строки (одной временной точки). Это не гарантирует
# одновременность измерений; её нужно учитывать при интерпретации.
# Используем VIS2020, а не VIS2020_0_excl: исходные нули не исключаем.
# При известном VIS2020 = 0 и положительном MAP получаем VDI = 0.
# Нули VIS, добавленные только по факту выписки (zero_after_ICU_discharge),
# не считаем измеренными: для них VDI остаётся NA даже при наличии MAP.
# Ноль при явно указанном отсутствии вазопрессоров допускается; его источник
# можно отличить от наблюдаемого значения по VIS2020_fill_rule.
# Пропуск, нечисловое/бесконечное значение, VIS < 0 или MAP <= 0 дают NA.
# Долю доступных VDI оцениваем после запуска на актуальных данных (QC ниже).
patients_tidy$VDI <- NA_real_
if (all(c("VIS2020", "avg_BP") %in% names(patients_tidy))) {
  .vdi_discharge_zero <- rep(FALSE, nrow(patients_tidy))
  if ("VIS2020_fill_rule" %in% names(patients_tidy)) {
    .vdi_discharge_zero <- patients_tidy$VIS2020_fill_rule %in%
      "zero_after_ICU_discharge"
  }
  patients_tidy <- patients_tidy %>%
    mutate(
      VDI = if_else(
        is.finite(VIS2020) & VIS2020 >= 0 &
          is.finite(avg_BP) & avg_BP > 0 & !.vdi_discharge_zero,
        VIS2020 / avg_BP,
        NA_real_
      )
    ) %>%
    relocate(VDI, .after = VIS2020)
  rm(.vdi_discharge_zero)
}

# SIC score (Sepsis-Induced Coagulopathy), 0–6 баллов:
# - тромбоциты: >=150 = 0; 100–<150 = 1; <100 = 2 (×10^9/л);
# - INR: <=1.2 = 0; >1.2–<=1.4 = 1; >1.4 = 2;
# - SOFA: 0 = 0; 1 = 1; >=2 = 2.
#
# SIC_positive: 1, если SIC_score >= 4 и сумма суббаллов тромбоцитов + INR >= 3;
# 0, если все три компонента известны, но критерии не выполнены; NA при отсутствии
# хотя бы одного компонента.
#
# В оригинальных критериях SIC используется сумма 4 органных компонентов SOFA
# (дыхательный, печёночный, сердечно-сосудистый и почечный), а не общий SOFA.
# В текущем реестре доступен только общий SOFA, поэтому здесь он используется как
# прагматичная замена, с ограничением вклада до 2 баллов. SIC_score и SIC_positive
# не следует интерпретировать как строго формальную верификацию SIC без компонент SOFA.
if (all(c("thrombocytes", "INR", "SOFA") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      .SIC_platelets = dplyr::case_when(
        thrombocytes < 100 ~ 2,
        thrombocytes >= 100 & thrombocytes < 150 ~ 1,
        thrombocytes >= 150 ~ 0,
        TRUE ~ NA_real_
      ),
      .SIC_INR = dplyr::case_when(
        INR > 1.4 ~ 2,
        INR > 1.2 & INR <= 1.4 ~ 1,
        INR <= 1.2 ~ 0,
        TRUE ~ NA_real_
      ),
      .SIC_SOFA = dplyr::case_when(
        SOFA >= 2 ~ 2,
        SOFA == 1 ~ 1,
        SOFA == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      SIC_score = dplyr::if_else(
        !is.na(.SIC_platelets) & !is.na(.SIC_INR) & !is.na(.SIC_SOFA),
        .SIC_platelets + .SIC_INR + .SIC_SOFA,
        NA_real_
      ),
      SIC_positive = dplyr::case_when(
        is.na(.SIC_platelets) | is.na(.SIC_INR) | is.na(.SIC_SOFA) ~ NA_real_,
        SIC_score >= 4 & (.SIC_platelets + .SIC_INR) >= 3 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(-.SIC_platelets, -.SIC_INR, -.SIC_SOFA)
} else {
  patients_tidy <- patients_tidy %>%
    mutate(
      SIC_score = NA_real_,
      SIC_positive = NA_real_
    )
}

# Единая колонка бактериальной флоры по грам-окраске + бинарные индикаторы по каждому посеву:
patients_tidy$gram_any_bin <- NA_integer_
patients_tidy$gram_score <- NA_integer_
if (all(c("pat_gram_positive", "pat_gram_negative") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      pat_gram_stain    = gram_stain_class(pat_gram_positive, pat_gram_negative),
      is_pat_gram_plus  = suppressWarnings(
        as.integer(as.character(culture_to_1_0_na(pat_gram_positive)))
      ),
      is_pat_gram_minus = suppressWarnings(
        as.integer(as.character(culture_to_1_0_na(pat_gram_negative)))
      ),
      # Любой положительный результат достаточен для 1.
      # 0 — оба отрицательные; неполное обследование без положительных — NA.
      gram_any_bin = dplyr::case_when(
        is_pat_gram_plus == 1L | is_pat_gram_minus == 1L ~ 1L,
        is_pat_gram_plus == 0L & is_pat_gram_minus == 0L ~ 0L,
        TRUE ~ NA_integer_
      ),
      # 0 означает только смешанную положительную флору.
      # Для чистой категории нужны известные результаты обоих полей.
      gram_score = dplyr::case_when(
        is_pat_gram_plus == 1L & is_pat_gram_minus == 1L ~ 0L,
        is_pat_gram_plus == 0L & is_pat_gram_minus == 1L ~ 1L,
        is_pat_gram_plus == 1L & is_pat_gram_minus == 0L ~ -1L,
        TRUE ~ NA_integer_
      )
    ) %>%
    dplyr::relocate(pat_gram_stain, is_pat_gram_plus, is_pat_gram_minus,
                    gram_any_bin, gram_score, .after = pat_gram_negative)
}

if ("pat_bacteremia" %in% names(patients_tidy)) {
  patients_tidy <- patients_tidy %>%
    mutate(
      is_bacteremic = suppressWarnings(
        as.integer(as.character(culture_to_1_0_na(pat_bacteremia)))
      )
    )
}

if ("pat_fungal" %in% names(patients_tidy)) {
  patients_tidy <- patients_tidy %>%
    mutate(
      is_pat_fungal = suppressWarnings(
        as.integer(as.character(culture_to_1_0_na(pat_fungal)))
      )
    )
}

# is_septic: 1, если есть признаки сепсиса/септ.шока по показаниям
# ИЛИ pat_gram_stain == gram- / mix ИЛИ бактериемия ИЛИ PCT > 10 на m12h_0h; иначе 0
if (all(c("pat_record_id", "HA_indications") %in% names(patients_tidy))) {
  
  if (all(c("timepoint", "procalcitonin") %in% names(patients_tidy))) {
    septic_pct_tbl <- patients_tidy %>%
      dplyr::group_by(pat_record_id) %>%
      dplyr::summarise(
        .septic_pct = any(timepoint == "m12h_0h" & !is.na(procalcitonin) & procalcitonin > 10, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    septic_pct_tbl <- patients_tidy %>%
      dplyr::distinct(pat_record_id) %>%
      dplyr::mutate(.septic_pct = FALSE)
  }
  
  patients_tidy <- patients_tidy %>%
    dplyr::left_join(septic_pct_tbl, by = "pat_record_id") %>%
    dplyr::mutate(
      is_septic = dplyr::if_else(
        stringr::str_detect(stringr::str_to_lower(dplyr::coalesce(HA_indications, "")), "\\bсепсис\\b|(?<!а)септическ") |
          dplyr::coalesce(ind_bin_sepsis, 0L) == 1L |
          dplyr::coalesce(ind_bin_septic_shock, 0L) == 1L |
          dplyr::coalesce(as.character(pat_gram_stain), "") %in% c("gram-", "mix") |
          dplyr::coalesce(as.character(is_bacteremic), "") == "1" |
          dplyr::coalesce(.septic_pct, FALSE),
        1L, 0L
      )
    ) %>%
    dplyr::select(-.septic_pct)
}


# 14. QA ------------------------------------------------------------------

# QC: какие id “выпали” при формировании patients_tidy
all_ids <- accepted_registry_ids
pt_ids <- patients_tidy %>%
  transmute(pat_record_id = clean_na(pat_record_id)) %>%
  filter(!is.na(pat_record_id), pat_record_id != "") %>%
  distinct(pat_record_id) %>%
  pull(pat_record_id)

missing_ids <- setdiff(all_ids, pt_ids)

cat("registry ids:", length(all_ids), "\n")
cat("patients_tidy ids:", length(pt_ids), "\n")
cat("missing ids:", length(missing_ids), "\n")

if (length(missing_ids) > 0) {
  print(head(missing_ids, 20))
}

dup_pat_time <- patients_tidy %>%
  count(pat_record_id, timepoint) %>%
  filter(n > 1)

cat("duplicate pat_record_id + timepoint:", nrow(dup_pat_time), "\n")
cat("patients_tidy rows:", nrow(patients_tidy), "\n")
cat("patients_tidy ids:", n_distinct(patients_tidy$pat_record_id), "\n")
cat("HA_tidy rows:", nrow(HA_tidy), "\n")
cat("HA_tidy ids:", n_distinct(HA_tidy$pat_record_id), "\n")

# Дополнительный ETL-QC ---------------------------------------------------

# Проверяем patient-level перекодировки исхода один раз на пациента.
outcome_recode_qc <- patients_tidy %>%
  dplyr::distinct(
    pat_record_id,
    outcome,
    outcome_status,
    outcome_dead_bin,
    outcome_survival_bin,
    outcome_dead_all_bin,
    outcome_logit,
    outcome_28d_status,
    outcome_death_28d_bin
  ) %>%
  dplyr::count(
    outcome,
    outcome_status,
    outcome_dead_bin,
    outcome_survival_bin,
    outcome_dead_all_bin,
    outcome_logit,
    outcome_28d_status,
    outcome_death_28d_bin,
    name = "n_patients"
  )

outcome_terminal_inconsistency <- patients_tidy %>%
  dplyr::distinct(
    pat_record_id,
    outcome_dead_bin,
    outcome_survival_bin
  ) %>%
  dplyr::filter(
    !is.na(outcome_dead_bin),
    !is.na(outcome_survival_bin),
    outcome_dead_bin + outcome_survival_bin != 1L
  )

outcome_28d_inconsistency <- patients_tidy %>%
  dplyr::distinct(
    pat_record_id,
    outcome_28d_status,
    outcome_death_28d_bin
  ) %>%
  dplyr::filter(
    (outcome_28d_status == "death" &
      (is.na(outcome_death_28d_bin) | outcome_death_28d_bin != 1L)) |
      (outcome_28d_status == "alive" &
        (is.na(outcome_death_28d_bin) | outcome_death_28d_bin != 0L)) |
      (outcome_28d_status == "unknown" & !is.na(outcome_death_28d_bin))
  )

outcome_28d_qc <- patients_tidy %>%
  dplyr::distinct(pat_record_id, outcome_28d_status) %>%
  dplyr::count(outcome_28d_status, name = "n_patients", .drop = FALSE)

cat("\nOutcome recode QC:\n")
print(outcome_recode_qc)
cat(
  "terminal death/survival inconsistencies:",
  nrow(outcome_terminal_inconsistency),
  "\n"
)
cat("28-day outcome QC:\n")
print(outcome_28d_qc)
cat(
  "28-day status/binary inconsistencies:",
  nrow(outcome_28d_inconsistency),
  "\n"
)

count_na_col <- function(df, col) {
  if (col %in% names(df)) sum(is.na(df[[col]])) else NA_integer_
}

# 1) распределение строк по таймпойнтам
tp_counts <- patients_tidy %>%
  count(timepoint, name = "n_rows") %>%
  arrange(timepoint)

cat("\npatients_tidy rows by timepoint:\n")
print(tp_counts)

# 2) дубли в HA_tidy
dup_ha_param <- HA_tidy %>%
  count(pat_record_id, HA_parameter) %>%
  filter(n > 1)

cat("duplicate pat_record_id + HA_parameter:", nrow(dup_ha_param), "\n")

# 2b) распределение и внутренняя согласованность устойчивых HA-перекодировок
ha_recode_qc <- HA_tidy %>%
  count(
    HA_anticoagulation_cat,
    HA_anticoag_heparin,
    HA_cartrige_type_std,
    HA_cartridge_LPS_bin,
    HA_with_RRT_bin,
    HA_other_methods_bin,
    HA_adverse_effects_bin,
    HA_adverse_effects_recorded_bin,
    name = "n_procedures"
  )

ha_recode_inconsistency <- HA_tidy %>%
  filter(
    (HA_anticoag_heparin == 1L & HA_anticoagulation_cat != "Гепарин") |
      (HA_anticoag_heparin == 0L & HA_anticoagulation_cat == "Гепарин") |
      (HA_cartridge_LPS_bin == 1L & HA_cartrige_type_std != "LPS") |
      (HA_cartridge_LPS_bin == 0L & HA_cartrige_type_std != "CT") |
      HA_adverse_effects_recorded_bin != coalesce(HA_adverse_effects_bin, 0L)
  )

cat("\nHA recode QC:\n")
print(ha_recode_qc)
cat("HA recode inconsistencies:", nrow(ha_recode_inconsistency), "\n")

# 3) HA_start_dt раньше ICU_in_dt
ha_before_icu <- HA_tidy %>%
  select(pat_record_id, HA_parameter, HA_start_dt) %>%
  left_join(
    patients_tidy %>%
      group_by(pat_record_id) %>%
      summarise(
        ICU_in_dt = first(na.omit(ICU_in_dt)),
        .groups = "drop"
      ),
    by = "pat_record_id"
  ) %>%
  filter(!is.na(HA_start_dt), !is.na(ICU_in_dt), HA_start_dt < ICU_in_dt)

cat("HA_start_dt earlier than ICU_in_dt:", nrow(ha_before_icu), "\n")

# 4) missingness ключевых полей
qc_missing_pat <- tibble::tibble(
  variable = c(
    "ICU_in_dt",
    "ICU_out_or_death_dt",
    "antibiotics_start_dt",
    "ICU_to_antibiotics_dt",
    "antibiotics_to_HA1_hours",
    "first_sorption_dt",
    "status",
    "INR",
    "SIC_score",
    "SIC_positive",
    "PaFiO2",
    "SpFiO2",
    "PaFiO2_calc",
    "gram_any_bin",
    "gram_score",
    "CLI",
    "VDI"
  ),
  n_missing = c(
    count_na_col(patients_tidy, "ICU_in_dt"),
    count_na_col(patients_tidy, "ICU_out_or_death_dt"),
    count_na_col(patients_tidy, "antibiotics_start_dt"),
    count_na_col(patients_tidy, "ICU_to_antibiotics_dt"),
    count_na_col(patients_tidy, "antibiotics_to_HA1_hours"),
    count_na_col(patients_tidy, "first_sorption_dt"),
    count_na_col(patients_tidy, "status"),
    count_na_col(patients_tidy, "INR"),
    count_na_col(patients_tidy, "SIC_score"),
    count_na_col(patients_tidy, "SIC_positive"),
    count_na_col(patients_tidy, "PaFiO2"),
    count_na_col(patients_tidy, "SpFiO2"),
    count_na_col(patients_tidy, "PaFiO2_calc"),
    count_na_col(patients_tidy, "gram_any_bin"),
    count_na_col(patients_tidy, "gram_score"),
    count_na_col(patients_tidy, "CLI"),
    count_na_col(patients_tidy, "VDI")
  )
)

qc_missing_ha <- tibble::tibble(
  variable = c("HA_start_dt", "HA_sorption_dose"),
  n_missing = c(count_na_col(HA_tidy, "HA_start_dt"),
                count_na_col(HA_tidy, "HA_sorption_dose"))
)

cat("\nMissingness in patients_tidy:\n")
print(qc_missing_pat)

cat("\nMissingness in HA_tidy:\n")
print(qc_missing_ha)

# Доступность VDI по временным точкам (строки patients_tidy).
vdi_coverage_qc <- patients_tidy %>%
  group_by(timepoint) %>%
  summarise(
    n_rows = n(),
    n_VDI = sum(!is.na(VDI)),
    pct_VDI = 100 * n_VDI / n_rows,
    .groups = "drop"
  )
cat("\nVDI coverage by timepoint:\n")
print(vdi_coverage_qc)

# 5) QC по PaFiO2_calc
if ("PaFiO2_calc" %in% names(patients_tidy)) {
  cat("\nPaFiO2 QC:\n")
  cat("PaFiO2 observed:", sum(!is.na(patients_tidy$PaFiO2)), "\n")
  cat(
    "PaFiO2 imputed from SpFiO2:",
    sum(is.na(patients_tidy$PaFiO2) & !is.na(patients_tidy$SpFiO2) & !is.na(patients_tidy$PaFiO2_calc)),
    "\n"
  )
  cat("PaFiO2 still missing after calc:", sum(is.na(patients_tidy$PaFiO2_calc)), "\n")
  
  if ("PaFiO2_calc_source" %in% names(patients_tidy)) {
    print(table(patients_tidy$PaFiO2_calc_source, useNA = "ifany"))
  }
}

if (!is.null(pafi_fit)) {
  cat(
    "\nPaFiO2 ~ SpFiO2 fit:",
    "intercept =", round(unname(coef(pafi_fit)[1]), 3),
    ", slope =", round(unname(coef(pafi_fit)[2]), 3),
    ", n =", nobs(pafi_fit), "\n"
  )
}


# 15. Save outputs --------------------------------------------------------

if (isTRUE(save_rds)) {
  saveRDS(patients_tidy, patients_out_rds)
  saveRDS(HA_tidy, ha_out_rds)
}

if (isTRUE(save_xlsx)) {
  openxlsx::write.xlsx(
    patients_tidy,
    patients_out_xlsx,
    asTable = TRUE,
    overwrite = TRUE
  )
  openxlsx::write.xlsx(
    HA_tidy,
    ha_out_xlsx,
    asTable = TRUE,
    overwrite = TRUE
  )
}
