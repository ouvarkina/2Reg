# 2Reg: build tidy datasets from JSON
# Цель:
# - прочитать JSON/XLSX из data/
# - собрать patients_tidy и HA_tidy
# - сохранить их в .rds и .xlsx

# 1. Packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(openxlsx)
  library(lubridate)
  library(jsonlite)
})

# 2. Paths and flags ------------------------------------------------------

# Directories

DATA_DIR    <- "data"

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

# Input paths

pick_path <- function(path_candidates) {
  hit <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(hit)) stop("Файл не найден: ", basename(path_candidates[[1]]))
  hit
}

optional_path <- function(path_candidates) {
  hit <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(hit)) NA_character_ else hit
}

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

# Ручная разметка старых free-text диагнозов/показаний к ГС.
# Файл опционален: если его нет, ETL продолжит работу только по JSON.
patients_indications_file <- optional_path(c(
  file.path(DATA_DIR, "2Reg_patients_indications.xlsx"),
  file.path("/mnt/data", DATA_DIR, "2Reg_patients_indications.xlsx"),
  "2Reg_patients_indications.xlsx"
))

# output paths / flags

patients_out_xlsx <- file.path(DATA_DIR, "2Reg_patients_tidy.xlsx")
ha_out_xlsx       <- file.path(DATA_DIR, "2Reg_HA_tidy.xlsx")
patients_out_rds  <- file.path(DATA_DIR, "patients_tidy.rds")
ha_out_rds        <- file.path(DATA_DIR, "HA_tidy.rds")

REPORT_TZ <- "UTC"
save_xlsx <- TRUE
save_rds  <- TRUE

# 3. Small helpers --------------------------------------------------------

# text / NA / numeric 
norm_txt <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "[\u2212\u2013\u2014]", "-")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

# Преобразует  подпись временной точки вида "Точка [-12;0] часов" -> "m12h_0h".
tp_code <- function(s){
  s <- norm_txt(s); if (is.na(s)) return(NA_character_)
  # единицы (часы/сутки) определяем по части строки с временной точкой
  tp_part <- str_extract(s, "Точка\\s*\\[[^\\]]+\\].*$")
  if (is.na(tp_part)) tp_part <- s
  tl <- str_to_lower(tp_part)
  u <- if (str_detect(tl, "час|\\bч\\b|\\bч\\.")) {
    "h"
  } else if (str_detect(tl, "сут|дн|день")) {
    "d"
  } else {
    "h"
  }
  
  m <- str_match(tp_part, "Точка\\s*\\[\\s*([-+]?\\d+)\\s*;\\s*([-+]?\\d+)\\s*\\]")
  if (is.na(m[1,1])) return(NA_character_)
  f <- function(z) if (z < 0) paste0("m", abs(z), u) else paste0(z, u)
  paste0(f(as.integer(m[1,2])), "_", f(as.integer(m[1,3])))
}

# Приводит разные текстовые обозначения пропуска к единому виду NA:
clean_na <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "[\r\n]", " ")
  x <- str_squish(x)
  xl <- str_to_lower(x)
  x[xl %in% c("нет значения","нет данных","n/a","na")] <- NA_character_
  x[x == ""] <- NA_character_
  x
}

# Пытается извлечь из строки одно числовое значение.
# Если число не найдено, вернёт NA.
to_num <- function(x){
  x <- clean_na(x)
  x <- str_split_fixed(x, "\\s*\\|\\s*", 2)[,1]
  m <- str_extract(x, "[-+]?\\d+(?:[\\.,]\\d+)?")
  as.numeric(str_replace_all(m, ",", "."))
}

# Разбирает строку лабораторного значения на числовую часть и единицу измерения.
# При impute_ineq = TRUE умеет грубо обрабатывать неравенства:
#   "<10" -> 5, ">10" -> 20.
split_val_unit <- function(x, impute_ineq = FALSE){
  x <- clean_na(x)
  x <- str_split_fixed(x, "\\s*\\|\\s*", 2)[,1]
  m <- str_match(x, "^\\s*([<>]=?|≤|≥)?\\s*([-+]?\\d+(?:[\\.,]\\d+)?)\\s*(.*)$")
  op <- str_trim(m[,2])
  val <- suppressWarnings(as.numeric(str_replace_all(m[,3], ",", ".")))
  unit <- str_squish(m[,4]); unit[unit == ""] <- NA_character_
  if (impute_ineq) val <- case_when(
    op %in% c("<","<=","≤") ~ val/2,
    op %in% c(">",">=","≥") ~ val*2,
    TRUE ~ val
  )
  list(value = val, unit = unit)
}

# Длительность в часы: "HH:MM" / "HH:MM:SS" -> numeric hours, иначе число трактуем как часы.
dur_to_hours <- function(x){
  x <- clean_na(x)
  x <- str_split_fixed(x, "\\s*\\|\\s*", 2)[,1]
  
  m <- str_match(x, "^\\s*(\\d{1,3})\\s*:\\s*(\\d{1,2})(?::\\s*(\\d{1,2}))?\\s*$")
  is_time <- !is.na(m[,1])
  
  out <- rep(NA_real_, length(x))
  if (any(is_time)) {
    hh <- as.numeric(m[is_time,2])
    mm <- as.numeric(m[is_time,3])
    ss <- suppressWarnings(as.numeric(m[is_time,4])); ss[is.na(ss)] <- 0
    out[is_time] <- hh + mm/60 + ss/3600
  }
  if (any(!is_time)) out[!is_time] <- to_num(x[!is_time])
  
  # Явно нормализуем -0 к 0 (на всякий случай)
  out[!is.na(out) & abs(out) < 1e-12] <- 0
  out
}

# ------------------------- datetime helpers -------------------------

# Универсальный парсер дат/времени: строки разных форматов + Excel-числа.
# tz оставляем "UTC" как "нейтральную" шкалу времени.
parse_dt <- function(x, tz = "UTC") {
  x <- clean_na(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  
  out <- rep(as.POSIXct(NA, tz = tz), length(x))
  
  # Excel serial numbers (дата как число)
  x_num <- suppressWarnings(as.numeric(str_replace_all(x, ",", ".")))
  is_num <- !is.na(x_num) & str_detect(x, "^\\d+(?:[\\.,]\\d+)?$")
  if (any(is_num)) {
    out[is_num] <- as.POSIXct(x_num[is_num] * 86400, origin = "1899-12-30", tz = tz)
  }
  
  idx <- which(!is_num & !is.na(x))
  if (length(idx)) {
    out[idx] <- suppressWarnings(lubridate::parse_date_time(
      x[idx],
      orders = c("dmy HMS","dmy HM","dmy", "ymd HMS","ymd HM","ymd", "mdy HMS","mdy HM","mdy"),
      tz = tz
    ))
  }
  out
}

# Добавляет новую datetime-колонку из исходной (src), если src есть; иначе создаёт NA.
add_dt <- function(df, src, new, tz = "UTC") {
  if (src %in% names(df)) {
    df[[new]] <- suppressWarnings(parse_dt(df[[src]], tz = tz))
  } else {
    df[[new]] <- as.POSIXct(NA, tz = tz)
  }
  df
}

# --------------------- JSON extraction  ----------------------

# Чтение JSON (как list-of-lists)
read_json_list <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)

# scalar-safe character extractor: NULL/length-0 -> NA_character_
chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x[[1]])
}

# ---------- helpers for nested extraction ----------
# Функции для работы со вложенными JSON-полями
pluck0 <- function(x, ..., .default = NA) {
  v <- purrr::pluck(x, ..., .default = .default)
  if (is.null(v) || length(v) == 0) .default else v
}

chr0 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  clean_na(x)
}

num0 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  to_num(x)
}

# Склеивает отдельные поля даты и времени в одну строку.
# Если времени нет, возвращает только дату.
combine_date_time <- function(date, time) {
  date <- clean_na(date); time <- clean_na(time)
  dplyr::case_when(
    !is.na(date) & !is.na(time) ~ paste(date, time),
    !is.na(date) ~ date,
    TRUE ~ NA_character_
  )
}

# Приводит разные варианты "true/false", "1/0", "yes/no" к виду "Да"/"Нет".
yn_ru <- function(x){
  if (length(x) == 0) return(NA_character_)
  if (is.logical(x)) {
    return(dplyr::case_when(
      isTRUE(x) ~ "Да",
      identical(x, FALSE) ~ "Нет",
      TRUE ~ NA_character_
    ))
  }
  xl <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    xl %in% c("true","t","1","да","yes","y") ~ "Да",
    xl %in% c("false","f","0","нет","no","n") ~ "Нет",
    TRUE ~ NA_character_
  )
}

# Разбор ISO-дат из JSON (например "2026-01-01T12:30:00Z").
parse_iso_utc <- function(x){
  x <- clean_na(x)
  if (all(is.na(x))) return(as.POSIXct(rep(NA, length(x)), tz = "UTC"))
  out <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE))
  miss <- is.na(out) & !is.na(x)
  if (any(miss)) out[miss] <- suppressWarnings(lubridate::ymd_hm(x[miss], tz = "UTC", quiet = TRUE))
  out
}

# Приводит дату/время к читаемому формату "дд.мм.гггг ЧЧ:ММ".
fmt_any_dmy_hm <- function(x, tz_out = REPORT_TZ){
  x <- clean_na(x)
  dplyr::if_else(
    is.na(x),
    NA_character_,
    dplyr::if_else(
      stringr::str_detect(x, "\\d{2}\\.\\d{2}\\.\\d{4}"),
      x,
      {
        dt <- parse_iso_utc(x)
        dt <- suppressWarnings(lubridate::with_tz(dt, tz_out))
        dplyr::if_else(is.na(dt), x, format(dt, "%d.%m.%Y %H:%M"))
      }
    )
  )
}

dur_days  <- function(block) num0(pluck0(block, "общаяДлительность", "ОбщаяДлительностьСуток",  .default = NA))
dur_hours <- function(block) num0(pluck0(block, "общаяДлительность", "ОбщаяДлительностьЧасов", .default = NA))

# Извлекает числовое лабораторное значение из JSON-объекта.
lab_num <- function(obj, impute_ineq = FALSE) {
  if (is.null(obj)) return(NA_real_)
  
  val <- if (is.list(obj)) pluck0(obj, "значение", .default = NA) else obj
  if (is.null(val) || (length(val) == 1 && is.na(val))) return(NA_real_)
  
  # в JSON иногда приходит строка вида ">100" — как в excel-версии (impute_ineq = TRUE только для PCT)
  if (is.character(val)) {
    sp <- split_val_unit(val, impute_ineq = isTRUE(impute_ineq))
    return(sp$value)
  }
  
  suppressWarnings(as.numeric(val))
}

# Извлекает единицу измерения из лабораторного JSON-объекта.
lab_unit <- function(obj) {
  if (is.null(obj)) return(NA_character_)
  if (!is.list(obj)) return(NA_character_)
  chr0(pluck0(obj, "единица", .default = NA_character_))
}

# Нормализует результаты посевов к небольшому числу стандартных вариантов.
norm_culture <- function(x){
  x <- norm_txt(x)
  dplyr::case_when(
    x %in% c("Положительный посев", "Положительный результат") ~ "Положительный посев",
    x %in% c("Отрицательный посев", "Отрицательный результат") ~ "Отрицательный посев",
    x == "Посев не проводился" ~ "Посев не проводился",
    x %in% c("Нет данных", "Нет значения") ~ NA_character_,
    TRUE ~ x
  )
}

# Унификация результатов посева для бинарной логики (например, бактериемии): 1 = положительный, 0 = отрицательный, N/A = посев не проводился/нет данных
culture_to_1_0_na <- function(x) {
  x <- norm_txt(x)
  dplyr::case_when(
    x %in% c("Положительный посев", "Положительный результат") ~ "1",
    x %in% c("Отрицательный посев", "Отрицательный результат") ~ "0",
    x %in% c("Посев не проводился", "Не проводился") ~ "N/A",
    TRUE ~ "N/A"
  ) |>
    factor(levels = c("1", "0", "N/A"))
}


# Единая классификация бактериальной флоры по грам-окраске.
# mix   = положительный и грам+ и грам- посев
# neg   = оба посева отрицательные
# gram+ = положительный только грам+ посев
# gram- = положительный только грам- посев
# N/A   = оба посева не проводились / нет данных
gram_stain_class <- function(gram_positive, gram_negative) {
  gp <- norm_txt(gram_positive)
  gn <- norm_txt(gram_negative)

  gp_pos <- gp %in% c("Положительный посев", "Положительный результат")
  gn_pos <- gn %in% c("Положительный посев", "Положительный результат")
  gp_neg <- gp %in% c("Отрицательный посев", "Отрицательный результат")
  gn_neg <- gn %in% c("Отрицательный посев", "Отрицательный результат")
  gp_na  <- is.na(gp) | gp %in% c("Посев не проводился", "Не проводился", "Нет данных", "Нет значения")
  gn_na  <- is.na(gn) | gn %in% c("Посев не проводился", "Не проводился", "Нет данных", "Нет значения")

  dplyr::case_when(
    gp_pos & gn_pos ~ "mix",
    gp_neg & gn_neg ~ "neg",
    gp_pos & !gn_pos ~ "gram+",
    !gp_pos & gn_pos ~ "gram-",
    gp_na & gn_na ~ "N/A",
    TRUE ~ "N/A"
  ) |>
    factor(levels = c("mix", "gram+", "gram-", "neg", "N/A"))
}

unit_norm <- function(u){
  u <- norm_txt(u)
  u <- str_to_lower(u)
  u <- str_replace_all(u, "μ", "µ")
  u <- str_replace_all(u, "\\s+", "")
  u
}


# 3b. Checkbox mappings for new JSON schema --------------------------------

# "ОсновнойДиагноз" (новая форма с selected/other)
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

# Только scalar character: если пришёл list (например новый checkbox-объект), возвращаем NA.
chr_scalar0 <- function(x) {
  if (is.null(x) || length(x) == 0 || is.list(x)) return(NA_character_)
  clean_na(as.character(x)[1])
}

# Нормализованный ключ для сопоставления checkbox-значений.
# Нужен, потому что в JSON встречаются варианты с/без скобок, с разными тире,
# а иногда без аббревиатур СПОН/ССВР.
checkbox_key <- function(x) {
  x <- norm_txt(x)
  x <- stringr::str_replace_all(x, "ё", "е")
  x <- stringr::str_replace_all(x, "Ё", "Е")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[\\(\\)]", " ")
  x <- stringr::str_replace_all(x, "[^[:alnum:]а-яА-Я]+", " ")
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

checkbox_selected <- function(x) {
  if (!is.list(x)) return(character(0))
  sel <- pluck0(x, "selected", .default = character(0))
  if (is.null(sel) || !length(sel)) return(character(0))
  norm_txt(unlist(sel, use.names = FALSE))
}

checkbox_other <- function(x) {
  if (!is.list(x)) return(NA_character_)
  chr0(pluck0(x, "other", .default = NA_character_))
}

checkbox_apply_aliases <- function(selected, aliases = NULL) {
  selected <- norm_txt(selected)
  if (!length(selected) || is.null(aliases) || !length(aliases)) return(selected)

  alias_keys <- checkbox_key(names(aliases))
  selected_keys <- checkbox_key(selected)
  hit <- match(selected_keys, alias_keys)

  out <- selected
  out[!is.na(hit)] <- unname(aliases)[hit[!is.na(hit)]]
  out
}

checkbox_with_extra_selected <- function(x, extra_selected = character(0), extra_other = character(0)) {
  extra_selected <- norm_txt(extra_selected)
  extra_selected <- extra_selected[!is.na(extra_selected) & extra_selected != ""]
  extra_other <- norm_txt(extra_other)
  extra_other <- extra_other[!is.na(extra_other) & extra_other != ""]

  if (!length(extra_selected) && !length(extra_other)) return(x)

  if (is.list(x) && any(c("selected", "other") %in% names(x))) {
    x$selected <- unique(c(checkbox_selected(x), extra_selected))
    other_pieces <- c(checkbox_other(x), extra_other)
    other_pieces <- other_pieces[!is.na(other_pieces) & other_pieces != ""]
    x$other <- if (length(other_pieces)) paste(unique(other_pieces), collapse = " | ") else NA_character_
    return(x)
  }

  list(
    selected = unique(extra_selected),
    other = if (length(extra_other)) paste(unique(extra_other), collapse = " | ") else NA_character_
  )
}

checkbox_drop_selected <- function(x, drop_selected = character(0)) {
  if (!is.list(x) || !any(c("selected", "other") %in% names(x))) return(x)

  drop_selected <- norm_txt(drop_selected)
  selected <- checkbox_selected(x)
  x$selected <- selected[!(selected %in% drop_selected)]
  x
}

# Для обратной совместимости:
# - если поле было строкой, используем её;
# - если поле стало checkbox-объектом, склеиваем selected (+ other) в одну строку.
checkbox_collapse <- function(x, aliases = NULL) {
  if (!is.list(x)) return(NA_character_)
  sel <- checkbox_apply_aliases(checkbox_selected(x), aliases = aliases)
  oth <- checkbox_other(x)
  pieces <- c(sel, oth)
  pieces <- pieces[!is.na(pieces) & pieces != ""]
  if (!length(pieces)) return(NA_character_)
  paste(unique(pieces), collapse = " | ")
}

# Возвращает список колонок dummy/text для одной checkbox-переменной.
# Старые записи без selected/other получают NA в новых колонках.
# Новые записи с checkbox-структурой получают 0/1 + text/raw.
checkbox_cols <- function(x, mapping, aliases = NULL, other_col, raw_col = NULL) {
  has_checkbox <- is.list(x) && any(c("selected", "other") %in% names(x))
  init_val <- if (has_checkbox) 0L else NA_integer_

  out <- as.list(rep(init_val, length(mapping)))
  names(out) <- unname(mapping)

  sel_raw <- checkbox_selected(x)
  sel_std <- checkbox_apply_aliases(sel_raw, aliases = aliases)

  # Сопоставляем не по буквальному тексту, а по нормализованному ключу:
  # это защищает от вариантов "СПОН"/без "СПОН", "ССВР"/без "ССВР",
  # разных тире и пробелов.
  map_keys <- checkbox_key(names(mapping))
  sel_keys <- checkbox_key(sel_std)
  hit <- match(sel_keys, map_keys)
  hit <- hit[!is.na(hit)]

  if (length(hit)) {
    matched_cols <- unique(unname(mapping[hit]))
    out[matched_cols] <- rep(list(1L), length(matched_cols))
  }

  out[[other_col]] <- if (has_checkbox) checkbox_other(x) else NA_character_

  if (!is.null(raw_col)) {
    out[[raw_col]] <- if (has_checkbox && length(sel_raw)) paste(sel_raw, collapse = " | ") else NA_character_
  }

  out
}


# Приводит ручные bin-колонки из Excel к 0/1/NA.
# Поддерживает числа 0/1 и частые текстовые варианты на случай ручного ввода.
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

  for (nm in manual_bin_cols) {
    manual_tbl[[nm]] <- manual_bin_to_int(manual_tbl[[nm]])
  }

  has_any_manual_bin <- length(manual_bin_cols) > 0 &&
    any(!is.na(unlist(manual_tbl[manual_bin_cols], use.names = FALSE)))

  if (length(manual_bin_cols) > 0 && isTRUE(blank_binary_as_zero) && has_any_manual_bin) {
    # После ручной проверки пустая bin-ячейка трактуется как 0,
    # но только если в файле вообще есть хотя бы одно заполненное bin-значение.
    for (nm in manual_bin_cols) {
      manual_tbl[[nm]][is.na(manual_tbl[[nm]])] <- 0L
    }
  }

  if (length(manual_text_cols) > 0) {
    for (nm in manual_text_cols) {
      manual_tbl[[nm]] <- clean_na(manual_tbl[[nm]])
    }
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
    dplyr::left_join(manual_tbl, by = "pat_record_id")

  for (nm in update_cols) {
    manual_nm <- paste0(nm, "__manual")
    idx <- !is.na(out[[manual_nm]])
    out[[nm]][idx] <- out[[manual_nm]][idx]
  }

  out <- out %>% dplyr::select(-dplyr::ends_with("__manual"))

  message(
    "Manual indications merged: ",
    dplyr::n_distinct(manual_raw$pat_record_id), " ids in file; ",
    sum(patients_tidy$pat_record_id %in% manual_raw$pat_record_id), " patients_tidy rows matched; ",
    length(update_cols), " columns updated."
  )

  out
}

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
    ICU_in_datetime = chr0(combine_date_time(
      pluck0(d, "ДатаВремяПоступленияОРИТ_Дата",  .default = NA_character_),
      pluck0(d, "ДатаВремяПоступленияОРИТ_Время", .default = NA_character_)
    )),
    ICU_out_or_death_date = chr0(combine_date_time(
      pluck0(outcome_block, "ДатаВремяВыпискиОРИТСмерти_Дата",  .default = NA_character_),
      pluck0(outcome_block, "ДатаВремяВыпискиОРИТСмерти_Время", .default = NA_character_)
    )),
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
      PaFiO2  = num0(pluck0(cl, "PaO2_FiO2", .default = NA)),
      SpFiO2  = num0(pluck0(cl, "SpO2_FiO2", .default = NA)),
      
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

# фиксируем порядок таймпойнтов 
tp_levels_core <- c("m12h_0h", "48h_72h", "4d_6d", "7d_10d")
tp_levels <- c(tp_levels_core, setdiff(sort(unique(patients_tidy$timepoint)), tp_levels_core))

patients_tidy <- patients_tidy %>%
  mutate(timepoint = factor(timepoint, levels = tp_levels, ordered = TRUE))
  
# 8. Build HA_tidy --------------------------------------------------------

# Извлекает все процедуры гемосорбции одного пациента из блока data.
extract_ha_rows <- function(pat_record_id, data) {
  procs <- pluck0(data, "ПроцедурыГемосорбции", .default = list())
  if (is.null(procs) || !length(procs)) return(tibble::tibble())
  
  ha0 <- purrr::map_dfr(procs, function(p) {
    tibble::tibble(
      pat_record_id = pat_record_id,
      HA_cartrige_type = pluck0(p, "DeviceType", .default = NA_character_),
      HA_serial_number = pluck0(p, "СерийныйНомерУстройства", .default = NA_character_),
      HA_start_datetime = combine_date_time(
        pluck0(p, "ДатаВремяНачалаГС_Дата",  .default = NA_character_),
        pluck0(p, "ДатаВремяНачалаГС_Время", .default = NA_character_)
      ),
      HA_duration = pluck0(p, "ДлительностьГС", .default = NA_character_),
      HA_avg_blood_flow = pluck0(p, "СредняяСкоростьПотока", .default = NA),
      HA_anticoagulation = pluck0(p, "Антикоагуляция", .default = NA_character_),
      HA_anticoagulation_other = pluck0(p, "АнтикоагуляцияДругое", .default = NA_character_),
      HA_other_methods = pluck0(p, "КомбинацияСДругимиЭкстракорпоральнымиМетодами", .default = NA_character_),
      HA_adverse_effects = pluck0(p, "НежелательныеЯвления", .default = NA_character_)
    )
  })
  
  # задаём HA_parameter как "Гемоперфузия 1/2/..." по времени старта (если старта нет — по порядку)
  ha0 <- ha0 %>%
    mutate(.start_dt_tmp = parse_dt(HA_start_datetime, tz = "UTC")) %>%
    arrange(.start_dt_tmp) %>%
    mutate(HA_parameter = paste0("Гемоперфузия ", dplyr::row_number())) %>%
    select(-.start_dt_tmp)
  
  ha0
}

HA_tidy <- purrr::pmap_dfr(
  reg_tbl %>% dplyr::select(pat_record_id, data),
  extract_ha_rows
) %>%
  mutate(
    HA_avg_blood_flow = to_num(HA_avg_blood_flow),
    HA_duration = round(dur_to_hours(HA_duration), 2)
  )

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
for (src in names(dt_map)) {
  patients_tidy <- add_dt(patients_tidy, src, dt_map[[src]], tz = "UTC")
}

# 3) удаляем "оригинальные" колонки, чтобы остались только *_raw и *_dt
patients_tidy <- patients_tidy %>%
  select(-any_of(names(dt_map)))

# HA: datetime + нормализация типа картриджа
HA_tidy <- HA_tidy %>%
  mutate(HA_start_datetime_raw = HA_start_datetime) %>%
  add_dt("HA_start_datetime", "HA_start_dt") %>%
  mutate(
    HA_cartrige_type_std = str_to_upper(str_squish(as.character(HA_cartrige_type)))
  ) %>%
  # Аналогично: оставляем только parsed-вариант даты старта (HA_start_dt)
  select(-any_of("HA_start_datetime"))

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
  patients_tidy$fibrinogen_raw      <- patients_tidy$fibrinogen
  patients_tidy$fibrinogen_unit_raw <- patients_tidy$fibrinogen_unit
  
  fu <- unit_norm(patients_tidy$fibrinogen_unit)
  
  is_gdl  <- !is.na(fu) & str_detect(fu, "^(г|g)/(дл|dl)\\.?$")
  is_gl   <- !is.na(fu) & str_detect(fu, "^(г|g)/(л|l)\\.?$")
  is_mgl  <- !is.na(fu) & str_detect(fu, "^(мг|mg)/(л|l)\\.?$")
  is_mgdl <- !is.na(fu) & str_detect(fu, "^(мг|mg)/(дл|dl)\\.?$")
  
  # Если указано г/дл, но значение > 100, считаем, что это ошибка ввода:
  # фактически значение уже в мг/дл, поэтому не умножаем на 1000.
  is_gdl_but_probably_mgdl <- is_gdl & !is.na(patients_tidy$fibrinogen) &
    patients_tidy$fibrinogen > 100
  
  patients_tidy$fibrinogen <- dplyr::case_when(
    is_gdl_but_probably_mgdl ~ patients_tidy$fibrinogen,          # likely mg/dl
    is_gdl                   ~ patients_tidy$fibrinogen * 1000,   # g/dl -> mg/dl
    is_gl                    ~ patients_tidy$fibrinogen * 100,    # g/l  -> mg/dl
    is_mgl                   ~ patients_tidy$fibrinogen / 10,     # mg/l -> mg/dl
    is_mgdl                  ~ patients_tidy$fibrinogen,          # mg/dl -> mg/dl
    TRUE                     ~ NA_real_
  )
  
  patients_tidy$fibrinogen_unit <- dplyr::case_when(
    is_gdl | is_gl | is_mgl | is_mgdl ~ "мг/дл",
    TRUE ~ NA_character_
  )
}


# Общий билирубин -> мкмоль/л
if (all(c("bilirubin_total", "bilirubin_total_unit") %in% names(patients_tidy))) {
  patients_tidy$bilirubin_total_raw      <- patients_tidy$bilirubin_total
  patients_tidy$bilirubin_total_unit_raw <- patients_tidy$bilirubin_total_unit
  
  bu <- unit_norm(patients_tidy$bilirubin_total_unit)
  
  is_mmol <- !is.na(bu) & str_detect(bu, "^(ммоль|mmol)/(л|l)\\.?$")
  is_umol <- !is.na(bu) & str_detect(bu, "^(мкмоль|umol|µmol)/(л|l)\\.?$")
  is_mgdl <- !is.na(bu) & str_detect(bu, "^(мг|mg)/(дл|dl)\\.?$")
  is_mgl  <- !is.na(bu) & str_detect(bu, "^(мг|mg)/(л|l)\\.?$")
  
  patients_tidy$bilirubin_total <- dplyr::case_when(
    is_mmol ~ patients_tidy$bilirubin_total * 1000,     # mmol/l -> µmol/l
    is_umol ~ patients_tidy$bilirubin_total,            # µmol/l -> µmol/l
    is_mgdl ~ patients_tidy$bilirubin_total * 17.104,   # mg/dl  -> µmol/l
    is_mgl  ~ patients_tidy$bilirubin_total * 1.7104,   # mg/l   -> µmol/l
    TRUE    ~ NA_real_
  )
  
  patients_tidy$bilirubin_total_unit <- dplyr::case_when(
    is_mmol | is_umol | is_mgdl | is_mgl ~ "мкмоль/л",
    TRUE ~ NA_character_
  )
}


# D-димер -> мг/л FEU
if (all(c("D_dimer", "D_dimer_unit") %in% names(patients_tidy))) {
  patients_tidy$D_dimer_raw      <- patients_tidy$D_dimer
  patients_tidy$D_dimer_unit_raw <- patients_tidy$D_dimer_unit
  
  du <- unit_norm(patients_tidy$D_dimer_unit)
  
  is_mgl  <- !is.na(du) & str_detect(du, "^(мг|mg)/(л|l)")
  is_mgdl <- !is.na(du) & str_detect(du, "^(мг|mg)/(дл|dl)")
  is_ugml <- !is.na(du) & str_detect(du, "^(мкг|µg|ug)/(мл|ml)")
  is_ugl  <- !is.na(du) & str_detect(du, "^(мкг|µg|ug)/(л|l)")
  is_ngml <- !is.na(du) & str_detect(du, "^(нг|ng)/(мл|ml)")
  
  is_feu  <- !is.na(du) & str_detect(du, "feu|феу")
  is_ddu  <- !is.na(du) & str_detect(du, "ddu")
  
  is_d_dimer_unit_known <- is_mgl | is_mgdl | is_ugml | is_ugl | is_ngml
  
  # Сначала приводим концентрацию к мг/л.
  d_mgl <- dplyr::case_when(
    is_mgdl ~ patients_tidy$D_dimer * 10,      # mg/dl -> mg/l
    is_mgl  ~ patients_tidy$D_dimer,           # mg/l  -> mg/l
    is_ugml ~ patients_tidy$D_dimer,           # µg/ml -> mg/l
    is_ugl  ~ patients_tidy$D_dimer / 1000,    # µg/l  -> mg/l
    is_ngml ~ patients_tidy$D_dimer / 1000,    # ng/ml -> mg/l
    TRUE    ~ NA_real_
  )
  
  # Если явно указано DDU, переводим в FEU.
  # Если FEU не указан, но единица концентрации известна, оставляем как мг/л
  # и считаем целевой единицей мг/л FEU.
  patients_tidy$D_dimer <- dplyr::case_when(
    is_d_dimer_unit_known & is_ddu ~ d_mgl * 2,
    is_d_dimer_unit_known          ~ d_mgl,
    TRUE                           ~ NA_real_
  )
  
  patients_tidy$D_dimer_unit <- dplyr::case_when(
    is_d_dimer_unit_known ~ "мг/л FEU",
    TRUE ~ NA_character_
  )
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
    icu_to_HA1_hours = round(as.numeric(difftime(first_sorption_dt,   ICU_in_dt, units = "hours")), 1)
  ) %>%
  select(
    pat_record_id,
    is_lps_72h,
    ICU_to_first_LPS,
    ICU_to_first_CT,
    icu_to_HA1_hours,
    first_sorption_dt,
    first_sorption_type
  )

patients_tidy <- patients_tidy %>%
  left_join(ha_by_id, by = "pat_record_id") %>%
  mutate(
    is_lps_72h = coalesce(as.integer(is_lps_72h), 0L)
  )

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
      .event_status   = dplyr::case_when(
        outcome == "Умер" ~ "dead",
        outcome == "Выписан из ОРИТ" ~ "out",
        outcome %in% c("Переведён в другой ОРИТ", "Продолжает находиться в ОРИТ") ~ "in",
        TRUE ~ "n/a"
      ),
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

# Единая колонка бактериальной флоры по грам-окраске + бинарные индикаторы по каждому посеву:
if (all(c("pat_gram_positive", "pat_gram_negative") %in% names(patients_tidy))) {
  patients_tidy <- patients_tidy %>%
    dplyr::mutate(
      pat_gram_stain    = gram_stain_class(pat_gram_positive, pat_gram_negative),
      is_pat_gram_plus  = culture_to_1_0_na(pat_gram_positive),
      is_pat_gram_minus = culture_to_1_0_na(pat_gram_negative)
    ) %>%
    dplyr::relocate(pat_gram_stain, is_pat_gram_plus, is_pat_gram_minus, .after = pat_gram_negative)
}

if ("pat_bacteremia" %in% names(patients_tidy)) {
  patients_tidy$is_bacteremic <- culture_to_1_0_na(patients_tidy$pat_bacteremia)
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
all_ids <- unique(clean_na(reg_tbl$pat_record_id))
pt_ids  <- unique(clean_na(patients_tidy$pat_record_id))

all_ids <- all_ids[!is.na(all_ids) & all_ids != ""]
pt_ids  <- pt_ids[!is.na(pt_ids) & pt_ids != ""]

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
    "first_sorption_dt",
    "status",
    "INR",
    "PaFiO2",
    "SpFiO2",
    "PaFiO2_calc"
  ),
  n_missing = c(
    count_na_col(patients_tidy, "ICU_in_dt"),
    count_na_col(patients_tidy, "ICU_out_or_death_dt"),
    count_na_col(patients_tidy, "first_sorption_dt"),
    count_na_col(patients_tidy, "status"),
    count_na_col(patients_tidy, "INR"),
    count_na_col(patients_tidy, "PaFiO2"),
    count_na_col(patients_tidy, "SpFiO2"),
    count_na_col(patients_tidy, "PaFiO2_calc")
  )
)

qc_missing_ha <- tibble::tibble(
  variable = c("HA_start_dt"),
  n_missing = c(count_na_col(HA_tidy, "HA_start_dt"))
)

cat("\nMissingness in patients_tidy:\n")
print(qc_missing_pat)

cat("\nMissingness in HA_tidy:\n")
print(qc_missing_ha)

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

# Сохраняем две итоговые таблицы в data/:
if (isTRUE(save_rds)) {
  saveRDS(patients_tidy, patients_out_rds)
  saveRDS(HA_tidy, ha_out_rds)
}

if (isTRUE(save_xlsx)) {
  openxlsx::write.xlsx(patients_tidy, patients_out_xlsx, asTable = TRUE, overwrite = TRUE)
  openxlsx::write.xlsx(HA_tidy, ha_out_xlsx, asTable = TRUE, overwrite = TRUE)
}