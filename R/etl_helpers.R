# 2Reg ETL: shared low-level helpers
#
# Только технические функции разбора/очистки данных.
# Клиническая логика и правила преобразования остаются в основных ETL-скриптах.


pick_path <- function(path_candidates) {
  hit <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(hit)) stop("Файл не найден: ", basename(path_candidates[[1]]))
  hit
}

optional_path <- function(path_candidates) {
  hit <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(hit)) NA_character_ else hit
}

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
  ) %>%
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
  ) %>%
    factor(levels = c("mix", "gram+", "gram-", "neg", "N/A"))
}

unit_norm <- function(u){
  u <- norm_txt(u)
  u <- str_to_lower(u)
  u <- str_replace_all(u, "μ", "µ")
  u <- str_replace_all(u, "\\s+", "")
  u
}



# Checkbox parsing helpers -------------------------------------------------

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

