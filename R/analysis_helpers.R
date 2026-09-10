# 2Reg: shared analysis helpers
# This file contains only reusable technical helpers.
# It does NOT read patients_tidy / HA_tidy and does NOT create analysis datasets.

pick_path <- function(path_candidates) {
  hit <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(hit)) {
    stop("Файл не найден: ", basename(path_candidates[[1]]))
  }
  hit
}

norm_txt <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, "[\u2212\u2013\u2014]", "-")
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

clean_chr <- function(x) {
  x <- norm_txt(x)
  x <- stringr::str_to_lower(x)
  x[x %in% c("", "na", "n/a", "нет данных", "нет значения")] <- NA_character_
  x
}

yes01 <- function(x) {
  x <- clean_chr(x)

  dplyr::case_when(
    x %in% c("да", "yes", "true", "1") ~ 1,
    x %in% c("нет", "no", "false", "0") ~ 0,
    TRUE ~ NA_real_
  )
}

bin_from_any <- function(x) {
  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (is.numeric(x)) {
    return(dplyr::case_when(
      x == 1 ~ 1,
      x == 0 ~ 0,
      TRUE ~ NA_real_
    ))
  }

  yes01(x)
}

num_from_any <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_extract(x, "-?\\d+(?:\\.\\d+)?")
  suppressWarnings(as.numeric(x))
}

present01 <- function(x) {
  x <- clean_chr(x)

  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x %in% c("нет", "no", "false", "0") ~ 0,
    stringr::str_detect(x, "не было|не примен|отсутств|без особенностей") ~ 0,
    TRUE ~ 1
  )
}

culture01 <- function(x) {
  x <- clean_chr(x)

  dplyr::case_when(
    stringr::str_detect(x, "полож") ~ 1,
    stringr::str_detect(x, "отриц") ~ 0,
    stringr::str_detect(x, "не провод") ~ NA_real_,
    TRUE ~ NA_real_
  )
}

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[[1]]
}

first_non_empty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & stringr::str_squish(x) != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

fmt_int <- function(x) formatC(as.integer(round(x)), format = "f", digits = 0)
fmt_1 <- function(x) formatC(x, format = "f", digits = 1)
fmt_2 <- function(x) formatC(x, format = "f", digits = 2)

unit_mode <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[[1]]
}

lab_u <- function(base_ru, unit) {
  if (!is.na(unit) && unit != "") {
    paste0(base_ru, " (", unit, ")")
  } else {
    base_ru
  }
}

pat_gram_stain_for_tables <- function(x) {
  x <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    as.character(x) == "mix" ~ "Смешанные",
    as.character(x) == "gram+" ~ "Грамположительные",
    as.character(x) == "gram-" ~ "Грамотрицательные",
    as.character(x) == "neg" ~ "Отрицательный посев",
    as.character(x) == "N/A" ~ "Посев не проводился",
    TRUE ~ NA_character_
  )

  factor(
    x,
    levels = c(
      "Смешанные",
      "Грамположительные",
      "Грамотрицательные",
      "Отрицательный посев",
      "Посев не проводился"
    )
  )
}

read_dictionary_labels <- function(path) {
  readxl::read_excel(path, col_types = "text") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::transmute(
      dataset = stringr::str_squish(stringr::str_replace_all(as.character(dataset), "\u00A0", " ")),
      short = stringr::str_squish(stringr::str_replace_all(as.character(short), "\u00A0", " ")),
      ru = stringr::str_squish(stringr::str_replace_all(as.character(ru), "\u00A0", " ")),
      kind = stringr::str_squish(stringr::str_replace_all(as.character(kind), "\u00A0", " "))
    ) %>%
    dplyr::mutate(
      dataset = dplyr::case_when(
        stringr::str_to_lower(dataset) %in% c("patients_tidy", "patients", "patient") ~ "patients",
        stringr::str_to_lower(dataset) %in% c("ha_tidy", "ha") ~ "ha",
        TRUE ~ stringr::str_to_lower(dataset)
      )
    )
}

make_ru_map <- function(dict, dataset_name = "patients") {
  dict %>%
    dplyr::filter(dataset == dataset_name) %>%
    dplyr::distinct(short, ru) %>%
    dplyr::filter(!is.na(short), short != "", !is.na(ru), ru != "") %>%
    tibble::deframe()
}

ru_or <- function(key, ru_map) {
  x <- unname(ru_map[[key]])
  if (is.null(x) || is.na(x) || x == "") key else x
}

assert_columns <- function(df, cols, object_name = deparse(substitute(df))) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "В ", object_name, " отсутствуют обязательные колонки: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  invisible(TRUE)
}
