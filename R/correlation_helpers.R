# 2Reg: helpers specific to correlation analyses

cor_matrix_safe <- function(x, method = "spearman", min_n = 5) {
  x <- as.data.frame(x)
  n <- ncol(x)

  r <- matrix(
    NA_real_,
    nrow = n,
    ncol = n,
    dimnames = list(names(x), names(x))
  )

  diag(r) <- 1

  if (n < 2) return(r)

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      pair <- x[, c(i, j), drop = FALSE]
      pair <- pair[stats::complete.cases(pair), , drop = FALSE]

      if (
        nrow(pair) >= min_n &&
        dplyr::n_distinct(pair[[1]], na.rm = TRUE) > 1 &&
        dplyr::n_distinct(pair[[2]], na.rm = TRUE) > 1
      ) {
        r_ij <- suppressWarnings(
          stats::cor(pair[[1]], pair[[2]], method = method)
        )

        r[i, j] <- r_ij
        r[j, i] <- r_ij
      }
    }
  }

  r
}

cor_pmat <- function(x, method = "spearman", min_n = 5) {
  x <- as.data.frame(x)
  n <- ncol(x)

  p <- matrix(
    NA_real_,
    nrow = n,
    ncol = n,
    dimnames = list(names(x), names(x))
  )

  diag(p) <- 0

  if (n < 2) return(p)

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      pair <- x[, c(i, j), drop = FALSE]
      pair <- pair[stats::complete.cases(pair), , drop = FALSE]

      if (
        nrow(pair) >= min_n &&
        dplyr::n_distinct(pair[[1]], na.rm = TRUE) > 1 &&
        dplyr::n_distinct(pair[[2]], na.rm = TRUE) > 1
      ) {
        p_ij <- tryCatch(
          suppressWarnings(
            stats::cor.test(
              pair[[1]],
              pair[[2]],
              method = method,
              exact = FALSE
            )$p.value
          ),
          error = function(e) NA_real_
        )

        p[i, j] <- p_ij
        p[j, i] <- p_ij
      }
    }
  }

  p
}

prep_p_for_corrplot <- function(P, M) {
  P_plot <- P
  P_plot[is.na(M)] <- NA
  P_plot[is.na(P_plot) & !is.na(M)] <- 1
  P_plot
}

partial_spearman_matrix_safe <- function(x, control_var = "SOFA", min_n = 5) {
  x <- as.data.frame(x)

  if (!control_var %in% names(x)) {
    stop("Не найдена переменная для поправки: ", control_var)
  }

  vars <- setdiff(names(x), control_var)
  k <- 1

  r <- matrix(
    NA_real_,
    nrow = length(vars),
    ncol = length(vars),
    dimnames = list(vars, vars)
  )

  p <- matrix(
    NA_real_,
    nrow = length(vars),
    ncol = length(vars),
    dimnames = list(vars, vars)
  )

  diag(r) <- 1
  diag(p) <- 0

  if (length(vars) < 2) {
    return(list(r = r, p = p))
  }

  for (i in seq_len(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
      v1 <- vars[[i]]
      v2 <- vars[[j]]

      dat <- x[, c(v1, v2, control_var), drop = FALSE]
      dat <- dat[stats::complete.cases(dat), , drop = FALSE]

      if (
        nrow(dat) >= min_n &&
        dplyr::n_distinct(dat[[v1]], na.rm = TRUE) > 1 &&
        dplyr::n_distinct(dat[[v2]], na.rm = TRUE) > 1 &&
        dplyr::n_distinct(dat[[control_var]], na.rm = TRUE) > 1 &&
        stats::sd(dat[[v1]], na.rm = TRUE) > 0 &&
        stats::sd(dat[[v2]], na.rm = TRUE) > 0 &&
        stats::sd(dat[[control_var]], na.rm = TRUE) > 0
      ) {
        dat_rank <- dat %>%
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ rank(.x, ties.method = "average", na.last = "keep")
            )
          )

        fit1 <- stats::lm(dat_rank[[v1]] ~ dat_rank[[control_var]])
        fit2 <- stats::lm(dat_rank[[v2]] ~ dat_rank[[control_var]])

        r_ij <- suppressWarnings(
          stats::cor(
            stats::residuals(fit1),
            stats::residuals(fit2),
            method = "pearson",
            use = "complete.obs"
          )
        )

        if (is.finite(r_ij)) {
          r_ij <- max(min(r_ij, 0.999999), -0.999999)
          df <- nrow(dat_rank) - k - 2

          p_ij <- if (df > 0) {
            t_ij <- r_ij * sqrt(df / (1 - r_ij^2))
            2 * stats::pt(abs(t_ij), df = df, lower.tail = FALSE)
          } else {
            NA_real_
          }

          r[i, j] <- r_ij
          r[j, i] <- r_ij
          p[i, j] <- p_ij
          p[j, i] <- p_ij
        }
      }
    }
  }

  list(r = r, p = p)
}

plot_corr_full <- function(
  M_plot,
  P_plot,
  title = NULL,
  tl_cex = 0.7,
  pch_cex = 0.5,
  show_numbers = FALSE,
  number_cex = 0.3
) {
  col_pal <- grDevices::colorRampPalette(c("blue", "white", "red"))(200)
  graphics::par(mar = c(1, 1, 4, 2))

  corrplot_args <- list(
    corr = M_plot,
    method = "color",
    type = "full",
    diag = TRUE,
    order = "original",
    col = col_pal,
    cl.lim = c(-1, 1),
    tl.pos = "lt",
    tl.col = "brown4",
    tl.cex = tl_cex,
    tl.srt = 90,
    addgrid.col = "grey85",
    na.label = " ",
    p.mat = P_plot,
    sig.level = c(0.001, 0.01, 0.05),
    insig = "label_sig",
    pch.col = "black",
    pch.cex = pch_cex,
    title = title,
    mar = c(0, 0, 2, 0)
  )

  if (show_numbers) {
    corrplot_args$addCoef.col <- "black"
    corrplot_args$number.cex <- number_cex
    corrplot_args$number.digits <- 2
  }

  do.call(corrplot::corrplot, corrplot_args)
}

plot_corr_tiles <- function(
  M_plot,
  P_plot = NULL,
  title = NULL,
  subtitle = NULL,
  show_stars = TRUE,
  show_diagonal = TRUE,
  text_size = 1.3,
  coef_size = 1.7
) {
  stopifnot(identical(rownames(M_plot), colnames(M_plot)))

  vars <- colnames(M_plot)

  cor_df <- as.data.frame(as.table(M_plot), stringsAsFactors = FALSE) %>%
    dplyr::rename(var_y = Var1, var_x = Var2, rho = Freq) %>%
    dplyr::mutate(
      var_x_chr = as.character(var_x),
      var_y_chr = as.character(var_y),
      var_x = factor(var_x_chr, levels = vars),
      var_y = factor(var_y_chr, levels = rev(vars))
    )

  if (!is.null(P_plot)) {
    p_df <- as.data.frame(as.table(P_plot), stringsAsFactors = FALSE) %>%
      dplyr::rename(var_y = Var1, var_x = Var2, p = Freq) %>%
      dplyr::transmute(
        var_y_chr = as.character(var_y),
        var_x_chr = as.character(var_x),
        p = p
      )

    cor_df <- cor_df %>%
      dplyr::left_join(p_df, by = c("var_y_chr", "var_x_chr"))
  } else {
    cor_df <- cor_df %>% dplyr::mutate(p = NA_real_)
  }

  cor_df <- cor_df %>%
    dplyr::mutate(
      is_diag = var_x_chr == var_y_chr,
      stars = dplyr::case_when(
        is.na(p) ~ "",
        p <= 0.001 ~ "***",
        p <= 0.01 ~ "**",
        p <= 0.05 ~ "*",
        TRUE ~ ""
      ),
      rho_lab = dplyr::if_else(is.na(rho), "", sprintf("%.2f", rho)),
      label_pair = dplyr::case_when(
        is.na(rho) ~ "",
        is_diag & show_diagonal ~ paste0(var_x_chr, "\n—"),
        is_diag & !show_diagonal ~ "",
        TRUE ~ paste0(var_y_chr, "\nvs\n", var_x_chr)
      ),
      label_coef = dplyr::case_when(
        is.na(rho) ~ "",
        is_diag ~ "",
        show_stars ~ paste0(rho_lab, stars),
        TRUE ~ rho_lab
      )
    )

  ggplot2::ggplot(cor_df, ggplot2::aes(x = var_x, y = var_y, fill = rho)) +
    ggplot2::geom_tile(color = "grey80", linewidth = 0.18) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_pair),
      size = text_size,
      lineheight = 0.8,
      color = "grey25"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_coef),
      size = coef_size,
      fontface = "bold",
      color = "black",
      nudge_y = -0.25
    ) +
    ggplot2::scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      na.value = "grey95",
      name = expression(rho)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
      axis.text.y = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(color = "grey30", hjust = 0.5)
    )
}
