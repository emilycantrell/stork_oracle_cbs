# Sensitivity of learning-curve estimates to parametric functional form
#
# Purpose
#   Compare five parametric learning curves fitted to validation-set R-squared
#   across 15 training sample sizes (100 to 3,696,041) for three feature sets. The analysis reports
#   each curve's estimated asymptote, fit error (RMSE), and the percentage of
#   its remaining asymptotic gap closed between n = 10,000 and n = 100,000.
#
# Input and output
#   Run from the repository root. Read the five random seeds from the Excel
#   exports in exports/export_2025-09-15_mark/; use the validation split by
#   default. Write a JPG figure, LaTeX table, full-precision results CSV, and
#   fitting diagnostics CSV to plots_and_tables_output/.


# Load packages unless they were already loaded by run_all.R
if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "scales", "minpack.lm", "readxl", "here", "knitr")
  invisible(suppressMessages(
    groundhog.library(packages, "2025-11-15", tolerate.R.version='4.3.3')
  ))
}

# The validation split is for the analysis used in the draft submitted for review. The official holdout
# can be selected explicitly by setting target_test_set before sourcing.
if (!exists("target_test_set", inherits = TRUE)) {
  target_test_set <- "evaluation_test_50_percent_split"
}
stopifnot(target_test_set %in% c("evaluation_test_50_percent_split", "official_holdout_set"))
test_label <- if (target_test_set == "evaluation_test_50_percent_split") "validation_set" else "holdout_set"
out_dir <- here::here("analysis/plots_and_tables_output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
prefix <- file.path(out_dir, paste0("appendix_functional_forms_", test_label))

# Combine the first seed's export with the file containing seeds 2--5.
# Each seed contributes observed R-squared values at multiple training sizes.
raw <- dplyr::bind_rows(
  readxl::read_excel(here::here("results/results_learning_curves_seed_1_2025-08.xlsx")),
  readxl::read_excel(here::here("results/results_learning_curves_seeds_2-5_2025-08.xlsx"))
)
# The full training set is named "training_set" in these exports; its sample
# size is supplied explicitly. Other training-set names end with their n.
# The rescaled variable x = n / 10,000 improves numerical optimization for
# some parameterizations without changing the fitted family of curves.
dat <- raw %>%
  dplyr::filter(evaluation_set == target_test_set) %>%
  dplyr::transmute(
    feature_set, training_set,
    n = dplyr::if_else(training_set == "training_set", 3696041,
                       as.numeric(stringr::str_extract(training_set, "\\d+$"))),
    y = estimate_R2_Holdout) %>%
  dplyr::mutate(x = n / 10000)

# Preserve a consistent feature-set order in output tables and figure panels.
# Separate plain-text and LaTeX labels prevent smart quotes being interpreted
# incorrectly when pdflatex typesets the table.
feature_sets <- c("without_leakage", "prefer_official_train", "ego_AS")
feature_labels <- c(without_leakage = "All features from winning model",
                    prefer_official_train = "“Starter pack” file", ego_AS = "Age and sex")
feature_labels_tex <- c(without_leakage = "All features from winning model",
                        prefer_official_train = "``Starter pack'' file", ego_AS = "Age and sex")
# POW3 is the original specification. The alternatives include another
# power-law form (MMF4), a Weibull form (WBL4), inverse-logarithmic
# convergence (ILOG2), and exponential convergence (EXP3).
forms <- c("POW3", "MMF4", "WBL4", "ILOG2", "EXP3")
form_labels <- c(POW3 = "Power law with offset", MMF4 = "Morgan-Mercer-Flodin",
                 WBL4 = "Weibull", ILOG2 = "Inverse logarithmic", EXP3 = "Exponential")

# MMF4 is fitted with positive parameters on log scales and an asymptote
# constrained to (0, 1). With x = n / 10,000, its fitted curve is
# f(x) = c - gap / (1 + (x/h)^d). It approaches a power law as h
# approaches zero while gap * h^d approaches a positive constant.
# Weibull uses a separate parameterization below to represent its power-law
# boundary without numerically extreme original Weibull parameters.
logit_safe <- function(p) qlogis(pmin(.999999, pmax(.000001, p)))
predict_flexible <- function(z, form, x) {
  cval <- plogis(z[1]); gap <- exp(z[2]); d <- exp(z[4])
  if (form == "MMF4") {
    cval - gap * plogis(d * (z[3] - log(x)))
  } else stop("Unexpected flexible form")
}
# Minimize the sum of squared residuals over the same observations used
# for every functional form. Nonfinite predictions receive a large penalty.
flex_loss <- function(z, form, df) {
  pred <- suppressWarnings(predict_flexible(z, form, df$x))
  if (any(!is.finite(pred))) return(1e100)
  value <- sum((df$y - pred)^2)
  if (is.finite(value)) value else 1e100
}
# Multiple dispersed initial values mitigate local-minimum and starting-point
# sensitivity in nonlinear optimization. Select the converged solution with
# smallest residual sum of squares, then refine it with another optimizer.
fit_flexible <- function(form, df) {
  c0 <- max(df$y) + .02
  c_start <- pmin(.95, pmax(.05, c(c0, c0 + .06)))
  # Starting grids vary the asymptote, gap, horizontal scale, and curvature.
  # These are starting values, not parameter bounds.
  starts <- expand.grid(c = c_start, gap = c(.3, 2, 20, 200),
                        scale = c(.0001, .01, 1), d = c(.2, .6),
                        KEEP.OUT.ATTRS = FALSE)
  best <- NULL
  success <- 0L
  for (i in seq_len(nrow(starts))) {
    s <- starts[i, ]
    z0 <- c(logit_safe(s$c), log(s$gap), log(s$scale), log(s$d))
    fit <- suppressWarnings(tryCatch(
      stats::optim(z0, flex_loss, form = form, df = df, method = "BFGS",
                   control = list(maxit = 2000, reltol = 1e-12)),
      error = function(e) NULL))
    if (is.null(fit) || fit$convergence != 0 || !is.finite(fit$value) ||
        fit$value >= 1e99) next
    success <- success + 1L
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  if (is.null(best)) stop("No converged multistart solution: ", form)
  # Nelder-Mead refinement is accepted only if it converges and reduces SSE.
  refinement <- suppressWarnings(tryCatch(
    stats::optim(best$par, flex_loss, form = form, df = df, method = "Nelder-Mead",
                 control = list(maxit = 8000, reltol = 1e-13)),
    error = function(e) NULL))
  if (!is.null(refinement) && refinement$convergence == 0 &&
      is.finite(refinement$value) && refinement$value < best$value) best <- refinement
  list(predict = function(n) predict_flexible(best$par, form, n / 10000),
       c = plogis(best$par[1]), converged = TRUE, starts_converged = success,
       # Large absolute transformed parameter values flag potentially weak identification.
       extreme = any(abs(best$par[2:4]) > 15),
       gap = exp(best$par[2]), transition_or_rate = exp(best$par[3]),
       d = exp(best$par[4]))
}

# Weibull model with an accessible power-law limit
#
# The conventional Weibull form is f(x) = c - G exp(-r*x^d), d > 0.
# We fit the algebraically equivalent form
#   f(x) = c - A exp(-b * (x^d - 1)/d), d > 0,
# where A = G exp(-r) and b = r*d. For d approaching zero,
# (x^d - 1)/d approaches log(x), giving f(x) = c - A*x^(-b).
# Thus d = 0 represents the power-law limit of Weibull. Including that
# boundary lets us evaluate whether the Weibull family improves on the
# power-law fit, rather than forcing optimization toward huge G and r.
# The boundary fit is a limiting Weibull curve, not a finite-parameter
# Weibull with strictly positive d.
weibull_log_ratio <- function(x, d) {
  lx <- log(x)
  if (d == 0) return(lx)
  t <- d * lx
  # Taylor expansion avoids cancellation when d*log(x) is near zero.
  ifelse(abs(t) < 1e-5,
         lx * (1 + t/2 + t^2/6 + t^3/24),
         expm1(t) / d)
}

predict_weibull <- function(z, x) {
  cval <- plogis(z[1])
  A <- exp(z[2])
  b <- exp(z[3])
  d <- z[4]                 # Nonnegative shape; d = 0 is the power-law limit.
  cval - A * exp(-b * weibull_log_ratio(x, d))
}

weibull_loss <- function(z, df) {
  pred <- suppressWarnings(predict_weibull(z, df$x))
  if (any(!is.finite(pred))) return(1e100)
  sse <- sum((df$y - pred)^2)
  if (is.finite(sse)) sse else 1e100
}

fit_weibull <- function(df, pow) {
  pow_par <- stats::coef(pow)
  # Express the original power law a*n^(-b)+c on the x=n/10,000 scale.
  # It has A=-a*10,000^(-b), b=b, and shape d=0.
  stopifnot(pow_par["a"] < 0, pow_par["b"] > 0,
            pow_par["c"] > 0, pow_par["c"] < 1)
  z_pow <- c(logit_safe(unname(pow_par["c"])),
             log(-unname(pow_par["a"])) - unname(pow_par["b"]) * log(10000),
             log(unname(pow_par["b"])), 0)
  best_par <- z_pow
  best_sse <- weibull_loss(z_pow, df)
  successes <- 0L

  # Fit from the power-law solution and from several positive shape values.
  # The power-law solution remains an explicit candidate even if optimizers
  # do not converge to a lower sum of squared errors.
  for (shape in c(0, 0.001, 0.01, 0.05, 0.2, 0.6, 1)) {
    z0 <- z_pow
    z0[4] <- shape
    fit <- suppressWarnings(tryCatch(
      stats::optim(z0, weibull_loss, df = df, method = "L-BFGS-B",
                   lower = c(-Inf, -Inf, -Inf, 0),
                   control = list(maxit = 5000, factr = 1e7)),
      error = function(e) NULL))
    if (is.null(fit) || fit$convergence != 0 || !is.finite(fit$value)) next
    successes <- successes + 1L
    if (fit$value < best_sse) {
      best_sse <- fit$value
      best_par <- fit$par
    }
  }

  # Report whether any Weibull optimization converged. The power-law-limit
  # candidate remains available even if every optimization attempt fails.
  list(predict = function(n) predict_weibull(best_par, n / 10000),
       c = plogis(best_par[1]), converged = successes > 0L,
       starts_converged = successes,
       extreme = any(abs(best_par[2:3]) > 15),
       weibull_shape = best_par[4])
}

# Exponential and inverse-logarithmic models
#
# Both forms are fitted by bounded nonlinear least squares (nlsLM) from
# multiple starts, retaining the converged fit with the smallest SSE.
# ILOG2: f(n) = c - gap / log(n). Using n (not x) matches this specification.
# EXP3:  f(n) = c - gap * exp(-rate * x), where x = n / 10,000.
# Here the bounds impose a positive gap/rate and 0 <= c <= 1; the finite
# upper limits on gap/rate constrain optimization and merit diagnostic review.
fit_other <- function(form, df) {
  ymax <- max(df$y)
  cs <- unique(pmin(.98, pmax(ymax + c(.01, .04, .12), .001)))
  if (form == "ILOG2") {
    formula <- y ~ c - gap / log(n)
    starts <- expand.grid(c = cs, gap = c(.1, .5, 1, 2, 4))
    lower <- c(c = max(0, ymax - .4), gap = 0)
    upper <- c(c = 1, gap = 20)
  } else if (form == "EXP3") {
    formula <- y ~ c - gap * exp(-rate * x)
    starts <- expand.grid(c = cs, gap = c(.2, .5, .9), rate = c(.03, .3, 2))
    lower <- c(c = max(0, ymax - .4), gap = 0, rate = 1e-9)
    upper <- c(c = 1, gap = 3, rate = 1e3)
  } else stop("Unexpected form")
  # Each candidate uses identical observations and the same SSE criterion.
  best <- NULL; best_sse <- Inf
  for (i in seq_len(nrow(starts))) {
    start <- unlist(starts[i, ], use.names = TRUE)
    fit <- suppressWarnings(tryCatch(
      minpack.lm::nlsLM(formula, data = df, start = start,
        lower = lower, upper = upper,
        control = minpack.lm::nls.lm.control(maxiter = 500)),
      error = function(e) NULL))
    if (is.null(fit) || !isTRUE(fit$convInfo$isConv)) next
    value <- sum(stats::resid(fit)^2)
    if (is.finite(value) && value < best_sse) {best <- fit; best_sse <- value}
  }
  if (is.null(best)) stop("No converged fit: ", form)
  pars <- stats::coef(best)
  # Identify parameters close to their fitting bounds using a scale-aware tolerance.
  near <- names(pars)[vapply(names(pars), function(k) {
    tol <- max(1e-9, 1e-6 * max(1, abs(pars[k])))
    abs(pars[k] - lower[k]) <= tol || abs(pars[k] - upper[k]) <= tol
  }, logical(1))]
  # Return fitted quantities on the original n scale. The EXP3 gap/rate
  # coefficients are also retained for cancellation-free gap calculations.
  list(predict = function(n) as.numeric(stats::predict(best,
            newdata = data.frame(n = n, x = n / 10000))),
       c = unname(pars["c"]), converged = TRUE, starts_converged = NA_integer_,
       extreme = length(near) > 0, bound_parameters = paste(near, collapse = ", "),
       gap = if (form == "EXP3") unname(pars["gap"]) else NA_real_,
       rate = if (form == "EXP3") unname(pars["rate"]) else NA_real_)
}

# Fit each functional form separately to each feature set. The original POW3
# uses n (rather than rescaled x), preserving its main-analysis specification.
# The Weibull fit includes the power-law limit as a candidate solution.
# The other functions return both predictions and the estimated asymptote.
results <- list(); diagnostics <- list(); lines <- list()
for (feature in feature_sets) {
  df <- dplyr::filter(dat, feature_set == feature)
  pow <- minpack.lm::nlsLM(y ~ a * n^(-b) + c, data = df,
      start = c(a = -.5, b = .5, c = .5),
      control = minpack.lm::nls.lm.control(maxiter = 500))
  if (!isTRUE(pow$convInfo$isConv)) stop("Power law failed: ", feature)
  for (form in forms) {
    fit <- if (form == "POW3") {
      list(predict = function(n) as.numeric(stats::predict(pow, newdata = data.frame(n=n))),
           c = unname(stats::coef(pow)["c"]), converged = TRUE,
           starts_converged = NA_integer_, extreme = FALSE)
    } else if (form == "MMF4") fit_flexible(form, df) else if (form == "WBL4") fit_weibull(df, pow) else fit_other(form, df)
    p <- fit$predict
    c_est <- fit$c
    # Measure convergence as the percentage reduction in the gap between
    # predicted performance and the estimated asymptote:
    #
    #   100 * (1 - (c - f(to)) / (c - f(from)))
    #
    # For most functional forms, calculate this using fitted predictions.
    # If the gap at the beginning of the interval is extremely small
    # (<= 1e-10), return NA to avoid numerical instability.
    gap_closed <- function(from, to) {
      y0 <- p(from)
      y1 <- p(to)
      if (any(!is.finite(c(c_est, y0, y1))) ||
          c_est - y0 <= 1e-10) return(NA_real_)
      100 * (1 - (c_est - y1) / (c_est - y0))
    }
    if (form == "EXP3") {
      # For the exponential model, the percentage reduction depends only
      # on the fitted rate parameter. Calculate it directly to avoid
      # numerical instability when predictions are close to the asymptote.
      #
      # Sample sizes are scaled by 10,000 when fitting the model, so the
      # interval from 10,000 to 100,000 corresponds to x = 1 to x = 10.
      # The resulting percentage is 100 * (1 - exp(-9 * rate)).
      # expm1 provides a numerically stable evaluation of this expression.
      gap_pct <- 100 * (-expm1(-9 * fit$rate))

      # Record the absolute remaining gap at each endpoint to describe
      # how close the fitted curve is to its asymptote at those sample sizes.
      gap_at_10k <- fit$gap * exp(-fit$rate)
      gap_at_100k <- fit$gap * exp(-10 * fit$rate)
    } else {
      gap_pct <- gap_closed(10000, 100000)
      gap_at_10k <- NA_real_
      gap_at_100k <- NA_real_
    }
    results[[length(results)+1L]] <- data.frame(
      feature_set = feature, form = form, asymptote = c_est,
      RMSE = sqrt(mean((df$y - p(df$n))^2)),
      gap_closed_10k_100k = gap_pct)
    # Compare each flexible model with the fitted power law over the observed
    # sample-size range. Small differences demonstrate agreement of the fitted
    # curves, but do not establish that an optimizer reached an exact limit.
    # Weibull explicitly includes the power-law limit when its shape d = 0.
    # MMF has a power-law tail for sufficiently large sample sizes; the
    # comparison below measures agreement on this dataset, not an exact limit.
    if (form %in% c("MMF4", "WBL4")) {
      pow_prediction <- function(n) as.numeric(stats::predict(
        pow, newdata = data.frame(n = n)))
      check_n <- exp(seq(log(min(df$n)), log(max(df$n)), length.out = 1000))
      max_curve_diff <- max(abs(p(check_n) - pow_prediction(check_n)))
      asymptote_diff <- c_est - unname(stats::coef(pow)["c"])
      pow_c <- unname(stats::coef(pow)["c"])
      pow_gap_closed <- 100 * (1 -
        (pow_c - pow_prediction(100000)) /
        (pow_c - pow_prediction(10000)))
      gap_closed_diff_pp <- gap_pct - pow_gap_closed
    } else {
      max_curve_diff <- NA_real_
      asymptote_diff <- NA_real_
      gap_closed_diff_pp <- NA_real_
    }
    # Diagnostic fields expose optimization concerns and test whether a
    # reported asymptote lies below the largest individual observed value.
    # Observations are noisy, so that comparison is a flag, not a proof of
    # invalidity. EXP3 gaps at both endpoints support direct interpretation.
    diagnostics[[length(diagnostics)+1L]] <- data.frame(
      feature_set = feature, form = form, converged = fit$converged,
      starts_converged = fit$starts_converged,
      extreme_or_near_bound = fit$extreme,
      bound_parameters = if (is.null(fit$bound_parameters)) "" else fit$bound_parameters,
      asymptote_below_observed_max = c_est < max(df$y) - 1e-7,
      power_law_max_abs_curve_difference = max_curve_diff,
      power_law_asymptote_difference = asymptote_diff,
      power_law_gap_closed_difference_pp = gap_closed_diff_pp,
      weibull_shape = if (form == "WBL4") fit$weibull_shape else NA_real_,
      exponential_rate_rescaled = if (form == "EXP3") fit$rate else NA_real_,
      exponential_gap_at_10k = gap_at_10k,
      exponential_gap_at_100k = gap_at_100k)
    # Plot curves over the observed training-size range.
    grid <- exp(seq(log(min(df$n)), log(max(df$n)), length.out = 300))
    lines[[length(lines)+1L]] <- data.frame(feature_set=feature, form=form,
                                             n=grid, fitted=p(grid))
  }
}
comparison <- dplyr::bind_rows(results) %>%
  dplyr::mutate(feature_set=factor(feature_set, levels=feature_sets),
                form=factor(form, levels=forms)) %>%
  dplyr::arrange(feature_set, form)
qc <- dplyr::bind_rows(diagnostics)

# Reproduction check: POW3 asymptotes on the validation split should match
# the paper's reported three-decimal estimates within a small tolerance.
# This is an internal consistency check, not a fit-quality guarantee.
if (test_label == "validation_set") {
  expected <- c(without_leakage=.351, prefer_official_train=.325, ego_AS=.104)
  for (feature in feature_sets) {
    found <- comparison$asymptote[comparison$feature_set == feature & comparison$form == "POW3"]
    if (length(found)!=1 || abs(found-expected[[feature]]) > .0015)
      stop("POW3 asymptote does not match main paper for ", feature)
  }
}
if (any(qc$extreme_or_near_bound))
  warning("Some alternative fits have extreme parameters or near-bound estimates; inspect diagnostics.")

# Save full-precision numeric results and separate fit diagnostics before
# rounding values for presentation in the LaTeX table.
utils::write.csv(comparison, paste0(prefix, "_results.csv"), row.names=FALSE)
utils::write.csv(qc, paste0(prefix, "_diagnostics.csv"), row.names=FALSE)
# Three decimals for asymptotes and four for RMSE reflect their different
# magnitudes; four RMSE decimals distinguish differences near 0.02.
# Gap-closed percentages are displayed to one decimal, with escaped \% signs.
# Numerically uncomputable gap percentages are displayed as "--".
fmt <- function(x, places) ifelse(is.finite(x), sprintf(paste0("%.",places,"f"),x), "--")
table_df <- data.frame(
  `Feature set` = unname(feature_labels_tex[as.character(comparison$feature_set)]),
  `Functional form` = unname(form_labels[as.character(comparison$form)]),
  RMSE = fmt(comparison$RMSE, 4),
  `Asymptote $c$` = fmt(comparison$asymptote, 3),
  `\\shortstack[l]{Gap closed, \\\\ 10k--100k}` =
    ifelse(
      is.finite(comparison$gap_closed_10k_100k),
      paste0(sprintf("%.1f", comparison$gap_closed_10k_100k), "\\%"),
      "--"
    ),
  check.names = FALSE)
# Match main-text table conventions: a title-case caption, a separate note,
# and a label for cross-references in the manuscript.
tex <- knitr::kable(table_df, format="latex", booktabs=TRUE, escape=FALSE,
  caption="Sensitivity of Estimated Asymptotes and Convergence Rates to Learning-Curve Functional Form")
# knitr places the caption above the tabular; append the explanatory note below
# the tabular, inside the table environment.
table_end <- "\\end{table}"
stopifnot(endsWith(tex, table_end))
tex <- paste0(
  substr(tex, 1, nchar(tex) - nchar(table_end)),
  "\\footnotetext{Note: RMSE indicates how closely the fitted curve matched the observed predictive performance values. Gap closed is the percentage reduction in the remaining gap to the estimated asymptote as training sample size increased from 10,000 to 100,000. This interval is illustrative; the percentage reduction may differ across intervals for functional forms other than the power law with offset.}\n",
  "\\label{table:learning_curve_sensitivity_analysis}\n",
  table_end
)
writeLines(tex, paste0(prefix, "_table.tex"))

# Figure: points show the seed-level measurements; lines show fitted values.
# A logarithmic horizontal scale displays training sizes spanning several
# orders of magnitude. Free y-scales preserve variation in each feature set.
curve_df <- dplyr::bind_rows(lines) %>%
  dplyr::mutate(feature_label=factor(unname(feature_labels[feature_set]),
                                       levels=unname(feature_labels[feature_sets])),
                form=factor(form, levels=forms))
points_df <- dplyr::filter(dat, feature_set %in% feature_sets) %>%
  dplyr::mutate(feature_label=factor(unname(feature_labels[feature_set]),
                                       levels=unname(feature_labels[feature_sets])))
p <- ggplot2::ggplot() +
  ggplot2::geom_point(data=points_df, ggplot2::aes(x=n,y=y), alpha=.35, size=1.1) +
  ggplot2::geom_line(data=curve_df,
                     ggplot2::aes(x=n,y=fitted,color=form,linetype=form), linewidth=.85) +
  ggplot2::facet_wrap(~feature_label,ncol=1,scales="free_y") +
  ggplot2::scale_x_log10(labels=scales::label_number(scale_cut=scales::cut_short_scale())) +
  # Use a color-vision-accessible Viridis palette and distinct linetypes to
  # make the five fits distinguishable in color and grayscale.
  ggplot2::scale_color_manual(values = setNames(grDevices::hcl.colors(6, "Viridis")[1:5], forms),
                              breaks = forms, labels = unname(form_labels[forms])) +
  ggplot2::scale_linetype_manual(values = setNames(c("solid", "dashed", "dotdash", "dotted", "longdash"), forms),
                                 breaks = forms, labels = unname(form_labels[forms])) +
  ggplot2::labs(x="Training sample size (log scale)",
       y=if (test_label=="validation_set") expression(R[Validation]^2) else expression(R[Holdout]^2),
       color="Functional form", linetype="Functional form") +
  ggplot2::theme_bw(base_size=12) +
  ggplot2::theme(
    legend.position = "bottom",
    strip.background = ggplot2::element_blank(),  # Keep facet headings unshaded
    strip.text = ggplot2::element_text(face = "bold", size = 13, margin = ggplot2::margin(b = 6, t = 6)),
    panel.spacing = grid::unit(1.2, "lines")
  )
# Export a print-resolution JPEG and render the same plot in the R session.
ggplot2::ggsave(paste0(prefix, "_plot.jpg"), p, width=9, height=9,
                dpi=300,bg="white",device="jpeg")
print(p)
print(comparison)
message("Saved JPG, LaTeX table, results CSV, diagnostics CSV in: ", out_dir)

