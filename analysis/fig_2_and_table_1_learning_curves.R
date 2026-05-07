if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "scales", "nlsr", "rlang", "minpack.lm", "patchwork", "readxl", "here", "kableExtra")
  invisible(suppressMessages(
    groundhog.library(packages, "2025-11-15")
  ))
}

#### SETTINGS ####

# Choose whether to test on evaluation set or holdout set
# "evaluation_test_50_percent_split" for validation set 
# "official_holdout_set" for holdout set
# Also, choose whether "AS_FS" should be included as a feature set. We are
# only using it for holdout results.
if (!exists("target_test_set", inherits = TRUE)) {
  target_test_set <- "evaluation_test_50_percent_split"
}
if (!exists("feature_sets_to_plot_for_sample_size", inherits = TRUE)) {
  feature_sets_to_plot_for_sample_size <- 
    c("without_leakage", "prefer_official_train", "ego_AS")
}

#### FUNCTION TO GENERATE OUTPUT FOR A GIVEN METRIC ####
run_metric <- function(my_metric) {
  
  # Print progress message
  if (my_metric == "R2_Holdout") { 
    message("Running Figure 2 and Table 1 code for metric: ", my_metric)
    } else { 
    message("Running Figure 2 code for metric: ", my_metric)
    }
  
  #### DATA PREP #### 
  
  # Read results & bind them together
  results_seed_1 <- results_seed_1 <- read_excel(here("results/results_learning_curves_seed_1_2025-08.xlsx"))
  results_seeds_2_5 <- read_excel(here("results/results_learning_curves_seeds_2-5_2025-08.xlsx"))
  results <- bind_rows(results_seed_1, results_seeds_2_5)
  
  # Build column names dynamically
  estimate_col <- paste0("estimate_", my_metric)
  ci_lower_col <- paste0("ci_lower_", my_metric)
  ci_upper_col <- paste0("ci_upper_", my_metric)
    
  # Select desired columns & create column of training sample sizes
  results <- results %>%
    filter(evaluation_set == target_test_set) %>%
    dplyr::select(training_set, feature_set, 
           !!estimate_col := all_of(estimate_col),
           !!ci_lower_col := all_of(ci_lower_col),
           !!ci_upper_col := all_of(ci_upper_col)) %>%
    rename(estimate_metric = !!estimate_col,
           ci_lower_metric = !!ci_lower_col,
           ci_upper_metric = !!ci_upper_col) 
  
  # Make column with sample sizes
  results <- results %>%
    mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
           n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set))
  
  #### PLOTS ####
  
  digits_for_plot <- ifelse(my_metric == "R2_Holdout", 1, 2)
  
  # Function to remove leading 0 from decimal numbers on plots
  strip_leading_zero <- function(x, digits = digits_for_plot) {
    gsub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
  }
  
  # Function to remove leading 0 from decimal numbers in character strings (for LaTeX table)
  strip_leading_zero_char <- function(x) {
    sub("^(-?)0\\.", "\\1.", x)
  }
  
  # Define y axis label based on my_metric
  # Note: Even though we called it R2_holdout in the code, we are
  # actually testing on the validation set when we use evaluation_test_50_percent_split, 
  # so the plot label should be "R^2_Validation"
  y_axis_label <- switch(
    my_metric,
    "LogLoss" = "Log Loss",
    "MSE" = "Mean Squared Error",
    "R2_Holdout" = if (target_test_set == "evaluation_test_50_percent_split") {
      bquote(R[Validation]^2)
    } else {
      bquote(R[Holdout]^2)
    },
    "F1_Score" = "F1 Score",
    "Accuracy" = "Accuracy",
    my_metric  # fallback
  )
    
  # Define feature sets to compare
  feature_sets_to_plot <- feature_sets_to_plot_for_sample_size
  
  # Initialize an empty dataframe for plot data
  data_for_plot <- tibble()
  
  # Initialize an empty list to store parameter values 
  fit_list <- list()
  
  # Loop through each feature set
  for (feature_i in feature_sets_to_plot) {
    # Filter results for the current feature set
    feature_data <- results %>%
      filter(feature_set == feature_i)
  
    # Fit scaling law
    scaling_formula <- estimate_metric ~ a * n_training_set^(-b) + c
    starting_points <- switch(my_metric,
                              "LogLoss" = c(a = 0.5, b = 0.5, c = 0.5),
                              "MSE" =     c(a = 0.5, b = 0.5, c = 0.1),
                              "R2_Holdout" = c(a = -0.5, b = 0.5, c = 0.5),
                              "F1_Score" = c(a = -0.5, b = 0.5, c = 0.5),
                              "Accuracy" = c(a = -0.5, b = 0.5, c = 0.85))
  
    fit_nlsLM <- try(minpack.lm::nlsLM(
      scaling_formula,
      start = starting_points,
      data = feature_data,
      control = minpack.lm::nls.lm.control(maxiter = 500)
    ))
  
    # Predict for all points
    feature_data$predicted <- predict(fit_nlsLM, newdata = feature_data)
    
    # Append to combined dataframe
    data_for_plot <- bind_rows(data_for_plot, feature_data)
    
    # Store parameter values if the fit was successful
    if (!inherits(fit_nlsLM, "try-error")) {
      fit_list[[feature_i]] <- fit_nlsLM
    }
  }
  
  # Make table of coefficients and standard errors
  coef_table <- map_dfr(names(fit_list), function(name) {
    fit <- fit_list[[name]]
    summ <- summary(fit)
    tibble(
      feature_set = name,
      a = coef(fit)["a"],
      a_se = summ$parameters["a", "Std. Error"],
      b = coef(fit)["b"],
      b_se = summ$parameters["b", "Std. Error"],
      c = coef(fit)["c"],
      c_se = summ$parameters["c", "Std. Error"]
    )
  })
  
  # Set the order for the legend
  data_for_plot <- data_for_plot %>%
    mutate(
      feature_set = factor(feature_set,
      levels = feature_sets_to_plot
    ))
  
  # Function to generate plot
  make_learning_curve_plot <- function(axis_type, legend_inside = FALSE) {
    x_axis_scale <- if (axis_type == "log10") {
      scale_x_log10(labels = scales::trans_format("log10", scales::math_format(10^.x)), 
                    guide = ggplot2::guide_axis_logticks())
    } else {
      scale_x_continuous(
        breaks = c(0, 1e6, 2e6, 3e6, 4e6),
        labels = c(
          "0",
          expression(1%*%10^6),
          expression(2%*%10^6),
          expression(3%*%10^6),
          expression(4%*%10^6)
        )
      )
    }
    
    x_axis_label <- if (axis_type == "log10") {
      "Sample Size (Log Scale)"
    } else {
      "Sample Size (Linear Scale)"
    }
    
    legend_position <- if (legend_inside) c(0.65, 0.15) else "right"
    
    if(length(feature_sets_to_plot) == 3) {
      colors <- c(
        "without_leakage" = "#0072B2",
        "prefer_official_train" = "#33B17C",
        "ego_AS" = "#E69F00"
      )
      labels <- c(
        "without_leakage" = "All features from winning model",
        "prefer_official_train" = '"Starter pack" file',
        "ego_AS" = "Ego's age & sex"
      )
    } else{
      colors <- c(
        "without_leakage" = "#0072B2",
        "prefer_official_train" = "#33B17C",
        "AS_FS" = "#D55E00",
        "ego_AS" = "#E69F00"
      )
      labels <- c(
        "without_leakage" = "All features from winning model",
        "prefer_official_train" = '"Starter pack" file',
        "AS_FS" = "Age, sex, & family structure",
        "ego_AS" = "Ego's age & sex"
        )
    }
    
    p <- ggplot(data_for_plot, aes(x = n_training_set, y = estimate_metric)) +
      geom_point(aes(color = feature_set), alpha = 0.4) +
      geom_errorbar(
        aes(ymin = ci_lower_metric, ymax = ci_upper_metric, color = feature_set), 
        width = if (axis_type == "log10") 0.07 else 50000, 
        alpha = 0.4
      ) + 
      geom_line(
        aes(y = predicted, color = feature_set, linetype = "Estimated learning curve")
      ) +
      scale_linetype_manual(
        values = c("Estimated learning curve" = "dashed")
      ) +
      scale_color_manual(
        values = colors,
        labels = labels
      ) +
      x_axis_scale +
      labs(
        shape = NULL,
        color = "Feature Set",
        x = x_axis_label,
        y = y_axis_label
      ) +
      guides(
        shape = guide_legend(order = 1, title = "Learning Curve"),
        linetype = guide_legend(order = 2, title = NULL),
        color = guide_legend(order = 3)
      ) +
      theme_bw() + 
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        legend.position = legend_position,
        legend.background = element_rect(fill = alpha("white", 0.7), color = "gray80")
      ) + 
      scale_y_continuous(labels = strip_leading_zero)
    
    return(p)
  }
  
  # Plot linear and log axis plots side by side
  plot_linear <- make_learning_curve_plot("linear") +
    labs(tag = "(A)") +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.5, 0.05),
      legend.justification = c(0.5, 0),
      legend.background = element_rect(fill = alpha("white", 0.7), color = "gray80"),
      panel.border = element_rect(color = "black", size = .75),
      plot.tag.position = c(0.17, 0.98),  # Inside upper-left corner of plot panel
      plot.tag = element_text(size = 14, face = "bold")
    )
  
  plot_log <- make_learning_curve_plot("log10") +
    labs(tag = "(B)") +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.border = element_rect(color = "black", size = .75),
      plot.tag.position = c(0.055, 0.98),  # Same position as (A)
      plot.tag = element_text(size = 14, face = "bold")
    )
  
  combined_plot <- plot_linear + plot_log +
    patchwork::plot_layout(guides = "keep", tag_level = "new")
  print(combined_plot)
  
  # Create labels for the file name
  # If R2_Holdout, just call it R2 (eval_set_label_for_saved_file will indicate validation or holdout)
  metric_label_for_saved_file <- ifelse(my_metric == "R2_Holdout", "R2", my_metric)
  eval_set_label_for_saved_file <- 
    if (target_test_set == "evaluation_test_50_percent_split") {
      "_validation_set"
    } else if (target_test_set == "official_holdout_set") {
      "_holdout_set"
    } else {
      stop("Unknown target_test_set value: ", target_test_set) 
      # Note: we also have results for the selection set but I don't 
      # expect to ever use it for these plots
    }
  
  # Save the plot
  ggsave(
     filename = paste0(here("analysis/plots_and_tables_output/fig_2_learning_curves_"), 
                       metric_label_for_saved_file, 
                       eval_set_label_for_saved_file, 
                       ".png"),
     plot = combined_plot,
     width = 10, height = 7, dpi = 300, bg = "white"
   )
  
  #### GENERATE LATEX TABLE WITH COEFFICIENTS ####
  
  if(my_metric == "R2_Holdout") { 
    
    # Relabel feature sets for clarity
    coef_table <- coef_table %>%
      mutate(feature_set = case_when(
        feature_set == "without_leakage"      ~ "All features from winning model",
        feature_set == "prefer_official_train" ~ '"Starter pack" file',
        feature_set == "AS_FS" ~ "Age, sex, & family structure",
        feature_set == "ego_AS"               ~ "Ego's age & sex",
        TRUE                                  ~ feature_set
      )) 
    
    # Prepare the table for LaTeX
    coef_table_for_latex <- coef_table %>%
      mutate(
        a    = strip_leading_zero_char(sprintf("%.3f", a)),
        a_se = paste0("(", strip_leading_zero_char(sprintf("%.3f", a_se)), ")"),
        b    = strip_leading_zero_char(sprintf("%.3f", b)),
        b_se = paste0("(", strip_leading_zero_char(sprintf("%.3f", b_se)), ")"),
        c    = strip_leading_zero_char(sprintf("%.3f", c)),
        c_se = paste0("(", strip_leading_zero_char(sprintf("%.3f", c_se)), ")")
      ) %>%
      transmute(
        `Feature Set` = feature_set,
        `$\\mathit{a}$ (SE)` = paste(a, a_se),
        `$\\mathit{b}$ (SE)` = paste(b, b_se),
        `$\\mathit{c}$ (SE)` = paste(c, c_se)
      ) %>%
      # escape literal "&" symbol
      mutate(`Feature Set` = str_replace_all(`Feature Set`, "&", "\\\\&"))
    
    # Create LaTeX code
    latex_table <- coef_table_for_latex %>%
      kbl(
        format = "latex",
        booktabs = TRUE,
        align = "lccc", 
        escape = FALSE # Allow latex math in headers
      ) %>%
      kable_styling(latex_options = c("hold_position")) %>%
      row_spec(0, bold = TRUE) %>%   # Make header row bold
      as.character()
    
    # Save the table
    filename_for_table = paste0("plots_and_tables_output/table_1_learning_curve_parameters_", 
                         metric_label_for_saved_file, 
                         eval_set_label_for_saved_file, 
                         ".tex")
    writeLines(latex_table, filename_for_table)
  
  }
  
  #### CALCULATE HOW QUICKLY THE GAP TO ASYMPOTOTE CLOSES ####
  
  # We want to fill in X:
  # "For every 10-fold increase in sample size, 
  # performance got about X% closer to the estimated aymptote."
  # Note: The asymptote is the estimated maximum performance value for this model type 
  # and feature set for metrics where higher values are better, or estimated minimum 
  # performance value for metrics where lower values are better.
  
  # Derivation: 
  # Given y=ax^(-b)+c, we know that c-y = -ax^(-b)
  # Now as x increases 10-fold, we have new y’ which satisfies c-y’ = -a(10x)^(-b) = -ax^(-b)*10^(-b)
  # As x increases 10-fold, the distance between c and y changes by a factor of 10^(-b), which is a constant
  
  if(my_metric == "R2_Holdout") { 
    # Calculate what percent closer to the asymptote the performance gets for each 10-fold increase, by feature set
    table_with_percent_closer_to_asymptote <- coef_table %>%
      mutate(percent_closer_to_asymptote_for_tenfold_increase_in_n = 100*(1 - (10 ^ (-b)))) %>%
      select(feature_set, percent_closer_to_asymptote_for_tenfold_increase_in_n) 
    
    # Print message to introduce table
    print(paste0("For the metric ", metric_label_for_saved_file, " and the test set ", target_test_set,
                 ", for every 10-fold increase in sample size, performance for each feature set got this percentage closer to the asymptote:" 
    )) 
    print(table_with_percent_closer_to_asymptote)
  }

} # End of run_metric

#### RUN FUNCTION FOR ALL METRICS ####
my_metrics <- c("R2_Holdout") # "LogLoss", "MSE", "Accuracy", "F1_Score"
for (metric in my_metrics) {
  run_metric(metric)
}

# Note: I printed the coefficient table and "percent closer to the asymptote" results 
# just for R2 because that is all we use in the paper, but this code can easily be 
# adjusted to print the same output for other metrics. 
# However, note that when accuracy is used as a metric with the "ego's age and sex" feature set, 
# the threshold to maximize performance simply predicts outcome = 0 for everyone, so
# all performance values are identical and we simply fit a horizontal line. The code is not
# currently set up to automatically produce coefficients reflecting that horizontal line.