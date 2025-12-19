# Scaling laws: how well can we extrapolate performance at larger sample sizes, 
# based on performance at smaller sample sizes?

if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "scales", "rlang", "nlsr",  "minpack.lm",
                "readxl", "here", "patchwork", "cowplot", "knitr")
  invisible(suppressMessages(
    groundhog.library(packages, "2025-11-15")
  ))
}

#### SETTINGS ####

# Choose whether to test on evaluation set or holdout set
# "evaluation_test_50_percent_split" for validation set 
# "official_holdout_set" for holdout set
if (!exists("target_test_set", inherits = TRUE)) {
  target_test_set <- "evaluation_test_50_percent_split"
}

# Create vector of metrics
metrics_to_run <- c("R2_Holdout", "LogLoss", "MSE", "Accuracy", "F1_Score")

# Choose an axis type
#axis_type <- "linear"
axis_type <- "log10"

# If txt file with tables already exists, remove it so that we start fresh on this run 
tables_file <- here(
  "plots_and_tables_for_paper/plots_and_tables_output/",
  "table_2_and_appendix_tables_extrapolation_abs_diff.txt"
)
if (file.exists(tables_file)) {
  file.remove(tables_file)
}

#### LOOP OVER THE METRICS ####
for (my_metric in metrics_to_run) {
  message("Running extrapolation for metric: ", my_metric)

  #### GENERATE EXTRAPOLATIONS ####
  
  # Initialize empty dataframes
  abs_gaps_all_seeds <- tibble()
  data_for_plot <- tibble()
  
  # For each main seed that was used to generate a training sample of n = 10k,
  # for each of the three feature sets, fit a curve on the performance at n <= 10k, 
  # and use it to predict performance at n > 10k
  for(main_seed in c(1:5)) {
      
    # Make seed into a string
    main_seed_string <- paste0(main_seed)
    
    # Read results for training points (n <= 10k) & bind them together
    lower_n_results <- switch(
      main_seed_string,
      "1" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seed_1_2025-08.xlsx")),
      "2" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seed_2_2025-08.xlsx")),
      "3" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seed_3_2025-08.xlsx")),
      "4" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seeds_4-5_2025-08.xlsx")) %>%
        filter(str_starts(sampling_file, "dms_samples_seed_4")),
      "5" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seeds_4-5_2025-08.xlsx")) %>%
        filter(str_starts(sampling_file, "dms_samples_seed_5"))
    )
    
    # Make column with sample sizes
    lower_n_results <- lower_n_results %>%
      mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
             n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set))
    
    # Read results for test points (n > 10k)
    higher_n_results <- if (main_seed_string == "1") {
      read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seed_1_2025-08.xlsx"))
    } else if (main_seed_string %in% c("2", "3", "4", "5")) {
      read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seeds_2-5_2025-08.xlsx")) %>%
        filter(str_starts(sampling_file, paste0("dms_samples_seed_", main_seed)))
    } else {
      stop("main_seed must be 1, 2, 3, 4, or 5")
    }
      
    # Filter to just n > 10k
    higher_n_results <- higher_n_results %>%
      mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
             n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) %>%
      filter(n_training_set > 10000) 
      
    # Combine all results
    results <- bind_rows(lower_n_results, higher_n_results)
    rm(lower_n_results, higher_n_results)
    
    # Filter to just three feature sets (we are holding the fourth back as an informal "holdout")
    results <- results %>%
      filter(feature_set %in% c("without_leakage", "prefer_official_train", "ego_AS"))
    
    # Build column names dynamically
    estimate_col <- paste0("estimate_", my_metric)
    ci_lower_col <- paste0("ci_lower_", my_metric)
    ci_upper_col <- paste0("ci_upper_", my_metric)
      
    # Select desired columns & create column of training sample sizes
    results <- results %>%
      filter(evaluation_set == target_test_set) %>%
      dplyr::select(training_set, feature_set, n_training_set, 
             !!estimate_col := all_of(estimate_col),
             !!ci_lower_col := all_of(ci_lower_col),
             !!ci_upper_col := all_of(ci_upper_col)) %>%
      rename(estimate_metric = !!estimate_col,
             ci_lower_metric = !!ci_lower_col,
             ci_upper_metric = !!ci_upper_col)
    
    # Define feature sets to compare
    feature_sets_to_plot <- c("without_leakage", "prefer_official_train", "ego_AS")
    
    # Loop through each feature set
    for (feature_i in feature_sets_to_plot) {
      # Filter results for the current feature set
      feature_data <- results %>%
        filter(feature_set == feature_i)
    
      # Create 4 additional copies of 10k results so that all points ≤ 10k are weighted equally
      # since all the smaller sample sizes appear 5 times but 10k results only appear 1 time
      feature_data <- feature_data %>%
        bind_rows(
          feature_data %>%
            filter(n_training_set == 10000) %>%
            slice_sample(n = 4, replace = TRUE)
        )
      
      # Select only points where training sample size ≤ 10000 for fitting
      training_data <- feature_data %>% 
        filter(n_training_set <= 10000) 
    
      feature_data <- feature_data %>% 
        mutate(split = ifelse(n_training_set <= 10000, "train", "test"))
      
      # If there's only one unique value, then fit a flat line instead of using nlsLM
      # This occurs for the "ego's age & sex" feature set when the metric is "Accuracy", because
      # the best way to maximize accuracy with these features is to predict outcome = 0 for everyone
      if (length(unique(training_data$estimate_metric)) == 1) {
        flat_value <- unique(training_data$estimate_metric)
        message("Flat curve detected for feature set: ", feature_i, 
                ". Using straight-line fit instead of nlsLM.")
        # Predict constant line
        feature_data$predicted <- flat_value
        # Document the seed number
        feature_data <- feature_data %>%
          mutate(main_seed = main_seed) 
        # Add to combined data
        data_for_plot <- bind_rows(data_for_plot, feature_data)
        # Skip nlsLM for this feature set
        next  
      }
      
      # Fit scaling law
      scaling_formula <- estimate_metric ~ a * n_training_set^(-b) + c
      starting_points <- switch(my_metric,
                                "LogLoss" = c(a = 0.5, b = 0.5, c = 0.5),
                                "MSE" =     c(a = 0.5, b = 0.5, c = 0.1),
                                "R2_Holdout" = c(a = -0.5, b = 0.5, c = 0.5),
                                "In_Sample_R2" = c(a = -0.5, b = 0.5, c = 0.5),
                                "AUC" =     c(a = -0.5, b = 0.5, c = 0.8),
                                "F1_Score" = c(a = -0.5, b = 0.5, c = 0.5),
                                "Accuracy" = c(a = -0.5, b = 0.5, c = 0.85))
      
      # Fit the learning curve
      fit_nlsLM <- try(nlsLM(scaling_formula,
                             start = starting_points,
                             data = training_data,
                             control = nls.lm.control(maxiter = 500)))
    
      # Predict for all points
      feature_data$predicted <- predict(fit_nlsLM, newdata = feature_data)
      
      # Document the seed number
      feature_data <- feature_data %>%
        mutate(main_seed = main_seed) 
      
      # Append to combined dataframe
      data_for_plot <- bind_rows(data_for_plot, feature_data)
    } # End loop for feature sets
  } # End loop for seeds
  
  
  #### PLOTS ####
  
  # Set the order for the legend
  data_for_plot <- data_for_plot %>%
    mutate(split = factor(split, levels = c("train", "test"))) %>%
    mutate(feature_set = factor(feature_set, levels = c(
      "without_leakage",          # top
      "prefer_official_train",    # middle
      "ego_AS"                    # bottom
    )))
  
  # Compute global y-axis limits for all panel plots
  global_ylim <- range(
    c(data_for_plot$estimate_metric, data_for_plot$predicted),
    na.rm = TRUE
  )
  
  # Create function to remove leading 0 from decimal numbers
  strip_leading_zero <- function(values) {
    # Try increasing number of digits until all formatted values are unique
    for (d in 1:3) {
      formatted <- gsub("^(-?)0\\.", "\\1.", formatC(values, format = "f", digits = d))
      if (length(unique(formatted)) == length(formatted)) {
        return(formatted)
      }
    }
  }
  
  # Define x-axis scale based on axis_type
  x_axis_scale <- if (axis_type == "log10") {
    scale_x_log10(labels = trans_format("log10", math_format(10^.x)), 
                  guide = guide_axis_logticks())
  } else {
    scale_x_continuous()
  }
  
  # Define x-axis label based on axis_type
  x_axis_label <- if (axis_type == "log10") {
    "Sample Size (Log Scale)"
  } else {
    "Sample Size"
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
  
  # Function to make plot
  make_plot_for_one_seed <- function(seed_number, for_panel = TRUE) {
    
    df <- data_for_plot %>% 
      filter(main_seed == seed_number)
    
    p <- ggplot(df, aes(x = n_training_set, y = estimate_metric)) +
      geom_point(aes(color = feature_set, shape = split), size = 2, alpha = 0.6) +
      geom_line(aes(y = predicted, color = feature_set,
                    linetype = "Estimated learning curve")) +
      scale_shape_manual(
        values = c("train" = 16, "test" = 1),
        labels = c("train" = "Data points used \nto fit curve\n",
                   "test"  = "Data points not \nobserved when \nfitting curve")
      ) +
      scale_linetype_manual(values = c("Estimated learning curve" = "dashed")) +
      scale_color_manual(
        values = c(
          "without_leakage" = "#0072B2",
          "prefer_official_train" = "#33B17C",
          "ego_AS" = "#E69F00"
        ),
        labels = c(
          "without_leakage" = "All features from winning model",
          "prefer_official_train" = "PreFer 'starter pack' file",
          "ego_AS" = "Ego's age & sex"
        )
      ) +
      x_axis_scale +
      labs(
        x = x_axis_label,
        y = y_axis_label,
        color = "Feature Set",
        shape = NULL
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12)
      ) + 
      guides(
        shape    = guide_legend(title = "Learning Curve", order = 1),
        linetype = guide_legend(title = NULL, order = 2),
        color    = guide_legend(title = "Feature Set", order = 3)
      )
    
    # Case 1: plot will go in a panel of plots (no legend, add seed label) 
    if (for_panel) {
      p <- p +
        theme(legend.position = "none") +
        annotate(
          "text",
          x = min(df$n_training_set),
          y = max(global_ylim),
          label = paste0("(Seed ", seed_number, ")"),
          hjust = -0.1, vjust = 1.1,
          size = 4, fontface = "bold"
        ) + 
        scale_y_continuous(
          limits = global_ylim,
          labels = strip_leading_zero
        )
      # Left-hand column (seeds 1,3,5) -> keep y label
      if (seed_number %in% c(1,3,5)) {
        p <- p + theme(axis.title.y = element_text(size = 14))
      } else {
        p <- p + theme(axis.title.y = element_blank())
      }
      # Bottom row (seeds 4,5) -> keep x label
      if (seed_number %in% c(4,5)) {
        p <- p + theme(axis.title.x = element_text(size = 14))
      } else {
        p <- p + theme(axis.title.x = element_blank())
      }
    }
    
    # Case 2: plot will be solo in the main paper
    if (!for_panel) {
      p <- p +
        theme(legend.position = "right") + 
        scale_y_continuous(labels = strip_leading_zero)
    }
    
    return(p)
  }
  
  # Generate plots for all seeds for panel for appendix
  p1 <- make_plot_for_one_seed(seed_number = 1)
  p2 <- make_plot_for_one_seed(seed_number = 2)
  p3 <- make_plot_for_one_seed(seed_number = 3)
  p4 <- make_plot_for_one_seed(seed_number = 4)
  p5 <- make_plot_for_one_seed(seed_number = 5)
  
  # Extract the legend
  legend_plot <- cowplot::get_legend(
    p1 +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14),
        legend.key.size = unit(1.4, "lines"),
        plot.margin = margin(0,0,0,0)   # remove extra padding
      )
  )
  
  # Wrap legend as a plot panel so patchwork can use it
  legend_for_panel <- patchwork::wrap_elements(full = legend_plot)
  
  # Assemble the 2×3 grid 
  panel_for_appendix <-
    (p1 | p2) /
    (p3 | p4) /
    (p5 | legend_for_panel)
  
  # Labels for saving file name
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
  
  # Save the panel
  panel_file_name <- paste0(
    "plots_and_tables_output/appendix_extrapolation_panel_",
    metric_label_for_saved_file,
    eval_set_label_for_saved_file,
    ".png"
  )
  ggsave(
    panel_file_name,
    panel_for_appendix,
    width = 14, height = 14, dpi = 300, bg = "white"
  )
  
  # Generate figure 3 (the R2 solo plot for seed 1 will go in the main paper)
  if(my_metric == "R2_Holdout") {
    solo_plot <- make_plot_for_one_seed(seed_number = 1, for_panel = FALSE)
    
    # Save the plot
    solo_plot_file_name <- paste0(
      "plots_and_tables_output/fig_3_extrapolation_",
      metric_label_for_saved_file,
      eval_set_label_for_saved_file,
      ".png"
    )
    ggsave(
      solo_plot_file_name,
      solo_plot,
      width = 8, height = 6, dpi = 300, bg = "white"
    )
  }
  
  #### TABLE WITH SUMMARY STATISTICS ABOUT EXTRAPOLATION PERFORMANCE ####
  
  # Strip leading zero from decimal numbers in strings (for table)
  strip_leading_zero_char <- function(x) {
    sub("^(-?)0\\.", "\\1.", x)
  }
  
  # Target sample size for which we want to assess extrapolation error
  target_n <- 3696041
  
  # 1) Calculate absolute diff in observed vs. predicted performance at the 
  # maximum sample size for each seed and feature set
  abs_gaps_all_seeds <- data_for_plot %>%
    filter(n_training_set == target_n) %>%
    mutate(
      abs_diff  = abs(predicted - estimate_metric),
      feature_set_pretty = case_when(
        feature_set == "without_leakage"       ~ "All features from winning model",
        feature_set == "prefer_official_train" ~ "PreFer 'starter pack' file",
        feature_set == "ego_AS"                ~ "Ego's age \\& sex",
        TRUE                                   ~ as.character(feature_set)
      )
    )
  
  # 2) Summarize across the five seeds:
  # One row per feature set, in the desired order
  summary_by_feature_set <- abs_gaps_all_seeds %>%
    mutate(
      feature_set_pretty = factor(
        feature_set_pretty,
        levels = c(
          "All features from winning model",
          "PreFer 'starter pack' file",
          "Ego's age \\& sex"
        )
      )
    ) %>%
    group_by(feature_set_pretty) %>%
    summarise(
      Mean_Absolute_Difference   = round(mean(abs_diff),   3),
      Median_Absolute_Difference = round(median(abs_diff), 3),
      Max_Absolute_Difference    = round(max(abs_diff),    3),
      .groups = "drop"
    ) %>%
    mutate(
      Mean_Absolute_Difference   = strip_leading_zero_char(sprintf("%.3f", Mean_Absolute_Difference)),
      Median_Absolute_Difference = strip_leading_zero_char(sprintf("%.3f", Median_Absolute_Difference)),
      Max_Absolute_Difference    = strip_leading_zero_char(sprintf("%.3f", Max_Absolute_Difference))
    ) %>%
    rename(`Feature Set` = feature_set_pretty)
  
  # 3) Create LaTeX header cells containing stacked, bold text
  stacked_mean   <- "\\textbf{\\begin{tabular}{c} Mean \\\\ Absolute \\\\ Difference \\end{tabular}}"
  stacked_median <- "\\textbf{\\begin{tabular}{c} Median \\\\ Absolute \\\\ Difference \\end{tabular}}"
  stacked_max    <- "\\textbf{\\begin{tabular}{c} Max \\\\ Absolute \\\\ Difference \\end{tabular}}"
  
  header <- c(
    "Feature Set" = 1,
    "Mean\\newline Absolute\\newline Difference" = 1,
    "Median\\newline Absolute\\newline Difference" = 1,
    "Max\\newline Absolute\\newline Difference" = 1
  )
  
  # 4) Build a metric label for the LaTeX caption
  metric_label_for_caption <- case_when(
    # R^2 on validation set
    my_metric == "R2_Holdout" & 
      target_test_set == "evaluation_test_50_percent_split" ~ 
      "$R^2_{\\text{Validation}}$",
    # R^2 on holdout set
    my_metric == "R2_Holdout" & 
      target_test_set == "official_holdout_set" ~ 
      "$R^2_{\\text{Holdout}}$",
    # Other metrics
    my_metric == "LogLoss" ~ "$\\text{Log Loss}$",
    my_metric == "MSE" ~ "$\\text{MSE}$",
    my_metric == "Accuracy" ~ "$\\text{Accuracy}$",
    my_metric == "F1_Score" ~ "$F1 Score$",
    # Fallback (should never be used)
    TRUE ~ my_metric
  )
  
  # 5) Caption text
  my_caption <- paste0(
    "Absolute difference between predicted ",
    metric_label_for_caption,
    " and observed ",
    metric_label_for_caption,
    " at $n =$ 3.7 million"
  )
  
  # 6) Create kable with multi-line headers, centered columns, bold header row
  latex_table <- kable(
    summary_by_feature_set,
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    col.names = c(
      "\\textbf{Feature Set}",
      stacked_mean,
      stacked_median,
      stacked_max
    ),
    align = c("l", "c", "c", "c"),
    caption = my_caption
  )
  
  # 7) Write / append LaTeX table to a single .txt file
  tables_file <- here("plots_and_tables_for_paper/plots_and_tables_output/table_2_and_appendix_tables_extrapolation_abs_diff.txt")
  
  if (!file.exists(tables_file)) {
    cat(latex_table, file = tables_file)
  } else {
    cat("\n\n", latex_table, file = tables_file, append = TRUE)
  }

} # End loop over metrics

# Note: When accuracy is used as a metric with the "ego's age and sex" feature set, 
# the threshold to maximize performance simply predicts outcome = 0 for everyone, so
# all performance values are identical and we simply fit a horizontal line. Therefore, 
# the extrapolation performance is perfect for accuracy for this feature set. This
# result should be interpreted with caution.