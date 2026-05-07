# Using "results/missingness.csv," this script calculates missingness rates for
# features that are not structurally missing as indicated by
# "data/table_a1_raw.csv." It then creates a LaTeX table with the feature
# descriptions and missingness rates, which is saved as
# "table_a1_feature_description.tex" in the output directory.
# In addition, this scripts creates "missingness_by_outcome.csv"
if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "here", "kableExtra")
  invisible(suppressMessages(
    groundhog.library(packages, "2025-11-15")
  ))
}

#### SETTINGS ####

# Choose whether to test on evaluation set or holdout set
# "evaluation_test_50_percent_split" for validation set
# "official_holdout_set" for holdout set
# If we are not looking at the official holdout set, we will exclude the
# official holdout set from the missingness calculations
if (!exists("target_test_set", inherits = TRUE)) {
  target_test_set <- "evaluation_test_50_percent_split"
}
missingness <- "results/missingness.csv" |>
  here() |>
  read.csv()
if (target_test_set == "evaluation_test_50_percent_split") {
  missingness <- filter(missingness, sample != "official_holdout_set")
}

# Calculate missingness
missingness <- missingness |>
  select(-sample, -prefer_set, -outcome_missing_from_prefer_set) |>
  mutate(has_partner = 0) # A variable that has no missingness by construction: 
  # one either lives or does not live with a partner recorded in
  # huishoudensbus, and everyone in PreFer has an entry in huishoudensbus.
missingness0 <- filter(missingness, outcome == 0)
missingness1 <- filter(missingness, outcome == 1)

# This function calculates missingness rates for each variable.
get_missingness_rates <- function(missingness_df) {
  missingness_df <- missingness_df |> 
    select(-outcome) |> 
    colSums()
  missingness_rates <- missingness_df / missingness_df[[1]] * 100
  tibble(
    Variable.Name = names(missingness_rates),
    missingness = missingness_rates
  )
}
missingness <- get_missingness_rates(missingness)
missingness0 <- get_missingness_rates(missingness0)
missingness1 <- get_missingness_rates(missingness1)

# Calculate average missingness across features that are not structurally
# missing
table <- "data/table_a1_raw.csv" %>%
  here() |>
  read.csv() %>%
  left_join(missingness, by = "Variable.Name") |>
  mutate(X..Missing = if_else(X..Missing == "Structurally missing", NA, missingness))
mean_missingness <- table |>
  pull(X..Missing) |>
  mean(na.rm = TRUE)
print(paste0(
  "Average missingness across features that are not structurally missing: ",
  mean_missingness, "%"
))

# Make a table comparing missingness rates for outcome 0 and outcome 1.
variable_names <- table |> 
  filter(!is.na(X..Missing)) |>
  pull(Variable.Name)
missingness0 <- filter(missingness0, Variable.Name %in% variable_names)
missingness1 <- filter(missingness1, Variable.Name %in% variable_names)
missingness0 |> 
  full_join(missingness1, by = "Variable.Name", suffix = c("_0", "_1")) |> 
  write_csv(here("analysis/plots_and_tables_output/missingness_by_outcome.csv"))

# This function implements rounding half up rule
round_half_up <- function(x, n) {
  posneg <- sign(x)
  output <- abs(x) * 10^n
  output <- output + 0.5 + sqrt(.Machine$double.eps)
  output <- trunc(output)
  output <- output / 10^n
  output * posneg
}

# Produce table
table <- table |>
  select(-missingness) |>
  mutate(
    Variable.Name = gsub("_", "\\\\_", Variable.Name),
    Description = gsub("%", "\\\\%", Description),
    Description = paste("\\raggedright \\hangindent=1em", Description),
    Source = gsub(
      ", ",
      paste0(
        "} \\\\parbox[t]{3.75cm}",
        "{\\\\vspace*{0.04cm} \\\\raggedright \\\\hangindent=1em "
      ),
      Source
    ),
    Source = paste0(
      "\\parbox[t]{3.75cm}{\\raggedright \\hangindent=1em ",
      Source,
      "}"
    ),
    Feature.Set.for.Sample.Size.Analyses = gsub(
      "&", "\\\\&",
      Feature.Set.for.Sample.Size.Analyses
    ),
    Feature.Set.for.Sample.Size.Analyses = gsub(
      "; ",
      "} \\\\parbox[t]{1.85cm}{\\\\raggedright \\\\hangindent=1em ",
      Feature.Set.for.Sample.Size.Analyses
    ),
    Feature.Set.for.Sample.Size.Analyses = paste0(
      "\\parbox[t]{1.85cm}{\\raggedright \\hangindent=1em ",
      Feature.Set.for.Sample.Size.Analyses,
      "}"
    ),
    Feature.Set.for.Feature.Set.Analyses = gsub(
      "&", "\\\\&",
      Feature.Set.for.Feature.Set.Analyses
    ),
    Feature.Set.for.Feature.Set.Analyses = paste(
      "\\raggedright \\hangindent=1em",
      Feature.Set.for.Feature.Set.Analyses
    ),
    X..Missing = if_else(is.na(X..Missing),
      "", format(round_half_up(X..Missing, 1), nsmall = 1)
    )
  ) %>%
  kable("latex",
    col.names = c(
      "\\parbox{.35cm}{\\centering \\#}",
      "\\parbox{5.45cm}{\\centering Variable name}",
      "\\parbox{3cm}{\\centering Description}",
      "\\parbox{3.75cm}{\\centering Source}",
      paste0(
        "\\parbox{1.85cm}",
        "{\\centering Feature set \\\\ for \\\\ sample size \\\\ analyses}"
      ),
      paste0(
        "\\parbox{1.5cm}",
        "{\\centering Feature set \\\\ for \\\\ feature set \\\\ analyses}"
      ),
      "\\parbox{.55cm}{\\centering \\% miss- \\\\ ing}"
    ),
    align = "cp{5.45cm}p{3cm}p{3.75cm}p{1.85cm}p{1.5cm}c",
    caption =
      "Features in the Winning Model",
    escape = FALSE,
    longtable = TRUE,
    booktabs = TRUE,
    linesep = ""
  ) %>%
  kable_styling(latex_options = c("repeat_header"))
"\\begin{landscape}\n\\tiny\n" %>%
  paste0(table, "\n\\end{landscape}") %>%
  str_replace(fixed("Description"), "Description\\footnotemark[1]") %>%
  str_replace(fixed("miss- \\\\ ing"), "miss- \\\\ ing\\footnotemark[2]") |>
  str_replace(
    fixed("\\endlastfoot"),
    paste0(
      "\\endlastfoot\n",
      "\\footnotetext[1]",
      "{\\tiny The number of categories mentioned in the descriptions refers ",
      "to all categories mentioned for the variable in data documentations ",
      "but do not include categories for missing data. However, our ",
      "algorithm treated any such category as just another category of ",
      "data.}\n",
      "\\footnotetext[2]",
      "{\\tiny We calculated missingness rates for all variables without ",
      "structural missingness. Structural missingness refers to the ",
      "situation in which a missingness code excludes a defined category of ",
      "people from having non-missing values or when a feature covers ",
      "information about a family member the focal individual may not have. ",
      "We counted officially imputed data as nonmissing because Statistics ",
      "Netherlands (CBS) often based imputations on a substantive amount of ",
      "nonimputed information. For example, education attainment estimates ",
      "reflect actual register records about degree achievement, but CBS ",
      "imputed a higher level of achievement for about 4 percent of the ",
      "records to better account for education received from abroad and from ",
      "private institutions ",
      "\\cite{statistics_netherlands_hoogsteopltab_2025}.}\n"
    )
  ) |>
  write(here(
    "analysis/plots_and_tables_output/table_a1_feature_description.tex"
  ))