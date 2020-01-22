
# Prepares the data
data_preparation <- function(raw_data = raw_data) {

  # Remove multiple participations in single year
  data <- dplyr::distinct(raw_data, id, comp_year, .keep_all = TRUE)

  # Calculate players age
  data <- dplyr::mutate(data, player_age = comp_year - birth_year)

  # Truncate participation with age >= 15
  data <- dplyr::filter(data, player_age >= 15)

  # Fix data format for survival analysis
  data <- format_observations(data)

  # Group dataframe by player for next calculations
  data <- dplyr::group_by(.data = data, id)

  # Calculate seasons since player debute and if selected
  data <- dplyr::mutate(
    .data = data,
    nr_seasons = player_age - debute + 1,
    selected = ifelse(player_age <= max_age, 1, 0)
  )

  # Ungrooup dataframe
  data <- dplyr::ungroup(data)

  # Remove first selection of each player (as it is not a reselection)
  data <- dplyr::filter(.data = data, debute < player_age)

  # Create category variable from combination of debute and player_age
  data$category <- paste(data$debute, data$player_age, sep = "_")

  # Log-transform the country_points
  data$log_points <- log(1 + data$country_points)

  # Standardise log points
  data$std_log_points <- scale(data$log_points)

  # Create birth_quarter variable
  data$birth_quarter <- dplyr::case_when(
    data$birth_month <= 3 ~ 1,
    data$birth_month <= 6 ~ 2,
    data$birth_month <= 9 ~ 3,
    data$birth_month <= 12 ~ 4,
  )

  # Center birth_quarter
  data$birth_quarter <- data$birth_quarter - 2.5

  data
}

# Function to prepare data for survival analysis
format_observations <- function(data) {
  # Create data frame with player info
  player_info <- dplyr::group_by(data, id)
  player_info <- dplyr::summarise(
    .data = player_info,
    gender = unique(gender),
    birth_year = unique(birth_year),
    birth_month = unique(birth_month),
    country = unique(country),
    country_points = unique(country_points),
    debute = min(player_age),
    max_age = max(player_age),
  )

  # Create dataframe with all combinations of player and season
  data <- tidyr::expand(data = data, id, player_age)

  # Add player info back to the new dataframe with all combinations
  data <- dplyr::left_join(x = data, y = player_info, by = "id")

  # Keep only observations from the players debute
  # until 1 season after last appearance (deselected season)
  dplyr::filter(.data = data, player_age >= debute, player_age <= max_age + 1)
}

# Make model
fit_model <- function(
                      formula = formula,
                      prior_intercept = prior_intercept,
                      prior = prior,
                      prior_covariance,
                      data = data,
                      chains = 1,
                      iter = 4000) {
  rstanarm::stan_glmer(
    formula = formula,
    family = binomial(link = "logit"),
    data = data,
    prior_intercept = prior_intercept,
    prior = prior,
    prior_covariance = prior_covariance,
    chains = chains,
    iter = iter
  )
}

# Extract coefficients from model with hdi
get_coefs <- function(model, .width) {
  draws <- tidybayes::tidy_draws(model)
  draws <- dplyr::select(
    .data = draws,
    -tidyselect::starts_with("b["), -tidyselect::ends_with("__")
  )
  draws <- tidybayes::gather_variables(draws)
  draws <- tidybayes::mean_hdi(draws, .width = .width)
  draws <- dplyr::select(draws, .variable, .value, .lower, .upper)
  model_name <- substitute(model)
  draws <- dplyr::rename_at(
    .tbl = draws,
    .vars = dplyr::vars(-.variable),
    .funs = ~ paste0(model_name, .)
  )
  draws
}

merge_coefs <- function(..., .width) {
  models <- list(...)
  coef_list <- purrr::map(models, get_coefs, .width = .width)
  purrr::reduce(coef_list, dplyr::full_join, by = ".variable")
}
