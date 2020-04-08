
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

  # Ungroup dataframe
  data <- dplyr::ungroup(data)

  # Remove first selection of each player (as it is not a reselection)
  data <- dplyr::filter(.data = data, debute < player_age)

  # Create category variable from combination of debute and player_age
  data$category <- paste(data$debute, data$player_age, sep = "_")

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
    c_log2_points = unique(c_log2_points),
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
  draws <- tidybayes::median_qi(draws, .width = .width)
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

get_draws_coef <- function(model) {
  draws <- tidybayes::gather_draws(
    model = model,
    genderMen,
    genderWomen,
    `genderMen:c_log2_points`,
    `genderWomen:c_log2_points`,
    `genderMen:birth_quarter`,
    `genderWomen:birth_quarter`
  )

  draws <- tidyr::separate(
    data = draws,
    col = .variable,
    into = c("gender", ".variable"),
    sep = ":",
    fill = "right")

  draws$.variable <- ifelse(is.na(draws$.variable), "intercept", draws$.variable)

  draws
}

get_draws_data <- function(model, new_data, re_formula = NULL) {

  draws <- tidybayes::add_fitted_draws(
    newdata = new_data,
    model = model,
    scale = "response",
    re_formula = re_formula
  )

  draws <- dplyr::ungroup(draws)

  draws <- tidyr::separate(
    data = draws,
    category,
    into = c("debute", "player_age"),
    sep = "_",
    convert = TRUE
  )

  # draws <- create_first_season_draws(draws = draws)

  draws <- calc_cum_prob_data(draws = draws)

  draws
}

# Add first seasons and cumulative probs to fitted draws
get_draws <- function(model, new_data, re_formula = NULL) {

  draws <- tidybayes::add_fitted_draws(
    newdata = new_data,
    model = model,
    scale = "response",
    re_formula = re_formula
  )

  draws <- dplyr::ungroup(draws)

  draws <- tidyr::separate(
    data = draws,
    category,
    into = c("debute", "player_age"),
    sep = "_",
    convert = TRUE
  )

  # draws <- create_first_season_draws(draws = draws)

  draws <- calc_cum_prob(draws = draws)

  draws
}

# Create first season to the fitted draws
# create_first_season_draws <- function(draws = draws) {
#   new_season <- dplyr::distinct(
#     .data = draws,
#     gender,
#     birth_quarter,
#     std_log_points,
#     debute,
#     .draw,
#     .chain,
#     .iteration
#   )
#   new_season <- dplyr::mutate(
#     player_age = debute,
#     .data = new_season,
#     .row = 0,
#     .value = 1
#   )
#   dplyr::bind_rows(draws, new_season)
# }

calc_cum_prob_data <- function(draws = draws) {
  draws <- dplyr::arrange(
    .data = draws,
    .draw,
    gender,
    debute,
    birth_quarter,
    c_log2_points,
    player_age
  )

  draws <- dplyr::group_by(
    .data = draws,
    .draw,
    id
  )

  draws <- dplyr::mutate(
    .data = draws,
    cum_prob = cumprod(.value)
  )

  dplyr::ungroup(draws)
}

calc_cum_prob <- function(draws = draws) {
  draws <- dplyr::arrange(
    .data = draws,
    .draw,
    gender,
    debute,
    birth_quarter,
    c_log2_points,
    player_age
  )

  draws <- dplyr::group_by(
    .data = draws,
    .draw,
    gender,
    debute,
    birth_quarter,
    c_log2_points
  )

  draws <- dplyr::mutate(
    .data = draws,
    cum_prob = cumprod(.value)
  )

  dplyr::ungroup(draws)
}
