

# Get model coefficients --------------------------------------------------

# Function to extract the coefficients from a model
get_coefs <- function(model) {
  draws <- tidybayes::tidy_draws(model)
  # Remove coefficients for random factors
  draws <- dplyr::select(
    .data = draws,
    -tidyselect::starts_with("b["), -tidyselect::ends_with("__")
  )
  tidyr::pivot_longer(data = draws, cols = -c(.chain, .iteration, .draw))
}

# Get posterior predictive simulations ------------------------------------


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
    into = c("debut", "player_age"),
    sep = "_",
    convert = TRUE
  )

  draws <- calc_cum_prob(draws = draws)

  draws
}

calc_cum_prob <- function(draws = draws) {
  draws <- dplyr::arrange(
    .data = draws,
    .draw,
    gender,
    debut,
    birth_quarter,
    scaled_log2_points,
    player_age
  )

  draws <- dplyr::group_by(
    .data = draws,
    .draw,
    gender,
    debut,
    birth_quarter,
    scaled_log2_points
  )

  draws <- dplyr::mutate(
    .data = draws,
    cum_prob = cumprod(.value)
  )

  dplyr::ungroup(draws)
}
