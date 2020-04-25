# Clean raw data ----------------------------------------------------------

# This function prepares the raw data to make it ready for the analysis.
data_preparation <- function(raw_data = raw_data) {

  # Remove multiple participations in single year
  data <- dplyr::distinct(raw_data, id, comp_year, .keep_all = TRUE)

  # Calculate players age
  data <- dplyr::mutate(data, player_age = comp_year - birth_year)

  # Truncate participation with age >= 15
  data <- dplyr::filter(data, player_age >= 15)

  # Fix data format for survival analysis
  data <- format_observations(data)

  # Calculate seasons since player debut and if selected
  data <- dplyr::group_by(.data = data, id)
  data <- dplyr::mutate(
    .data = data,
    nr_seasons = player_age - debut + 1,
    selected = ifelse(player_age <= max_age, 1, 0)
  )
  data <- dplyr::ungroup(data)

  # Remove first selection of each player (as it is not a reselection)
  data <- dplyr::filter(.data = data, debut < player_age)

  # Create category variable from combination of debut and player_age
  data$category <- paste(data$debut, data$player_age, sep = "_")

  # Create and center birth_quarter variable
  data$birth_quarter <- dplyr::case_when(
    data$birth_month <= 3 ~ -1.5,
    data$birth_month <= 6 ~ -.5,
    data$birth_month <= 9 ~ .5,
    data$birth_month <= 12 ~ 1.5,
  )

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
    scaled_log2_points = unique(scaled_log2_points),
    debut = min(player_age),
    max_age = max(player_age),
  )

  # Create data frame with all combinations of player and season
  data <- tidyr::expand(data = data, id, player_age)

  # Add player info back to the new data frame with all combinations
  data <- dplyr::left_join(x = data, y = player_info, by = "id")

  # Keep only observations from the players debut
  # until 1 season after last appearance (deselected season)
  dplyr::filter(.data = data, player_age >= debut, player_age <= max_age + 1)
}
