
# Figure 1 ----------------------------------------------------------------

create_figure_1_data <- function(data) {

  # Remove multiple participations in single year
  data <- dplyr::distinct(data, id, comp_year, .keep_all = TRUE)

  # Calculate players age
  data <- dplyr::mutate(data, player_age = comp_year - birth_year)

  # Truncate participation with age >= 15
  data <- dplyr::filter(data, player_age >= 15)

  # Fix data format for survival analysis
  data <- format_observations(data)

  # Calculate birth quarter
  data$birth_quarter <- dplyr::case_when(
    data$birth_month <= 3 ~ 1,
    data$birth_month <= 6 ~ 2,
    data$birth_month <= 9 ~ 3,
    data$birth_month <= 12 ~ 4,
  )

  # Remove seasons not selected
  data <- dplyr::filter(
    .data = data,
    player_age <= max_age
  )

  # Create columns for initial selection, re-selection and de-selection
  data$initial_selection <- data$player_age == data$debut
  data$participation <- TRUE
  data$re_selection <- data$player_age < data$max_age
  data$de_selection <- data$player_age == data$max_age

  # Count total and per quarter for each of the columns of the flowchart
  results <- list(
    initial_selection = make_column_data(data, initial_selection),
    participation = make_column_data(data, participation),
    re_selection = make_column_data(data, re_selection),
    de_selection = make_column_data(data, de_selection)
  )

  # Bind above together to one dataframe
  results <- purrr::reduce(
    results,
    dplyr::full_join,
    by = c("gender", "player_age")
  )

  # Calculate total number of players per gender
  per_player <- dplyr::distinct(.data = data, id, gender)
  n_men <- sum(per_player$gender == "Men")
  n_women <- sum(per_player$gender == "Women")

  results <- dplyr::mutate(
    .data = results,
    gender = dplyr::case_when(
      gender == "Men" ~ paste0(gender, " (n = ", n_men, ")"),
      gender == "Women" ~ paste0(gender, " (n = ", n_women, ")"),
    )
  )
  results <- calculate_percentages(results)
  results
}

make_column_data <- function(data, column) {
  data <- dplyr::filter(data, {{ column }})
  data <- dplyr::group_by(
    .data = data,
    gender,
    player_age
  )
  result <- dplyr::summarise(
    .data = data,
    .groups = "keep",
    "{{column}}_n" := dplyr::n(),
    "{{column}}_Q1" := sum(birth_quarter == 1),
    "{{column}}_Q2" := sum(birth_quarter == 2),
    "{{column}}_Q3" := sum(birth_quarter == 3),
    "{{column}}_Q4" := sum(birth_quarter == 4)
  )

  result
}

make_participation_data <- function(data) {

  data <- dplyr::group_by(
    .data = data_initial_selection,
    gender,
    player_age
  )

  initial_selection <- dplyr::summarise(
    .data = data_initial_selection,
    .groups = "keep",
    n = dplyr::n(),
    Q1 = sum(birth_quarter == 1),
    Q2 = sum(birth_quarter == 2),
    Q3 = sum(birth_quarter == 3),
    Q4 = sum(birth_quarter == 4)
  )

  initial_selection
}

calculate_percentages <- function(results) {
  results <- dplyr::ungroup(results)
  dplyr::mutate(
    .data = results,
    initial_selection_Q1 = initial_selection_Q1 / initial_selection_n * 100,
    initial_selection_Q2 = initial_selection_Q2 / initial_selection_n * 100,
    initial_selection_Q3 = initial_selection_Q3 / initial_selection_n * 100,
    initial_selection_Q4 = initial_selection_Q4 / initial_selection_n * 100,
    re_selection_Q1 = re_selection_Q1 / participation_Q1 * 100,
    re_selection_Q2 = re_selection_Q2 / participation_Q2 * 100,
    re_selection_Q3 = re_selection_Q3 / participation_Q3 * 100,
    re_selection_Q4 = re_selection_Q4 / participation_Q4 * 100,
    de_selection_Q1 = de_selection_Q1 / participation_Q1 * 100,
    de_selection_Q2 = de_selection_Q2 / participation_Q2 * 100,
    de_selection_Q3 = de_selection_Q3 / participation_Q3 * 100,
    de_selection_Q4 = de_selection_Q4 / participation_Q4 * 100,
    participation_Q1 = participation_Q1 / participation_n * 100,
    participation_Q2 = participation_Q2 / participation_n * 100,
    participation_Q3 = participation_Q3 / participation_n * 100,
    participation_Q4 = participation_Q4 / participation_n * 100
  )
}

# Figure 2 ----------------------------------------------------------------
create_figure_2 <- function(data, draws, .width, theme) {
  plot_data <- prep_data_figure_2(data = data, draws = draws, .width = .width)
  plot <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = player_age,
      y = cum_prob
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(y = avg_cum_prob),
      color = "gray70",
      size = .3
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .lower, ymax = .upper),
      alpha = .2
    ) +
    ggplot2::geom_line(
      size = .3
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(debut),
      rows = ggplot2::vars(gender),
      labeller = ggplot2::labeller(debut = debut_labels),
      switch = "y"
    ) + theme
  plot
}

# Function to prepare data for plotting figure 2
prep_data_figure_2 <- function(data, draws, .width) {

  # Calculate mean and ci for re-selection create a first season with prob = 1
  first_year <- create_first_year_probs(draws = draws, gender, debut)
  summarised_data <- prepare_plot_data(draws, gender, debut, player_age, .width = .width)

  # Prepare the observed average cumulative probabiliry
  avg_cum_prob <- prepare_cum_prob(data = data)

  # Join all
  plot_data <- dplyr::bind_rows(summarised_data, first_year)
  plot_data <- dplyr::left_join(
    avg_cum_prob,
    plot_data,
    by = c("gender", "debut", "player_age")
  )
  plot_data
}

# Function to prepare overall average cumulative re-selection for figure 2
prepare_cum_prob <- function(data) {
  cum_reselection <- calculate_cumulative_reselection(data = data)
  cum_reselection <- dplyr::filter(.data = cum_reselection, debut == "Mean")
  avg_cum <- tidyr::pivot_longer(
    data = cum_reselection,
    cols = `16`:`20`,
    names_to = "player_age",
    names_transform = list(player_age = as.integer),
    values_to = "avg_cum_prob"
  )
  avg_cum$avg_cum_prob <- avg_cum$avg_cum_prob / 100
  avg_cum <- tidyr::expand(
    data = avg_cum,
    tidyr::nesting(gender, player_age, avg_cum_prob),
    debut = 15:19
  )
  avg_cum
}

# General helper functions ------------------------------------------------

# Function to create data for first year probability (1)
create_first_year_probs <- function(draws, ...) {
  first_year <- dplyr::distinct(.data = draws, ...)
  first_year_probs <- dplyr::mutate(
    .data = first_year,
    player_age = debut,
    cum_prob = 1,
    .lower = 1,
    .upper = 1
  )
  first_year_probs
}

# Function to calculate data for figures
prepare_plot_data <- function(draws, ..., .width) {
  grouped_data <- dplyr::group_by(.data = draws, ...)
  summarised_data <- tidybayes::mean_hdci(
    .data = grouped_data,
    cum_prob,
    .width = .width
  )
  summarised_data <- dplyr::select(
    .data = summarised_data,
    -c(.width, .point, .interval)
  )
  ungrouped_data <- dplyr::ungroup(summarised_data)
  ungrouped_data
}

# Labeller for debut age facet labels
debut_labels <- function(string) {paste("Debut", string)}
