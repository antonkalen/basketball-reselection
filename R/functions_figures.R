
# Figure 1 ----------------------------------------------------------------
create_figure_1 <- function(data, draws, .width, theme) {
  plot_data <- prep_data_figure_1(data = data, draws = draws, .width = .width)
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

# Function to prepare data for plotting figure 1
prep_data_figure_1 <- function(data, draws, .width) {

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

# Function to prepare overall average cumulative re-selection for figure 1
prepare_cum_prob <- function(data) {
  cum_reselection <- calculate_cumulative_reselection(data = data)
  cum_reselection <- dplyr::filter(.data = cum_reselection, debut == "Mean")
  avg_cum <- tidyr::pivot_longer(
    data = cum_reselection,
    cols = `16`:`20`,
    names_to = "player_age",
    names_ptypes = list(player_age = integer()),
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

# Figure 2 ----------------------------------------------------------------
create_figure_2 <- function(draws, .width, theme) {
  plot_data <- prep_data_figure_2(draws = draws, .width = .width)
  plot <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = player_age,
      y = cum_prob,
      linetype = birth_quarter
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .lower, ymax = .upper),
      show.legend = FALSE,
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
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        override.aes = list(fill = NA)
      )
    ) + theme
  plot
}

# Function to prepare data for plotting figure 2
prep_data_figure_2 <- function(draws, .width) {

  # Calculate mean and ci for re-selection create a first season with prob = 1
  first_year <- create_first_year_probs(
    draws = draws,
    gender,
    debut,
    birth_quarter
  )
  summarised_data <- prepare_plot_data(
    draws = draws,
    gender,
    birth_quarter,
    debut,
    player_age,
    .width = .width
  )

  # Join data and keep only first and last quarter
  plot_data <- dplyr::bind_rows(summarised_data, first_year)
  plot_data <- dplyr::filter(
    .data = plot_data,
    birth_quarter %in% c(-1.5, 1.5)
  )
  plot_data <- dplyr::mutate(
    .data = plot_data,
    birth_quarter = dplyr::case_when(
      birth_quarter == -1.5 ~ "Quarter 1",
      birth_quarter == 1.5 ~ "Quarter 4",
    )
  )
}

# Figure 3 ----------------------------------------------------------------

create_figure_3 <- function(draws, .width, theme) {
  plot_data <- prep_data_figure_3(draws = draws, .width = .width)
  plot <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = player_age,
      y = cum_prob,
      linetype = scaled_log2_points
    )
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
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        override.aes = list(fill = NA)
      )
    ) + theme
  plot
}

# Function to prepare data for plotting figure 2
prep_data_figure_3 <- function(draws, .width) {

  # Calculate mean and ci for re-selection create a first season with prob = 1
  first_year <- create_first_year_probs(
    draws = draws,
    gender,
    debut,
    scaled_log2_points
  )
  summarised_data <- prepare_plot_data(
    draws = draws,
    gender,
    scaled_log2_points,
    debut,
    player_age,
    .width = .width
  )

  # Join data and keep only first and last quarter
  plot_data <- dplyr::bind_rows(summarised_data, first_year)
  plot_data <- dplyr::group_by(.data = plot_data, gender)
  plot_data <- dplyr::filter(
    .data = plot_data,
    scaled_log2_points %in% c(min(scaled_log2_points), max(scaled_log2_points))
  )
  plot_data <- dplyr::mutate(
    .data = plot_data,
    scaled_log2_points = dplyr::case_when(
      scaled_log2_points == max(scaled_log2_points) ~ "Top ranked",
      scaled_log2_points == min(scaled_log2_points) ~ "Bottom ranked"
    ),
    scaled_log2_points = factor(
      scaled_log2_points,
      levels = c("Top ranked", "Bottom ranked")
    )
  )
  plot_data <- dplyr::ungroup(plot_data)
  plot_data
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
