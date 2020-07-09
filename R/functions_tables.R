
# Table 1 -----------------------------------------------------------------

create_table_1 <- function(data) {
  # Caculate n, re-selection and cumulative re-selection
  n_col <- calculate_n(data = data)
  reselection_table <- calculate_reselection_percentage(data = data)
  cumulative_table <- calculate_cumulative_reselection(data = data)

  # Combine into one table
  full_table <- dplyr::left_join(
    n_col,
    reselection_table,
    by = c("gender", "debut")
  )
  dplyr::left_join(
    full_table,
    cumulative_table,
    by = c("gender", "debut"),
    suffix = c("", "_cum")
  )
}


calculate_n <- function(data) {

  # Create one row per player
  data <- dplyr::group_by(.data = data, id)
  summarised_data <- dplyr::summarise(
    .data = data,
    gender = unique(gender),
    debut = unique(debut)
  )

  # Calculate the n for each debut age and gender
  n_debut <- dplyr::count(x = summarised_data, gender, debut)
  n_debut$debut <- as.character(n_debut$debut)

  # Calculate total n per gender
  n_gender <- dplyr::count(x = summarised_data, gender)
  n_gender$debut <- "Mean"

  # Add n gender total to n debut
  n_col <- rbind(n_debut, n_gender)
  dplyr::arrange(.data = n_col, gender, debut)
}

# Calculate season-to-season re-selection percentage
calculate_reselection_percentage <- function(data) {

  # Re-selection for each debut age, player age and gender
  grouped_all <- dplyr::group_by(.data = data, gender, debut, player_age)
  reselection_all <- dplyr::summarise(
    .data = grouped_all,
    reselection = mean(selected) * 100
  )
  reselection_all <- tidyr::pivot_wider(
    data = reselection_all,
    names_from = player_age,
    values_from = reselection
  )
  reselection_all <- dplyr::ungroup(x = reselection_all)
  reselection_all$debut <- as.character(reselection_all$debut)

  # Mean re-selection for each player age and gender
  grouped_player_age <- dplyr::group_by(.data = data, gender, player_age)
  reselection_player_age <- dplyr::summarise(
    .data = grouped_player_age,
    reselection = mean(selected) * 100
  )
  reselection_player_age <- tidyr::pivot_wider(
    data = reselection_player_age,
    names_from = player_age,
    values_from = reselection
  )
  reselection_player_age$debut <- "Mean"

  # Mean re-selection for each debut age and gender
  grouped_debut <- dplyr::group_by(.data = data, gender, debut)
  reselection_debut <- dplyr::summarise(
    .data = grouped_debut,
    mean = mean(selected) * 100
  )
  reselection_debut$debut <- as.character(reselection_debut$debut)

  # Mean re-selection for each gender
  grouped_gender <- dplyr::group_by(.data = data, gender)
  reselection_gender <- dplyr::summarise(
    .data = grouped_gender,
    mean = mean(selected) * 100
  )
  reselection_gender$debut <- "Mean"

  # Join together
  reselection_table <- dplyr::bind_rows(reselection_all, reselection_player_age)
  reselection_tot_col <- dplyr::bind_rows(reselection_debut, reselection_gender)
  dplyr::left_join(
    reselection_table,
    reselection_tot_col,
    by = c("gender", "debut"),
  )
}

calculate_cumulative_reselection <- function(data) {

  # Cumulative re-selection for each debut age, player age and gender
  grouped_all <- dplyr::group_by(.data = data, gender, debut, player_age)
  summarised_all <- dplyr::summarise(
    .data = grouped_all, reselection = mean(selected)
  )
  cumulative_all <- dplyr::mutate(
    .data = summarised_all,
    cum_reselection = cumprod(reselection) * 100
  )
  cumulative_all <- tidyr::pivot_wider(
    data = cumulative_all,
    id_cols = c(gender, debut),
    names_from = player_age,
    values_from = cum_reselection
  )
  cumulative_all <- dplyr::ungroup(cumulative_all)
  cumulative_all$debut <- as.character(cumulative_all$debut)

  # Cumulative re-selection for each debut age and gender
  ## Calculate n for each debut age and gender
  grouped_id <- dplyr::group_by(.data = data, id)
  summarised_data <- dplyr::summarise(
    .data = grouped_id,
    gender = unique(gender),
    debut = unique(debut)
  )
  n_debut <- dplyr::count(x = summarised_data, gender, debut)
  n_debut$debut <- as.character(n_debut$debut)

  ## Merge n with cumulative re-selection
  cumulative_debut <- dplyr::left_join(
    cumulative_all,
    n_debut,
    by = c("gender", "debut")
  )

  ## Calculate cumulative re-selection for each debut age
  cumulative_debut <- dplyr::group_by(.data = cumulative_debut, gender)
  cumulative_debut <- dplyr::summarise_at(
    .tbl = cumulative_debut,
    dplyr::vars(`16`:`20`),
    ~weighted.mean(., n, na.rm = TRUE)
  )
  cumulative_debut$debut <- "Mean"

  # Join together
  cumulative_table <- dplyr::bind_rows(cumulative_all, cumulative_debut)
  dplyr::arrange(.data = cumulative_table, gender, debut)
}



# Table 2 -----------------------------------------------------------------


create_table_2 <- function(coefs, .width, var_names) {
  # Create table with model coefficients
  coef_table <- make_model_coef_table(coefs = coefs, .width = .width)

  # Compare men and women
  coef_comp <- make_coef_comp(coefs = coefs, .width = .width)

  # Join coefficients with comparisons (on the women coefficient lines)
  coef_comp <- dplyr::mutate(
    .data = coef_comp,
    name = ifelse(
      name == "intercept",
      "genderWomen",
      paste0("genderWomen:", name))
  )
  coef_table_full <- dplyr::left_join(
    coef_table,
    coef_comp,
    by = "name",
    suffix = c("", "_comp"))

  # Order and rename parameters
  coef_table_full <- dplyr::mutate(
    .data = coef_table_full,
    name = dplyr::recode(name, !!!var_names),
  )
  coef_table_full <- dplyr::arrange(
    .data = coef_table_full,
    factor(name, levels = var_names)
  )
  coef_table_full <- dplyr::select(
    .data = coef_table_full,
    Parameter = name,
    Est = value,
    `Est LL` = .lower,
    `Est UL` = .upper,
    Diff = value_comp,
    `Diff LL` = .lower_comp,
    `Diff UL` = .upper_comp
  )
  coef_table_full
}

# Function to create model coefficient table
make_model_coef_table <- function(coefs, .width) {

  # Calculate mean and high density interval
  grouped_coefs <- dplyr::group_by(.data = coefs, name)
  coef_table <- tidybayes::mean_hdi(.data = grouped_coefs, value, .width = .95)

  # Remove unnecessary columns
  coef_table <- dplyr::select(.data = coef_table, name, value, .lower, .upper)
  coef_table
}

# Function to compare model coefficients between men and women
make_coef_comp <- function(coefs, .width) {

  # Remove random factors, and separate gender and variable names
  coefs <- coefs[!grepl("Sigma", coefs$name),]
  coefs <- tidyr::separate(
    data = coefs,
    col = name,
    into = c("gender", "name"),
    sep = ":",
    fill = "right"
  )
  coefs$name <- ifelse(is.na(coefs$name), "intercept", coefs$name)

  # Calculate difference between genders
  grouped_coefs <- dplyr::group_by(.data = coefs, name)
  coef_compared <- tidybayes::compare_levels(
    data = grouped_coefs,
    variable = value,
    by = gender
  )
  coef_compared <- tidybayes::mean_hdi(.data = coef_compared, .width = .width)
  coef_compared <- dplyr::ungroup(coef_compared)
  dplyr::select(.data = coef_compared, name, value, .lower, .upper)
}

# Table 3 -----------------------------------------------------------------

create_table_3 <- function(draws, .width) {
  draws <- dplyr::filter(.data = draws, player_age == 20)
  draws$debut <- paste("Age", draws$debut)
  comps <- tidybayes::compare_levels(
    data = draws,
    cum_prob,
    by = debut,
    fun = `/`,
    draw_indices = c("gender", ".draw"),
    comparison = list(
      c("Age 15", "Age 16"),
      c("Age 15", "Age 17"),
      c("Age 15", "Age 18"),
      c("Age 15", "Age 19"),
      c("Age 16", "Age 17"),
      c("Age 16", "Age 18"),
      c("Age 16", "Age 19"),
      c("Age 17", "Age 18"),
      c("Age 17", "Age 19"),
      c("Age 18", "Age 19")
    )
  )
  comps_grouped <- dplyr::group_by(.data = comps, gender, debut)
  comps_table <- tidybayes::mean_hdi(
    .data = comps_grouped,
    cum_prob,
    .width = .width
  )
  comps_table <- dplyr::select(
    .data = comps_table,
    gender,
    debut,
    relative_risk = cum_prob,
    .lower,
    .upper
  )
  comps_table <- tidyr::pivot_wider(
    data = comps_table,
    names_from = gender,
    values_from = c(relative_risk, .lower, .upper)
  )
  comps_table <- dplyr::select(
    .data = comps_table,
    Debut = debut,
    `RR Men` = relative_risk_Men,
    `LL Men` = .lower_Men,
    `UL Men` = .upper_Men,
    `RR Women` = relative_risk_Women,
    `LL Women` = .lower_Women,
    `UL Women` = .upper_Women
  )
  comps_table
}


# Table 4 -----------------------------------------------------------------

create_table_4 <- function(draws, .width) {

  # Filter and prepare data
  draws <- prepare_birth_quarter_draws(draws = draws)
  draws$debut <- paste("Age", draws$debut)

  # Calculate probabilities for q4 and q1
  probabilities <- caclulate_birth_quarter_reselection(draws = draws, .width = .width)

  # Calculate relative risk
  relative_risk <- calculate_birth_quarter_rr(draws = draws, .width = .width)

  # Join probabilities and relative risk
  dplyr::left_join(probabilities, relative_risk, by = c("gender", "debut"))
}

prepare_birth_quarter_draws <- function(draws) {
  draws_grouped <- dplyr::group_by(.data = draws, gender)
  draws_filtered <- dplyr::filter(
    .data = draws_grouped,
    player_age == 20,
    birth_quarter %in% c(min(birth_quarter), max(birth_quarter))
  )
  draws_filtered <- dplyr::mutate(
    .data = draws_filtered,
    birth_quarter = dplyr::case_when(
      birth_quarter == min(birth_quarter) ~ "q1",
      birth_quarter == max(birth_quarter) ~ "q4",
    )
  )
  draws_filtered
}

caclulate_birth_quarter_reselection <- function(draws, .width) {
  draws_grouped <- dplyr::group_by(.data = draws, gender, debut, birth_quarter)
  reselection_probability <- tidybayes::mean_hdci(
    .data = draws_grouped,
    cum_prob,
    .width = .width
  )
  reselection_probability <- dplyr::select(
    .data = reselection_probability,
    gender,
    debut,
    birth_quarter,
    value = cum_prob,
    .lower,
    .upper
  )
  reselection_wide <- tidyr::pivot_wider(
    data = reselection_probability,
    names_from = birth_quarter,
    values_from = c(value, .lower, .upper)
  )
  reselection_wide <- dplyr::select(
    .data = reselection_wide,
    gender,
    debut,
    value_q4,
    .lower_q4,
    .upper_q4,
    value_q1,
    .lower_q1,
    .upper_q1
  )
  reselection_wide <- dplyr::ungroup(reselection_wide)
  reselection_wide
}

calculate_birth_quarter_rr <- function(draws = draws, .width = .width) {
  draw_comps <- tidybayes::compare_levels(
    data = draws,
    cum_prob,
    by = birth_quarter,
    fun = `/`,
    draw_indices = c("gender", "debut", ".draw")
  )
  comps_grouped <- dplyr::group_by(.data = draw_comps, gender, debut)
  relative_risk <- tidybayes::mean_hdci(
    .data = comps_grouped,
    cum_prob,
    .width = .width
  )
  relative_risk <- dplyr::select(
    .data = relative_risk,
    gender,
    debut,
    rr = cum_prob,
    .lower,
    .upper
  )
  relative_risk <- dplyr::ungroup(relative_risk)
  relative_risk
}

# Table 5 -----------------------------------------------------------------

create_table_5 <- function(draws, .width) {

  # Filter and prepare data
  draws <- prepare_ranking_draws(draws = draws)
  draws$debut <- paste("Age", draws$debut)

  # Calculate probabilities for top and bottom ranks
  probabilities <- caclulate_ranking_reselection(draws = draws, .width = .width)

  # Calculate relative risk
  relative_risk <- calculate_ranking_rr(draws = draws, .width = .width)

  # Join probabilities and relative risk
  dplyr::left_join(probabilities, relative_risk, by = c("gender", "debut"))
}

prepare_ranking_draws <- function(draws) {
  draws_grouped <- dplyr::group_by(.data = draws, gender)
  draws_filtered <- dplyr::filter(
    .data = draws_grouped,
    player_age == 20,
    scaled_log2_points %in% c(min(scaled_log2_points), max(scaled_log2_points))
  )
  draws_filtered <- dplyr::mutate(
    .data = draws_filtered,
    ranking_points = dplyr::case_when(
      scaled_log2_points == min(scaled_log2_points) ~ "bottom",
      scaled_log2_points == max(scaled_log2_points) ~ "top",
    )
  )
  draws_filtered
}

caclulate_ranking_reselection <- function(draws, .width) {
  draws_grouped <- dplyr::group_by(.data = draws, gender, debut, ranking_points)
  reselection_probability <- tidybayes::mean_hdci(
    .data = draws_grouped,
    cum_prob,
    .width = .width
  )
  reselection_probability <- dplyr::select(
    .data = reselection_probability,
    gender,
    debut,
    ranking_points,
    value = cum_prob,
    .lower,
    .upper
  )
  reselection_wide <- tidyr::pivot_wider(
    data = reselection_probability,
    names_from = ranking_points,
    values_from = c(value, .lower, .upper)
  )
  reselection_wide <- dplyr::select(
    .data = reselection_wide,
    gender,
    debut,
    value_top,
    .lower_top,
    .upper_top,
    value_bottom,
    .lower_bottom,
    .upper_bottom
  )
  reselection_wide <- dplyr::ungroup(reselection_wide)
  reselection_wide
}

calculate_ranking_rr <- function(draws = draws, .width = .width) {
  draw_comps <- tidybayes::compare_levels(
    data = draws,
    cum_prob,
    by = ranking_points,
    fun = `/`,
    draw_indices = c("gender", "debut", ".draw")
  )
  comps_grouped <- dplyr::group_by(.data = draw_comps, gender, debut)
  relative_risk <- tidybayes::mean_hdci(
    .data = comps_grouped,
    cum_prob,
    .width = .width
  )
  relative_risk <- dplyr::select(
    .data = relative_risk,
    gender,
    debut,
    rr = cum_prob,
    .lower,
    .upper
  )
  relative_risk <- dplyr::ungroup(relative_risk)
  relative_risk
}
