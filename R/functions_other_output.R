
# Birth quarter text results ----------------------------------------------

create_quarter_results <- function(draws, .width) {
  # Filter and prepare data
  draws <- prepare_quarter_draws(draws = draws)

  # Calculate probabilities for top and bottom ranks
  probabilities <- caclulate_quarter_reselection(draws = draws, .width = .width)

  # Calculate relative risk
  relative_risk <- calculate_quarter_rr(draws = draws, .width = .width)

  # Join probabilities and relative risk
  dplyr::left_join(probabilities, relative_risk, by = c("gender"))
}

prepare_quarter_draws <- function(draws) {
  draws_filtered <- dplyr::filter(
    .data = draws,
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

caclulate_quarter_reselection <- function(draws, .width) {
  draws_grouped <- dplyr::group_by(.data = draws, gender, birth_quarter)
  reselection_probability <- tidybayes::mean_hdci(
    .data = draws_grouped,
    cum_prob,
    .width = .width
  )
  reselection_probability <- dplyr::select(
    .data = reselection_probability,
    gender,
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
    value_q1,
    .lower_q1,
    .upper_q1,
    value_q4,
    .lower_q4,
    .upper_q4
  )
  reselection_wide <- dplyr::ungroup(reselection_wide)
  reselection_wide
}

calculate_quarter_rr <- function(draws = draws, .width = .width) {
  draw_comps <- tidybayes::compare_levels(
    data = draws,
    cum_prob,
    by = birth_quarter,
    fun = `/`,
    draw_indices = c("gender", "debut", ".draw")
  )
  comps_grouped <- dplyr::group_by(.data = draw_comps, gender)
  relative_risk <- tidybayes::mean_hdci(
    .data = comps_grouped,
    cum_prob,
    .width = .width
  )
  relative_risk <- dplyr::select(
    .data = relative_risk,
    gender,
    rr = cum_prob,
    .lower_rr = .lower,
    .upper_rr = .upper
  )
  relative_risk <- dplyr::ungroup(relative_risk)
  relative_risk
}

# Supplementary table 1 ---------------------------------------------------


create_supplementary_table_1 <- function(coefs, loos, .width, var_names) {
  loos <- prepare_loos(loos = loos)
  coefs <- prepare_coefs(coefs = coefs, .width = .width, var_names = var_names)
  table <- dplyr::bind_rows(coefs, loos)
  table <- dplyr::select(
    .data = table,
    Parameters = name,
    `Est Model 1` = value_model_1,
    `LL Model 1` = .lower_model_1,
    `UL Model 1` = .upper_model_1,
    `Est Model 2` = value_model_2,
    `LL Model 2` = .lower_model_2,
    `UL Model 2` = .upper_model_2,
    `Est Model 3` = value_model_3,
    `LL Model 3` = .lower_model_3,
    `UL Model 3` = .upper_model_3,
    `Est Model 4` = value_model_4,
    `LL Model 4` = .lower_model_4,
    `UL Model 4` = .upper_model_4
  )
  table
}

# Function to format loo values for the supplementary material
prepare_loos <- function(loos) {
  loos <- as.data.frame(loos)
  loos <- tibble::rownames_to_column(loos)
  loos <- dplyr::select(
    .data = loos,
    name = rowname,
    value = elpd_diff,
    .lower = se_diff
  )
  loos <- tidyr::pivot_longer(
    data = loos,
    cols = c(value, .lower),
    names_to = "names")
  loos <- dplyr::mutate(
    .data = loos,
    value = ifelse(names == "value", value * -2, value * 2),
    names = paste(names, name, sep = "_"),
    name = "Relative LOO-IC (SE)"
  )
  loos <- tidyr::pivot_wider(
    data = loos,
    names_from = names,
    values_from = value
  )
  loos
}

prepare_coefs <- function(coefs, .width, var_names) {
  coefs_grouped <- dplyr::group_by(.data = coefs, model, name)
  coef_summarised <- tidybayes::mean_hdci(
    .data = coefs_grouped,
    value,
    .width = .width
  )
  coef_summarised <- dplyr::select(
    .data = coef_summarised,
    model,
    name,
    value,
    .lower,
    .upper
  )
  coef_summarised <- tidyr::pivot_wider(
    data = coef_summarised,
    names_from = model,
    names_prefix = "model_",
    values_from = c(value, .lower, .upper)
  )
  coef_summarised <- dplyr::mutate(
    .data = coef_summarised,
    name = dplyr::recode(name, !!!var_names),
  )
  coef_summarised <- dplyr::arrange(
    .data = coef_summarised,
    factor(name, levels = var_names)
  )
  coef_summarised
}
