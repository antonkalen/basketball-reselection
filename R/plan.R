plan <- drake::drake_plan(

  # Import the data
  raw_data = readr::read_csv(here::here("data", "nt_player_participations.csv")),

  # Prep data for model (create group and scale variables)
  data = data_preparation(raw_data = raw_data),

  # Set up the formulas for the models to run
  model_formulas = c(
    formula(selected ~ 0 + gender + gender:std_log_points + gender:birth_quarter + (1|gender:category)),
    formula(selected ~ 0 + gender + gender:std_log_points + gender:birth_quarter + gender:std_log_points:birth_quarter + (1|gender:category)),
    formula(selected ~ 0 + gender + gender:std_log_points + gender:birth_quarter + (1 + std_log_points||gender:category)),
    formula(selected ~ 0 + gender + gender:std_log_points + gender:birth_quarter + (1 + birth_quarter||gender:category))
  ),

  # Specify the priors to use in the models
  model_priors = list(
    prior_intercept = rstanarm::normal(location = 0, scale = 1, autoscale = FALSE),
    prior = rstanarm::normal(location = 0, scale = .75, autoscale = FALSE),
    prior_covariance = rstanarm::decov(regularization = 1, concentration = 1, shape = 1, scale = .5)
  ),

  # Fit the models
  model = target(
    fit_model(
      formula = model_formulas[[model_nr]],
      prior_intercept = model_priors$prior_intercept,
      prior = model_priors$prior,
      prior_covariance = model_priors$prior_covariance,
      data = data,
      chains = 1,
      iter = 4000
    ),
    transform = map(model_nr = c(1, 2, 3, 4))
  ),

  # Calculate loo values for each model
  loo = target(
    rstanarm::loo(model),
    transform = map(model)
  ),

  # Compare loo values
  loo_compare = target(
    rstanarm::loo_compare(loo),
    transform = combine(loo)
  ),

  # Extract coefficients from the models
  coefs = target(
    get_coefs(model, .width = .95),
    transform = map(model)
  ),

  # Merge coefficients from the models
  coef_table = target(
    purrr::reduce(list(coefs), dplyr::full_join, by = ".variable"),
    transform = combine(coefs)
  ),

  # Sample from the posterior prediction of model 3

  fitted_draws_gender_model_3 = tidybayes::add_fitted_draws(
    newdata = data.frame(
      gender = c("Men", "Women"),
      birth_quarter = 0,
      std_log_points = 0,
      category = "newgroup"
    ),
    model = model_3,
    scale = "response"
  ),

  fitted_draws_model_3 = tidybayes::add_fitted_draws(
      newdata = modelr::data_grid(
        data = data,
        gender,
        birth_quarter = birth_quarter -2.5,
        std_log_points = seq(
          min(data$std_log_points),
          max(data$std_log_points),
          by = .1
        ),
        category
      ),
      model = model_3,
      scale = "response"
    )

  # Knit manuscript
  # report = rmarkdown::render(
  #   knitr_in("analysis/manuscript.Rmd"),
  #   output_file = file_out("analysis/manuscript.html"),
  #   output_dir = "analysis",
  #   quiet = TRUE
  # )
)
