plan <- drake::drake_plan(

  # Import the data
  raw_data = readr::read_csv(file_in("data/nt_player_participations.csv")),

  # Prep data for model (create group and scale variables)
  data = data_preparation(raw_data = raw_data),

  # Set up the formulas for the models to run
  model_formulas = c(
    formula(selected ~ 0 + gender + gender:c_log2_points + gender:birth_quarter + (1|gender:category)),
    formula(selected ~ 0 + gender + gender:c_log2_points + gender:birth_quarter + gender:c_log2_points:birth_quarter + (1|gender:category)),
    formula(selected ~ 0 + gender + gender:c_log2_points + gender:birth_quarter + (1 + c_log2_points||gender:category)),
    formula(selected ~ 0 + gender + gender:c_log2_points + gender:birth_quarter + (1 + birth_quarter||gender:category))
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

  draws_gender = get_draws(
    model = model_3,
    new_data = tidyr::expand(
      data = data,
      gender,
      birth_quarter = 0,
      c_log2_points = 0,
      category
    )
  ),

  draws_gender_avg = get_draws(
    model = model_3,
    re_formula = NA,
    new_data = tidyr::expand(
      data = data,
      gender,
      birth_quarter = 0,
      c_log2_points = 0,
      category = "00_00"
    )
  ),

  draws_category = get_draws(
    model = model_3,
    new_data = tidyr::expand(
      data = data,
      gender,
      birth_quarter = 0,
      c_log2_points = 0,
      category
    )
  ),

  draws_birth_quarter_avg = get_draws(
    model = model_3,
    re_formula = NA,
    new_data = tidyr::expand(
      data = data,
      gender,
      birth_quarter,
      c_log2_points = 0,
      category = "00_00"
    )
  ),

  draws_birth_quarter = get_draws(
    model = model_3,
    new_data = tidyr::expand(
      data = data,
      gender,
      birth_quarter,
      c_log2_points = 0,
      category
    )
  ),

  draws_points_avg = get_draws(
    model = model_3,
    re_formula = NA,
    new_data = tidyr::expand(
      data = data,
      tidyr::nesting(
        gender,
        c_log2_points,
        birth_quarter = 0,
        category = "00_00"
      )
    )
  ),

  draws_points = get_draws(
    model = model_3,
    new_data = tidyr::expand(
      data = data,
      tidyr::nesting(
        gender,
        c_log2_points
      ),
      birth_quarter = 0,
      category
    )
  )

  # Knit manuscript
  # report = rmarkdown::render(
  #   knitr_in("analysis/manuscript.Rmd"),
  #   output_file = file_out("analysis/manuscript.pdf"),
  #   output_dir = "analysis",
  #   quiet = TRUE
  # )
)
