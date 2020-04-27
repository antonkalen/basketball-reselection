plan <- drake::drake_plan(

  # Model specifications ----------------------------------------------------

  # Model formulas
  model_formulas =  c(
    formula(selected ~ 0 + gender + gender:scaled_log2_points + gender:birth_quarter + (1|gender:category)),
    formula(selected ~ 0 + gender + gender:scaled_log2_points + gender:birth_quarter + gender:scaled_log2_points:birth_quarter + (1|gender:category)),
    formula(selected ~ 0 + gender + gender:scaled_log2_points + gender:birth_quarter + (1 + scaled_log2_points||gender:category)),
    formula(selected ~ 0 + gender + gender:scaled_log2_points + gender:birth_quarter + (1 + birth_quarter||gender:category))
  ),

  # Model priors
  model_priors = list(
    prior_intercept = rstanarm::normal(location = 0, scale = 1, autoscale = FALSE),
    prior = rstanarm::normal(location = 0, scale = .75, autoscale = FALSE),
    prior_covariance = rstanarm::decov(regularization = 1, concentration = 1, shape = 1, scale = .5)
  ),

  # Chains and iterations
  chains = 1,
  iterations = 10000,

  # Set compatibility interval width
  ci_width = .95,


  # Output specifications ---------------------------------------------------


  # Parameter names

  var_names = c(
    `genderMen` = "Men: Intercept",
    `genderWomen` = "Women: Intercept",
    `genderMen:birth_quarter` = "Men: Birth Quarter",
    `genderWomen:birth_quarter` = "Women: Birth Quarter",
    `genderMen:scaled_log2_points` = "Men: Ranking Points",
    `genderWomen:scaled_log2_points` = "Women: Ranking Points",
    `genderMen:scaled_log2_points:birth_quarter` = "Men: Birth Quarter × Ranking Points",
    `genderWomen:scaled_log2_points:birth_quarter` = "Women: Birth Quarter × Ranking Points",
    `Sigma[gender:category:(Intercept),(Intercept)]` = "Category",
    `Sigma[gender:category:scaled_log2_points,scaled_log2_points]` = "Category × Ranking Points",
    `Sigma[gender:category:birth_quarter,birth_quarter]` = "Category × Birth Quarter"
  ),


  # Plot theme
  plot_theme = ggplot2::theme_void(
      base_size = 10,
      base_family = "Helvetica"
    ) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = c(0, 0),
        legend.justification = c("left", "bottom"),
        legend.text = ggplot2::element_text(
          size = ggplot2::rel(.7),
          color = "gray20"
        ),
        strip.placement = "outside",
        panel.border = ggplot2::element_rect(
          fill = NA,
          size = .5,
          color = "gray40"
        ),
        axis.ticks = ggplot2::element_line(
          size = .3,
          color = "gray40"
        ),
        axis.ticks.length = ggplot2::unit(.5, "mm"),
        axis.text = ggplot2::element_text(
          size = ggplot2::rel(.7),
          color = "gray20",
          margin = ggplot2::margin(.5, .5, .5, .5, "mm")
        ),
      ),

  # Data preparation --------------------------------------------------------

  # Import the data
  raw_data = readr::read_csv(drake::file_in("data/nt_player_participations.csv")),

  # Prep data for model (create group and scale variables)
  data = data_preparation(raw_data = raw_data),


  # Model fitting -----------------------------------------------------------

  model = target(
    rstanarm::stan_glmer(
      formula = model_formulas[[model_nr]],
      family = binomial(link = "logit"),
      prior_intercept = model_priors$prior_intercept,
      prior = model_priors$prior,
      prior_covariance = model_priors$prior_covariance,
      data = data,
      chains = chains,
      iter = iterations
    ),
    transform = map(model_nr = c(1, 2, 3, 4))
  ),


# Model coefficients and loo ----------------------------------------------

  # Get the coefficient draws
  coefs = target(
      get_coefs(model),
      transform = map(model),
      format = "fst_tbl"
    ),

  # Get the loo for the models
  loo = target(
    rstanarm::loo(model),
    transform = map(model)
  ),

  # Compare loo values
  loo_compare = target(
    rstanarm::loo_compare(loo),
    transform = combine(loo)
  ),

  # Merge coefficients from the models
  coef_table = target(
    dplyr::bind_rows(coefs, .id = "model"),
    transform = combine(coefs),
    format = "fst_tbl"
  ),

  # Posterior draws and predictions -----------------------------------------

  # Get posterior prediction for average birth quarter and ranking points
  draws_gender = target(
    get_draws(
      model = model_3,
      new_data = tidyr::expand(
        data = data,
        gender,
        birth_quarter = 0,
        scaled_log2_points = 0,
        category
      )
    ),
    format = "fst_tbl"
  ),

  # Get posterior prediction for median ranking points
  draws_birth_quarter = target(
    get_draws(
      model = model_3,
      new_data = tidyr::expand(
        data = data,
        gender,
        birth_quarter,
        scaled_log2_points = 0,
        category
      )
    ),
    format = "fst_tbl"
  ),

  # Get posterior prediction for middle birth quarter
  draws_points = target(
    get_draws(
      model = model_3,
      new_data = tidyr::expand(
        data = data,
        tidyr::nesting(
          gender,
          scaled_log2_points
        ),
        birth_quarter = 0,
        category
      )
    ),
    format = "fst_tbl"
  ),

  # Create results ----------------------------------------------------------

  # Tables
  table_1 = create_table_1(data = data),

  table_2 = create_table_2(
    coefs = coefs_model_3,
    .width = ci_width,
    var_names = var_names
  ),

  table_3 = create_table_3(
    draws = draws_gender,
    .width = ci_width
  ),

  table_4 = create_table_4(
    draws = draws_points,
    .width = ci_width
  ),

  # Figures

  figure_1 = create_figure_1(
    data = data,
    draws = draws_gender,
    .width = ci_width,
    theme = plot_theme
  ),

  figure_2 = create_figure_2(
    draws = draws_birth_quarter,
    .width = ci_width,
    theme = plot_theme
  ),

  figure_3 = create_figure_3(
    draws = draws_points,
    .width = ci_width,
    theme = plot_theme
  ),

  # Text results
  birth_quarter_results = create_quarter_results(
    draws = draws_birth_quarter,
    .width = ci_width
  ),

  # Supplementary material
  supplementary_table = create_supplementary_table_1(
    coefs = coef_table,
    loos = loo_compare,
    .width = ci_width,
    var_names = var_names
  ),

  # Write outputs -----------------------------------------------------------

  tables_out = target(
    readr::write_csv(x = table, path = path, na = ""),
    transform = map(
      table = c(
        table_1,
        table_2,
        table_3,
        table_4,
        birth_quarter_results,
        supplementary_table
      ),
      path = c(
        "output/table_1.csv",
        "output/table_2.csv",
        "output/table_3.csv",
        "output/table_4.csv",
        "output/birth_quarter_results.csv",
        "output/supplementary_table.csv"
      )
    )
  ),
  figures_out = target(
    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = cairo_ps,
      path = "output/",
      width = 160,
      height = 60,
      units = "mm",
      fallback_resolution = 1200
    ),
    transform = map(
      plot = c(figure_1, figure_2, figure_3),
      filename = c("figure_1.eps", "figure_2.eps", "figure_3.eps")
    )
  ),

  # Manuscript
  manuscript = rmarkdown::render(
    knitr_in("manuscript/manuscript.Rmd"),
    output_file = file_out("manuscript/manuscript.pdf"),
    output_dir = "manuscript",
    quiet = TRUE
  ),

  # Supplementary material
  suppl_material = rmarkdown::render(
    knitr_in("manuscript/supplementary-material.Rmd"),
    output_file = file_out("manuscript/supplementary-material.pdf"),
    output_dir = "manuscript",
    quiet = TRUE
  )
)
