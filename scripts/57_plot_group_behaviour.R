# Plot group-level behavioural results

input_file <- "results/group_behaviour_by_nback.csv"

accuracy_output_file <- "figures/group_behavioural_accuracy.png"
rt_output_file <- "figures/group_behavioural_rt.png"

stopifnot(file.exists(input_file))

dir.create(
  "figures",
  recursive = TRUE,
  showWarnings = FALSE
)

behaviour <- readr::read_csv(
  input_file,
  col_types = readr::cols(
    n_back = readr::col_double(),
    n_participants = readr::col_double(),

    mean_accuracy = readr::col_double(),
    sd_accuracy = readr::col_double(),
    se_accuracy = readr::col_double(),
    accuracy_ci_95_lower = readr::col_double(),
    accuracy_ci_95_upper = readr::col_double(),

    mean_hit_rate = readr::col_double(),
    sd_hit_rate = readr::col_double(),
    se_hit_rate = readr::col_double(),
    hit_rate_ci_95_lower = readr::col_double(),
    hit_rate_ci_95_upper = readr::col_double(),

    mean_false_alarm_rate = readr::col_double(),
    sd_false_alarm_rate = readr::col_double(),
    se_false_alarm_rate = readr::col_double(),
    false_alarm_ci_95_lower = readr::col_double(),
    false_alarm_ci_95_upper = readr::col_double(),

    mean_rt_ms = readr::col_double(),
    sd_rt_ms = readr::col_double(),
    se_rt_ms = readr::col_double(),
    rt_ci_95_lower = readr::col_double(),
    rt_ci_95_upper = readr::col_double()
  )
)

required_columns <- c(
  "n_back",
  "mean_accuracy",
  "accuracy_ci_95_lower",
  "accuracy_ci_95_upper",
  "mean_rt_ms",
  "rt_ci_95_lower",
  "rt_ci_95_upper"
)

stopifnot(all(required_columns %in% names(behaviour)))

condition_colours <- c(
  "1" = "#2C7BB6",
  "2" = "#74ADD1",
  "3" = "#AB8AC4",
  "4" = "#E0827A",
  "5" = "#D7191C"
)

behaviour <- behaviour |>
  dplyr::mutate(
    n_back_factor = factor(
      n_back,
      levels = 1:5
    )
  )

# Accuracy

accuracy_plot <- ggplot2::ggplot(
  behaviour,
  ggplot2::aes(
    x = n_back,
    y = mean_accuracy
  )
) +
  ggplot2::geom_line(
    colour = "grey40",
    linewidth = 0.8
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = accuracy_ci_95_lower,
      ymax = accuracy_ci_95_upper
    ),
    width = 0.08,
    colour = "grey40"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(colour = n_back_factor),
    size = 3
  ) +
  ggplot2::scale_colour_manual(
    values = condition_colours,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = 1:5
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1)
  ) +
  ggplot2::labs(
    x = "N-back level",
    y = "Accuracy",
    title = "Behavioural accuracy across n-back levels",
    caption = "Points show group means; error bars show 95% confidence intervals."
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  accuracy_output_file,
  plot = accuracy_plot,
  width = 7,
  height = 5,
  dpi = 300
)

# Reaction time

rt_plot <- ggplot2::ggplot(
  behaviour,
  ggplot2::aes(
    x = n_back,
    y = mean_rt_ms
  )
) +
  ggplot2::geom_line(
    colour = "grey40",
    linewidth = 0.8
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = rt_ci_95_lower,
      ymax = rt_ci_95_upper
    ),
    width = 0.08,
    colour = "grey40"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(colour = n_back_factor),
    size = 3
  ) +
  ggplot2::scale_colour_manual(
    values = condition_colours,
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = 1:5
  ) +
  ggplot2::labs(
    x = "N-back level",
    y = "Mean correct target RT (ms)",
    title = "Reaction time across n-back levels",
    caption = "Points show group means; error bars show 95% confidence intervals."
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(
  rt_output_file,
  plot = rt_plot,
  width = 7,
  height = 5,
  dpi = 300
)

cat(
  "\nGroup behavioural figures completed.\n",
  "Accuracy figure:", accuracy_output_file, "\n",
  "RT figure:", rt_output_file, "\n"
)