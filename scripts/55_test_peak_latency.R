# Test whether peak decoding latency increases with n-back load

input_file <- "results/participant_peak_decoding_by_nback.csv"
output_file <- "results/group_peak_latency_trend_test.csv"

stopifnot(file.exists(input_file))

peak_data <- readr::read_csv(
  input_file,
  col_types = readr::cols(
    participant = readr::col_character(),
    n_back = readr::col_double(),
    n_sessions = readr::col_double(),
    mean_peak_latency_ms = readr::col_double(),
    mean_peak_accuracy = readr::col_double()
  )
)

required_columns <- c(
  "participant",
  "n_back",
  "mean_peak_latency_ms"
)

stopifnot(all(required_columns %in% names(peak_data)))

# Check data

stopifnot(
  all(is.finite(peak_data$n_back)),
  all(is.finite(peak_data$mean_peak_latency_ms))
)

# Test the linear change in peak latency with increasing n-back load

peak_model <- stats::lm(
  mean_peak_latency_ms ~ n_back + factor(participant),
  data = peak_data
)

model_summary <- summary(peak_model)

estimate <- unname(
  stats::coef(peak_model)["n_back"]
)

standard_error <- unname(
  model_summary$coefficients["n_back", "Std. Error"]
)

t_value <- unname(
  model_summary$coefficients["n_back", "t value"]
)

p_value <- unname(
  model_summary$coefficients["n_back", "Pr(>|t|)"]
)

degrees_freedom <- stats::df.residual(peak_model)

confidence_interval <- stats::confint(
  peak_model,
  "n_back",
  level = 0.95
)

trend_result <- tibble::tibble(
  n_participants = dplyr::n_distinct(peak_data$participant),
  n_observations = nrow(peak_data),
  estimate_ms_per_nback = estimate,
  standard_error = standard_error,
  ci_95_lower = confidence_interval[1, 1],
  ci_95_upper = confidence_interval[1, 2],
  t_value = t_value,
  degrees_freedom = degrees_freedom,
  p_value = p_value
)

readr::write_csv(
  trend_result,
  output_file
)

cat(
  "\nPeak latency trend test completed.\n",
  "Participants:", trend_result$n_participants, "\n",
  "Estimated shift per n-back level:",
  round(trend_result$estimate_ms_per_nback, 2), "ms\n",
  "95% CI:",
  round(trend_result$ci_95_lower, 2), "to",
  round(trend_result$ci_95_upper, 2), "ms\n",
  "p-value:",
  signif(trend_result$p_value, 3), "\n",
  "Output:", output_file, "\n"
)