# Summarise behavioural results across participants

input_file <- "results/group_behavioural_summary.csv"

participant_output_file <- "results/participant_behaviour_by_nback.csv"
group_output_file <- "results/group_behaviour_by_nback.csv"

stopifnot(file.exists(input_file))

behaviour <- readr::read_csv(
  input_file,
  col_types = readr::cols(
    participant = readr::col_character(),
    session = readr::col_character(),
    n_back = readr::col_double(),
    n_trials = readr::col_double(),
    n_targets = readr::col_double(),
    n_nontargets = readr::col_double(),
    accuracy = readr::col_double(),
    hit_rate = readr::col_double(),
    false_alarm_rate = readr::col_double(),
    n_correct_target_rt = readr::col_double(),
    mean_correct_target_rt_ms = readr::col_double()
  )
)

required_columns <- c(
  "participant",
  "session",
  "n_back",
  "accuracy",
  "hit_rate",
  "false_alarm_rate",
  "mean_correct_target_rt_ms"
)

stopifnot(all(required_columns %in% names(behaviour)))

# Average sessions within each participant

participant_behaviour <- behaviour |>
  dplyr::group_by(participant, n_back) |>
  dplyr::summarise(
    n_sessions = dplyr::n_distinct(session),
    accuracy = mean(accuracy, na.rm = TRUE),
    hit_rate = mean(hit_rate, na.rm = TRUE),
    false_alarm_rate = mean(false_alarm_rate, na.rm = TRUE),
    mean_correct_target_rt_ms = mean(
      mean_correct_target_rt_ms,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(participant, n_back)

readr::write_csv(
  participant_behaviour,
  participant_output_file
)

# Summarise across participants

group_behaviour <- participant_behaviour |>
  dplyr::group_by(n_back) |>
  dplyr::summarise(
    n_participants = dplyr::n(),

    mean_accuracy = mean(accuracy, na.rm = TRUE),
    sd_accuracy = stats::sd(accuracy, na.rm = TRUE),
    se_accuracy = sd_accuracy / sqrt(n_participants),

    mean_hit_rate = mean(hit_rate, na.rm = TRUE),
    sd_hit_rate = stats::sd(hit_rate, na.rm = TRUE),
    se_hit_rate = sd_hit_rate / sqrt(n_participants),

    mean_false_alarm_rate = mean(false_alarm_rate, na.rm = TRUE),
    sd_false_alarm_rate = stats::sd(false_alarm_rate, na.rm = TRUE),
    se_false_alarm_rate = sd_false_alarm_rate / sqrt(n_participants),

    mean_rt_ms = mean(mean_correct_target_rt_ms, na.rm = TRUE),
    sd_rt_ms = stats::sd(mean_correct_target_rt_ms, na.rm = TRUE),
    se_rt_ms = sd_rt_ms / sqrt(n_participants),

    .groups = "drop"
  ) |>
  dplyr::mutate(
    t_critical = stats::qt(
      0.975,
      df = n_participants - 1
    ),

    accuracy_ci_95_lower =
      mean_accuracy - t_critical * se_accuracy,
    accuracy_ci_95_upper =
      mean_accuracy + t_critical * se_accuracy,

    hit_rate_ci_95_lower =
      mean_hit_rate - t_critical * se_hit_rate,
    hit_rate_ci_95_upper =
      mean_hit_rate + t_critical * se_hit_rate,

    false_alarm_ci_95_lower =
      mean_false_alarm_rate - t_critical * se_false_alarm_rate,
    false_alarm_ci_95_upper =
      mean_false_alarm_rate + t_critical * se_false_alarm_rate,

    rt_ci_95_lower =
      mean_rt_ms - t_critical * se_rt_ms,
    rt_ci_95_upper =
      mean_rt_ms + t_critical * se_rt_ms
  ) |>
  dplyr::select(-t_critical) |>
  dplyr::arrange(n_back)

readr::write_csv(
  group_behaviour,
  group_output_file
)

cat(
  "\nBehavioural group summary completed.\n",
  "Participants:", dplyr::n_distinct(participant_behaviour$participant), "\n",
  "Participant-level output:", participant_output_file, "\n",
  "Group-level output:", group_output_file, "\n"
)