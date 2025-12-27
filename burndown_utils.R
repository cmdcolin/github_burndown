library(tidyverse)
library(lubridate)
library(gh)
library(glue)
library(cli)

PLOT_WIDTH <- 8
PLOT_HEIGHT <- 4

ORGREPO_PATTERN <- "^[a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+$"

validate_orgrepo <- function(orgrepo) {
  if (is.null(orgrepo) || orgrepo == "") {
    return(list(valid = FALSE, error = "Repository name cannot be empty"))
  }

  if (!str_detect(orgrepo, "/")) {
    return(list(valid = FALSE, error = "Repository must be in 'org/repo' format (missing '/')"))
  }

  parts <- str_split_1(orgrepo, "/")
  if (length(parts) != 2) {
    return(list(valid = FALSE, error = "Repository must be in 'org/repo' format (too many '/')"))
  }

  org <- parts[1]
  repo <- parts[2]

  if (org == "") {
    return(list(valid = FALSE, error = "Organization/owner name cannot be empty"))
  }

  if (repo == "") {
    return(list(valid = FALSE, error = "Repository name cannot be empty"))
  }

  if (!str_detect(orgrepo, ORGREPO_PATTERN)) {
    return(list(
      valid = FALSE,
      error = "Invalid characters in org/repo (allowed: letters, numbers, hyphens, underscores, dots)"
    ))
  }

  list(valid = TRUE, error = NULL)
}

get_cache_path <- function(orgrepo) {
  safe_name <- str_replace(orgrepo, "/", "_")
  fs::path("cache", glue("{safe_name}.rds"))
}

empty_issues_tibble <- function() {
  tibble(
    start = as.POSIXct(character()),
    end = character(),
    title = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    months = numeric(),
    issue_number = integer()
  )
}

wait_for_rate_limit <- function(token = NULL) {
  result <- if (!is.null(token) && token != "") {
    gh_rate_limit(.token = token)
  } else {
    gh_rate_limit()
  }

  if (result$remaining == 0) {
    wait_time <- as.numeric(difftime(result$reset, Sys.time(), units = "secs"))
    if (wait_time > 0) {
      cli_alert_warning("Rate limited. Waiting {round(wait_time)} seconds...")
      Sys.sleep(wait_time + 1)
    }
  }
}

fetch_all_issues <- function(orgrepo, token = NULL) {
  all_issues <- list()
  page <- 1

  cli_progress_bar("Fetching issues", type = "tasks")

 repeat {
    wait_for_rate_limit(token)

    endpoint <- glue("GET /repos/{orgrepo}/issues?state=all")
    data <- if (!is.null(token) && token != "") {
      gh(endpoint, page = page, .token = token)
    } else {
      gh(endpoint, page = page)
    }

    if (length(data) == 0) break

    all_issues <- c(all_issues, data)
    cli_progress_update(set = length(all_issues), status = glue("page {page}"))
    page <- page + 1
  }

  cli_progress_done()
  cli_alert_success("Fetched {length(all_issues)} issues")
  all_issues
}

parse_issues_to_tibble <- function(issues) {
  if (length(issues) == 0) {
    return(empty_issues_tibble())
  }

  tibble(
    title = map_chr(issues, "title"),
    issue_number = map_int(issues, "number"),
    created_at = map_chr(issues, "created_at"),
    closed_at = map_chr(issues, "closed_at", .default = NA_character_)
  ) |>
    mutate(
      start = ymd_hms(created_at),
      end = closed_at,
      start_date = as.Date(start),
      end_date = as.Date(end),
      months = as.numeric(end_date - start_date) / 30
    ) |>
    select(start, end, title, start_date, end_date, months, issue_number)
}

load_or_fetch_issues <- function(orgrepo, token = NULL, force_refresh = FALSE) {
  validation <- validate_orgrepo(orgrepo)
  if (!validation$valid) {
    cli_abort(validation$error)
  }

  cache_path <- get_cache_path(orgrepo)

  if (!force_refresh && fs::file_exists(cache_path)) {
    cli_alert_info("Loading cached data from {cache_path}")
    return(readRDS(cache_path))
  }

  fs::dir_create("cache")
  issues <- fetch_all_issues(orgrepo, token)
  df <- parse_issues_to_tibble(issues)
  saveRDS(df, cache_path)
  cli_alert_success("Cached data to {cache_path}")
  df
}

get_color_scale <- function(use_viridis = FALSE) {
  if (use_viridis) {
    scale_color_viridis_c()
  } else {
    scale_color_gradientn(colours = rainbow(5))
  }
}

filter_closed_issues <- function(df) {
  filter(df, !is.na(end_date))
}

create_months_vs_timespan_plot <- function(df, use_viridis = FALSE) {
  df |>
    filter_closed_issues() |>
    ggplot(aes(x = months, ymin = end_date, ymax = start_date,
               color = issue_number, text = title)) +
    geom_linerange(linewidth = 1) +
    get_color_scale(use_viridis) +
    labs(
      x = "Months taken",
      y = "Time span that issue covered",
      title = "Months to complete vs Time span that issue covered"
    ) +
    theme_classic()
}

create_issues_timeline_plot <- function(df, use_viridis = FALSE) {
  df |>
    filter_closed_issues() |>
    ggplot(aes(x = issue_number, ymin = end_date, ymax = start_date,
               color = months, text = title)) +
    geom_linerange(linewidth = 1) +
    get_color_scale(use_viridis) +
    labs(
      x = "Issue number",
      y = "Time taken to close issue",
      title = "Issues completed over time"
    ) +
    theme_classic()
}

create_completion_time_plot <- function(df, use_viridis = FALSE) {
  df |>
    filter_closed_issues() |>
    ggplot(aes(x = end_date, y = months, color = issue_number, text = title)) +
    geom_point(size = 1) +
    get_color_scale(use_viridis) +
    labs(
      x = "Close date",
      y = "Months taken to finish",
      title = "Issue number vs Time taken to complete"
    ) +
    theme_classic()
}

create_close_date_plot <- function(df, use_viridis = FALSE) {
  df |>
    filter_closed_issues() |>
    ggplot(aes(x = end_date, y = issue_number, color = months, text = title)) +
    geom_point(size = 1) +
    get_color_scale(use_viridis) +
    labs(
      x = "Close date",
      y = "Issue number",
      title = "Issue close date vs Issue number"
    ) +
    theme_classic()
}

create_completion_histogram_plot <- function(df, use_viridis = FALSE) {
  df |>
    filter_closed_issues() |>
    ggplot(aes(x = months)) +
    geom_histogram(bins = 30) +
    scale_y_sqrt() +
    labs(
      x = "Months taken to complete",
      y = "sqrt(#issues in bin)",
      title = "Binned distribution of time taken to complete issues"
    ) +
    theme_classic()
}

create_open_issues_plot <- function(df, use_viridis = FALSE, max_points = 1000) {
  df_prepared <- df |>
    mutate(
      start = as.POSIXct(start),
      end = as.POSIXct(if_else(is.na(end), format(Sys.time()), end))
    )

  total_days <- as.numeric(difftime(Sys.time(), min(df_prepared$start), units = "days"))

  interval <- if (total_days > max_points) {
    ceiling(total_days / max_points) * 86400
  } else {
    86400
  }

  days <- seq(min(df_prepared$start), Sys.time(), by = interval) |> as.Date()
  total <- rowSums(outer(days, df_prepared$start, ">=") &
                   outer(days, df_prepared$end, "<="))

  tibble(days, total) |>
    ggplot(aes(x = days, y = total)) +
    geom_line() +
    geom_point(size = 0.5) +
    labs(
      x = "Time",
      y = "Issues open",
      title = "Total issues open at any given time"
    ) +
    theme_classic()
}

create_closed_per_day_plot <- function(df, use_viridis = FALSE) {
  df |>
    mutate(day = floor_date(end_date, unit = "day")) |>
    filter(!is.na(day)) |>
    count(day, name = "count") |>
    ggplot(aes(x = count)) +
    geom_bar() +
    labs(title = "Issues closed per day") +
    theme_classic()
}

get_longest_open_issues <- function(df, n = 100) {
  df |>
    arrange(desc(months)) |>
    select(issue_number, title, months) |>
    slice_head(n = n)
}

save_plot <- function(plot, name, output_dir, width = PLOT_WIDTH,
                      height = PLOT_HEIGHT, formats = c("pdf", "png")) {
  for (fmt in formats) {
    filename <- file.path(output_dir, paste0(name, ".", fmt))
    ggsave(filename, plot = plot, device = fmt,
           width = width, height = height)
    cli_alert_success("Saved {filename}")
  }
}

get_plot_functions <- function() {
  list(
    months_vs_timespan = create_months_vs_timespan_plot,
    issues_timeline = create_issues_timeline_plot,
    completion_time = create_completion_time_plot,
    close_date = create_close_date_plot,
    completion_histogram = create_completion_histogram_plot,
    open_issues = create_open_issues_plot,
    closed_per_day = create_closed_per_day_plot
  )
}

generate_all_plots <- function(df, output_dir = "output", use_viridis = FALSE,
                               width = PLOT_WIDTH, height = PLOT_HEIGHT) {
  fs::dir_create(output_dir)

  plot_fns <- get_plot_functions()
  plot_names <- names(plot_fns)

  plots <- list()
  for (name in plot_names) {
    cli_alert_info("Creating {name}...")
    plots[[name]] <- plot_fns[[name]](df, use_viridis = use_viridis)
    save_plot(plots[[name]], name, output_dir, width = width, height = height)
  }

  plots
}
