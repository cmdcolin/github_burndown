library(tidyverse)
library(lubridate)
library(gh)
library(glue)

GLOBAL_SIZE <- 18
PLOT_WIDTH <- 8
PLOT_HEIGHT <- 4
PLOT_SCALE <- 2

get_cache_path <- function(orgrepo) {
  safe_name <- str_replace(orgrepo, "/", "_")
  fs::path("cache", glue("{safe_name}.rds"))
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
      message(glue("Rate limited. Waiting {round(wait_time)} seconds..."))
      Sys.sleep(wait_time + 1)
    }
  }
}

fetch_all_issues <- function(orgrepo, token = NULL) {
  all_issues <- list()
  page <- 1

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
    message(glue("Fetched page {page} ({length(all_issues)} issues so far)"))
    page <- page + 1
  }

  message(glue("Done fetching {length(all_issues)} issues"))
  all_issues
}

parse_issues_to_tibble <- function(issues) {
  if (length(issues) == 0) {
    return(tibble(
      start = as.POSIXct(character()),
      end = character(),
      title = character(),
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      months = numeric(),
      issue_number = integer()
    ))
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
  cache_path <- get_cache_path(orgrepo)

  if (!force_refresh && fs::file_exists(cache_path)) {
    message(glue("Loading cached data from {cache_path}"))
    return(readRDS(cache_path))
  }

  fs::dir_create("cache")
  issues <- fetch_all_issues(orgrepo, token)
  df <- parse_issues_to_tibble(issues)
  saveRDS(df, cache_path)
  message(glue("Cached data to {cache_path}"))
  df
}

create_plot1 <- function(df) {
  ggplot(df, aes(x = months, ymin = end_date, ymax = start_date,
                 color = issue_number, text = title)) +
    geom_linerange(linewidth = 1) +
    scale_color_gradientn(colours = rainbow(5)) +
    labs(
      x = "Months taken",
      y = "Time span that issue covered",
      title = "Months to complete vs Time span that issue covered"
    ) +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot2 <- function(df) {
  ggplot(df, aes(x = issue_number, ymin = end_date, ymax = start_date,
                 color = months, text = title)) +
    geom_linerange(linewidth = 1) +
    scale_color_gradientn(colours = rainbow(5)) +
    labs(
      x = "Issue number",
      y = "Time taken to close issue",
      title = "Issues completed over time"
    ) +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot3 <- function(df) {
  ggplot(df, aes(x = end_date, y = months, color = issue_number, text = title)) +
    geom_point(size = 1) +
    scale_color_gradientn(colours = rainbow(5)) +
    labs(
      x = "Close date",
      y = "Months taken to finish",
      title = "Issue number vs Time taken to complete"
    ) +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot4 <- function(df) {
  ggplot(df, aes(x = end_date, y = issue_number, color = months, text = title)) +
    geom_point(size = 1) +
    scale_color_gradientn(colours = rainbow(5)) +
    labs(
      x = "Close date",
      y = "Issue number",
      title = "Issue close date vs Issue number"
    ) +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot5 <- function(df) {
  ggplot(df, aes(x = months)) +
    geom_histogram(bins = 30) +
    scale_y_log10() +
    labs(
      x = "Months taken to complete",
      y = "log10(#issues in bin)",
      title = "Binned distribution of time taken to complete issues"
    ) +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot6 <- function(df) {
  df_prepared <- df |>
    mutate(
      start = as.POSIXct(start),
      end = as.POSIXct(if_else(is.na(end), format(Sys.time()), end))
    )

  days <- seq(min(df_prepared$start), Sys.time(), by = 86400) |> as.Date()
  total <- rowSums(outer(days, df_prepared$start, ">") &
                   outer(days, df_prepared$end, "<"))

  tibble(days, total) |>
    ggplot(aes(x = days, y = total)) +
    geom_line() +
    geom_point(size = 0.5) +
    labs(
      x = "Time",
      y = "Issues open",
      title = "Total issues open at any given time"
    ) +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot7 <- function(df) {
  df |>
    mutate(day = floor_date(end_date, unit = "day")) |>
    filter(!is.na(day)) |>
    count(day, name = "count") |>
    ggplot(aes(x = count)) +
    geom_bar() +
    labs(title = "Issues closed per day") +
    theme_classic(base_size = GLOBAL_SIZE)
}

get_longest_open_issues <- function(df, n = 100) {
  df |>
    arrange(desc(months)) |>
    select(issue_number, title, months) |>
    slice_head(n = n)
}

save_plot <- function(plot, name, output_dir, formats = c("pdf", "png")) {
  walk(formats, \(fmt) {
    path <- fs::path(output_dir, glue("{name}.{fmt}"))
    ggsave(path, plot = plot, device = fmt,
           width = PLOT_WIDTH, height = PLOT_HEIGHT, scale = PLOT_SCALE)
    message(glue("Saved {path}"))
  })
}

generate_all_plots <- function(df, output_dir = "output") {
  fs::dir_create(output_dir)

  plots <- list(
    plot1_months_vs_timespan = create_plot1(df),
    plot2_issues_over_time = create_plot2(df),
    plot3_completion_time = create_plot3(df),
    plot4_close_date_vs_number = create_plot4(df),
    plot5_completion_histogram = create_plot5(df),
    plot6_open_issues_timeline = create_plot6(df),
    plot7_closed_per_day = create_plot7(df)
  )

  iwalk(plots, \(plot, name) save_plot(plot, name, output_dir))

  plots
}
