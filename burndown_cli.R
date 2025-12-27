#!/usr/bin/env Rscript

library(ggplot2)
library(plyr)
library(lubridate)
library(gh)
library(dplyr)

GLOBAL_SIZE <- 18
PLOT_WIDTH <- 8
PLOT_HEIGHT <- 4
PLOT_SCALE <- 2

get_cache_path <- function(orgrepo) {
  safe_name <- gsub("/", "_", orgrepo)
  file.path("cache", paste0(safe_name, ".rds"))
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
      message(sprintf("Rate limited. Waiting %.0f seconds...", wait_time))
      Sys.sleep(wait_time + 1)
    }
  }
}

fetch_all_issues <- function(orgrepo, token = NULL) {
  res <- list()
  page <- 1

  repeat {
    wait_for_rate_limit(token)

    endpoint <- paste0("GET /repos/", orgrepo, "/issues?state=all")
    data <- if (!is.null(token) && token != "") {
      gh(endpoint, page = page, .token = token)
    } else {
      gh(endpoint, page = page)
    }

    if (length(data) == 0) break

    res <- c(res, data)
    message(sprintf("Fetched page %d (%d issues so far)", page, length(res)))
    page <- page + 1
  }

  message(sprintf("Done fetching %d issues", length(res)))
  res
}

parse_issues_to_df <- function(issues) {
  if (length(issues) == 0) {
    return(data.frame(
      start = as.POSIXct(character()),
      end = character(),
      title = character(),
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      months = numeric(),
      issue_number = integer(),
      stringsAsFactors = FALSE
    ))
  }

  title <- sapply(issues, function(row) row[["title"]])
  closed_at <- sapply(issues, function(row) row[["closed_at"]])
  created_at <- sapply(issues, function(row) row[["created_at"]])
  issue_number <- sapply(issues, function(row) row[["number"]])

  start <- ymd_hms(created_at)
  end <- sapply(closed_at, function(x) if (is.null(x)) NA_character_ else x)
  start_date <- as.Date(start)
  end_date <- as.Date(end)
  months <- as.numeric(end_date - start_date) / 30

  data.frame(start, end, title, start_date, end_date, months, issue_number)
}

load_or_fetch_issues <- function(orgrepo, token = NULL, force_refresh = FALSE) {
  cache_path <- get_cache_path(orgrepo)

  if (!force_refresh && file.exists(cache_path)) {
    message(sprintf("Loading cached data from %s", cache_path))
    return(readRDS(cache_path))
  }

  dir.create("cache", showWarnings = FALSE)
  issues <- fetch_all_issues(orgrepo, token)
  df <- parse_issues_to_df(issues)
  saveRDS(df, cache_path)
  message(sprintf("Cached data to %s", cache_path))
  df
}

create_plot1 <- function(df) {
  ggplot(df) +
    geom_linerange(aes(
      ymin = end_date, ymax = start_date,
      x = months, color = issue_number, size = I(1)
    )) +
    scale_color_gradientn(colours = rainbow(5)) +
    xlab("Months taken") +
    ylab("Time span that issue covered") +
    ggtitle("Months to complete vs Time span that issue covered") +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot2 <- function(df) {
  ggplot(df) +
    geom_linerange(aes(
      ymin = end_date, ymax = start_date,
      x = issue_number, color = months, size = I(1)
    )) +
    scale_color_gradientn(colours = rainbow(5)) +
    xlab("Issue number") +
    ylab("Time taken to close issue") +
    ggtitle("Issues completed over time") +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot3 <- function(df) {
  ggplot(df) +
    geom_point(aes(
      y = months, x = end_date, color = issue_number, size = I(1)
    )) +
    scale_color_gradientn(colours = rainbow(5)) +
    xlab("Close date") +
    ylab("Months taken to finish") +
    ggtitle("Issue number vs Time taken to complete") +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot4 <- function(df) {
  ggplot(df) +
    geom_point(aes(
      y = issue_number, x = end_date, color = months, size = I(1)
    )) +
    scale_color_gradientn(colours = rainbow(5)) +
    xlab("Close date") +
    ylab("Issue number") +
    ggtitle("Issue close date vs Issue number") +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot5 <- function(df) {
  ggplot(df, aes(x = months)) +
    geom_histogram(bins = 30) +
    scale_y_log10() +
    xlab("Months taken to complete") +
    ylab("log10(#issues in bin)") +
    ggtitle("Binned distribution of time taken to complete issues") +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot6 <- function(df) {
  df2 <- df
  df2$start <- as.POSIXct(df2$start)
  df2$end <- as.POSIXct(df2$end)
  df2$end[is.na(df2$end)] <- Sys.time()

  days <- as.Date(seq(min(df2$start), Sys.time(), by = 86400))
  total <- rowSums(outer(days, df2$start, ">") & outer(days, df2$end, "<"))
  timeline_df <- data.frame(days, total)

  ggplot(timeline_df, aes(x = days, y = total)) +
    geom_line() +
    geom_point(size = 0.5) +
    xlab("Time") +
    ylab("Issues open") +
    ggtitle("Total issues open at any given time") +
    theme_classic(base_size = GLOBAL_SIZE)
}

create_plot7 <- function(df) {
  df2 <- df
  df2$Day <- as.character(round_date(df2$end_date, unit = "day"))
  stats <- ddply(df2, .(Day), summarise, Count = length(Day)) %>%
    filter(!is.na(Day))

  ggplot(stats, aes(x = Count)) +
    geom_bar() +
    ggtitle("Issues closed per day") +
    theme_classic(base_size = GLOBAL_SIZE)
}

save_plot <- function(plot, name, output_dir, formats = c("pdf", "png")) {
  for (fmt in formats) {
    path <- file.path(output_dir, paste0(name, ".", fmt))
    ggsave(path, plot = plot, device = fmt,
           width = PLOT_WIDTH, height = PLOT_HEIGHT, scale = PLOT_SCALE)
    message(sprintf("Saved %s", path))
  }
}

get_longest_open_issues <- function(df, n = 100) {
  df[order(-df$months), c("issue_number", "title", "months")] %>%
    head(n)
}

generate_all_plots <- function(df, output_dir = "output") {
  dir.create(output_dir, showWarnings = FALSE)

  plots <- list(
    plot1_months_vs_timespan = create_plot1(df),
    plot2_issues_over_time = create_plot2(df),
    plot3_completion_time = create_plot3(df),
    plot4_close_date_vs_number = create_plot4(df),
    plot5_completion_histogram = create_plot5(df),
    plot6_open_issues_timeline = create_plot6(df),
    plot7_closed_per_day = create_plot7(df)
  )

  for (name in names(plots)) {
    save_plot(plots[[name]], name, output_dir)
  }

  plots
}

print_summary <- function(df) {
  message("\n=== Issue Summary ===")
  message(sprintf("Total issues: %d", nrow(df)))
  message(sprintf("Open issues: %d", sum(is.na(df$end_date))))
  message(sprintf("Closed issues: %d", sum(!is.na(df$end_date))))

  if (any(!is.na(df$months))) {
    message(sprintf("Median time to close: %.1f months",
                    median(df$months, na.rm = TRUE)))
    message(sprintf("Mean time to close: %.1f months",
                    mean(df$months, na.rm = TRUE)))
  }

  message("\n=== Top 10 Longest Open Issues ===")
  top <- get_longest_open_issues(df, 10)
  for (i in seq_len(nrow(top))) {
    row <- top[i, ]
    message(sprintf("#%d: %.1f months - %s",
                    row$issue_number, row$months, substr(row$title, 1, 60)))
  }
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 1) {
    message("Usage: Rscript burndown_cli.R <org/repo> [--refresh] [--output-dir=DIR]")
    message("Example: Rscript burndown_cli.R GMOD/bam-js")
    message("\nOptions:")
    message("  --refresh      Force refresh of cached data")
    message("  --output-dir=  Directory for output plots (default: output)")
    message("  --no-plots     Skip generating plots, just show summary")
    quit(status = 1)
  }

  orgrepo <- args[1]
  force_refresh <- "--refresh" %in% args
  no_plots <- "--no-plots" %in% args

  output_dir <- "output"
  for (arg in args) {
    if (grepl("^--output-dir=", arg)) {
      output_dir <- sub("^--output-dir=", "", arg)
    }
  }

  token <- Sys.getenv("GITHUB_TOKEN")
  if (token == "") token <- NULL

  df <- load_or_fetch_issues(orgrepo, token, force_refresh)
  print_summary(df)

  if (!no_plots) {
    message("\nGenerating plots...")
    generate_all_plots(df, output_dir)
    message(sprintf("\nPlots saved to %s/", output_dir))
  }
}

if (!interactive()) {
  main()
}
