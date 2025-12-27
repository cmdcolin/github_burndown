#!/usr/bin/env Rscript

library(cli)

source("burndown_utils.R")

print_summary <- function(df) {
  cli_h1("Issue Summary")
  cli_bullets(c(
    "*" = "Total issues: {nrow(df)}",
    "*" = "Open issues: {sum(is.na(df$end_date))}",
    "*" = "Closed issues: {sum(!is.na(df$end_date))}"
  ))

  if (any(!is.na(df$months))) {
    cli_bullets(c(
      "*" = "Median time to close: {round(median(df$months, na.rm = TRUE), 1)} months",
      "*" = "Mean time to close: {round(mean(df$months, na.rm = TRUE), 1)} months"
    ))
  }

  cli_h2("Top 10 Longest Open Issues")
  top <- get_longest_open_issues(df, 10)

  pwalk(top, \(issue_number, title, months) {
    cli_alert("#{issue_number}: {round(months, 1)} months - {str_trunc(title, 60)}")
  })
}

parse_args <- function(args) {
  list(
    orgrepo = args[1],
    force_refresh = "--refresh" %in% args,
    no_plots = "--no-plots" %in% args,
    output_dir = args |>
      keep(\(x) str_starts(x, "--output-dir=")) |>
      str_remove("^--output-dir=") |>
      first() %||% "output"
  )
}

print_usage <- function() {
  cli_h1("GitHub Issue Burndown CLI")
  cli_text("Usage: {.file Rscript burndown_cli.R} {.arg <org/repo>} [options]")
  cli_text("")
  cli_text("Example: {.code Rscript burndown_cli.R GMOD/bam-js}")
  cli_h2("Options")
  cli_bullets(c(
    "*" = "{.arg --refresh}: Force refresh of cached data",
    "*" = "{.arg --output-dir=DIR}: Directory for output plots (default: output)",
    "*" = "{.arg --no-plots}: Skip generating plots, just show summary"
  ))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 1) {
    print_usage()
    quit(status = 1)
  }

  opts <- parse_args(args)

  token <- Sys.getenv("GITHUB_TOKEN")
  if (token == "") token <- NULL

  df <- load_or_fetch_issues(opts$orgrepo, token, opts$force_refresh)
  print_summary(df)

  if (!opts$no_plots) {
    cli_h1("Generating plots")
    generate_all_plots(df, opts$output_dir)
    cli_alert_success("Plots saved to {opts$output_dir}/")
  }
}

if (!interactive()) {
  main()
}
