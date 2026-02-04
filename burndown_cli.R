#!/usr/bin/env Rscript

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

parse_named_arg <- function(args, prefix, default) {
  matches <- args[str_starts(args, paste0("--", prefix, "="))]
  if (length(matches) > 0) {
    str_remove(matches[1], paste0("^--", prefix, "="))
  } else {
    default
  }
}

parse_args <- function(args) {
  list(
    orgrepo = args[1],
    force_refresh = "--refresh" %in% args,
    incremental = "--update" %in% args,
    no_plots = "--no-plots" %in% args,
    use_viridis = "--viridis" %in% args,
    output_dir = parse_named_arg(args, "output-dir", "output"),
    width = as.numeric(parse_named_arg(args, "width", "8")),
    height = as.numeric(parse_named_arg(args, "height", "4"))
  )
}

print_usage <- function() {
  cli_h1("GitHub Issue Burndown CLI")
  cli_text("Usage: {.file Rscript burndown_cli.R} {.arg <org/repo>} [options]")
  cli_text("")
  cli_text("Example: {.code Rscript burndown_cli.R GMOD/bam-js}")
  cli_h2("Options")
  cli_bullets(c(
    "*" = "{.arg --refresh}: Force full refresh of cached data",
    "*" = "{.arg --update}: Incremental update (fetch only recently updated issues)",
    "*" = "{.arg --output-dir=DIR}: Directory for output plots (default: output)",
    "*" = "{.arg --no-plots}: Skip generating plots, just show summary",
    "*" = "{.arg --viridis}: Use colorblind-friendly viridis palette",
    "*" = "{.arg --width=N}: Plot width in inches (default: 8)",
    "*" = "{.arg --height=N}: Plot height in inches (default: 4)"
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
  if (token == "") {
    token <- NULL
    cli_alert_warning("GITHUB_TOKEN not set - using unauthenticated requests (60/hour limit)")
  } else {
    cli_alert_success("Using GITHUB_TOKEN for authenticated requests (5000/hour limit)")
  }

  df <- load_or_fetch_issues(opts$orgrepo, token, opts$force_refresh, opts$incremental)
  print_summary(df)

  if (!opts$no_plots) {
    cli_h1("Generating plots")
    generate_all_plots(df, opts$output_dir,
                       use_viridis = opts$use_viridis,
                       width = opts$width,
                       height = opts$height)
    cli_alert_success("Plots saved to {opts$output_dir}/")
  }
}

if (!interactive()) {
  main()
}
