library(shiny)
library(plotly)

source("burndown_utils.R")

readRenviron(".Renviron")

make_plot_tab <- function(id, note = "Note: hover over top of bar for tooltip") {
  tabPanel(
    str_to_title(str_replace_all(id, "_", " ")),
    div(
      if (!is.null(note)) p(note),
      downloadButton(paste0("pdf_", id), label = "Export PDF"),
      downloadButton(paste0("png_", id), label = "Export PNG"),
      plotlyOutput(paste0("plot_", id))
    )
  )
}

ui <- fluidPage(
  titlePanel("Issue burndown charts"),
  a(href = "https://github.com/cmdcolin/shinyburndown", "Source code repo"),
  br(),
  textInput(
    "orgrepo",
    "Enter org/repo e.g. GMOD/bam-js. Note that repos with many issues will take awhile to complete and may exhaust API call limits",
    "GMOD/bam-js"
  ),
  textInput(
    "token",
    "Optional custom token. Please use if submitting large calls jobs. See https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token turn on \"repos\" access",
    ""
  ),
  submitButton(text = "Submit"),
  fluidRow(
    column(
      8,
      tabsetPanel(
        type = "tabs",
        make_plot_tab("months_vs_timespan"),
        make_plot_tab("issues_over_time"),
        make_plot_tab("completion_time", note = NULL),
        make_plot_tab("close_date_vs_number", note = NULL),
        make_plot_tab("completion_histogram", note = NULL),
        make_plot_tab("open_issues_timeline", note = NULL),
        make_plot_tab("closed_per_day", note = NULL),
        tabPanel("Longest open issues", dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  fetch_data <- reactive({
    if (input$orgrepo == "") {
      tibble(
        start = as.POSIXct(character()),
        end = character(),
        title = character(),
        start_date = as.Date(character()),
        end_date = as.Date(character()),
        months = numeric(),
        issue_number = integer()
      )
    } else {
      token <- if (input$token != "") input$token else NULL
      load_or_fetch_issues(input$orgrepo, token)
    }
  })

  plots <- reactiveValues()

  plot_configs <- list(
    months_vs_timespan = create_plot1,
    issues_over_time = create_plot2,
    completion_time = create_plot3,
    close_date_vs_number = create_plot4,
    completion_histogram = create_plot5,
    open_issues_timeline = create_plot6,
    closed_per_day = create_plot7
  )

  iwalk(plot_configs, \(create_fn, name) {
    output[[paste0("plot_", name)]] <- renderPlotly({
      df <- fetch_data()
      plots[[name]] <- create_fn(df)
      ggplotly(plots[[name]], width = 1000, height = 600)
    })

    output[[paste0("pdf_", name)]] <- downloadHandler(
      filename = \() glue("{name}.pdf"),
      content = \(file) ggsave(file, plot = plots[[name]], device = "pdf",
                               width = PLOT_WIDTH, height = PLOT_HEIGHT, scale = PLOT_SCALE)
    )

    output[[paste0("png_", name)]] <- downloadHandler(
      filename = \() glue("{name}.png"),
      content = \(file) ggsave(file, plot = plots[[name]], device = "png",
                               width = PLOT_WIDTH, height = PLOT_HEIGHT, scale = PLOT_SCALE)
    )
  })

  output$table <- renderDataTable({
    fetch_data() |>
      get_longest_open_issues(100) |>
      mutate(months = round(months, 2))
  })
}

shinyApp(ui = ui, server = server)
