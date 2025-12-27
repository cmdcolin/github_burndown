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
  checkboxInput(
    "use_viridis",
    "Use colorblind-friendly viridis palette",
    FALSE
  ),
  submitButton(text = "Submit"),
  fluidRow(
    column(
      8,
      tabsetPanel(
        type = "tabs",
        make_plot_tab("months_vs_timespan"),
        make_plot_tab("issues_timeline"),
        make_plot_tab("completion_time", note = NULL),
        make_plot_tab("close_date", note = NULL),
        make_plot_tab("completion_histogram", note = NULL),
        make_plot_tab("open_issues", note = NULL),
        make_plot_tab("closed_per_day", note = NULL),
        tabPanel("Longest open issues", dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  validated_orgrepo <- reactive({
    validation <- validate_orgrepo(input$orgrepo)
    validate(need(validation$valid, validation$error))
    input$orgrepo
  })

  fetch_data <- reactive({
    orgrepo <- validated_orgrepo()
    token <- if (input$token != "") input$token else NULL
    load_or_fetch_issues(orgrepo, token)
  })

  plots <- reactiveValues()

  plot_configs <- get_plot_functions()

  iwalk(plot_configs, \(create_fn, name) {
    output[[paste0("plot_", name)]] <- renderPlotly({
      df <- fetch_data()
      plots[[name]] <- create_fn(df, use_viridis = input$use_viridis)
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
