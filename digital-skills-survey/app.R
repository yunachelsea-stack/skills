library(shiny)
library(DT)
library(dplyr)
library(readr)

source("R/export.R")

items_all <- read_csv("data/items.csv", show_col_types = FALSE) |>
  mutate(recommended_core = as.logical(recommended_core))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Digital Skills Survey Builder",

  # Survey Builder
  tabPanel("Survey Builder",
    tags$head(tags$script(HTML("
      $(document).on('change', '.q-toggle', function() {
        Shiny.setInputValue(
          'toggle_q',
          { id: $(this).data('id'), val: $(this).is(':checked') },
          { priority: 'event' }
        );
      });
    "
    ))),
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Filters"),
        checkboxGroupInput(
          "sel_module", "Module",
          choices  = sort(unique(items_all$module)),
          selected = unique(items_all$module)
        ),
        hr(),
        uiOutput("domain_filter_ui"),
        hr(),
        tags$strong(textOutput("count_label"))
      ),
      mainPanel(width = 9,
        DTOutput("items_tbl"),
        br(),
        downloadButton("dl_xlsform", "Export XLSForm (.xlsx)")
      )
    )
  ),

  # Stub pages
  tabPanel("Landing",
    fluidPage(h2("Landing"), p("Coming soon."))
  ),
  tabPanel("Conceptual Framework",
    fluidPage(h2("Conceptual Framework"), p("Coming soon."))
  ),
  tabPanel("Sampling",
    fluidPage(h2("Sampling"), p("Coming soon."))
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Dynamic competency-domain filter (depends on selected modules)
  output$domain_filter_ui <- renderUI({
    domains <- items_all |>
      filter(module %in% input$sel_module) |>
      pull(competency_domain) |>
      unique() |> sort()
    checkboxGroupInput(
      "sel_domain", "Competency Domain",
      choices = domains, selected = domains
    )
  })

  # Per-question inclusion state; core items stay TRUE permanently
  inc <- reactiveValues()
  observe({
    for (id in items_all$id) {
      if (is.null(inc[[id]])) inc[[id]] <- TRUE
    }
  })

  observeEvent(input$toggle_q, {
    ev  <- input$toggle_q
    row <- items_all[items_all$id == ev$id, ]
    if (nrow(row) > 0 && !row$recommended_core) {
      inc[[ev$id]] <- ev$val
    }
  })

  # Items matching current filters
  filtered_df <- reactive({
    req(input$sel_domain)
    items_all |>
      filter(
        module             %in% input$sel_module,
        competency_domain  %in% input$sel_domain
      )
  })

  # Table data: add inclusion status + checkbox HTML
  tbl_data <- reactive({
    df <- filtered_df()
    df |> mutate(
      included   = vapply(id, function(i) isTRUE(inc[[i]]), logical(1)),
      check_html = mapply(function(i, core, included) {
        if (core) {
          '<input type="checkbox" checked disabled
             title="Required core item"
             style="accent-color:#e67e22;cursor:not-allowed;">'
        } else {
          sprintf(
            '<input type="checkbox" class="q-toggle" data-id="%s"%s>',
            i, if (included) " checked" else ""
          )
        }
      }, id, recommended_core, included, SIMPLIFY = TRUE)
    )
  })

  output$items_tbl <- renderDT({
    df <- tbl_data()
    display <- df |> select(
      ` `                 = check_html,
      ID                  = id,
      Module              = module,
      `Competency Domain` = competency_domain,
      Core                = recommended_core,
      Question            = question
    )
    datatable(
      display,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength = 25,
        dom        = "tip",
        columnDefs = list(
          list(orderable = FALSE, targets = 0),
          list(visible   = FALSE, targets = 4)  # Core hidden; used only for row colour
        )
      )
    ) |>
      formatStyle(
        "Core",
        target          = "row",
        backgroundColor = styleEqual(TRUE, "#fff3cd"),
        fontWeight      = styleEqual(TRUE, "600")
      )
  }, server = FALSE)

  output$count_label <- renderText({
    paste("Selected questions:", sum(tbl_data()$included))
  })

  output$dl_xlsform <- downloadHandler(
    filename = "digital_skills_survey.xlsx",
    content  = function(file) {
      export_xlsform(tbl_data() |> filter(included), file)
    }
  )
}

shinyApp(ui, server)
