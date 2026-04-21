library(shiny)
library(DT)
library(dplyr)
library(readr)

source("R/export.R")

items_all <- read_csv("data/items.csv", show_col_types = FALSE) |>
  mutate(
    recommended_core   = as.logical(recommended_core),
    cognitively_tested = as.logical(cognitively_tested)
  )

foundational_mods <- items_all |>
  filter(section == "1.4.1 Foundational") |>
  pull(module) |> unique()

device_mods <- items_all |>
  filter(section != "1.4.1 Foundational") |>
  pull(module) |> unique()

all_mods <- c(foundational_mods, device_mods)

# Safe output ID from module name
tab_out_id <- function(m) paste0("dt_", gsub("[^A-Za-z0-9]", "_", m))

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Digital Skills Survey Builder",

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
        tags$p(tags$strong("Foundational Digital Skills"),
               style = "margin-bottom:4px;"),
        tags$div(
          style = "border-left:3px solid #e67e22; padding-left:10px;",
          checkboxGroupInput(
            "sel_foundational", label = NULL,
            choices = foundational_mods, selected = foundational_mods
          )
        ),
        tags$hr(),
        checkboxGroupInput(
          "sel_device", label = NULL,
          choices = device_mods, selected = device_mods
        ),
        tags$hr(),
        tags$strong(textOutput("count_label"))
      ),

      mainPanel(width = 9,
        uiOutput("module_tabs_ui"),
        br(),
        downloadButton("dl_xlsform", "Export XLSForm (.xlsx)")
      )
    )
  ),

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

  # Per-question inclusion state
  inc <- reactiveValues()
  observe({
    for (id in items_all$id) {
      if (is.null(inc[[id]])) inc[[id]] <- TRUE
    }
  })

  observeEvent(input$toggle_q, {
    ev  <- input$toggle_q
    row <- items_all[items_all$id == ev$id, ]
    if (nrow(row) > 0 && !row$recommended_core) inc[[ev$id]] <- ev$val
  })

  sel_modules <- reactive(c(input$sel_foundational, input$sel_device))

  # Build display + inclusion data for one module (reactive-aware)
  module_tbl <- function(mod) {
    df <- items_all |> filter(module == mod)
    df |> mutate(
      number     = sub("_.*$", "", id),
      included   = vapply(id, function(i) isTRUE(inc[[i]]), logical(1)),
      check_html = mapply(function(i, core, inc_val) {
        if (core) {
          '<input type="checkbox" checked disabled
             title="Required core item"
             style="accent-color:#e67e22;cursor:not-allowed;">'
        } else {
          sprintf('<input type="checkbox" class="q-toggle" data-id="%s"%s>',
                  i, if (inc_val) " checked" else "")
        }
      }, id, recommended_core, included, SIMPLIFY = TRUE)
    )
  }

  # All selected data (for count + export)
  all_tbl_data <- reactive({
    mods <- sel_modules()
    if (length(mods) == 0) return(items_all[0, ])
    bind_rows(lapply(mods, module_tbl))
  })

  # Dynamic tab panel: one tab per selected module
  output$module_tabs_ui <- renderUI({
    mods <- sel_modules()
    if (length(mods) == 0)
      return(p("Select at least one competency area from the sidebar.",
               style = "color:#888; padding:20px;"))
    tabs <- lapply(mods, function(m) {
      tabPanel(title = m, value = m, br(), DTOutput(tab_out_id(m)))
    })
    do.call(tabsetPanel, c(list(id = "module_tabs", type = "tabs"), tabs))
  })

  # Register a renderDT for every possible module upfront
  for (m in all_mods) {
    local({
      mod <- m
      output[[tab_out_id(mod)]] <- renderDT({
        df <- module_tbl(mod)
        display <- df |> select(
          ` `          = check_html,
          `No.`        = number,
          `Competency` = competency_domain,
          `Skill`      = skill_area,
          Core         = recommended_core,   # hidden; used for row colour
          Question     = question
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
              list(visible   = FALSE, targets = 4)  # hide Core
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
    })
  }

  # When a new module is checked, navigate to its tab
  prev_mods <- reactiveVal(all_mods)
  observeEvent(sel_modules(), {
    newly_added <- setdiff(sel_modules(), prev_mods())
    if (length(newly_added) > 0)
      updateTabsetPanel(session, "module_tabs", selected = newly_added[1])
    prev_mods(sel_modules())
  }, ignoreInit = TRUE)

  output$count_label <- renderText({
    paste("Selected questions:", sum(all_tbl_data()$included))
  })

  output$dl_xlsform <- downloadHandler(
    filename = "digital_skills_survey.xlsx",
    content  = function(file) {
      export_xlsform(all_tbl_data() |> filter(included), file)
    }
  )
}

shinyApp(ui, server)
