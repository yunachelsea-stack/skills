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

tab_out_id <- function(m) paste0("dt_", gsub("[^A-Za-z0-9]", "_", m))

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Digital Skills Measurement Toolkit",

  # ── Landing page ──────────────────────────────────────────────────────────────
  tabPanel("Home",
    tags$style(HTML("
      .lp-hero {
        background: linear-gradient(135deg, #003366 0%, #0a5fa8 100%);
        color: white;
        padding: 64px 40px 52px;
        margin: -15px -15px 0;
      }
      .lp-hero h1 {
        font-size: 2.2em; font-weight: 700;
        line-height: 1.25; margin-bottom: 16px;
      }
      .lp-lead {
        font-size: 1.05em; opacity: 0.92;
        max-width: 820px; line-height: 1.65;
        margin-bottom: 28px;
      }
      .lp-section {
        padding: 48px 40px;
        max-width: 1100px; margin: 0 auto;
      }
      .lp-section h2 {
        font-size: 1.4em; font-weight: 600;
        color: #222; margin-bottom: 6px;
      }
      .lp-section-sub {
        color: #888; margin-bottom: 32px; font-size: 0.95em;
      }
      .lp-card {
        background: #fff;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.07);
        padding: 24px 22px;
        height: 100%;
        margin-bottom: 20px;
        border-top: 4px solid #ccc;
        transition: box-shadow 0.2s;
      }
      .lp-card:hover { box-shadow: 0 6px 20px rgba(0,0,0,0.12); }
      .lp-card.active  { border-top-color: #e67e22; }
      .lp-card.planned { border-top-color: #aabfcc; }
      .lp-card-num {
        font-size: 1.6em; font-weight: 700;
        color: #aabfcc; margin-bottom: 6px;
      }
      .lp-card.active .lp-card-num { color: #e67e22; }
      .lp-card h4 {
        font-size: 1em; font-weight: 600;
        color: #222; margin-bottom: 8px;
      }
      .lp-card p { font-size: 0.88em; color: #666; line-height: 1.55; margin: 0; }
      .lp-pill {
        display: inline-block; font-size: 0.7em; font-weight: 600;
        padding: 2px 9px; border-radius: 10px; margin-bottom: 10px;
        text-transform: uppercase; letter-spacing: 0.04em;
      }
      .lp-pill.active  { background: #fef0e0; color: #e67e22; }
      .lp-pill.planned { background: #f0f4f6; color: #7a99aa; }
    ")),

    # Hero
    div(class = "lp-hero",
      div(style = "max-width:860px;",
        tags$h1("Measure digital skills.", tags$br(),
                "Build better surveys.", tags$br(),
                "Generate actionable data."),
        tags$p(class = "lp-lead",
          "Design and customize digital skills surveys in minutes using a validated,
           modular toolkit. Select the questions you need, ensure comparability,
           and export ready-to-field instruments for your data collection teams."
        ),
        tags$p(style = "font-size:0.85em; opacity:0.75; line-height:1.6; max-width:820px;",
          tags$em("Source: A Practitioner's Toolkit for Inclusive Digital Skills Measurement
                   in Low- and Middle-Income Countries"),
          ", developed by a consortium of partners from the ",
          tags$strong("World Bank"),
          " and the ",
          tags$strong("Evidence for Digital Transformation Consortium (EDiT)"),
          " led by the ",
          tags$strong("University of Cape Town's School of Public Health"), "."
        )
      )
    ),

    # Feature cards
    div(class = "lp-section",
      tags$h2("What you’ll find on this platform"),
      tags$p(class = "lp-section-sub",
             "Six integrated resources supporting end-to-end digital skills measurement."),

      fluidRow(
        column(4,
          div(class = "lp-card active",
            span(class = "lp-pill active", "Available"),
            div(class = "lp-card-num", "01"),
            tags$h4("Survey Builder"),
            tags$p("Build a digital skills measurement survey including required and optional
                    questions recommended in the toolkit. Export as XLSForm or Word.")
          )
        ),
        column(4,
          div(class = "lp-card planned",
            span(class = "lp-pill planned", "Coming soon"),
            div(class = "lp-card-num", "02"),
            tags$h4("Conceptual Adaptations"),
            tags$p("Steps for adapting survey items to local contexts
                    through cognitive interview protocols.")
          )
        ),
        column(4,
          div(class = "lp-card planned",
            span(class = "lp-pill planned", "Coming soon"),
            div(class = "lp-card-num", "03"),
            tags$h4("Sampling Methods"),
            tags$p("Guidance on probability and non-probability sampling
                    approaches for digital skills surveys in LMICs.")
          )
        )
      ),
      fluidRow(
        column(4,
          div(class = "lp-card planned",
            span(class = "lp-pill planned", "Coming soon"),
            div(class = "lp-card-num", "04"),
            tags$h4("Survey Implementation"),
            tags$p("Practical approaches to field implementation, enumerator
                    training, and data collection protocols.")
          )
        ),
        column(4,
          div(class = "lp-card planned",
            span(class = "lp-pill planned", "Coming soon"),
            div(class = "lp-card-num", "05"),
            tags$h4("Data Quality Assurance"),
            tags$p("A framework for monitoring and ensuring the quality
                    of digital skills measurement data.")
          )
        ),
        column(4,
          div(class = "lp-card planned",
            span(class = "lp-pill planned", "Coming soon"),
            div(class = "lp-card-num", "06"),
            tags$h4("Analyzing Digital Skills"),
            tags$p("Analytical approaches and tools for interpreting
                    and reporting digital skills data.")
          )
        )
      )
    )
  ),

  # ── Survey Builder ────────────────────────────────────────────────────────────
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
        tags$div(
          style = "display:flex; gap:10px;",
          downloadButton("dl_xlsform", "Export XLSForm (.xlsx)"),
          downloadButton("dl_word",   "Export Survey (.docx)")
        )
      )
    )
  ),

  # ── Stub pages ─────────────────────────────────────────────────────────────────
  tabPanel("Conceptual Framework",
    fluidPage(h2("Conceptual Framework"), p("Coming soon."))
  ),
  tabPanel("Sampling",
    fluidPage(h2("Sampling"), p("Coming soon."))
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

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

  module_tbl <- function(mod) {
    df <- items_all |> filter(module == mod)
    domains <- unique(df$competency_domain)
    df |> mutate(
      number     = sub("_.*$", "", id),
      included   = vapply(id, function(i) isTRUE(inc[[i]]), logical(1)),
      band       = ifelse(match(competency_domain, domains) %% 2 == 1, "odd", "even"),
      question_html = ifelse(
        !is.na(response_options) & trimws(response_options) != "",
        paste0(question,
               "<br><small style='color:#999; font-style:italic;'>",
               gsub(";", " &nbsp;&middot;&nbsp; ", response_options),
               "</small>"),
        question
      ),
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

  all_tbl_data <- reactive({
    mods <- sel_modules()
    if (length(mods) == 0) return(items_all[0, ])
    bind_rows(lapply(mods, module_tbl))
  })

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
          Question     = question_html,
          Core         = recommended_core,
          band         = band
        )
        datatable(
          display,
          escape    = FALSE,
          rownames  = FALSE,
          selection = "none",
          class     = "hover row-border",
          options   = list(
            pageLength = 25,
            dom        = "tip",
            columnDefs = list(
              list(orderable = FALSE, targets = 0),
              list(visible   = FALSE, targets = c(5, 6))
            )
          )
        ) |>
          formatStyle("band", target = "row",
            backgroundColor = styleEqual(c("odd", "even"), c("#f2f2f2", "#ffffff"))) |>
          formatStyle("Core", target = "row",
            fontWeight = styleEqual(TRUE, "600"))
      }, server = FALSE)
    })
  }

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
    filename    = "digital_skills_survey.xlsx",
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      export_xlsform(all_tbl_data() |> filter(included), file)
    }
  )

  output$dl_word <- downloadHandler(
    filename    = "digital_skills_survey.docx",
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content     = function(file) {
      export_word(all_tbl_data() |> filter(included), file)
    }
  )
}

shinyApp(ui, server)
