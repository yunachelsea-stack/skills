library(shiny)
library(DT)
library(dplyr)
library(readr)

source("R/export.R")

items_all <- read_csv("data/items.csv", show_col_types = FALSE) |>
  mutate(
    recommended_core   = as.logical(recommended_core),
    cognitively_tested = as.logical(cognitively_tested),
    competency_domain  = sub("^Device Use\\s*-\\s*", "", competency_domain)
  )

items_pop <- read_csv("data/items_population_specific.csv", show_col_types = FALSE) |>
  mutate(
    recommended_core   = as.logical(recommended_core),
    cognitively_tested = as.logical(cognitively_tested)
  )

foundational_mods <- items_all |>
  filter(section == "1 Foundational Skills") |>
  pull(module) |> unique()

device_mods <- items_all |>
  filter(section != "1 Foundational Skills") |>
  pull(module) |> unique()

pop_section_map <- c(
  "Community Health Workers" = "4. Community Health Workers",
  "Persons with Disabilities" = "5. Persons with Disabilities",
  "OOS Youth"                 = "6. OOS Job-Seeking Youth"
)
pop_mods <- names(pop_section_map)

all_mods <- c(foundational_mods, device_mods, pop_mods)

required_mods <- items_all |>
  group_by(module) |>
  summarise(has_core = any(recommended_core, na.rm = TRUE)) |>
  filter(has_core) |>
  pull(module)

module_checkbox_group <- function(input_id, mods, selected, req_mods = required_mods) {
  item_tags <- lapply(mods, function(m) {
    is_req <- m %in% req_mods
    inp <- if (is_req) {
      tags$input(type = "checkbox", name = input_id, value = m,
                 checked = "checked", disabled = "disabled")
    } else if (m %in% selected) {
      tags$input(type = "checkbox", name = input_id, value = m, checked = "checked")
    } else {
      tags$input(type = "checkbox", name = input_id, value = m)
    }
    tags$div(class = "checkbox",
      tags$label(
        style = if (is_req) "cursor:default;" else "",
        inp,
        tags$span(style = "margin-left:4px;", m),
        if (is_req) tags$div(
          tags$small("Some questions in this module are required",
            style = "color:#aaa; font-style:italic; margin-left:20px; display:block;")
        )
      )
    )
  })
  tags$div(id = input_id, class = "shiny-input-checkboxgroup shiny-input-container",
    tags$div(class = "shiny-options-group", item_tags)
  )
}

tab_out_id     <- function(m) paste0("dt_",      gsub("[^A-Za-z0-9]", "_", m))
chw_tab_out_id <- function(d) paste0("dt_CHW__", gsub("[^A-Za-z0-9]", "_", d))
pwd_tab_out_id <- function(d) paste0("dt_PWD__", gsub("[^A-Za-z0-9]", "_", d))

chw_domains <- items_pop |>
  filter(section == pop_section_map[["Community Health Workers"]]) |>
  pull(module) |> unique()

pwd_domains <- items_pop |>
  filter(section == pop_section_map[["Persons with Disabilities"]]) |>
  pull(competency_domain) |> unique()

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ID formats: "IDL1_desc" → "IDL1"   "DU_HEA1_desc" → "HEA1"   "CHW_IDL1_desc" → "IDL1"
extract_num <- function(id) {
  ifelse(grepl("^[A-Z]+_", id),
         gsub("^[^_]+_([^_]+)_.*$", "\\1", id),
         sub("_.*$", "", id))
}

# Modules that use sequential numbering instead of ID-derived numbers.
seq_prefixes <- c(
  "Device Access"             = "DA",
  "Device Use"                = "DU",
  "Persons with Disabilities" = "PWD",
  "OOS Youth"                 = "YTH"
)

# Column used to compute row-banding. Default is competency_domain.
band_cols <- list(
  "Device Access" = "skill_area",
  "OOS Youth"     = "skill_area"
)

# Column config: names = display labels, values = source column names.
# Add an entry here to change what any tab shows — no touching render code.
tab_cols <- list(
  default = c(
    ` `          = "check_html",
    `No.`        = "number",
    `Competency` = "competency_domain",
    `Skill`      = "skill_area",
    `Question`   = "question_html"
  ),
  `OOS Youth` = c(
    ` `        = "check_html",
    `No.`      = "number",
    `Skill`    = "skill_area",
    `Question` = "question_html"
  ),
  `Device Access` = c(
    ` `        = "check_html",
    `No.`      = "number",
    `Topic`    = "skill_area",
    `Question` = "question_html"
  ),
  `Device Use` = c(
    ` `          = "check_html",
    `No.`        = "number",
    `Use Case`   = "competency_domain",
    `Question`   = "question_html"
  ),
  `PWD::Accessibility settings` = c(
    ` `        = "check_html",
    `No.`      = "number",
    `Setting`  = "skill_area",
    `Question` = "question_html"
  ),
  `PWD::Assistive software` = c(
    ` `        = "check_html",
    `No.`      = "number",
    `Software` = "skill_area",
    `Question` = "question_html"
  )
)

make_display <- function(df, key) {
  cfg <- tab_cols[[key]] %||% tab_cols[["default"]]
  vis <- df[, cfg, drop = FALSE]
  colnames(vis) <- names(cfg)
  cbind(vis, Core = df$recommended_core, band = df$band)
}

render_module_dt <- function(df, key, page_len = 25) {
  display <- make_display(df, key)
  n_vis   <- ncol(display) - 2L
  datatable(
    display,
    escape = FALSE, rownames = FALSE, selection = "none",
    class  = "hover row-border",
    options = list(
      pageLength = page_len, dom = "tip",
      columnDefs = list(
        list(orderable = FALSE, targets = 0),
        list(visible   = FALSE, targets = c(n_vis, n_vis + 1L))
      )
    )
  ) |>
    formatStyle("band", target = "row",
      backgroundColor = styleEqual(c("odd", "even"), c("#ffffff", "#f2f2f2"))) |>
    formatStyle("Core", target = "row",
      fontWeight = styleEqual(TRUE, "600"))
}

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

    # What you can do here
    div(class = "lp-section", style = "background:#f7f9fb; max-width:100%; padding: 48px 40px;",
      div(style = "max-width:1100px; margin:0 auto;",
        tags$h2("What you can do here"),
        tags$br(),
        fluidRow(
          column(4,
            div(style = "padding-right:24px;",
              tags$h4(style = "font-weight:600; color:#003366; margin-bottom:8px;",
                      "\U0001F4CB  Build your survey"),
              tags$p(style = "color:#555; line-height:1.65;",
                "Choose from pre-validated modules covering different areas of digital skills.
                 Required questions ensure consistency across projects and countries;
                 optional modules allow flexibility."
              )
            )
          ),
          column(4,
            div(style = "padding-right:24px;",
              tags$h4(style = "font-weight:600; color:#003366; margin-bottom:8px;",
                      "\U0001F4E5  Export ready-to-use instruments"),
              tags$p(style = "color:#555; line-height:1.65;",
                "Download your survey in formats ready for implementation by survey firms
                 or data collection teams."
              )
            )
          ),
          column(4,
            div(
              tags$h4(style = "font-weight:600; color:#003366; margin-bottom:8px;",
                      "\U0001F4DA  Access implementation guidance"),
              tags$p(style = "color:#555; line-height:1.65;",
                "Get practical resources on cognitive testing, sampling, survey implementation
                 and quality assurance, digital skills analysis."
              )
            )
          )
        )
      )
    ),

    # Measurement framework
    div(class = "lp-section",
      tags$h2("The measurement framework"),
      tags$br(),

      # Two cards stacked vertically
      div(
        div(style = "background:#fff; border-radius:10px; padding:24px 22px;
                     box-shadow:0 2px 10px rgba(0,0,0,0.07); margin-bottom:16px;
                     border-left:4px solid #003366;",
            tags$h4(style = "font-weight:600; color:#003366; margin-bottom:14px;",
                    "The framework includes"),
            tags$ol(style = "padding-left:18px; color:#444; line-height:1.75;",
              tags$li("Foundational digital skills and foundational AI skills"),
              tags$li("Population-specific digital skills (community health workers as an example
                       of last-mile service providers, persons with disabilities, and
                       out-of-school / job-seeking youth)"),
              tags$li("Device access and device use")
            )
        ),
        div(style = "background:#fff; border-radius:10px; padding:24px 22px;
                     box-shadow:0 2px 10px rgba(0,0,0,0.07);
                     border-left:4px solid #0a5fa8;",
            tags$h4(style = "font-weight:600; color:#003366; margin-bottom:14px;",
                    "Built on several frameworks"),
            tags$ol(style = "padding-left:18px; color:#444; line-height:1.7;",
              tags$li(
                tags$strong("DigComp Framework"),
                " developed by the Joint Research Centre (JRC) of the European Commission,
                 defines 21 competences organised across five key areas:",
                tags$ul(style = "margin-top:8px; margin-bottom:8px;",
                  tags$li(tags$strong("Information and data literacy: "),
                    "The ability to identify, locate, evaluate, and effectively manage
                     digital information and data."),
                  tags$li(tags$strong("Communication and collaboration: "),
                    "The capacity to interact, share, and work together through digital
                     technologies in safe, inclusive, and responsible ways."),
                  tags$li(tags$strong("Digital content creation: "),
                    "The ability to develop, edit, and share digital content in multiple
                     formats (such as text, images, video, or code), while respecting
                     copyright and licensing rules."),
                  tags$li(tags$strong("Safety: "),
                    "The competence to protect devices, personal data, health, well-being,
                     and the environment when using digital technologies."),
                  tags$li(tags$strong("Problem solving: "),
                    "The ability to identify digital needs, address technical challenges,
                     and innovate by adapting and applying digital technologies.")
                )
              ),
              tags$li(
                "An additional competency area, ",
                tags$strong("Devices and software operations"),
                ", was adapted from the ",
                tags$em("Digital Literacy Global Framework"),
                " developed for the UNESCO Institute for Statistics."
              ),
              tags$li(
                "AI and digital skills: This toolkit focuses narrowly on ",
                tags$strong("foundational AI skills"),
                ". Measures on AI use are integrated within each competency area."
              )
            )
          )
      ),

      tags$br(), tags$br(),

      # Framework table
      tags$h3(style = "font-size:1.05em; font-weight:600; color:#444; margin-bottom:16px;",
              "Illustrative digital skills linked to competences"),

      tags$style(HTML("
        .fw-table { width:100%; border-collapse:collapse; font-size:0.88em; }
        .fw-table th {
          background:#003366; color:#fff; padding:10px 14px;
          text-align:left; font-weight:600;
        }
        .fw-table td { padding:9px 14px; vertical-align:top; border-bottom:1px solid #e8ecef; }
        .fw-table tr:last-child td { border-bottom:none; }
        .fw-table .fw-area {
          font-weight:600; color:#fff; writing-mode:horizontal-tb;
          background:#0a5fa8; text-align:center;
        }
        .fw-table tr.fw-row-idl td { background:#f7f9fb; }
        .fw-table tr.fw-row-cc  td { background:#fff; }
        .fw-table tr.fw-row-dcc td { background:#f7f9fb; }
        .fw-table tr.fw-row-saf td { background:#fff; }
        .fw-table tr.fw-row-ps  td { background:#f7f9fb; }
        .fw-table tr.fw-row-dso td { background:#fff; }
        .fw-table tr.fw-row-car td { background:#f7f9fb; }
        .fw-table .fw-area-cell {
          background:inherit; color:#222; font-weight:600;
          font-size:0.92em; vertical-align:middle; text-align:center;
          padding:10px 12px;
        }
        .fw-skill-list { margin:0; padding-left:16px; }
        .fw-skill-list li { margin-bottom:2px; }
      ")),

      tags$table(class = "fw-table",
        tags$thead(
          tags$tr(
            tags$th(style="width:14%", "Competency area"),
            tags$th(style="width:26%", "Competence"),
            tags$th("Illustrative digital skills")
          )
        ),
        tags$tbody(
          # Information and data literacy
          tags$tr(class="fw-row-idl",
            tags$td(class="fw-area-cell", rowspan="3", "Information and data literacy"),
            tags$td("1.1 Browsing, searching and filtering data, information, and digital content"),
            tags$td(tags$ul(class="fw-skill-list", tags$li("Using search engines")))
          ),
          tags$tr(class="fw-row-idl",
            tags$td("1.2 Evaluating data, information, and digital content"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Fact-checking websites"), tags$li("Comparing with multiple sources")))
          ),
          tags$tr(class="fw-row-idl",
            tags$td("1.3 Managing data, information, and digital content"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Using spreadsheets/databases"),
              tags$li("Organising files in cloud storage"),
              tags$li("Creating folder hierarchies")))
          ),
          # Communication and collaboration
          tags$tr(class="fw-row-cc",
            tags$td(class="fw-area-cell", rowspan="4", "Communication and collaboration"),
            tags$td("2.1 Interacting through digital technologies"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Using email, instant messaging, video conferencing tools (Zoom, Teams)"),
              tags$li("Commenting on shared documents"),
              tags$li("Participating in forums")))
          ),
          tags$tr(class="fw-row-cc",
            tags$td("2.2 Sharing through digital technologies"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Sharing files via Google Drive/Dropbox"),
              tags$li("Posting on social media responsibly"),
              tags$li("Using collaborative project tools (Trello, Slack)")))
          ),
          tags$tr(class="fw-row-cc",
            tags$td("2.3 Engaging in citizenship through digital technologies"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Participating in online petitions"),
              tags$li("Joining e-government services"),
              tags$li("Contributing to digital communities, voicing opinions via social media"),
              tags$li("Practicing netiquette")))
          ),
          tags$tr(class="fw-row-cc",
            tags$td("2.4 Collaborating through digital technologies"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Co-editing documents in Google Docs"),
              tags$li("Using shared calendars")))
          ),
          # Digital content creation
          tags$tr(class="fw-row-dcc",
            tags$td(class="fw-area-cell", rowspan="4", "Digital content creation"),
            tags$td("3.1 Developing digital content"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Creating presentations (PowerPoint)"),
              tags$li("Posting on social media"),
              tags$li("Writing blogs"),
              tags$li("Editing videos, photos")))
          ),
          tags$tr(class="fw-row-dcc",
            tags$td("3.2 Integrating and re-elaborating digital content"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Embedding charts into reports"),
              tags$li("Curating content into newsletters")))
          ),
          tags$tr(class="fw-row-dcc",
            tags$td("3.3 Copyright and licenses"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Applying Creative Commons licenses"),
              tags$li("Citing digital sources properly"),
              tags$li("Checking license types")))
          ),
          tags$tr(class="fw-row-dcc",
            tags$td("3.4 Programming"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Writing scripts in Python/JavaScript"),
              tags$li("Building simple apps/websites"),
              tags$li("Automating tasks with code")))
          ),
          # Safety
          tags$tr(class="fw-row-saf",
            tags$td(class="fw-area-cell", rowspan="4", "Safety"),
            tags$td("4.1 Protecting devices"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Installing antivirus software"),
              tags$li("Updating operating systems"),
              tags$li("Configuring firewalls"),
              tags$li("Using secure passwords")))
          ),
          tags$tr(class="fw-row-saf",
            tags$td("4.2 Protecting personal data and privacy"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Setting social media privacy settings"),
              tags$li("Enabling two-factor authentication"),
              tags$li("Managing cookies and permissions")))
          ),
          tags$tr(class="fw-row-saf",
            tags$td("4.3 Protecting health and well-being, including reducing digital harms"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Adjusting screen brightness"),
              tags$li("Using blue-light filters"),
              tags$li("Managing screen time")))
          ),
          tags$tr(class="fw-row-saf",
            tags$td("4.4 Protecting the environment"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Recycling e-waste"),
              tags$li("Reducing printing"),
              tags$li("Using energy-saving modes on devices")))
          ),
          # Problem solving
          tags$tr(class="fw-row-ps",
            tags$td(class="fw-area-cell", rowspan="4", "Problem solving"),
            tags$td("5.1 Solving technical problems"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Troubleshooting Wi-Fi issues"),
              tags$li("Reinstalling software"),
              tags$li("Clearing cache")))
          ),
          tags$tr(class="fw-row-ps",
            tags$td("5.2 Identifying needs and technological responses"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Selecting the right app for project management"),
              tags$li("Choosing appropriate data visualisation tools"),
              tags$li("Evaluating software alternatives")))
          ),
          tags$tr(class="fw-row-ps",
            tags$td("5.3 Creatively using digital technologies"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Using AI tools for idea generation"),
              tags$li("Creating infographics"),
              tags$li("Designing interactive learning modules")))
          ),
          tags$tr(class="fw-row-ps",
            tags$td("5.4 Identifying digital competence gaps"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Using self-assessment tools"),
              tags$li("Tracking skill progress"),
              tags$li("Identifying training needs")))
          ),
          # Device and software operations
          tags$tr(class="fw-row-dso",
            tags$td(class="fw-area-cell", rowspan="2", "Device and software operations"),
            tags$td("6.1 Physical operations of digital devices"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Setting up a printer"),
              tags$li("Troubleshooting hardware"),
              tags$li("Managing Wi-Fi routers")))
          ),
          tags$tr(class="fw-row-dso",
            tags$td("6.2 Software operations in digital devices"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Installing/uninstalling programs"),
              tags$li("Updating applications"),
              tags$li("Configuring settings"),
              tags$li("Managing cloud apps")))
          ),
          # Career specific competencies
          tags$tr(class="fw-row-car",
            tags$td(class="fw-area-cell", rowspan="2", "Career specific competencies"),
            tags$td("7.1 Operating specialised digital technologies"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Using industry-specific software"),
              tags$li("Using point-of-sale systems"),
              tags$li("Using point of care devices")))
          ),
          tags$tr(class="fw-row-car",
            tags$td("7.2 Interpreting and manipulating data, information and digital content"),
            tags$td(tags$ul(class="fw-skill-list",
              tags$li("Using Excel for data analysis"),
              tags$li("Creating dashboards (Tableau, Power BI)"),
              tags$li("Running statistical tests")))
          )
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
    tags$style(HTML("
      .section-dimmed { opacity: 0.35; pointer-events: none; transition: opacity 0.2s; }
      #chw-excl-note { display:none; color:#e67e22; font-size:0.8em;
                       font-style:italic; margin-top:4px; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'sel_pop') {
          var chw = Array.isArray(e.value) &&
                    e.value.indexOf('Community Health Workers') >= 0;
          $('#foundational-section').toggleClass('section-dimmed', chw);
          $('#chw-excl-note').toggle(chw);
        }
      });
    ")),
    sidebarLayout(
      sidebarPanel(width = 3,
        tags$div(id = "foundational-section",
          tags$p(tags$strong("Foundational Digital Skills"),
                 style = "margin-bottom:4px;"),
          tags$div(
            style = "border-left:3px solid #e67e22; padding-left:10px;",
            module_checkbox_group("sel_foundational", foundational_mods,
                                 selected = foundational_mods)
          ),
          tags$p(id = "chw-excl-note",
                 "Not included — CHW module selected")
        ),
        tags$hr(),
        tags$p(tags$strong("Population-specific Digital Skills"),
               style = "margin-bottom:4px;"),
        tags$div(
          style = "border-left:3px solid #27ae60; padding-left:10px;",
          module_checkbox_group("sel_pop", pop_mods, selected = NULL,
                               req_mods = character(0))
        ),
        tags$hr(),
        module_checkbox_group("sel_device", device_mods, selected = device_mods),
        tags$hr(),
        tags$strong(textOutput("count_label")),
        br(),
        actionButton("btn_preview", "Preview Survey",
                     icon = icon("eye"),
                     style = "width:100%; margin-top:4px;")
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
    for (id in c(items_all$id, items_pop$id)) {
      if (is.null(inc[[id]])) inc[[id]] <- TRUE
    }
  })

  observeEvent(input$toggle_q, {
    ev  <- input$toggle_q
    row <- items_all[items_all$id == ev$id, ]
    if (nrow(row) == 0) row <- items_pop[items_pop$id == ev$id, ]
    if (nrow(row) > 0 && !isTRUE(row$recommended_core)) inc[[ev$id]] <- ev$val
  })

  sel_modules <- reactive({
    chw_active <- "Community Health Workers" %in% input$sel_pop
    foundational <- if (chw_active) character(0) else input$sel_foundational
    c(foundational, input$sel_pop, input$sel_device)
  })

  module_tbl <- function(mod) {
    is_pop <- mod %in% pop_mods
    if (is_pop) {
      df <- items_pop |>
        filter(section == pop_section_map[[mod]]) |>
        mutate(module = mod)
    } else {
      df <- items_all |> filter(module == mod)
    }
    band_col <- band_cols[[mod]] %||% "competency_domain"
    domains  <- unique(df[[band_col]])
    df |> mutate(
      number = if (mod %in% names(seq_prefixes))
        paste0(seq_prefixes[[mod]], seq_len(n()))
      else
        extract_num(id),
      included   = vapply(id, function(i) isTRUE(inc[[i]]), logical(1)),
      band       = ifelse(match(.data[[band_col]], domains) %% 2 == 1, "odd", "even"),
      question_html = ifelse(
        !is.na(response_options) & trimws(response_options) != "",
        paste0(question,
               "<br><small style='color:#999; font-style:italic;'>",
               gsub(";", " &nbsp;&middot;&nbsp; ", response_options),
               "</small>"),
        question
      ),
      check_html = mapply(function(i, core, inc_val) {
        if (isTRUE(core)) {
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

  prep_pop_df <- function(df, band_by) {
    groups <- unique(df[[band_by]])
    df |> mutate(
      number    = extract_num(id),
      included  = vapply(id, function(i) isTRUE(inc[[i]]), logical(1)),
      band      = ifelse(match(.data[[band_by]], groups) %% 2 == 1, "odd", "even"),
      question_html = ifelse(
        !is.na(response_options) & trimws(response_options) != "",
        paste0(question, "<br><small style='color:#999;font-style:italic;'>",
               gsub(";", " &nbsp;&middot;&nbsp;", response_options), "</small>"),
        question
      ),
      check_html = mapply(function(i, core, inc_val) {
        if (isTRUE(core))
          '<input type="checkbox" checked disabled title="Required core item" style="accent-color:#e67e22;cursor:not-allowed;">'
        else
          sprintf('<input type="checkbox" class="q-toggle" data-id="%s"%s>',
                  i, if (inc_val) " checked" else "")
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
      if (m == "Community Health Workers") {
        domain_tabs <- lapply(chw_domains, function(d) {
          tabPanel(title = d, value = d, br(), DTOutput(chw_tab_out_id(d)))
        })
        tabPanel(title = m, value = m, br(),
          do.call(tabsetPanel, c(list(id = "chw_domain_tabs", type = "tabs"), domain_tabs))
        )
      } else if (m == "Persons with Disabilities") {
        domain_tabs <- lapply(pwd_domains, function(d) {
          tabPanel(title = d, value = d, br(), DTOutput(pwd_tab_out_id(d)))
        })
        tabPanel(title = m, value = m, br(),
          do.call(tabsetPanel, c(list(id = "pwd_domain_tabs", type = "tabs"), domain_tabs))
        )
      } else {
        tabPanel(title = m, value = m, br(), DTOutput(tab_out_id(m)))
      }
    })
    do.call(tabsetPanel, c(list(id = "module_tabs", type = "tabs"), tabs))
  })

  for (m in all_mods) {
    local({
      mod <- m
      output[[tab_out_id(mod)]] <- renderDT({
        render_module_dt(module_tbl(mod), key = mod)
      }, server = FALSE)
    })
  }

  for (d in chw_domains) {
    local({
      domain <- d
      output[[chw_tab_out_id(domain)]] <- renderDT({
        df <- items_pop |>
          filter(section == pop_section_map[["Community Health Workers"]],
                 module == domain) |>
          prep_pop_df(band_by = "competency_domain")
        render_module_dt(df, key = "default", page_len = 50)
      }, server = FALSE)
    })
  }

  for (d in pwd_domains) {
    local({
      domain <- d
      output[[pwd_tab_out_id(domain)]] <- renderDT({
        df <- items_pop |>
          filter(section == pop_section_map[["Persons with Disabilities"]],
                 competency_domain == domain) |>
          prep_pop_df(band_by = "skill_area") |>
          mutate(number = paste0("PWD", seq_len(n())))
        render_module_dt(df, key = paste0("PWD::", domain), page_len = 50)
      }, server = FALSE)
    })
  }

  prev_mods <- reactiveVal(c(foundational_mods, device_mods))
  observeEvent(sel_modules(), {
    newly_added <- setdiff(sel_modules(), prev_mods())
    if (length(newly_added) > 0)
      updateTabsetPanel(session, "module_tabs", selected = newly_added[1])
    prev_mods(sel_modules())
  }, ignoreInit = TRUE)

  output$count_label <- renderText({
    paste("Selected questions:", sum(all_tbl_data()$included))
  })

  observeEvent(input$btn_preview, {
    inc_ids <- all_tbl_data() |> filter(included) |> pull(id)
    n_total <- length(inc_ids)
    if (n_total == 0) {
      showModal(modalDialog(
        title = "Preview Survey", "No questions selected.",
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }

    h3_style <- "font-size:1.1em; font-weight:700; color:#003366;
                 border-bottom:2px solid #003366; padding-bottom:5px;
                 margin-top:28px; margin-bottom:10px;"
    h4_style <- "font-size:0.97em; font-weight:600; color:#333;
                 margin-top:14px; margin-bottom:4px;"

    q_rows <- function(df) {
      lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        num <- if (!is.null(row$num_override)) row$num_override else extract_num(row$id)
        opts_html <- if (!is.na(row$response_options) &&
                         trimws(row$response_options) != "") {
          opts <- trimws(strsplit(row$response_options, ";")[[1]])
          tags$p(style = "color:#888; font-style:italic; font-size:0.87em;
                          margin:2px 0 8px 18px; line-height:1.6;",
                 paste(opts, collapse = "  ·  "))
        }
        tags$div(style = "margin:5px 0;",
          tags$p(style = "margin:0; line-height:1.6;",
            tags$span(style = "font-weight:600; color:#555; margin-right:5px;",
                      paste0(num, ".")),
            row$question
          ),
          opts_html
        )
      })
    }

    sections <- list()

    # Foundational modules: module → domain → questions
    f_df <- items_all |> filter(id %in% inc_ids, section == "1 Foundational Skills")
    for (mod in intersect(foundational_mods, unique(f_df$module))) {
      mdf <- f_df[f_df$module == mod, ]
      sections[[length(sections)+1]] <- tagList(
        tags$h3(style = h3_style, mod),
        lapply(unique(mdf$competency_domain), function(dom)
          tagList(tags$h4(style = h4_style, dom),
                  q_rows(mdf[mdf$competency_domain == dom, ])))
      )
    }

    # CHW: original domain (module col in items_pop) → questions
    chw_df <- items_pop |>
      filter(id %in% inc_ids, section == pop_section_map[["Community Health Workers"]])
    for (dom in intersect(chw_domains, unique(chw_df$module))) {
      ddf <- chw_df[chw_df$module == dom, ]
      sections[[length(sections)+1]] <- tagList(
        tags$h3(style = h3_style, dom),
        q_rows(ddf)
      )
    }

    # PWD: module heading → competency_domain (Setting / Software) → questions
    pwd_df <- items_pop |>
      filter(id %in% inc_ids, section == pop_section_map[["Persons with Disabilities"]])
    if (nrow(pwd_df) > 0) {
      pwd_df$num_override <- paste0("PWD", seq_len(nrow(pwd_df)))
      sections[[length(sections)+1]] <- tagList(
        tags$h3(style = h3_style, "Persons with Disabilities"),
        lapply(intersect(pwd_domains, unique(pwd_df$competency_domain)), function(dom) {
          ddf <- pwd_df[pwd_df$competency_domain == dom, ]
          tagList(tags$h4(style = h4_style, dom), q_rows(ddf))
        })
      )
    }

    # OOS Youth: module heading then flat list of questions
    oos_df <- items_pop |>
      filter(id %in% inc_ids, section == pop_section_map[["OOS Youth"]])
    if (nrow(oos_df) > 0) {
      oos_df$num_override <- paste0("YTH", seq_len(nrow(oos_df)))
      sections[[length(sections)+1]] <- tagList(
        tags$h3(style = h3_style, "Out-of-school / Job-seeking Youth"),
        q_rows(oos_df)
      )
    }

    # Device modules (always last): Access = flat list; Use = domain → questions
    d_df <- items_all |> filter(id %in% inc_ids, section != "1 Foundational Skills")
    for (mod in intersect(device_mods, unique(d_df$module))) {
      mdf <- d_df[d_df$module == mod, ]
      is_access <- unique(mdf$section) == "2 Device Access"
      mdf$num_override <- if (is_access)
        paste0("DA", seq_len(nrow(mdf)))
      else
        paste0("DU", seq_len(nrow(mdf)))
      sections[[length(sections)+1]] <- tagList(
        tags$h3(style = h3_style, mod),
        if (is_access)
          q_rows(mdf)
        else
          lapply(unique(mdf$competency_domain), function(dom)
            tagList(tags$h4(style = h4_style, dom),
                    q_rows(mdf[mdf$competency_domain == dom, ])))
      )
    }

    showModal(modalDialog(
      title     = tags$span(style = "font-weight:700;",
                            paste0("Survey Preview  ·  ", n_total, " questions")),
      size      = "l",
      easyClose = TRUE,
      footer    = tagList(
        modalButton("Close"),
        downloadButton("dl_word_preview",    "Export Word (.docx)",
                       style = "margin-left:8px;"),
        downloadButton("dl_xlsform_preview", "Export XLSForm (.xlsx)",
                       style = "margin-left:4px;")
      ),
      tags$div(style = "max-height:70vh; overflow-y:auto; padding:0 8px;",
               tagList(sections))
    ))
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

  output$dl_word_preview <- downloadHandler(
    filename    = "digital_skills_survey.docx",
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content     = function(file) {
      export_word(all_tbl_data() |> filter(included), file)
    }
  )

  output$dl_xlsform_preview <- downloadHandler(
    filename    = "digital_skills_survey.xlsx",
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      export_xlsform(all_tbl_data() |> filter(included), file)
    }
  )
}

shinyApp(ui, server)
