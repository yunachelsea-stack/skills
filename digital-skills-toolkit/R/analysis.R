daui_skills <- c(
  "Sent an SMS or chat-app message",
  "Navigated an automated voice prompt (IVR)",
  "Made a phone or app call",
  "Shared media through an app",
  "Took a photo or video",
  "Used social media",
  "Made a post, reel or story on social media",
  "Downloaded an app",
  "Created a mobile hotspot",
  "Searched for information on the internet",
  "Scanned a QR code",
  "Sent or received money through a payment app",
  "Accessed a bank account on a mobile phone",
  "Blocked a phone number"
)

daui_band <- function(total) {
  if (total == 0) list(label = "No access", class = "none")
  else if (total <= 10) list(label = "Low", class = "low")
  else if (total <= 20) list(label = "Medium", class = "medium")
  else list(label = "High", class = "high")
}

analysis_tab <- function() {
  section  <- tab_section
  step     <- tab_step
  sub_head <- function(title) {
    tags$p(style = "font-weight:700; color:#0d3b5e; margin:20px 0 6px;", title)
  }
  score_row <- function(component, measures, rule, max, weight) {
    tags$tr(tags$th(scope = "row", component), tags$td(measures), tags$td(rule),
            tags$td(class = "num", max), tags$td(class = "num", weight))
  }
  skill_row <- function(skill, question, counts) {
    tags$tr(tags$th(scope = "row", skill), tags$td(question), tags$td(counts))
  }
  weight_seg <- function(cls, pct) {
    div(class = paste("daui-seg", cls), style = paste0("width:", pct, "%"))
  }
  legend_item <- function(cls, text) {
    tags$span(class = "daui-legend-item", tags$span(class = paste("daui-swatch", cls)), text)
  }
  band_chip <- function(cls, name, range) {
    div(class = paste("daui-band", cls), tags$strong(name), tags$span(range))
  }

  tabPanel("Analyzing Digital Skills", value = "analysis",
    tags$style(HTML("
      .daui-table td.num,.daui-table th.num{text-align:right;white-space:nowrap}
      .daui-table tfoot td,.daui-table tfoot th{font-weight:700;border-top:2px solid #0d3b5e}
      .daui-bar{display:flex;height:26px;border-radius:4px;overflow:hidden;margin:18px 0 8px;gap:2px}
      .daui-seg.comp{background:#0d3b5e}
      .daui-seg.access{background:#3d7ea6}
      .daui-seg.safety{background:#e8a800}
      .daui-seg.agency{background:#b5651d}
      .daui-legend{display:flex;flex-wrap:wrap;gap:6px 18px;font-size:13px;color:#4a5e6d}
      .daui-legend-item{display:inline-flex;align-items:center;gap:6px}
      .daui-swatch{display:inline-block;width:12px;height:12px;border-radius:2px}
      .daui-swatch.comp{background:#0d3b5e}
      .daui-swatch.access{background:#3d7ea6}
      .daui-swatch.safety{background:#e8a800}
      .daui-swatch.agency{background:#b5651d}
      .daui-bands{display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:10px;margin:14px 0}
      .daui-band{border-radius:4px;padding:10px 14px;display:flex;flex-direction:column;gap:2px}
      .daui-band span{font-size:13px}
      .daui-band.none{background:#eef1f4;color:#4a5e6d}
      .daui-band.low{background:#fdf1d6;color:#7a5600}
      .daui-band.medium{background:#d8e8f2;color:#0d3b5e}
      .daui-band.high{background:#0d3b5e;color:#fff}
      .daui-calc{border:1px solid #dce3e8;border-radius:6px;background:#f8fafb;padding:20px 22px;margin-top:18px}
      .daui-calc h4{color:#0d3b5e;margin:0 0 4px;font-size:15px;font-weight:700}
      .daui-calc .form-group{margin-bottom:16px}
      .daui-calc .control-label{font-weight:600;color:#263e50}
      .daui-calc .checkbox,.daui-calc .radio{margin-top:2px;margin-bottom:2px}
      .daui-group{background:#fff;border:1px solid #e3e9ee;border-radius:4px;padding:14px 16px;margin-bottom:14px}
      .daui-result{background:#fff;border:1px solid #e3e9ee;border-radius:4px;padding:18px;position:sticky;top:70px}
      .daui-total{display:flex;align-items:baseline;gap:8px;margin:4px 0 10px}
      .daui-total .big{font-size:44px;font-weight:700;color:#0d3b5e;line-height:1}
      .daui-total .of{font-size:18px;color:#607381}
      .daui-result .daui-band{display:inline-flex;flex-direction:row;padding:4px 12px;border-radius:14px;font-size:13px}
      .daui-comp{margin:14px 0 0}
      .daui-comp-head{display:flex;justify-content:space-between;font-size:13px;margin-bottom:3px}
      .daui-track{background:#eef1f4;height:10px;border-radius:5px;overflow:hidden}
      .daui-fill{height:100%;border-radius:5px}
      .daui-sub{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-top:16px;border-top:1px solid #e3e9ee;padding-top:14px}
      .daui-sub div{font-size:13px;color:#4a5e6d}
      .daui-sub strong{display:block;font-size:20px;color:#0d3b5e}
      @media (max-width:767px){
        .daui-calc .row{display:flex;flex-direction:column-reverse}
        .daui-result{position:static;margin-bottom:14px}
      }
      .daui-flag{font-size:13px;background:#fff8e4;border-left:3px solid #e8a800;padding:8px 12px;margin-top:14px}
    ")),
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Methods for Analyzing Digital Skills")
      ),

      section(NULL,
        tags$p("Once data are collected, responses to the skill questions are turned into scores. Scores can then be compared across groups and linked to outcomes. Digital skills measurement has not been standardized, so choose a scoring method that matches how your questionnaire was built.")
      ),

      section("1. Scoring Digital Skills",
        sub_head("Simple additive score"),
        tags$p("Give one point for each skill the respondent has ever performed and add the points into a raw total. This is transparent and easy to explain, but a long list of skills makes the questionnaire longer."),
        sub_head("Domain-based score"),
        tags$p("Assess competence within each skill domain used in the questionnaire, then combine the results across domains."),
        div(style = "overflow-x:auto;",
          tags$table(class = "table",
            tags$caption("Levels of competence"),
            tags$colgroup(
              tags$col(style = "width:22%"),
              tags$col(style = "width:39%"),
              tags$col(style = "width:39%")
            ),
            tags$thead(tags$tr(
              tags$th(scope = "col", "Level"),
              tags$th(scope = "col", "Within a domain"),
              tags$th(scope = "col", "Overall")
            )),
            tags$tbody(
              tags$tr(tags$th(scope = "row", "Basic"),
                tags$td("At least one skill in the domain performed"),
                tags$td("Basic competence in every domain")),
              tags$tr(tags$th(scope = "row", "Comprehensive"),
                tags$td("More than one skill in the domain performed"),
                tags$td("Comprehensive competence in more than two domains"))
            )
          )
        ),
        sub_head("Reduced-item score"),
        tags$p("A shorter set of skills can also produce a score, as long as it includes skills from every domain. The DAUI below uses a minimum set of 14 skills.")
      ),

      section("2. The Digital Access and Use Index (DAUI)",
        tags$p("The DAUI is a composite score of how well people in low-resource settings can access and use mobile phones and the internet. It looks beyond whether someone owns a phone to the quality of their access, their skills, their safety practices and their control over the phone, which makes it useful for measuring the digital gender gap. Its questions were developed through cognitive testing in India, Kenya and Nigeria."),
        div(style = "overflow-x:auto;",
          tags$table(class = "table daui-table",
            tags$caption("How the DAUI is scored"),
            tags$thead(tags$tr(
              tags$th(scope = "col", "Component"),
              tags$th(scope = "col", "What is measured"),
              tags$th(scope = "col", "Scoring"),
              tags$th(scope = "col", class = "num", "Max points"),
              tags$th(scope = "col", class = "num", "Weight")
            )),
            tags$tbody(
              score_row("Digital competency", "14 skills, asked through 19 questions",
                "1 point per skill ever performed", "14", "50%"),
              score_row("Physical access", "Ownership (A), phone type (B), phone condition (C), time of day the phone is available (D)",
                "(A × B) + C + D", "11", "39%"),
              score_row("Safety and security", "Lock on the phone; lock on banking or other apps",
                "1 point each", "2", "7%"),
              score_row("Digital agency", "Who decides who can use the phone and when",
                "1 point if the respondent decides alone", "1", "4%")
            ),
            tags$tfoot(tags$tr(
              tags$th(scope = "row", "Total"), tags$td(), tags$td(),
              tags$td(class = "num", "28"), tags$td(class = "num", "100%")
            ))
          )
        ),
        div(class = "daui-bar", role = "img",
          `aria-label` = "Share of the 28 DAUI points: digital competency 50 percent, physical access 39 percent, safety and security 7 percent, digital agency 4 percent",
          weight_seg("comp", 50), weight_seg("access", 39.3),
          weight_seg("safety", 7.1), weight_seg("agency", 3.6)
        ),
        div(class = "daui-legend",
          legend_item("comp", "Digital competency 14"),
          legend_item("access", "Physical access 11"),
          legend_item("safety", "Safety and security 2"),
          legend_item("agency", "Digital agency 1")
        ),

        sub_head("Score bands"),
        tags$p("The total score (0–28) is grouped into four bands:"),
        div(class = "daui-bands",
          band_chip("none", "No access", "0"),
          band_chip("low", "Low", "1–10"),
          band_chip("medium", "Medium", "11–20"),
          band_chip("high", "High", "21–28")
        ),
        tags$p("Because skills count toward the total, someone with no phone today who has used one in the past will score above 0. To identify people without current access, look at the physical access score as well."),

        step("Physical access scoring codes",
          tags$ul(
            tags$li(tags$strong("A. Ownership: "), "no access 0, shares a phone 1, owns a phone 2."),
            tags$li(tags$strong("B. Phone type: "), "no access 0, basic phone 1, feature phone 2, smartphone 3."),
            tags$li(tags$strong("C. Phone condition: "), "no access 0, some components not working 1, all components working 2. A phone counts as not fully working if it cannot stay on without the charger, the screen is too cracked to read, or the touchscreen or keys do not work."),
            tags$li(tags$strong("D. Time of day the phone was within reach yesterday: "), "not at all 0, evening or night only 1, morning or afternoon only 2, whole day 3.")
          ),
          tags$p("Ownership is multiplied by phone type, so a smartphone owner scores 6 and someone who shares a basic phone scores 1.")
        ),
        step("The 14 digital competency skills",
          tags$p("Some skills are asked through two questions. A yes to either question counts as one point for that skill."),
          div(style = "overflow-x:auto;",
            tags$table(class = "table table-striped",
              tags$colgroup(
                tags$col(style = "width:28%"),
                tags$col(style = "width:52%"),
                tags$col(style = "width:20%")
              ),
              tags$thead(tags$tr(
                tags$th(scope = "col", "Skill"),
                tags$th(scope = "col", "Question(s)"),
                tags$th(scope = "col", "Point if")
              )),
              tags$tbody(
                skill_row(daui_skills[1], "Have you ever typed and sent a message on WhatsApp, Facebook Messenger or other chat apps? / Have you ever written and sent an SMS text message?", "Yes to either"),
                skill_row(daui_skills[2], "After hearing an IVR audio sample: Which key would you press if you wanted to talk to a doctor?", "Task completed"),
                skill_row(daui_skills[3], "Have you ever made a phone call by dialing a number? / Have you ever made a call on WhatsApp, Facebook Messenger or any other such app?", "Yes to either"),
                skill_row(daui_skills[4], "Have you ever shared a document, picture or video through a message on WhatsApp, Facebook Messenger or other such apps?", "Yes"),
                skill_row(daui_skills[5], "Have you ever taken a photo with a mobile phone? / Have you ever taken a video with a mobile phone?", "Yes to either"),
                skill_row(daui_skills[6], "Have you ever used Facebook, Instagram or any similar social media app on a mobile phone?", "Yes"),
                skill_row(daui_skills[7], "Have you ever made a reel, story or shorts on YouTube, Facebook, Instagram and so on?", "Yes"),
                skill_row(daui_skills[8], "Have you ever downloaded an app on a mobile phone?", "Yes"),
                skill_row(daui_skills[9], "Have you ever created a hotspot using a mobile phone?", "Yes"),
                skill_row(daui_skills[10], "Have you ever searched for information on the internet (for example, Google or YouTube)?", "Yes"),
                skill_row(daui_skills[11], "Have you ever used a mobile phone to scan a QR code? / …to scan a QR code and buy something?", "Yes to either"),
                skill_row(daui_skills[12], "Have you ever used [local payment apps] to receive money? / …to send money?", "Yes to either"),
                skill_row(daui_skills[13], "Can you access your bank account using your mobile phone?", "Yes"),
                skill_row(daui_skills[14], "Have you ever blocked a number on a mobile phone?", "Yes")
              )
            )
          )
        ),

        sub_head("Try it: DAUI calculator"),
        tags$p("Enter one respondent’s answers to see how the score is built up."),
        div(class = "daui-calc",
          fluidRow(
            column(7,
              div(class = "daui-group",
                tags$h4("Physical access"),
                radioButtons("daui_own", "A. Phone ownership",
                  choices = c("No access" = 0, "Shares a phone" = 1, "Owns a phone" = 2),
                  selected = 2),
                conditionalPanel("input.daui_own != '0'",
                  radioButtons("daui_type", "B. Phone type",
                    choices = c("Basic phone" = 1, "Feature phone" = 2, "Smartphone" = 3),
                    selected = 1, inline = TRUE),
                  radioButtons("daui_cond", "C. Phone condition",
                    choices = c("Some components not working" = 1, "All components working" = 2),
                    selected = 2),
                  radioButtons("daui_time", "D. Phone within reach yesterday",
                    choices = c("Not at all" = 0, "Evening or night only" = 1,
                                "Morning or afternoon only" = 2, "Whole day" = 3),
                    selected = 3)
                )
              ),
              div(class = "daui-group",
                tags$h4("Safety and agency"),
                checkboxInput("daui_lock_phone", "Has a lock, PIN or passcode on the phone", FALSE),
                checkboxInput("daui_lock_app", "Has a lock on banking or other apps", FALSE),
                checkboxInput("daui_agency", "Decides alone who can use the phone and when", FALSE)
              ),
              div(class = "daui-group",
                tags$h4("Digital competency"),
                checkboxGroupInput("daui_skills", "Skills ever performed",
                  choices = daui_skills,
                  selected = daui_skills[c(1, 3, 5)])
              )
            ),
            column(5, uiOutput("daui_result"))
          )
        )
      ),

      section("3. Analyzing the Results",
        sub_head("Compare groups"),
        tags$p("Compare score distributions across groups such as women and men, age groups, or rural and urban areas. Looking at the whole distribution, not only the average, shows where gaps are concentrated."),
        sub_head("Separate use from access"),
        tags$p("Physical access often drives differences in the total score. Analyzing a use score (skills 14 + safety 2 + agency 1 = 17 points) on its own shows how people use the phones they can reach."),
        sub_head("Control for type of access"),
        tags$p("Restrict comparisons to people with the same kind of access, for example smartphone owners only. Remaining differences in use are then not explained by phone type or access quality."),
        sub_head("Link scores to outcomes"),
        tags$p("Use skill scores as predictors or stratifiers to examine links with social and economic participation. The High Impact Use Case framework groups uses into domains such as health, economic activity, online learning, e-governance and agriculture. Check whether people with higher scores are more likely to take part in these areas."),
        tags$p("In regression analysis, include the index as a categorical variable or the raw score as a continuous variable. These comparisons show associations; on their own they do not establish that skills cause the outcome.")
      )
    )
  )
}

analysis_server <- function(input, output, session) {
  daui <- reactive({
    own <- as.integer(input$daui_own)
    if (length(own) == 0 || own == 0) {
      phys <- 0L
    } else {
      phys <- own * as.integer(input$daui_type) + as.integer(input$daui_cond) + as.integer(input$daui_time)
    }
    comp   <- length(input$daui_skills)
    safety <- as.integer(isTRUE(input$daui_lock_phone)) + as.integer(isTRUE(input$daui_lock_app))
    agency <- as.integer(isTRUE(input$daui_agency))
    list(phys = phys, comp = comp, safety = safety, agency = agency,
         total = phys + comp + safety + agency, use = comp + safety + agency)
  })

  output$daui_result <- renderUI({
    s    <- daui()
    band <- daui_band(s$total)
    comp_bar <- function(label, value, max, colour) {
      div(class = "daui-comp",
        div(class = "daui-comp-head", tags$span(label), tags$span(paste(value, "/", max))),
        div(class = "daui-track",
          div(class = "daui-fill", style = sprintf("width:%.1f%%;background:%s", 100 * value / max, colour))
        )
      )
    }
    div(class = "daui-result", `aria-live` = "polite",
      tags$h4("DAUI score"),
      div(class = "daui-total", tags$span(class = "big", s$total), tags$span(class = "of", "/ 28")),
      div(class = paste("daui-band", band$class), tags$strong(band$label)),
      comp_bar("Digital competency", s$comp, 14, "#0d3b5e"),
      comp_bar("Physical access", s$phys, 11, "#3d7ea6"),
      comp_bar("Safety and security", s$safety, 2, "#e8a800"),
      comp_bar("Digital agency", s$agency, 1, "#b5651d"),
      div(class = "daui-sub",
        div(tags$strong(paste(s$use, "/ 17")), "Use score"),
        div(tags$strong(paste(s$phys, "/ 11")), "Physical access score")
      ),
      if (s$phys == 0 && s$total > 0)
        div(class = "daui-flag", "This respondent has no current phone access but scores above 0 because of other components.")
    )
  })
}
