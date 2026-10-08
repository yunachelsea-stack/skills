analysis_tab <- function() {
  section     <- tab_section
  step        <- tab_step
  source_note <- tab_source_note
  trow        <- tab_trow

  tabPanel("Analyzing Digital Skills", value = "analysis",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$small("RESOURCE 06 · PHASE FOUR: DATA ANALYTICS"),
        tags$h2("Methods for Analyzing Digital Skills"),
        tags$p("Group skills by competency, construct scores, explore digital access and use, and relate skills to social and economic participation."),, Chapter 10, sections 10.1–10.4.")
      ),
      section("Choose an approach that fits the study",
        tags$p("The report notes that digital skills measurement has not been standardized. It describes several approaches rather than prescribing one universal measure."),
        div(class = "ca-note",
          tags$strong("Analytical guidance, not a scoring engine"),
          tags$p("This tab summarizes the report. It does not analyze respondent data, calculate scores or change Survey Builder selections. Study findings below are attributed examples, not results from your survey.")
        )
      ),
      section("Competency-Based Analyses",
        tags$p("Group individual skills into broader domains to compare strengths and gaps within and across areas of competence. Alignment with international frameworks can support comparability."),
        step("Organize skills into competence domains",
          tags$ul(
            tags$li("Communication and collaboration."),
            tags$li("Information and data literacy."),
            tags$li("Safety and privacy."),
            tags$li("Digital content creation."),
            tags$li("Problem-solving or technical use.")
          ),
          tags$p("The report also describes more intuitive “functional domains” tailored to study objectives, the skills measured and stakeholder needs.")
        ),
        step("Maintain coverage when shortening the questionnaire",
          tags$p("The report recommends covering at least one or two skills per domain, even when using a reduced set of questions, so assessments span the range of competencies."),
          tags$p("Within-domain basic and comprehensive competence can distinguish having performed one skill from having performed multiple skills. The specific definitions used in section 10.2 are summarized below.")
        ),
      ),
      section("Digital Skills Score or Index",
        step("Simple additive scoring",
          tags$p("Assign one point for each skill ever performed and sum the points into a raw total. This is transparent and straightforward, although a longer list of skills increases questionnaire length.")
        ),
        step("Domain-based scoring",
          tags$p("Assess competence within each area, then aggregate across areas. The report gives these definitions:"),
          tags$ul(
            tags$li(tags$strong("Basic competence in an area: "), "Proficiency in at least one item in that area."),
            tags$li(tags$strong("Overall basic competence: "), "Basic competence in all areas."),
            tags$li(tags$strong("Comprehensive competence in an area: "), "Proficiency in more than one item in that area."),
            tags$li(tags$strong("Overall comprehensive competence: "), "Comprehensive competence in more than two areas.")
          )
        ),
        step("Reduced-item scoring",
          tags$p("A shorter set of questions can be used to create a score, provided it includes skills from all domains. Section 10.3 describes the report’s proposed minimum digital competency set.")
        ),
      ),
      section("Digital Access and Use Index (DAUI)",
        tags$p("DAUI is a composite measure of access to and use of mobile phone and internet technologies in low-resource settings. It extends beyond device ownership to consider access quality, skills, agency, safety and the real-life relevance of digital activities."),
        tags$p("The accompanying questions were developed following cognitive testing in India, Kenya and Nigeria."),
        step("Broader components of digital access and use",
          tags$p("Table 10.1 describes the broader measurement framework; the scoring subset in Tables 10.2–10.3 does not score every component separately."),
          tags$ul(
            tags$li(tags$strong("Access: "), "Connectivity (network, SIM cards and electricity); physical access (ownership, sharing, phone type, condition and timing); and affordability."),
            tags$li(tags$strong("Use: "), "Digital competency; safety and security; social norms and attitudes; and digital agency, including decision-making, permissions and restrictions.")
          ),
        ),
        step("Physical access, safety and agency scoring",
          tags$p(tags$strong("Physical access: "), "The report gives the formula (A × B) + C + D."),
          tags$ul(
            tags$li("A — Ownership: no access = 0, sharer = 1, owner = 2."),
            tags$li("B — Phone type: no access = 0, basic phone = 1, feature phone = 2, smartphone = 3."),
            tags$li("C — Phone condition: no access = 0, some components not working = 1, all components working = 2."),
            tags$li("D — Timing of access: no access/not at all = 0, evening or night only = 1, morning or afternoon only = 2, whole day = 3.")
          ),
          tags$p(tags$strong("Safety and security: "), "One point for a lock on the phone and one for a lock on a banking app."),
          tags$p(tags$strong("Digital agency: "), "One point if the respondent alone decides who can use the phone and when."),
          tags$p("These are summaries of the report’s scoring categories, not a complete implementation specification for missing responses or every possible combination of answers."),
        ),
        step("The 14 digital competency skills",
          tags$p("Table 10.2 uses 19 questions to cover 14 skills. Where questions are paired, a positive response to either counts for the single skill, rather than two separate points."),
          tags$ol(
            tags$li("Typed and sent a chat-app message or an SMS."),
            tags$li("Navigated an interactive voice response (IVR) prompt successfully."),
            tags$li("Made a call by dialing a number or using a calling app."),
            tags$li("Shared a document, picture or video through a messaging app."),
            tags$li("Taken a photo or video."),
            tags$li("Used a social media app."),
            tags$li("Created a reel, story or short on social media."),
            tags$li("Downloaded an app."),
            tags$li("Created a mobile hotspot."),
            tags$li("Searched for information on the internet."),
            tags$li("Scanned a QR code, including to buy something."),
            tags$li("Sent or received money through a payment app."),
            tags$li("Accessed a bank account using a mobile phone."),
            tags$li("Blocked a phone number.")
          ),
          tags$p("The report describes competency as x out of 14 skills, with up to 14 points contributing to the composite index. The IVR item is recorded as task completion."),
        ),
        div(style = "overflow-x:auto;",
          tags$table(class = "table table-striped",
            tags$caption("DAUI component maximum scores"),
            tags$thead(tags$tr(tags$th(scope = "col", "Component"), tags$th(scope = "col", "Maximum points"))),
            tags$tbody(
              trow("Digital competency", "14"),
              trow("Ownership and phone type", "6"),
              trow("Phone condition", "2"),
              trow("Access during the day", "3"),
              trow("Lock on phone", "1"),
              trow("Lock on banking app", "1"),
              trow("Decision-making over phone use", "1"),
              trow("Total shown in Table 10.3", "28")
            )
          )
        ),
        div(class = "ca-note",
          tags$strong("Source inconsistency: confirm category limits before scoring"),
          tags$p("The narrative lists No Access (0), Low (1–10), Medium (11–20) and High (21–29), but Table 10.3 totals 28 possible points. Both are reported here as written; this toolkit does not resolve the discrepancy or apply these categories automatically.")
        ),
        step("Interpret subgroup differences and access constraints",
          tags$p("The report illustrates comparisons between men and women, separates the use component from access, and restricts comparisons to smartphone users to explore usage patterns within the same phone-access group."),
          tags$p("Table 10.4 reports a competency mean of 7.4 and median of 8.0 for men, compared with a mean of 3.7 and median of 2.0 for women in the Bihar study, on the 14-skill measure."),
          tags$p("These are study-specific findings. No distributions or charts are reconstructed here from unavailable underlying respondent data.")
        )
      ),
      section("Outcome Analysis",
        tags$p("Use digital skills scores as predictors or stratifiers to examine links with social and economic participation."),
        tags$p("The report’s High Impact Use Case examples include health, economic activity, online learning, eGovernance and agriculture. Compare participation across skill levels to examine the real-world relevance of skills."),
        tags$p("In regression analysis, an index can be included as a categorical covariate, or the raw score can be included as a continuous covariate. Such comparisons describe relationships; they do not by themselves establish a causal effect.")
      )
    )
  )
}
