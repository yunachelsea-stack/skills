adaptations_tab <- function() {
  section     <- tab_section
  step        <- tab_step
  source_note <- tab_source_note

  qa_pair <- function(component, issue, original, revised) {
    div(class = "ca-example",
      div(class = "ca-example-head", component),
      div(class = "ca-example-body",
        tags$p(class = "ca-example-issue", issue),
        div(class = "ca-qa-pair",
          div(class = "ca-qa-original",
            div(class = "ca-qa-label", "Original question"),
            tags$p(original)
          ),
          div(class = "ca-qa-revised",
            div(class = "ca-qa-label", "Revised question"),
            tags$p(revised)
          )
        )
      )
    )
  }

  numbered_step <- function(n, title, ...) {
    tags$details(class = "ca-step",
      tags$summary(
        tags$span(class = "ca-step-num", n),
        title
      ),
      div(class = "ca-step-body", ...)
    )
  }

  tabPanel("Conceptual Adaptations", value = "adaptations",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Contextual Adaptations through Cognitive Interviews"),
      ),

      section(NULL,
        tags$p("Cognitive interviewing is a qualitative method used to debug survey questions — checking how respondents interpret items, retrieve memories, and map answers to options — so the final instrument actually measures what you intend, especially across language and cultural gaps."),
        tags$h4("How cognitive interviews fit into tool development"),
        div(class = "ca-flow",
          div(class = "ca-flow-step", "Item generation"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Translation"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Cognitive interviews"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Revise & retest"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Pilot testing"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Survey implementation")
        ),
      ),

      section("Five steps for conducting cognitive interviews",
        numbered_step("1", "Define the scope",
          tags$p("Prioritize newly developed, conceptually complex or central questions, especially those entering a new cultural or linguistic setting. Testing every question is ideal but may not be feasible because probing takes time."),
          tags$p("For longer instruments, divide priority questions into smaller interview guides while retaining a logical flow. In large surveys, researchers often create multiple guides, each covering a different section, so the workload stays manageable.")
        ),
        numbered_step("2", "Select and train researchers",
          tags$p("Recruit researchers fluent in the source and local languages, skilled in qualitative interviewing, and familiar with structured survey administration. Consider how gender, age and social background may affect rapport."),
          tags$p("Training should cover study objectives, question intent, research ethics and probing technique. Use role plays and practice interviews to help researchers shift between asking questions exactly as written and exploring understanding in depth.")
        ),
        numbered_step("3", "Select sample participants",
          tags$p("Recruit people with the same or a similar profile to the intended survey population. Include people with lower literacy or education, limited survey experience, and marginalized groups whose difficulties reveal inaccessible wording."),
          tags$p("The report describes two to three rounds with approximately 8–12 participants per round, revising and retesting after each round. Cover relevant experiences and oversample rare experiences where necessary."),
        ),
        numbered_step("4", "Collect data iteratively, debrief daily and analyse near real-time",
          tags$p("Typically, one researcher interviews while another takes detailed notes. Ask the original question, record the answer, then use scripted or emerging probes to explore confusing words, interpretation and response choices."),
          tags$p("After a small set of interviews, debrief and systematically review each question, document emerging issues, propose revisions, and test revised wording in the next round. Repeat until interpretation consistently matches question intent.")
        ),
        numbered_step("5", "Feed findings into enumerator training",
          tags$p("Involve cognitive-interview researchers in quantitative enumerator training. They can explain why certain wording was chosen, flag common misunderstandings, and highlight culturally sensitive issues."),
          tags$p("This step strengthens the link between tool development and field deployment, reducing the likelihood of improvisation or unintentional misinterpretation in the field.")
        )
      ),

      section("What to look for — and how wording can change",
        qa_pair(
          "Word choice",
          "Some respondents did not recognize WhatsApp or YouTube use as internet use; “any location and any device” distracted respondents away from the core question.",
          "Have you ever used the internet from any location and any device?",
          "Have you ever used the internet? For example, WhatsApp, Facebook, YouTube, Google, and so on [add other locally relevant examples of internet]."
        ),

        qa_pair(
          "Syntax — simplify long or complex sentences",
          "Mentioning phone types in the question led respondents to focus on the type of phone rather than whether they had ever used one at all.",
          "Have you ever used a mobile phone? This could be any type of mobile phone, including a smartphone.",
          "Have you ever used a mobile phone?"
        ),

        qa_pair(
          "Question structure — use stand-alone questions",
          "Stem-and-leaf style questions (one opening instruction followed by subquestions A, B, C…) placed a high cognitive burden on respondents; most did not retain information from the “stem.”",
          "I will now ask you about activities you may have done on a computer or phone during the last 3 months. Did you… A. Use a copy-and-paste tool… B. Send a message with an attached file…",
          "Separate each activity into its own stand-alone question."
        ),

        qa_pair(
          "Response options — test whether distinctions resonate",
          "Likert-scale options (1 = Not true for me … 5 = Very true) were frequently ignored; respondents collapsed them into a simple yes/no.",
          "I know how to make a phone call by dialing a number. (1 Not true for me — 5 Very true for me)",
          "Do you know how to make a phone call by dialing a number? (Yes / No)"
        ),

        qa_pair(
          "Resonance with local realities — avoid email-centric wording",
          "“Attached file” is email-centric and may not resonate with mobile-first users who primarily use SMS or WhatsApp.",
          "Did you send a message, for example by e-mail or SMS, with an attached file, for example a document, picture, or video?",
          "Have you ever added a picture, video, or document to an email, SMS, or WhatsApp message?"
        ),

        qa_pair(
          "Cognitive mismatch — explain ambiguous concepts",
          "“Personal information” was often understood as family secrets or private thoughts rather than identifying digital details. An explainer box was added.",
          "When using the internet on a mobile phone, have you experienced any of the following situations in the last 12 months? A. Having personal information or photos used, taken, or shared without your consent…",
          "Has someone ever shared your personal information on the internet without your permission? [Explainer: Personal information means any details that can be used to identify you, such as your name, contact details, photos, ID numbers, bank details, or location.]"
        ),

        qa_pair(
          "Memory — separate experience from timing",
          "Fixed recall windows required respondents to calculate dates while recalling activities, leading many to respond in natural language rather than fitting their answer into the defined time frame.",
          "In the last 12 months, have you used the internet?",
          "Have you ever used the internet? — followed by — When was the last time you used the internet?"
        ),

      ),

      section("Pilot Testing",
        tags$p("Pilot testing evaluates the operational performance of the complete survey under field conditions. Where cognitive interviews ask “Are respondents understanding questions as intended?”, pilot testing asks “Can this survey be implemented smoothly?”"),
        div(style = "overflow-x:auto;",
          tags$table(class = "table table-striped",
            tags$caption("Cognitive interviewing vs. pilot testing"),
            tags$thead(tags$tr(
              tags$th(scope = "col", ""),
              tags$th(scope = "col", "Cognitive interviewing"),
              tags$th(scope = "col", "Pilot testing")
            )),
            tags$tbody(
              tags$tr(tags$th(scope = "row", "Focus"), tags$td("Cognitive match between intent and interpretation"), tags$td("Practical feasibility of implementation")),
              tags$tr(tags$th(scope = "row", "What it catches"), tags$td("Cognitive failures requiring qualitative probing"), tags$td("Obvious content or translation problems")),
              tags$tr(tags$th(scope = "row", "Researchers"), tags$td("Specially trained qualitative researchers"), tags$td("Quantitative survey team")),
              tags$tr(tags$th(scope = "row", "Scope"), tags$td("A prioritized subset of questions"), tags$td("The whole survey tool")),
              tags$tr(tags$th(scope = "row", "Key output"), tags$td("Refined question wording and improved validity"), tags$td("Refined survey tool and implementation plan"))
            )
          )
        ),
        tags$p("Pilot the refined instrument before full-scale implementation to test question sequence, survey length, skip patterns, programming logic, interviewer instructions, respondent burden and logistics.")
      )
    )
  )
}
