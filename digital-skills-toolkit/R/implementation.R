implementation_tab <- function() {
  section     <- tab_section
  step        <- tab_step
  source_note <- tab_source_note
  comparison_row <- function(aspect, observed, reported) {
    tags$tr(tags$th(scope = "row", aspect), tags$td(observed), tags$td(reported))
  }

  tabPanel("Survey Implementation", value = "implementation",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$small("RESOURCE 04 · PHASE THREE: SURVEY IMPLEMENTATION"),
        tags$h2("Implementing the Digital Skills Measurement Survey"),
        tags$p("Choose how to administer the survey and measure skills while balancing accuracy, feasibility and respondent burden."),
        tags$p(class = "ca-source",
          "Adapted from the supplied Digital Skills Measurement Toolkit (2026), Chapter 8, sections 8.1–8.3, Boxes 8.1–8.2 and Table 8.1. This guidance does not automatically change Survey Builder questions.")
      ),
      section("8.1 Modality of Survey Implementation",
        tags$p("Implementation involves several separate choices: how skills are measured, whether assistance is recorded, which time frame questions capture, and whether responses are collected on paper or digitally."),
        step("Self-reported vs. observed skills",
          tags$p("Self-reported questions ask whether respondents have ever performed a task. Observed assessments ask respondents to demonstrate a task during the interview."),
          tags$p("Self-reports are quicker and may be sufficient when time or logistics prevent demonstrations. Observation provides stronger validation but requires more time. Consider the measurement trade-offs in section 8.3 below.")
        ),
        step("Assisted vs. self-performed skills",
          tags$p("Record whether help was needed the last time the respondent carried out a task. Reporting that a task was completed does not necessarily mean it was completed independently."),
          tags$p("This distinction adds detail about actual ability, including for more advanced activities such as creating a social media account or making calls through an app.")
        ),
        step("Recency vs. frequency",
          tags$p("Recency captures how recently a skill was performed and is easier to standardize. Frequency captures how often it is performed and can provide richer insights, but is more prone to recall error."),
          div(class = "ca-note",
            tags$strong("The report’s recommendation"),
            tags$p("Collect “ever performed” skill items, supplemented with recency or frequency questions where relevant.")
          )
        ),
        step("Paper vs. digital data collection",
          tags$p("Paper remains a practical option in some low-resource settings because of its familiarity, relative ease and cost."),
          tags$p("Digital tools such as computer-assisted personal interviewing (CAPI) require investment but can streamline data management, strengthen quality assurance and control, reduce errors, and shorten data entry and processing time. The report emphasizes their value for large-scale surveys."),
          tags$p("Select the modality in light of the setting, available resources and survey type.")
        ),
        source_note("section 8.1 and Box 8.1.")
      ),
      section("8.2 Facilitated vs. Self-Administered Surveys",
        step("Self-administered surveys: efficiency with access limitations",
          tags$p("Self-administered surveys can be more efficient and cost-effective. However, online platforms often require respondents to use a phone or computer and log in, assuming a baseline level of digital skill."),
          tags$ul(
            tags$li("Limited device access and low literacy can prevent participation or completion."),
            tags$li("Complex constructs may be difficult to measure without interviewer support."),
            tags$li("Online surveys may have lower response rates."),
            tags$li("It can be difficult to verify who completed the questionnaire.")
          )
        ),
        step("Facilitated surveys: interviewer support",
          tags$p("An interviewer supports the respondent through the survey. This can reduce errors caused by misunderstanding, misinterpretation or low confidence in navigating the questionnaire independently."),
          tags$p("Facilitated surveys typically achieve higher response rates and allow assessment of a broader and deeper range of skills, but require trained interviewers and additional time and resources.")
        ),
        div(class = "ca-note",
          tags$strong("Start with the population’s needs"),
          tags$p("Given digital-access and literacy barriers, the report often recommends facilitated surveys as a starting point. The administration method should fit the population being surveyed.")
        ),
        source_note("section 8.2.")
      ),
      section("8.3 Measurement Method (Observed vs. Reported)",
        tags$p("Direct observation provides evidence of task performance, while self-reporting relies on what respondents recall and believe they can do. Observation is useful for validation but can be impractical in some survey contexts."),
        div(style = "overflow-x:auto;",
          tags$table(class = "table table-striped",
            tags$caption("Observed and self-reported digital skills: trade-offs"),
            tags$thead(tags$tr(
              tags$th(scope = "col", "Aspect"),
              tags$th(scope = "col", "Observed / demonstrated"),
              tags$th(scope = "col", "Self-reported")
            )),
            tags$tbody(
              comparison_row("Validity", "Direct evidence of ability", "Relies on recall and perception"),
              comparison_row("Time and resources", "More time-consuming; requires trained enumerators", "Faster and less resource-intensive"),
              comparison_row("Privacy and ethics", "Sensitive photos or messages may be exposed", "Lower privacy risks"),
              comparison_row("Task-related costs", "Demonstrations may use data, calls or SMS", "No direct demonstration costs"),
              comparison_row("Bias risks", "The report describes minimal bias in recorded ability", "Over- or under-reporting is possible"),
              comparison_row("Feasibility", "Less practical at scale; can burden large samples", "More practical for large surveys"),
              comparison_row("Key limitation", "Logistical difficulty and respondent burden", "Some people report “never” but can actually perform the task"),
              comparison_row("Best use", "When precision is critical and resources allow", "When scale, efficiency and feasibility are priorities")
            )
          )
        ),
        source_note("Table 8.1, adapted. SMS = Short Message Service."),
        step("What the report’s Bihar study found",
          tags$p("The report cites Date et al. (2026): across 16 assessed digital skills, the mean difference between observed and reported estimates was about 2 percent among men and women."),
          tags$p("It also reports a maximum gap of 5 percent between independent and assisted performance across the skills evaluated, with minimal differences for most skills."),
          tags$p("These are findings from the study described in the report, not a guarantee that self-reports will match demonstrations in every population."),
          source_note("sections 8.1 and 8.3, citing Date et al. (2026).")
        )
      ),
      section("Practical considerations before fieldwork",
        tags$ol(
          tags$li(tags$strong("Choose the right method. "), "Not all skills can be observed. Select observation or self-reporting based on feasibility, study objectives and resource constraints."),
          tags$li(tags$strong("Use familiar devices. "), "Assess skills on the device the respondent regularly uses, whether personally owned or employer-provided, to reflect real-world ability."),
          tags$li(tags$strong("Protect privacy. "), "Train researchers to handle sensitive content that may appear during demonstrations, such as text messages or photos."),
          tags$li(tags$strong("Minimize respondent costs. "), "Avoid unintended expenses from mobile data, SMS or calls when designing observed tasks."),
          tags$li(tags$strong("Balance the trade-offs. "), "Consider safety, feasibility and data robustness together, without imposing unnecessary risks or burdens.")
        ),
        source_note("section 8.3 and Box 8.2.")
      )
    )
  )
}
