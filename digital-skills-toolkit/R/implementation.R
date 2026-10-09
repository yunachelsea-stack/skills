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
        tags$h2("Implementing the Digital Skills Measurement Survey")
      ),
      section(“Modality of Survey Implementation”,
        tags$p(“Implementation involves several separate choices: how skills are measured, whether assistance is recorded, which time frame questions capture, and whether responses are collected on paper or digitally.”),
        div(style = “border:1px solid #dce3e8; border-radius:4px; background:#fff;”,
          div(style = “padding:14px 18px; border-bottom:1px solid #dce3e8;”,
            tags$p(style = “margin:0;”,
              tags$strong(“Self-reported vs. observed skills: “),
              “Self-reported questions ask whether respondents have ever performed a task; observed assessments ask them to demonstrate it during the interview. Self-reports are quicker and may be sufficient when logistics prevent demonstrations. Observation provides stronger validation but requires more time and resources.”
            )
          ),
          div(style = “padding:14px 18px; border-bottom:1px solid #dce3e8;”,
            tags$p(style = “margin:0;”,
              tags$strong(“Assisted vs. self-performed skills: “),
              “Record whether help was needed the last time the respondent carried out a task. This adds detail about actual ability, including for more advanced activities such as creating a social media account or making calls through an app.”
            )
          ),
          div(style = “padding:14px 18px; border-bottom:1px solid #dce3e8;”,
            tags$p(style = “margin:0;”,
              tags$strong(“Recency vs. frequency: “),
              “Recency captures how recently a skill was performed and is easier to standardize. Frequency captures how often and can provide richer insights, but is more prone to recall error. The report recommends collecting “ever performed” skill items, supplemented with recency or frequency questions where relevant.”
            )
          ),
          div(style = “padding:14px 18px;”,
            tags$p(style = “margin:0;”,
              tags$strong(“Paper vs. digital data collection: “),
              “Paper remains practical in some low-resource settings. Digital tools such as computer-assisted personal interviewing (CAPI) require investment but streamline data management, strengthen quality assurance and control, reduce errors, and shorten processing time. Select the modality in light of the setting, available resources and survey type.”
            )
          )
        )
      ),
      section("Facilitated vs. Self-Administered Surveys",
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
        )
      ),
      section("Measurement Method (Observed vs. Reported)",
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
        )
      ),
      section("Practical considerations before fieldwork",
        tags$ol(
          tags$li(tags$strong("Choose the right method. "), "Not all skills can be observed. Select observation or self-reporting based on feasibility, study objectives and resource constraints."),
          tags$li(tags$strong("Use familiar devices. "), "Assess skills on the device the respondent regularly uses, whether personally owned or employer-provided, to reflect real-world ability."),
          tags$li(tags$strong("Protect privacy. "), "Train researchers to handle sensitive content that may appear during demonstrations, such as text messages or photos."),
          tags$li(tags$strong("Minimize respondent costs. "), "Avoid unintended expenses from mobile data, SMS or calls when designing observed tasks."),
          tags$li(tags$strong("Balance the trade-offs. "), "Consider safety, feasibility and data robustness together, without imposing unnecessary risks or burdens.")
        )
      )
    )
  )
}
