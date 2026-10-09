implementation_tab <- function() {
  section        <- tab_section
  step           <- tab_step
  comparison_row <- function(aspect, observed, reported) {
    tags$tr(tags$th(scope = "row", aspect), tags$td(observed), tags$td(reported))
  }

  tabPanel("Survey Implementation", value = "implementation",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Implementing the Digital Skills Measurement Survey")
      ),

      section(NULL,
        tags$p("Designing a digital skills survey involves three sets of decisions: how the survey is administered, how skills are measured, and how individual skill items are framed. Each involves trade-offs between accuracy, feasibility, and respondent burden.")
      ),

      section("1. Mode of Administration",
        step("Facilitated (in-person) surveys",
          tags$p("An interviewer supports the respondent through the survey. This can reduce errors caused by misunderstanding, misinterpretation, or low confidence in navigating the questionnaire independently. Facilitated surveys typically achieve higher response rates and allow assessment of a broader and deeper range of skills, including observed tasks. However, they require trained interviewers and additional time and resources. For most digital skills surveys, especially in populations with limited access or literacy, a facilitated survey is the recommended starting point.")
        ),
        step("Phone surveys",
          tags$p("Phone surveys are faster and less costly than in-person interviews, and still allow an interviewer to clarify questions. However, they reach only people who can be contacted by phone. They also leave out people without their own phone or with limited access to one, which are the groups digital inclusion surveys most need to capture. Observed skills assessments are also difficult to conduct by phone.")
        ),
        step("Self-administered surveys",
          tags$p("Self-administered surveys can be more efficient and cost-effective. However, online platforms often require respondents to use a phone or computer and log in, assuming a baseline level of digital skill."),
          tags$ul(
            tags$li("Limited device access and low literacy can prevent participation or completion."),
            tags$li("Complex constructs may be difficult to measure without interviewer support."),
            tags$li("Online surveys may have lower response rates."),
            tags$li("It can be difficult to verify who completed the questionnaire.")
          )
        ),
        div(class = "ca-note",
          tags$strong("A note on selection bias"),
          tags$p("Phone and online modes reach only people who already have some digital access and ability. When the survey's purpose is to measure that access and ability, these modes will tend to overstate digital skills in the wider population. Results from such surveys should be interpreted as describing connected respondents, not the population as a whole.")
        ),
        tags$p(
          tags$strong("Paper vs. digital data collection."),
          " Paper remains practical in some low-resource settings. Digital tools such as computer-assisted personal interviewing (CAPI) require investment but streamline data management, strengthen quality assurance and control, reduce errors, and shorten processing time."
        )
      ),

      section("2. Measuring Skills: Reported vs. Observed",
        tags$p("Self-reported questions ask whether respondents have ever performed a task. Observed assessments ask them to demonstrate it during the interview. Direct observation provides evidence of task performance, while self-reporting relies on what respondents recall and believe they can do. Observation is generally regarded as the stronger form of validation, but it can be impractical in many survey contexts."),
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
              comparison_row("Time, resources and scale", "More time-consuming; requires trained enumerators; less practical in large samples", "Faster, less resource-intensive, and more practical for large surveys"),
              comparison_row("Privacy and ethics", "Sensitive photos or messages may be exposed", "Lower privacy risks"),
              comparison_row("Task-related costs", "Demonstrations may use data, calls, or SMS", "No direct demonstration costs"),
              comparison_row("Bias risks", "Avoids reporting bias, but performance can be affected by nervousness, an unfamiliar device, or the presence of the enumerator", "Over- or under-reporting is possible"),
              comparison_row("Best use", "When precision is critical and resources allow", "When scale, efficiency, and feasibility are priorities")
            )
          )
        ),
      ),

      section("3. Framing Skill Items",
        step("Assisted vs. self-performed skills",
          tags$p("Record whether help was needed the last time the respondent carried out a task. This adds detail about actual ability. In Bihar, the gap between performing a skill independently and with assistance was at most 5 percentage points across the skills assessed. This held even for more advanced activities, such as creating a social media account or making calls through an app.")
        ),
        step("Ever performed, recency, and frequency",
          tags$p("Recency captures how recently a skill was performed and is easier to standardize. Frequency captures how often a skill is performed and can provide richer insights, but is more prone to recall error. The recommended approach is to collect “ever performed” skill items, supplemented with recency or frequency questions where relevant. This is consistent with cognitive interview findings. Questions tied to a fixed period, such as “in the last 12 months,” were difficult for respondents, while asking “Have you ever…?” followed by “When was the last time…?” worked better.")
        )
      )
    )
  )
}
