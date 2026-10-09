quality_tab <- function() {
  section  <- tab_section
  step     <- tab_step

  tabPanel("Data Quality Assurance", value = "quality",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Quality Assurance and Quality Control")
      ),

      section(NULL,
        div(class = "ca-note",
          tags$strong("Further reading"),
          tags$p("More detailed guidance on data quality assurance is available from:"),
          tags$ul(
            tags$li(tags$a(href = "https://dimewiki.worldbank.org/Data_Quality_Assurance_Plan", target = "_blank", "World Bank DIME Wiki: Data Quality Assurance Plan")),
            tags$li(tags$a(href = "https://www.povertyactionlab.org/resource/data-quality-checks", target = "_blank", "J-PAL: Data Quality Checks")),
            tags$li(tags$a(href = "https://data.poverty-action.org/data-collection/", target = "_blank", "IPA: Data Collection"))
          )
        ),
        tags$p(tags$strong("Quality assurance"), " is about planning preventive processes that ensure adherence to protocols and early detection of errors."),
        tags$p(tags$strong("Quality control"), " focuses on monitoring outputs from those processes and verifying that they meet established standards."),
        tags$p(tags$strong("Quality improvement"), " is a proactive effort to continuously strengthen quality assurance and quality control systems."),
        tags$p("A robust QA/QC framework combines an error detection pipeline with targeted, timely feedback. Its value lies in detecting and resolving problems during data collection, not only after fieldwork ends.")
      ),

      section("1. Building Checks into the Survey Tool",
        tags$p("During tool development, survey questions should be assessed for potential opportunities for error, and appropriate safeguards should be designed to prevent or capture errors early. Once these safeguards are built into the survey tool itself, the next step is to define rules for identifying other errors as data are processed, either in real time during data collection or after.")
      ),

      section("2. Three Layers of Checks",
        tags$p("The framework uses three complementary layers: real-time checks prevent errors at the point of data entry, rule-based flags catch clear errors, and machine learning–based anomaly detection identifies subtle patterns suggestive of poor-quality or fabricated data."),
        step("Layer 1: Real-time checks during data collection",
          tags$p("Embed logical skips and range checks in the CAPI tool to catch inconsistencies and data entry errors in real time. Validation rules should be used selectively—too many can slow interviews. Complement automated checks with supervisor spot checks, partial interview observations, and re-interviews of a subsample, typically 5–10 percent.")
        ),
        step("Layer 2: Rule-based error flags",
          tags$p("Set binary flags for key error types, including:"),
          tags$ul(
            tags$li("Logical inconsistencies (for example, contradictory responses)"),
            tags$li("Range violations (for example, impossible ages or durations)"),
            tags$li("Interview duration thresholds (flagging rushed interviews)"),
            tags$li("Duplicate or missing identifiers")
          ),
          tags$p("Update rules iteratively as new error patterns emerge during data collection.")
        ),
        step("Layer 3: Anomaly detection",
          tags$p("A machine learning algorithm such as Isolation Forest identifies interviews with unusual combinations of errors. Inputs can include “don’t know” patterns, missing values, and suspiciously consistent answers. The top 5 percent of interviews by anomaly score are typically targeted for review. Flagged interviews are not necessarily errors—they indicate where closer review is needed.")
        )
      ),

      section("3. Turning Flags into Action",
        tags$p("Feedback is the key part of the framework. Errors and outliers should be turned into an actionable format—such as an error sheet or dashboard—and shared with field teams promptly."),
        tags$p("A structured feedback system should include:"),
        tags$ul(
          tags$li("Regular dashboards and error reports giving supervisors a clear view of enumerator- and team-level performance."),
          tags$li("Periodic review meetings with data scientists, survey managers, and supervisors to discuss recurring patterns and agree on corrective actions."),
          tags$li("Field follow-up: supervisors review error reports with enumerators and, where necessary, recontact respondents to resolve inconsistencies.")
        ),
        tags$p("Consistent application of this approach can substantially reduce error rates over the course of data collection.")
      ),

      section("4. Adapting the Framework",
        tags$p("The framework is modular and scalable—it can be applied in small pilots and national surveys by adjusting the depth of checks. Real-time and rule-based checks require few resources. Anomaly detection needs modest computing power and staff with data analysis skills."),
        tags$p("Larger surveys may benefit from additional feedback layers, such as SMS feedback to enumerators. Further extensions can include paradata analysis (keystroke tracking, GPS location), voice recordings, and integration of large language models, depending on data security and storage feasibility.")
      )
    )
  )
}
