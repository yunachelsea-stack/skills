quality_tab <- function() {
  section  <- tab_section
  sub_head <- function(title) {
    tags$p(style = "font-weight:700; color:#0d3b5e; margin:20px 0 6px;", title)
  }

  tabPanel("Data Quality Assurance", value = "quality",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Quality Assurance and Quality Control")
      ),

      section(NULL,
        tags$ul(
          tags$li(tags$strong("Quality assurance"), " is about planning preventive processes that ensure adherence to protocols and early detection of errors."),
          tags$li(tags$strong("Quality control"), " focuses on monitoring outputs from those processes and verifying that they meet the established standards."),
          tags$li(tags$strong("Quality improvement"), " is a proactive effort to continuously strengthen quality assurance and quality control systems.")
        ),
        tags$p("To ensure data quality and integrity during implementation, a multifaceted quality assurance and quality control (QA/QC) framework should be established. Its value comes from a robust error detection pipeline combined with targeted, real-time feedback.")
      ),

      section("1. Building Checks into the Survey Tool",
        tags$p("During tool development, survey questions should be assessed for potential opportunities for error, and appropriate safeguards should be designed to prevent or capture errors early. Once these safeguards are built into the survey tool itself, the next step is to define the rules for identifying other errors as data are processed, either in real time during data collection or after.")
      ),

      section("2. Three Layers of Checks",
        tags$p("The framework uses three layers. Real-time checks prevent errors at the point of data entry, rule-based flags catch clear errors, and machine learning–based anomaly detection identifies subtle patterns suggestive of poor-quality or fabricated data. This layered structure ensures that both obvious and hidden issues are addressed."),
        sub_head("Layer 1: Real-time checks during data collection"),
        tags$p("Embed logical skips and range checks in the computer-assisted personal interviewing (CAPI) tool to catch inconsistencies and data entry errors in real time, while allowing flexibility for complex questions. Validation rules should be used selectively, since too many can slow interviews. These checks are complemented by supervisor spot checks, partial interview observations, and re-interviews of a subsample, typically 5–10 percent."),
        sub_head("Layer 2: Rule-based error flags"),
        tags$p("These are binary “flags” for key error types, including:"),
        tags$ul(
          tags$li("Logical inconsistencies (for example, contradictory responses)"),
          tags$li("Range violations (for example, impossible ages or durations)"),
          tags$li("Interview duration thresholds (flagging rushed interviews)"),
          tags$li("Duplicate or missing identifiers")
        ),
        tags$p("Rules should be updated iteratively during data collection as new error patterns emerge."),
        sub_head("Layer 3: Anomaly detection"),
        tags$p("A machine learning algorithm designed for identifying anomalies, such as Isolation Forest, is used to identify interviews that show unusual combinations of errors. Inputs to the model can include “don’t know” patterns, missing values, and suspiciously consistent answers; the top 5 percent of interviews by anomaly score are typically targeted for review. Flagged interviews are not necessarily errors; they indicate where closer review is needed.")
      ),

      section("3. Turning Flags into Action",
        tags$p("Feedback is the key part of the framework. Data should be monitored in real time, or as rapidly as possible, so errors are caught early. Once errors and outliers are identified, the information should be processed into an actionable format, such as an error sheet or dashboard, and shared with the field team for corrective action. Quick, real-time, and targeted feedback is a key factor of effective quality control."),
        tags$p("A structured system of reporting and feedback should be established so that flagged issues translate into meaningful improvements:"),
        tags$ul(
          tags$li("Weekly dashboards and detailed Excel error reports gave supervisors a clear view of enumerator- and team-level performance."),
          tags$li("Weekly calls with data scientists, survey managers, and supervisors were used to review flagged cases, discuss recurring patterns, and decide on corrective actions."),
          tags$li("In the field, supervisors reviewed error reports with enumerators, addressed issues, and, where necessary, recontacted respondents to resolve inconsistencies.")
        ),
        tags$p("Consistent application of this approach can substantially reduce error rates over the course of data collection.")
      ),

      section("4. Adapting the Framework",
        tags$p("The framework is modular and scalable by design. It can be implemented in both small pilots and national surveys by adjusting the depth of checks. Real-time and rule-based checks require few resources. Anomaly detection requires modest computing power but does need staff with data analysis skills."),
        tags$p("Regular and quick feedback is what makes the system most effective. Larger surveys may benefit from additional feedback layers, such as SMS feedback to enumerators, which may not be feasible in all contexts due to complexity and cost. Depending on data security and storage feasibility, further layers can be added, such as analysis of paradata (keystroke tracking, GPS location), voice recordings, and integration of large language models.")
      )
    )
  )
}
