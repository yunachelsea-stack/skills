quality_tab <- function() {
  section     <- tab_section
  step        <- tab_step
  source_note <- tab_source_note

  tabPanel("Data Quality Assurance", value = "quality",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$small("RESOURCE 05 · PHASE FOUR: DATA ANALYTICS"),
        tags$h2("Quality Assurance and Quality Control"),
        tags$p("Plan safeguards, detect errors during data collection, and provide timely feedback so field teams can resolve problems."),
        tags$p(class = "ca-source",
          "Adapted from the supplied Digital Skills Measurement Toolkit (2026), Chapter 9, sections 9.1–9.2 and Box 9.1.")
      ),
      section("9.1 Defining Data Quality Assurance and Quality Control",
        tags$p("Quality assurance, quality control and quality improvement have distinct roles. The report recommends a multifaceted framework to protect data quality and integrity during implementation."),
        step("Quality assurance: prevent and detect errors early",
          tags$p("Plan preventive processes that support adherence to survey protocols and early detection of errors.")
        ),
        step("Quality control: check the outputs",
          tags$p("Monitor the outputs of those processes and verify that they meet established standards.")
        ),
        step("Quality improvement: strengthen the system",
          tags$p("Continuously improve quality assurance and quality control systems rather than treating quality as a one-time check.")
        ),
        source_note("section 9.1 and Box 9.1.")
      ),
      section("9.2 Quality Analytics and Quality Control Framework",
        tags$p("The framework combines an error-detection pipeline with targeted, timely feedback. Its purpose is to detect and resolve problems during data collection, not only after fieldwork ends."),
        div(class = "ca-note",
          tags$strong("Guidance, not an automated data checker"),
          tags$p("This tab explains the report’s framework. It does not analyze uploaded responses, run machine learning, generate error reports or modify the survey automatically.")
        ),
        step("Quality assurance · Step 1: Build safeguards into the CAPI tool",
          tags$p("During tool development, review questions for potential errors and design safeguards to prevent or capture them early. Decide the core components of the quality framework at this stage."),
          tags$p("CAPI means computer-assisted personal interviewing.")
        ),
        step("Quality assurance · Step 2: Define error flags and algorithm protocols",
          tags$p("After identifying error opportunities and building internal safeguards, define rules for detecting other errors as data are processed, either during collection or afterward."),
          tags$p("Where anomaly detection is used, establish its protocols alongside the rule-based checks. The three layers described below complement one another.")
        ),
        step("Quality control · Step 3: Run checks regularly",
          tags$p("Continuously monitor collected data against the safeguards and error rules. Track errors and performance in real time, or as rapidly as possible, so problems are identified early.")
        ),
        step("Quality control · Step 4: Generate actionable error reports",
          tags$p("Turn identified errors and outliers into an error sheet or dashboard that supports feedback and correction, rather than simply listing problems.")
        ),
        step("Feedback · Step 5: Share findings and take corrective action",
          tags$p("Provide frequent feedback through meetings, messages or retraining. Address errors by correcting or recollecting problematic data."),
          tags$p("The report describes weekly dashboards and detailed Excel error reports showing enumerator- and team-level performance. Weekly calls brought together data scientists, survey managers and supervisors to review cases, discuss recurring patterns and agree corrective actions."),
          tags$p("Supervisors then reviewed reports with enumerators and, where necessary, recontacted respondents to resolve inconsistencies.")
        ),
        source_note("section 9.2, Quality Assurance, Quality Control and Feedback stages.")
      ),
      section("Three complementary layers of quality checks",
        step("I. Real-time quality assurance feedback",
          tags$p("Embed logical skips and range checks in the CAPI tool to identify inconsistencies and data-entry errors during the interview. Apply validation selectively, preserving flexibility for complex questions and balancing error prevention with detection of interviewer attentiveness."),
          tags$p("Complement automated checks with supervisor spot checks, partial interview observations and re-interviews of a subsample. The report describes a typical re-interview subsample of 5–10 percent; this is guidance from the report, not an automatic setting.")
        ),
        step("II. Rule-based error flags",
          tags$p("Use binary flags for key error types:"),
          tags$ul(
            tags$li("Logical inconsistencies, such as contradictory responses."),
            tags$li("Range violations, such as impossible ages or durations."),
            tags$li("Interview duration thresholds that identify potentially rushed interviews."),
            tags$li("Duplicate or missing identifiers.")
          ),
          tags$p("The study updated rules iteratively as new error patterns emerged and generated weekly error reports to track issues and support corrective action.")
        ),
        step("III. Anomaly detection",
          tags$p("An unsupervised method such as Isolation Forest can identify unusual combinations of errors that simpler rules may miss."),
          tags$p("The study used “don’t know” patterns, missing values and suspiciously consistent answers as model inputs. The top 5 percent of interviews by anomaly score were targeted for review."),
          tags$p("This is the study’s review approach, not a universal threshold. An anomaly identifies a case for review; it does not by itself establish that data were fabricated.")
        ),
        source_note("section 9.2, Components of a Quality Assurance Framework.")
      ),
      section("Evidence and adaptability",
        step("Experience from the Bihar population survey",
          tags$p("The report states that the framework improved error rates by over 85 percent during the Bihar population survey (Date et al. 2026). It builds on an earlier framework implemented in Kilkari, India (Shah et al. 2021)."),
          tags$p("This is a result reported for that study, not a promised improvement for every survey."),
          source_note("section 9.2.")
        ),
        step("Adjust the depth of checks to the survey",
          tags$p("The framework is modular and scalable, from small pilots to national surveys. Adapt the depth of checks while retaining regular, rapid feedback."),
          tags$p("The Bihar survey did not use an SMS feedback system because of cost and complexity. The report notes that larger surveys might benefit from additional feedback layers."),
          tags$p("Depending on data security and storage feasibility, the report also discusses additional analyses of paradata, such as keystroke tracking, GPS locations and voice recordings, and integration of large language models. These are optional extensions, not features enabled in this toolkit."),
          source_note("section 9.2, Adaptability for Other Surveys.")
        ),
        div(class = "ca-note",
          tags$strong("Keep the feedback loop active"),
          tags$p("Detect problems early, turn findings into actionable reports, review them with field teams, and resolve or recollect problematic data. Timely, targeted feedback is central to the report’s approach.")
        )
      )
    )
  )
}
