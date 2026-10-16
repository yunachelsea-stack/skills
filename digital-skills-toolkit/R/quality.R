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
        tags$p("A robust QA/QC framework combines an error detection pipeline with targeted, timely feedback. Its value lies in detecting and resolving problems during data collection, not only after fieldwork ends."),
        tags$p("The figure below shows the stages of setting up a QA/QC framework and the contextual factors that affect the quality of survey data."),
        tags$figure(class = "ca-figure",
          div(class = "ca-figure-scroll", HTML("<svg viewBox='0 0 960 262' role='img' aria-labelledby='qa-fig-title qa-fig-desc' style='width:100%;min-width:720px;height:auto;font-family:inherit'><title id='qa-fig-title'>Setting up a QA/QC framework</title><desc id='qa-fig-desc'>Contextual factors affecting all stages: network availability, personnel for supervision, weather and gender dynamics. Five stages: 1 tool development, which asks whether we are asking the right questions and draws on cognitive interviews and pilot testing; 2 enumerator recruitment and training and 3 field coordination, which ask whether the right people are prepared to ask the questions; 4 field monitoring and 5 data analytics, including implementation of AI or machine learning, which ask whether the data being collected are reliable and valid. Across stages 2 to 5, data quality assessment iteratively feeds back results to inform training, supervision and ongoing implementation.</desc><rect x='0' y='0' width='960' height='38' rx='4' fill='#eef3f7'/><text x='480' y='24' text-anchor='middle' font-size='15' fill='#0d3b5e'><tspan font-weight='700'>Contextual factors:</tspan> network availability, personnel for supervision, weather, gender dynamics</text><text x='100' y='70' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>Are we asking</text><text x='100' y='88' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>the right questions?</text><text x='385' y='70' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>Are the right people prepared</text><text x='385' y='88' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>to ask the questions?</text><text x='765' y='70' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>Are the data we are collecting</text><text x='765' y='88' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>reliable and valid?</text><line x1='577' y1='56' x2='577' y2='96' stroke='#0d3b5e' stroke-width='1.5'/><polygon points='0,108 172,108 200,144.0 172,180 0,180' fill='#0d3b5e' stroke='#fff' stroke-width='2'/><circle cx='22' cy='144.0' r='16' fill='#3d7ea6' stroke='#fff' stroke-width='2'/><text x='22' y='149.0' text-anchor='middle' font-size='14' font-weight='700' fill='#fff'>1</text><text x='46' y='140.4' text-anchor='start' font-size='14' font-weight='700' fill='#fff'>Tool</text><text x='46' y='157.4' text-anchor='start' font-size='14' font-weight='700' fill='#fff'>development</text><polygon points='190,108 362,108 390,144.0 362,180 190,180 218,144.0' fill='#f9d27a' stroke='#fff' stroke-width='2'/><circle cx='238' cy='144.0' r='16' fill='#3d7ea6' stroke='#fff' stroke-width='2'/><text x='238' y='149.0' text-anchor='middle' font-size='14' font-weight='700' fill='#fff'>2</text><text x='262' y='131.9' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>Enumerator</text><text x='262' y='148.9' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>recruitment</text><text x='262' y='165.9' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>and training</text><polygon points='380,108 552,108 580,144.0 552,180 380,180 408,144.0' fill='#f9d27a' stroke='#fff' stroke-width='2'/><circle cx='428' cy='144.0' r='16' fill='#3d7ea6' stroke='#fff' stroke-width='2'/><text x='428' y='149.0' text-anchor='middle' font-size='14' font-weight='700' fill='#fff'>3</text><text x='452' y='140.4' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>Field</text><text x='452' y='157.4' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>coordination</text><polygon points='570,108 742,108 770,144.0 742,180 570,180 598,144.0' fill='#f9d27a' stroke='#fff' stroke-width='2'/><circle cx='618' cy='144.0' r='16' fill='#3d7ea6' stroke='#fff' stroke-width='2'/><text x='618' y='149.0' text-anchor='middle' font-size='14' font-weight='700' fill='#fff'>4</text><text x='642' y='140.4' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>Field</text><text x='642' y='157.4' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>monitoring</text><polygon points='760,108 932,108 960,144.0 932,180 760,180 788,144.0' fill='#f9d27a' stroke='#fff' stroke-width='2'/><circle cx='808' cy='144.0' r='16' fill='#3d7ea6' stroke='#fff' stroke-width='2'/><text x='808' y='149.0' text-anchor='middle' font-size='14' font-weight='700' fill='#fff'>5</text><text x='832' y='126.4' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>Data</text><text x='832' y='143.4' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>analytics</text><text x='832' y='157.8' text-anchor='start' font-size='11' font-weight='400' fill='#0d3b5e' font-style='italic'>Implementation</text><text x='832' y='171.8' text-anchor='start' font-size='11' font-weight='400' fill='#0d3b5e' font-style='italic'>of AI or ML</text><path d='M12,192 V199 H178 V192' fill='none' stroke='#9aa8b4' stroke-width='1.5'/><path d='M202,192 V199 H948 V192' fill='none' stroke='#9aa8b4' stroke-width='1.5'/><text x='95' y='220' text-anchor='middle' font-size='13' font-weight='700' fill='#0d3b5e'>Cognitive interviews</text><text x='95' y='237' text-anchor='middle' font-size='13' font-weight='700' fill='#0d3b5e'>and pilot testing</text><text x='318' y='220' text-anchor='middle' font-size='13' font-weight='700' fill='#0d3b5e'>Data quality</text><text x='318' y='237' text-anchor='middle' font-size='13' font-weight='700' fill='#0d3b5e'>assessment</text><path d='M372,224 H402' fill='none' stroke='#0d3b5e' stroke-width='1.6' marker-end='url(#qa-arrow)'/><defs><marker id='qa-arrow' viewBox='0 0 10 10' refX='9' refY='5' markerWidth='7' markerHeight='7' orient='auto'><path d='M0,0 L10,5 L0,10 z' fill='#0d3b5e'/></marker></defs><text x='414' y='220' text-anchor='start' font-size='13' font-weight='700' fill='#0d3b5e'>Iteratively feed back results to inform training,</text><text x='414' y='237' text-anchor='start' font-size='13' font-weight='700' fill='#0d3b5e'>supervision, and ongoing implementation</text></svg>")),
          tags$figcaption(class = "ca-source", "Source: A Practitioner\u2019s Toolkit for Assessing Digital Skills in Low- and Middle-Income Countries. AI = artificial intelligence; ML = machine learning.")
        )
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
