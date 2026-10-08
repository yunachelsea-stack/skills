sampling_tab <- function() {
  section     <- tab_section
  step        <- tab_step
  source_note <- tab_source_note

  tabPanel("Sampling Methods", value = "sampling",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$small("RESOURCE 03 · SAMPLING"),
        tags$h2("Determining the Sample Population"),
        tags$p("Define who the survey represents, choose how to reach them, and plan a sample suited to the assessment’s objectives and resources."),
      ),
      section("Plan the sample before fieldwork",
        tags$p("Sampling determines how individuals or households are selected to represent a larger population. A poorly designed sample can introduce bias, reduce precision and limit the conclusions that can be drawn."),
        tags$ol(
          tags$li("Define the target population and the indicators to measure."),
          tags$li("Identify or construct a sampling frame where possible."),
          tags$li("Choose the sampling method."),
          tags$li("Determine sample size for the estimates and subgroups that matter.")
        ),
        div(class = "ca-note",
          tags$strong("A conceptual guide, not a complete field protocol."),
          tags$p("Chapter 7 explains what to consider and why. Design choices depend on survey objectives, available resources, the setting and the availability of population lists. This resource does not select participants or calculate a project-specific sample size.")
        )
      ),
      section("Conceptual and Operational Definitions",
        step("Target population: who should the findings describe?",
          tags$p("Specify the people for whom indicators will be measured. Program or policy eligibility criteria may define this group."),
          tags$p("Box 7.1 illustrates this with unemployed individuals ages 19–24 residing in a selected community. This is the report’s example, not a default age range for your survey.")
        ),
        step("Indicators: what will be measured?",
          tags$p("Define the variables used to evaluate conditions, monitor progress or assess an intervention. These may include demographic, economic, access and perception-based measures.")
        ),
        step("Inclusion and exclusion criteria: who is eligible?",
          tags$p("Document who qualifies for the full interview and who does not. Use these criteria to build the screening instrument when eligibility is not already known, and include them in the survey manual.")
        ),
      ),
      section("Survey Design Scenarios",
        tags$p("A sampling frame is a complete list of elements in the target population from which a sample can be drawn. Chapter 7 distinguishes two situations: a known frame, and an unknown frame that requires household screening."),
        div(style = "overflow-x:auto;",
          tags$table(class = "table table-striped",
            tags$caption("Comparison of sampling-frame scenarios"),
            tags$thead(tags$tr(
              tags$th(scope = "col", "Consideration"),
              tags$th(scope = "col", "Known frame"),
              tags$th(scope = "col", "Unknown frame / screening")
            )),
            tags$tbody(
              tags$tr(tags$th(scope = "row", "Sampling efficiency"), tags$td("High"), tags$td("Medium to low")),
              tags$tr(tags$th(scope = "row", "Field effort"), tags$td("Low"), tags$td("High, due to screening")),
              tags$tr(tags$th(scope = "row", "Eligibility"), tags$td("Predetermined"), tags$td("Established in the field")),
              tags$tr(tags$th(scope = "row", "Ethical complexity"), tags$td("Lower"), tags$td("Higher, due to potential intrusion")),
              tags$tr(tags$th(scope = "row", "Typical suitability"), tags$td("Program evaluations"), tags$td("Community prevalence")),
              tags$tr(tags$th(scope = "row", "Timeline"), tags$td("Shorter"), tags$td("Longer"))
            )
          )
        ),
      ),
      section("Scenario 1: The sampling frame is known",
        tags$p("Possible sources include program participant databases, voter registries, census microdata or master sampling frames, and ministry or local-government administrative databases."),
        step("1. Define the unit of analysis",
          tags$p("Choose individuals when indicators describe people, or households when collecting household-level conditions.")
        ),
        step("2. Finalize the frame using eligibility criteria",
          tags$p("Use appropriate data sources to list the units in the frame. Filter the list using the survey’s eligibility criteria where needed.")
        ),
        step("3. Choose a sampling method",
          tags$ul(
            tags$li(tags$strong("Simple random sampling: "), "Select randomly from a complete, accurate list so each individual has an equal chance of selection."),
            tags$li(tags$strong("Systematic sampling: "), "Select at regular intervals after a random starting point."),
            tags$li(tags$strong("Cluster sampling: "), "Select groups such as villages or neighborhoods first, then sample individuals within them. This is useful when cluster lists exist but a complete individual-level list does not."),
            tags$li(tags$strong("Stratified sampling: "), "Divide the frame into subgroups such as gender, age group, urban/rural location or program participation. Draw samples proportionally or equally to support representation of key subgroups.")
          )
        ),
        step("4. Determine sample size for the required estimates",
          tags$p("Calculate sample size for each stratum where precise indicator estimates are required. Consider confidence level, margin of error, expected indicator prevalence and the design effect for clustered samples."),
          tags$p("The report discusses design-effect multipliers of 2 or 3 in LMIC surveys, but advises using comparable studies or a survey specialist for more precise estimates. These values are not automatic settings for every study."),
          tags$p("Allow for non-response. For small populations, consider finite population correction; Box 7.2 identifies a sample of at least 5% of the population as the point at which this may be relevant.")
        ),
        step("5. Contact and interview the selected sample",
          tags$p("Enumerators locate and interview the selected individuals directly from the sample list.")
        ),
      ),
      section("Sample-size illustration from the report",
        tags$p("Box 7.2 illustrates a proportion-based calculation for youth completing an online job application, using an indicator of independently completing an online form."),
        tags$ul(
          tags$li("Unknown prevalence: p = 0.50."),
          tags$li("Confidence level: 95% (Z = 1.96)."),
          tags$li("Margin of error: 0.05."),
          tags$li("Design effect: 2.5."),
          tags$li("Expected response rate: 0.85.")
        ),
        tags$p("The sequence is to calculate the base simple-random-sampling size, apply the design effect, then adjust for non-response."),
        div(class = "ca-note",
          tags$strong("Report result: 1,130 attempted interviews per stratum."),
          tags$p("This is the report’s worked illustration, not a recommended sample size for your assessment. Use assumptions justified by your own population and study design.")
        ),
      ),
      section("Scenario 2: The sampling frame is unknown",
        tags$p("Where no list of eligible individuals exists, household-level screening identifies the target group. The report combines multistage cluster sampling, household listing, eligibility identification and final sample selection."),
        step("Plan administrative areas, clusters and field effort",
          tags$p("Define the administrative divisions for which results should be representative. Identify primary sampling units (PSUs), such as villages, urban blocks, wards or enumeration areas, using government or national statistical agency records."),
          tags$p("After calculating sample size, plan how many clusters to visit. The report gives 20–40 households per cluster as a rule of thumb, noting that rare target populations may require larger clusters. Account for how many eligible individuals households are expected to yield.")
        ),
        step("Stage 1. Select clusters",
          tags$p("Select PSUs using simple random sampling or probability proportional to size (PPS), which reflects population size. The report discusses self-weighting under appropriate allocation and constant cluster-size conditions; do not assume every multistage design is self-weighting.")
        ),
        step("Stage 2. List and randomly select households",
          tags$p("Map and list all households in each selected PSU. Randomly select the planned number of households from those lists."),
          tags$p("Collect the household information needed to assess all eligibility criteria, such as household size, member ages and employment status.")
        ),
        step("Stage 3. Identify and select eligible individuals",
          tags$p("Flag households with eligible people. Depending on the design, interview all eligible members or randomly select from the eligible household members. The report notes that selecting everyone may be inefficient when their information would be identical."),
          tags$p("Use a random number generator or lottery based on roster line numbers when selecting one eligible individual, rather than choosing the easiest person to reach.")
        ),
        step("Use a short household screening questionnaire",
          tags$p("Administer the screener to the household head or a senior member. Include household member counts, ages and genders, and employment status for the relevant age group."),
          tags$p("Use a roster grid and probe for temporary residents or migrants. Ensure screening questions match your defined eligibility criteria.")
        ),
      ),
      section("Allow for eligibility, non-response and weighting",
        tags$p("When screening is required, distinguish the final sample of eligible individuals from the number of households that must be screened. Box 7.3 identifies eligibility rate, design effect and expected non-response as necessary planning inputs."),
        tags$p("Allocate clusters across the administrative divisions of interest. The report discusses proportional allocation and conditions for self-weighting; where the allocation or selection approach is not self-weighting, appropriate sample weights must be applied."),
        tags$p("Keep these choices tied to the population and subgroups that the survey is intended to represent, rather than treating the number of completed interviews alone as evidence of representativeness.")
      )
    )
  )
}
