sampling_tab <- function() {
  section     <- tab_section

  numbered_step <- function(n, title, ...) {
    tags$details(class = "ca-step",
      tags$summary(
        tags$span(class = "ca-step-num", n),
        title
      ),
      div(class = "ca-step-body", ...)
    )
  }

  when_to_use <- function(text) {
    div(style = "background:#f0f9f4; border-left:3px solid #1a7a4a;
                 padding:10px 14px; margin-bottom:18px; border-radius:0 4px 4px 0;",
      tags$span(style = "font-size:0.78em; font-weight:700; text-transform:uppercase;
                          letter-spacing:.06em; color:#1a7a4a; display:block; margin-bottom:4px;",
                "Best for"),
      tags$p(style = "margin:0; font-size:0.9em; color:#263e50;", text)
    )
  }

  eg <- function(...) {
    div(style = "background:#f3f7fa; border-left:3px solid #8db4c8;
                 padding:10px 14px; margin-top:12px;",
      tags$span(style = "font-size:0.75em; font-weight:700; text-transform:uppercase;
                          letter-spacing:.06em; color:#607381; display:block; margin-bottom:4px;",
                "Example"),
      ...
    )
  }

  phase_head <- function(title) {
    tags$p(style = "font-weight:700; color:#0d3b5e; margin:28px 0 14px;
                    font-size:0.95em; border-bottom:2px solid #e8a800;
                    padding-bottom:6px;",
           title)
  }

  tabPanel("Sampling Methods", value = "sampling",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Determining the Sample Population")
      ),

      section(NULL,
        tags$p("Sampling determines how individuals or households are selected to represent a larger population. A well-designed sample ensures findings are unbiased and generalizable. This tab explains what to consider and why — design choices depend on survey objectives, available resources, the setting, and the availability of population lists.")
      ),

      section("Key Definitions",
        div(style = "border:1px solid #dce3e8; border-radius:4px; background:#fff;",
          div(style = "padding:14px 18px; border-bottom:1px solid #dce3e8;",
            tags$p(style = "margin:0;",
              tags$strong("Target population: "),
              "The group the survey is intended to study and for whom indicators will be measured.
               Often defined by program or policy eligibility criteria."
            )
          ),
          div(style = "padding:14px 18px; border-bottom:1px solid #dce3e8;",
            tags$p(style = "margin:0;",
              tags$strong("Indicators: "),
              "The variables used to evaluate conditions, monitor progress or assess interventions,
               including demographic, economic, access and perception-based measures."
            )
          ),
          div(style = "padding:14px 18px;",
            tags$p(style = "margin:0;",
              tags$strong("Inclusion and exclusion criteria: "),
              "Who qualifies for the full interview and who does not. Document these clearly —
               they inform the screening instrument and should appear in the survey manual."
            )
          )
        )
      ),

      section("Two Sampling Scenarios",
        tags$p("Sampling design depends on whether a list of your target population already exists. The two sections below walk through each scenario in detail.")
      ),

      section("Scenario 1: Sampling Frame Is Known",
        tags$p("A sampling frame is a complete list of all elements in the target population from which a sample can be drawn, such as a program participant database. When such a frame exists, the survey can use probability sampling, giving a high level of statistical rigor. Advantages include high efficiency, clear eligibility, and low screening costs."),
        tags$p("Sampling frames typically draw from four main sources:"),
        tags$ul(
          tags$li("Program participant databases (for example, lists from job training initiatives)"),
          tags$li("Voter registries"),
          tags$li("Census microdata or master sampling frames"),
          tags$li("Administrative databases from ministries or local governments")
        ),
        eg(
          tags$p(style = "margin:0;",
            "A youth employment program has a database of 8,000 participants. The survey aims
             to measure digital skills among unemployed participants aged 19–24, and requires
             1,000 completed interviews.")
        ),

        numbered_step("1", "Define the unit of analysis",
          tags$p("The unit of analysis may be individual-based, if indicators are specific to persons,
                  or household-based, if collecting data on household conditions."),
          eg(tags$p(style = "margin:0;",
            "Digital skills are measured for each person, so the unit of analysis is the individual."))
        ),

        numbered_step("2", "Finalize the sampling frame using eligibility criteria",
          tags$p("Use the appropriate data sources to list the units in the sampling frame.
                  If necessary, filter the frame using the eligibility criteria."),
          eg(tags$p(style = "margin:0;",
            "Filtering the database to participants who are currently aged 19–24 and unemployed
             leaves 6,500 people. This is the sampling frame."))
        ),

        numbered_step("3", "Choose a sampling method",
          tags$p("Common methods include:"),
          tags$ul(
            tags$li(tags$strong("Simple random sampling: "),
              "Every individual in the frame has an equal chance of being selected."),
            tags$li(tags$strong("Systematic sampling: "),
              "Individuals are selected at regular intervals following a random starting point."),
            tags$li(tags$strong("Stratified sampling: "),
              "The frame is divided into subgroups, such as gender or urban/rural, and a sample
               is drawn from each, either proportionally or equally. If strata are sampled equally,
               weights are needed when combining results.")
          ),
          eg(tags$p(style = "margin:0;",
            "Participants are selected by simple random sampling from the list of 6,500."))
        ),

        numbered_step("4", "Calculate the sample size",
          tags$p("A design effect is applied only if cluster sampling is used. When individuals are
                  selected directly from a list, no design effect is needed. Adjust for expected
                  non-response by dividing the required number of completed interviews by the
                  response rate. See Box 7.2 in the full report for the step-by-step formula."),
          eg(tags$p(style = "margin:0;",
            "With an expected response rate of 85 percent, 1,177 participants need to be selected
             to achieve 1,000 completed interviews."))
        ),

        numbered_step("5", "Contact and interview sample members",
          tags$p("Enumerators locate and interview the selected individuals directly from the list."),
          eg(tags$p(style = "margin:0;",
            "Enumerators receive the 1,177 names and contact details and aim to complete
             1,000 interviews."))
        )
      ),

      section("Scenario 2: Sampling Frame Is Unknown",
        tags$p("In many field contexts, especially in low-income or rural settings, no list of eligible
                individuals exists. In such cases, household screening is needed to identify members
                of the target group. This involves multistage cluster sampling, household listing,
                eligibility screening, and final sample selection."),
        tags$p("A screening tool is used to gather basic eligibility information. Sample size estimation
                must account for eligibility and response rates. The design effect must account for
                clustering, since people living near each other tend to give similar answers."),
        tags$p("Sampling without a frame happens in two parts: planning decisions made before fieldwork,
                followed by selection in the field in three stages — clusters, then households,
                then individuals."),
        eg(tags$p(style = "margin:0;",
          "The survey now aims to represent all unemployed youth aged 19–24 in a region with
           five districts, not just program participants. No list of these young people exists.
           The target remains 1,000 completed interviews.")),

        phase_head("Planning the Sample"),

        numbered_step("1", "Define the administrative divisions",
          tags$p("Define the administrative divisions for which the data should be representative.
                  Details can be obtained from government records."),
          eg(tags$p(style = "margin:0;",
            "Results should be representative of the region, with all five districts included."))
        ),

        numbered_step("2", "Define the clusters",
          tags$p("Determine the primary sampling units (clusters) to visit. Readily identifiable units
                  such as villages, urban blocks, wards, or enumeration areas may serve as primary
                  sampling units. Enumeration area lists are available from national statistical
                  agencies that conduct censuses or large surveys such as the DHS or MICS."),
          eg(tags$p(style = "margin:0;", "Census enumeration areas are used as clusters."))
        ),

        numbered_step("3", "Calculate the number of clusters to visit",
          tags$p("Not every household will contain an eligible person, so calculate the number of
                  households to screen first, then divide by the number of households per cluster.
                  A general rule of thumb is 20 to 40 households per cluster. See Box 7.3 in the
                  full report for the formula."),
          eg(tags$p(style = "margin:0;",
            "If 20 percent of households contain an eligible youth and 90 percent of those complete
             the interview, 5,556 households need to be screened. With 30 households per cluster,
             186 clusters are needed."))
        ),

        numbered_step("4", "Determine appropriate sample weights",
          tags$p("Divide the clusters among the administrative divisions. If clusters are allocated
                  in proportion to population size and selected using probability proportional to size
                  (see Stage 1 below), the sample is self-weighted. If other allocation methods are
                  used, appropriate weights must be applied."),
          eg(tags$p(style = "margin:0;",
            "A district with 30 percent of the region’s population receives 30 percent of
             the clusters — about 56 of the 186."))
        ),

        phase_head("Selecting the Sample in the Field"),

        numbered_step("1", "Select clusters (primary sampling units)",
          tags$p("Clusters may be selected using simple random sampling or probability proportional
                  to size (PPS), where larger clusters are more likely to be selected. PPS results
                  in a self-weighted sample when the same number of households is selected in
                  every cluster."),
          eg(tags$p(style = "margin:0;",
            "Within each district, enumeration areas are selected using PPS, and 30 households
             are selected in each."))
        ),

        numbered_step("2", "Conduct household listing within clusters",
          tags$p("Enumerators list all households in each selected cluster. From this list, households
                  are randomly selected, with the number equal to the cluster size determined in Step 3.
                  Each selected household is screened for eligibility. The screening data should cover
                  all the eligibility criteria."),
          eg(tags$p(style = "margin:0;",
            "In each enumeration area, enumerators list every household, randomly select 30,
             and screen each one."))
        ),

        numbered_step("3", "Identify eligible individuals",
          tags$p("Households with eligible members are flagged for the survey. In households with more
                  than one eligible member, either all eligible members are interviewed, or one is
                  selected at random using their roster line number. When one person is selected at
                  random, a weight equal to the number of eligible members in the household must be
                  applied at analysis."),
          eg(tags$p(style = "margin:0;",
            "A household has two unemployed youth aged 19–24. One is selected at random,
             and that interview receives a weight of 2."))
        )
      )
    )
  )
}
