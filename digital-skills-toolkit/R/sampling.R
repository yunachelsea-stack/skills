sampling_tab <- function() {
  section     <- tab_section
  step        <- tab_step

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

  tabPanel("Sampling Methods", value = "sampling",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Determining the Sample Population"),
        tags$p("Define who the survey represents, choose how to reach them, and plan a sample suited to your objectives and resources.")
      ),

      section(NULL,
        tags$p("Sampling determines how individuals or households are selected to represent a larger population. A well-designed sample ensures findings are unbiased and generalizable. Four decisions shape every sampling plan:"),
        div(class = "ca-flow",
          div(class = "ca-flow-step", "Define population"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Find or build frame"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Choose method"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Calculate size"),
          div(class = "ca-flow-arrow", "→"),
          div(class = "ca-flow-step", "Field")
        ),
        tags$p(style = "font-size:0.88em; color:#607381;",
          "This tab explains what to consider and why. Design choices depend on survey objectives,
           available resources, the setting, and the availability of population lists.")
      ),

      section("Key Definitions",
        div(style = "border:1px solid #dce3e8; border-radius:4px; background:#fff;",
          div(style = "padding:14px 18px; border-bottom:1px solid #dce3e8;",
            tags$p(style = "margin:0;",
              tags$strong("Target population: "),
              "The group the survey is intended to study and for whom indicators will be measured.
               Often defined by program or policy eligibility criteria — for example, unemployed
               individuals ages 19–24 residing in a selected community."
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

      section("Scenario 1: Sampling Frame is Known",
        when_to_use("program evaluations or interventions where a participant list, registry, or administrative database already exists."),
        tags$p("Possible sources include program participant databases, voter registries, census microdata or master sampling frames, and ministry or local-government administrative databases."),

        numbered_step("1", "Define the unit of analysis",
          tags$p("Choose individuals when indicators describe people, or households when collecting household-level conditions.")
        ),
        numbered_step("2", "Finalize the frame using eligibility criteria",
          tags$p("Use appropriate data sources to list the units in the frame. Filter using the survey's eligibility criteria where needed.")
        ),
        numbered_step("3", "Choose a sampling method",
          tags$ul(
            tags$li(tags$strong("Simple random sampling: "), "Every individual in the frame has an equal and independent chance of selection. Straightforward and statistically rigorous, but requires a complete and accurate list."),
            tags$li(tags$strong("Systematic sampling: "), "Select at regular intervals after a random starting point. Efficient and easy to implement with ordered lists."),
            tags$li(tags$strong("Cluster sampling: "), "Select groups such as villages or neighborhoods first, then sample individuals within them. Useful when cluster lists exist but a complete individual-level list does not."),
            tags$li(tags$strong("Stratified sampling: "), "Divide the frame into subgroups — by gender, age, urban/rural location or program participation — and draw samples proportionally or equally to ensure representation of key subgroups.")
          )
        ),
        numbered_step("4", "Calculate sample size",
          tags$p("Calculate for each stratum where precise estimates are required. The key inputs are:"),
          tags$ul(
            tags$li("Desired confidence level (e.g., 95%)"),
            tags$li("Margin of error (e.g., ±5%)"),
            tags$li("Expected indicator prevalence"),
            tags$li("Design effect (DEFF) — a multiplier for clustered samples; values of 2 or 3 are typical in LMIC surveys")
          ),
          tags$p(tags$strong("Worked example: "), "Proportion of youth (ages 10–24) completing an online job application. Assumptions: prevalence unknown (p = 0.50), 95% confidence (Z = 1.96), margin of error 0.05, DEFF = 2.5, response rate = 0.85."),
          div(class = "ca-flow", style = "margin-top:12px;",
            div(class = "ca-flow-step", "Base SRS: 384"),
            div(class = "ca-flow-arrow", "× 2.5 DEFF →"),
            div(class = "ca-flow-step", "960"),
            div(class = "ca-flow-arrow", "÷ 0.85 response →"),
            div(class = "ca-flow-step", "1,130 interviews")
          ),
          tags$p(style = "font-size:0.88em; color:#607381; margin-top:10px;",
            "This is the report's worked illustration, not a recommended size for your study.
             Use assumptions justified by your own population and design."),
          tags$p("For small populations, apply finite population correction (FPC) when the sample
                  exceeds 5% of the total population size.")
        ),
        numbered_step("5", "Contact and interview the selected sample",
          tags$p("Enumerators locate and interview the selected individuals directly from the sample list.")
        )
      ),

      section("Scenario 2: Sampling Frame is Unknown",
        when_to_use("community prevalence studies where no list of eligible individuals exists — household-level screening is used to identify and reach the target group."),
        tags$p("This approach combines multistage cluster sampling, household listing, eligibility screening, and final sample selection."),

        step("Stage 1: Select clusters (PSUs)",
          tags$p("Define the administrative divisions for which results should be representative. Identify primary sampling units (PSUs) — villages, urban blocks, wards or enumeration areas — from government or national statistical agency records (e.g., DHS or MICS enumeration areas)."),
          tags$p("Select PSUs using simple random sampling or probability proportional to size (PPS), which weights selection by population density and can produce a self-weighting sample under constant cluster sizes."),
          tags$p("A general rule of thumb is to visit 20–40 households per cluster; rare target populations may require larger clusters. Divide the total required sample by the cluster size to determine the number of clusters to visit.")
        ),
        step("Stage 2: List and randomly select households",
          tags$p("Map and list all households in each selected PSU. Randomly select the planned number from those lists."),
          tags$p("Collect household information needed to assess all eligibility criteria — such as household size, member ages and employment status.")
        ),
        step("Stage 3: Identify and select eligible individuals",
          tags$p("Flag households with eligible members. Depending on the design, interview all eligible members or randomly select one using a random number generator or lottery based on roster line numbers — not simply the easiest person to reach."),
          tags$p("Selecting all eligible members may be inefficient when their responses would be identical.")
        ),
        step("Household screening questionnaire",
          tags$p("Administer a short screener to the household head or a senior member. Key elements to collect:"),
          tags$ul(
            tags$li("Number of household members"),
            tags$li("Ages and genders of members"),
            tags$li("Employment status of the relevant age group")
          ),
          tags$p("Use a roster grid and probe for temporary residents or migrants. Ensure screening questions match your eligibility criteria exactly.")
        ),
      )
    )
  )
}
