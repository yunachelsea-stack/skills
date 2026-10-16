adaptations_tab <- function() {
  section     <- tab_section
  step        <- tab_step
  source_note <- tab_source_note

  qa_pair <- function(component, issue, original, revised) {
    div(class = "ca-example",
      div(class = "ca-example-head", component),
      div(class = "ca-example-body",
        div(class = "ca-qa-pair",
          div(class = "ca-qa-original",
            div(class = "ca-qa-label", "Original question"),
            tags$p(original)
          ),
          div(class = "ca-qa-issue",
            div(class = "ca-qa-label", "Issue"),
            tags$p(issue)
          ),
          div(class = "ca-qa-revised",
            div(class = "ca-qa-label", "Revised question"),
            tags$p(revised)
          )
        )
      )
    )
  }

  numbered_step <- function(n, title, ...) {
    tags$details(class = "ca-step",
      tags$summary(
        tags$span(class = "ca-step-num", n),
        title
      ),
      div(class = "ca-step-body", ...)
    )
  }

  ci_process_figure <- function() {
    tags$figure(class = "ca-figure",
      div(class = "ca-figure-scroll", HTML("<svg viewBox='0 0 960 462' role='img' aria-labelledby='ci-fig-title ci-fig-desc' style='width:100%;min-width:700px;height:auto;font-family:inherit'><title id='ci-fig-title'>Situating cognitive interviewing within the process for developing survey tools</title><desc id='ci-fig-desc'>Item generation (qualitative research, literature review, existing tools, expert judgement) leads to translation if required. The cognitive interviewing phase follows: decide scope of cognitive interviewing, select and train researchers, sample participants, interviews, research team debrief, then tool revision. If more data are needed, the process returns to sampling participants; if not, it moves to pilot testing, final revisions and survey implementation.</desc><defs><marker id='ci-arrow' viewBox='0 0 10 10' refX='9' refY='5' markerWidth='7' markerHeight='7' orient='auto-start-reverse'><path d='M0,0 L10,5 L0,10 z' fill='#4a5e6d'/></marker></defs><rect x='10' y='10' width='160' height='108' rx='4' fill='#f9d27a'/><text x='22' y='34' font-size='14' font-weight='700' fill='#0d3b5e'>Item generation</text><text x='22' y='56' font-size='12' fill='#263e50'>&#8226; Qualitative research</text><text x='22' y='73' font-size='12' fill='#263e50'>&#8226; Literature review</text><text x='22' y='90' font-size='12' fill='#263e50'>&#8226; Existing tools</text><text x='22' y='107' font-size='12' fill='#263e50'>&#8226; Expert judgement</text><path d='M170,58 H194' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='196' y='28' width='124' height='60' rx='4' fill='#f9d27a'/><text x='258' y='55.9' text-anchor='middle' font-size='14' font-weight='700' fill='#0d3b5e'>Translation</text><text x='258' y='75.2' text-anchor='middle' font-size='12' font-weight='400' fill='#4a5e6d'>if required</text><rect x='10' y='146' width='940' height='206' rx='8' fill='#eef3f7'/><text x='28' y='217.9' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>Cognitive</text><text x='28' y='234.9' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>interviewing</text><text x='28' y='251.9' text-anchor='start' font-size='14' font-weight='700' fill='#0d3b5e'>phase</text><path d='M258,88 V170' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='196' y='172' width='124' height='66' rx='4' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='258.0' y='193.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Decide scope</text><text x='258.0' y='209.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>of cognitive</text><text x='258.0' y='225.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>interviewing</text><path d='M320,205 H348' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='350' y='172' width='124' height='66' rx='4' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='412.0' y='193.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Select and</text><text x='412.0' y='209.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>train</text><text x='412.0' y='225.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>researchers</text><path d='M474,205 H502' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='504' y='172' width='124' height='66' rx='4' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='566.0' y='201.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Sample</text><text x='566.0' y='217.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>participants</text><path d='M628,205 H656' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='658' y='172' width='124' height='66' rx='4' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='720.0' y='209.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Interviews</text><path d='M782,205 H810' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='812' y='172' width='124' height='66' rx='4' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='874.0' y='201.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Research team</text><text x='874.0' y='217.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>debrief</text><path d='M874,238 V268' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='812' y='270' width='124' height='52' rx='4' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='874.0' y='292.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Tool</text><text x='874.0' y='308.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>revision</text><polygon points='654,296 720,262 786,296 720,330' fill='#fff' stroke='#0d3b5e' stroke-width='1.5'/><text x='720' y='292.7' text-anchor='middle' font-size='12' font-weight='600' fill='#0d3b5e'>More data</text><text x='720' y='307.7' text-anchor='middle' font-size='12' font-weight='600' fill='#0d3b5e'>needed?</text><path d='M812,296 H788' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><path d='M654,296 H566 V240' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><text x='610' y='290.6' text-anchor='middle' font-size='13' font-weight='700' fill='#0d3b5e'>Yes</text><path d='M720,330 V380 H268 V398' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><text x='494' y='374.6' text-anchor='middle' font-size='13' font-weight='700' fill='#0d3b5e'>No</text><rect x='196' y='400' width='144' height='52' rx='4' fill='#f9d27a'/><text x='268.0' y='430.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Pilot testing</text><path d='M340,426 H366' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='368' y='400' width='144' height='52' rx='4' fill='#f9d27a'/><text x='440.0' y='430.6' text-anchor='middle' font-size='13' font-weight='600' fill='#0d3b5e'>Final revisions</text><path d='M512,426 H538' fill='none' stroke='#4a5e6d' stroke-width='1.6' marker-end='url(#ci-arrow)'/><rect x='540' y='400' width='164' height='52' rx='4' fill='#0d3b5e'/><text x='622.0' y='422.6' text-anchor='middle' font-size='13' font-weight='600' fill='#fff'>Survey</text><text x='622.0' y='438.6' text-anchor='middle' font-size='13' font-weight='600' fill='#fff'>implementation</text></svg>")),
      tags$figcaption(class = "ca-source", "Source: Scott, Ummer, and LeFevre 2021.")
    )
  }

  tabPanel("Contextual Adaptations", value = "adaptations",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$h2("Contextual Adaptations through Cognitive Interviews"),
      ),

      section(NULL,
        tags$p("Cognitive interviewing is a qualitative method used to debug survey questions — checking how respondents interpret items, retrieve memories, and map answers to options — so the final instrument actually measures what you intend, especially across language and cultural gaps."),
        tags$p(tags$strong("How cognitive interviews fit into tool development")),
        tags$p("Survey tool development begins with item generation, drawing on qualitative research, literature reviews, existing survey tools and expert judgement. Draft items are translated where needed and then refined through rounds of cognitive interviews, debriefs and revisions, repeated until no more data are needed. The refined tool is then pilot tested with a small sample of the target population before it is finalized."),
        ci_process_figure()
      ),

      section("Five steps for conducting cognitive interviews",
        numbered_step("1", "Define the scope",
          tags$p("Prioritize newly developed, conceptually complex or central questions, especially those entering a new cultural or linguistic setting. Testing every question is ideal but may not be feasible because probing takes time."),
          tags$p("For longer instruments, divide priority questions into smaller interview guides while retaining a logical flow. In large surveys, researchers often create multiple guides, each covering a different section, so the workload stays manageable.")
        ),
        numbered_step("2", "Select and train researchers",
          tags$p("Recruit researchers fluent in the source and local languages, skilled in qualitative interviewing, and familiar with structured survey administration. Consider how gender, age and social background may affect rapport."),
          tags$p("Training should cover study objectives, question intent, research ethics and probing technique. Use role plays and practice interviews to help researchers shift between asking questions exactly as written and exploring understanding in depth.")
        ),
        numbered_step("3", "Select sample participants",
          tags$p("Recruit people with the same or a similar profile to the intended survey population. Include people with lower literacy or education, limited survey experience, and marginalized groups whose difficulties reveal inaccessible wording."),
          tags$p("The report describes two to three rounds with approximately 8–12 participants per round, revising and retesting after each round. Cover relevant experiences and oversample rare experiences where necessary."),
        ),
        numbered_step("4", "Collect data iteratively, debrief daily and analyse near real-time",
          tags$p("Typically, one researcher interviews while another takes detailed notes. Ask the original question, record the answer, then use scripted or emerging probes to explore confusing words, interpretation and response choices."),
          tags$p("After a small set of interviews, debrief and systematically review each question, document emerging issues, propose revisions, and test revised wording in the next round. Repeat until interpretation consistently matches question intent.")
        ),
        numbered_step("5", "Feed findings into enumerator training",
          tags$p("Involve cognitive-interview researchers in quantitative enumerator training. They can explain why certain wording was chosen, flag common misunderstandings, and highlight culturally sensitive issues."),
          tags$p("This step strengthens the link between tool development and field deployment, reducing the likelihood of improvisation or unintentional misinterpretation in the field.")
        )
      ),

      section("What to look for — and how wording can change",
        tags$p(class = "ca-source", style = "margin-top:-6px;", "Source: Scott et al. 2026."),
        qa_pair(
          "Word choice",
          "Some respondents did not recognize WhatsApp or YouTube use as internet use; “any location and any device” distracted respondents away from the core question.",
          "Have you ever used the internet from any location and any device?",
          "Have you ever used the internet? For example, WhatsApp, Facebook, YouTube, Google, and so on [add other locally relevant examples of internet]."
        ),

        qa_pair(
          "Syntax — simplify long or complex sentences",
          "Mentioning phone types in the question led respondents to focus on the type of phone rather than whether they had ever used one at all.",
          "Have you ever used a mobile phone? This could be any type of mobile phone, including a smartphone.",
          "Have you ever used a mobile phone?"
        ),

        qa_pair(
          "Question structure — use stand-alone questions",
          "Stem-and-leaf style questions (one opening instruction followed by subquestions A, B, C…) placed a high cognitive burden on respondents; most did not retain information from the “stem.”",
          "I will now ask you about activities you may have done on a computer or phone during the last 3 months. Did you… A. Use a copy-and-paste tool… B. Send a message with an attached file…",
          "Separate each activity into its own stand-alone question."
        ),

        qa_pair(
          "Response options — test whether distinctions resonate",
          "Likert-scale options (1 = Not true for me … 5 = Very true) were frequently ignored; respondents collapsed them into a simple yes/no.",
          "I know how to make a phone call by dialing a number. (1 Not true for me — 5 Very true for me)",
          "Do you know how to make a phone call by dialing a number? (Yes / No)"
        ),

        qa_pair(
          "Resonance with local realities — avoid email-centric wording",
          "“Attached file” is email-centric and may not resonate with mobile-first users who primarily use SMS or WhatsApp.",
          "Did you send a message, for example by e-mail, messaging service, or SMS, with an attached file, for example a document, picture, or video?",
          "Have you ever added a picture, video, or document to an email, SMS, or WhatsApp message?"
        ),

        qa_pair(
          "Cognitive mismatch — explain ambiguous concepts",
          "“Personal information” was often understood as family secrets or private thoughts rather than identifying digital details. An explainer box was added.",
          "When using the internet on a mobile phone, have you experienced any of the following situations in the last 12 months? A. Having personal information or photos used, taken, or shared without your consent…",
          "Has someone ever shared your personal information on the internet without your permission? [Explainer: Personal information means any details that can be used to identify you, such as your name, contact details, photos, ID numbers, bank details, or location.]"
        ),

        qa_pair(
          "Memory — separate experience from timing",
          "Fixed recall windows required respondents to calculate dates while recalling activities, leading many to respond in natural language rather than fitting their answer into the defined time frame.",
          "In the last 12 months, have you used the internet?",
          "Have you ever used the internet? — followed by — When was the last time you used the internet?"
        ),

      ),

      section("Pilot Testing",
        tags$p("Pilot testing evaluates the operational performance of the complete survey under field conditions. Where cognitive interviews ask “Are respondents understanding questions as intended?”, pilot testing asks “Can this survey be implemented smoothly?”"),
        div(style = "overflow-x:auto;",
          tags$table(class = "table table-striped",
            tags$caption("Cognitive interviewing vs. pilot testing"),
            tags$thead(tags$tr(
              tags$th(scope = "col", ""),
              tags$th(scope = "col", "Cognitive interviewing"),
              tags$th(scope = "col", "Pilot testing")
            )),
            tags$tbody(
              tags$tr(tags$th(scope = "row", "Focus"), tags$td("Cognitive match between intent and interpretation"), tags$td("Practical feasibility of implementation")),
              tags$tr(tags$th(scope = "row", "What it catches"), tags$td("Cognitive failures requiring qualitative probing"), tags$td("Obvious content or translation problems")),
              tags$tr(tags$th(scope = "row", "Researchers"), tags$td("Specially trained qualitative researchers"), tags$td("Quantitative survey team")),
              tags$tr(tags$th(scope = "row", "Scope"), tags$td("A prioritized subset of questions"), tags$td("The whole survey tool")),
              tags$tr(tags$th(scope = "row", "Key output"), tags$td("Refined question wording and improved validity"), tags$td("Refined survey tool and implementation plan"))
            )
          )
        ),
        tags$p("Pilot the refined instrument before full-scale implementation to test question sequence, survey length, skip patterns, programming logic, interviewer instructions, respondent burden and logistics.")
      )
    )
  )
}
