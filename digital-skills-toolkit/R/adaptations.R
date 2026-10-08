adaptations_tab <- function() {
  section <- tab_section
  step    <- tab_step
  tabPanel("Conceptual Adaptations", value = "adaptations",
    div(class = "ca-page",
      div(class = "ca-header",
        tags$small("RESOURCE 02 · SURVEY TOOL DEVELOPMENT"),
        tags$h2("Refining the Survey Tool"),
        tags$p("Adapt the survey to local languages, technologies and experiences while preserving the intended meaning of each question."),
        tags$p(class = "ca-source",
          "Source: Digital Skills Measurement Toolkit (2026), Chapter 6: Refining the Survey Tool, sections 6.1–6.3. This page summarizes the report; it does not automatically change Survey Builder questions.")
      ),
      section("6.1 Finalize Digital Skills for Assessment",
        tags$p("Consult government agencies, implementing partners, technical and sector experts, researchers, and representatives of the intended survey population. Confirm that the selected competencies match respondents’ roles, locally used technologies and workflows, and the assessment objectives."),
        tags$p("Consider both current needs and emerging digital systems. Repeat consultations where consensus has not been reached, and periodically review the instrument as technologies evolve."),
        tags$p("Consult stakeholders before refinement to establish priorities, then return to them after cognitive interviews and pilot testing to review the evidence, endorse the final instrument and prioritize critical items if questionnaire length is a concern.")
      ),
      section("6.2 Contextual Adaptations through Cognitive Interviews",
        tags$p("A literal translation is not enough. Cognitive interviews explore whether respondents understand a question as the researcher intends, how they recall relevant experiences, and how they choose an answer. Qualitative probing reveals problems that an ordinary survey response may hide."),
        tags$h4("Where cognitive interviews fit"),
        tags$ol(
          tags$li("Generate draft items through interviews, literature reviews, existing survey tools and expert input."),
          tags$li("Translate as needed and conduct cognitive interviews to check clarity and comprehension."),
          tags$li("Revise, refine or remove items using expert feedback and respondent input."),
          tags$li("Pilot the refined tool with the target population before finalizing it for wider use.")
        ),
        div(class = "ca-note",
          tags$strong("Ask the survey question first, exactly as written."),
          tags$p("Record the answer using the available response options. Then probe the respondent’s interpretation—for example, “What does this word mean to you?”—without treating the probe as part of the final survey.")
        )
      ),
      section("Five steps for conducting cognitive interviews",
        tags$p("Open each step for practical guidance from section 6.2."),
        step("1. Define the scope",
          tags$p("Prioritize newly developed, conceptually complex or central questions, especially those entering a new cultural or linguistic setting. Testing every question is ideal but may not be feasible because probing takes time."),
          tags$p("For longer instruments, divide priority questions into smaller interview guides while retaining a logical flow.")
        ),
        step("2. Select and train researchers",
          tags$p("Recruit researchers fluent in the source and local languages, skilled in qualitative interviewing, and familiar with structured survey administration. Consider how gender, age and social background may affect rapport."),
          tags$p("Train on study objectives, question intent, research ethics and probing. Use role plays and practice interviews to switch between asking the exact survey wording and exploring understanding.")
        ),
        step("3. Select sample participants",
          tags$p("Recruit people with the same or a similar profile to the intended survey population. Include people with lower literacy or education, limited survey experience, and marginalized groups whose difficulties can reveal inaccessible wording."),
          tags$p("The report describes two to three rounds with approximately 8–12 participants per round, revising and retesting after each round. Cover relevant experiences and consider oversampling rare experiences where necessary."),
          tags$p(class = "ca-source", "Source: supplied toolkit (2026), section 6.2, Step 3. These are cognitive-interview guidelines, not sample-size guidance for the main survey.")
        ),
        step("4. Collect, debrief, revise and retest",
          tags$p("Typically, one researcher interviews while another takes detailed notes. Ask the original question, record the answer, then use scripted or emerging probes to explore confusing words, interpretation and response choices."),
          tags$p("Debrief daily and analyze findings near real time. Review each question, document problems, propose changes, and test revised wording in the next round. Repeat until interpretation aligns with question intent.")
        ),
        step("5. Support quantitative survey training",
          tags$p("Involve cognitive-interview researchers in enumerator training. Explain why wording was chosen, common misunderstandings, culturally sensitive issues and how to handle unexpected responses."),
          tags$p("Help enumerators understand the rationale while asking questions exactly as tested, reducing improvisation and inconsistent administration.")
        )
      ),
      section("What to look for—and how wording can improve",
        tags$p("Examples below are condensed from Table 6.1. Test adaptations locally rather than treating these examples as universally suitable replacements."),
        step("Word choice: connect “internet” to recognizable activities",
          tags$p("Some respondents did not recognize WhatsApp or YouTube use as internet use; “any location and any device” also distracted respondents."),
          tags$blockquote("Have you ever used the internet? For example, WhatsApp, Facebook, YouTube, Google, and so on [add other locally relevant examples of internet].")
        ),
        step("Syntax and structure: keep questions short and independent",
          tags$p("Mentioning phone types distracted some respondents from whether they had ever used a phone. The revised question is:"),
          tags$blockquote("Have you ever used a mobile phone?"),
          tags$p("Avoid long introductory instructions followed by many subquestions. Make each item a stand-alone question so respondents do not have to retain the earlier instructions.")
        ),
        step("Response options: test whether distinctions are meaningful",
          tags$p("In the report’s example, respondents often ignored graded options such as “a bit true,” “mostly true” and “very true,” instead answering yes or no. The statement “I know how to make a phone call by dialing a number” was revised to:"),
          tags$blockquote("Do you know how to make a phone call by dialing a number?"),
          tags$p("Response options: Yes / No. This is a finding from the report’s cognitive testing, not a rule to replace every rating scale.")
        ),
        step("Local relevance: avoid email-only terminology",
          tags$p("“Attached file” may not resonate with mobile-first users. The report revises this to:"),
          tags$blockquote("Have you ever added a picture, video, or document to an email, SMS, or WhatsApp message?")
        ),
        step("Conceptual match: explain ambiguous terms",
          tags$p("“Personal information” was sometimes interpreted as family secrets or private thoughts. The report adds an explainer describing identifying details such as names, contact details, photos, ID numbers, login details and location information."),
          tags$blockquote("Has someone ever shared your personal information on the internet without your permission?")
        ),
        step("Recall: separate experience from timing",
          tags$p("Fixed recall windows can require respondents to calculate dates while recalling activities. The report separates the questions:"),
          tags$blockquote("Have you ever used the internet?"),
          tags$blockquote("When was the last time you used the internet?")
        ),
        tags$p(class = "ca-source", "Source: supplied toolkit (2026), Table 6.1, citing Scott et al. (2026).")
      ),
      section("6.3 Pilot Testing",
        tags$p("Pilot testing is the final opportunity to refine the complete instrument and field procedures before full-scale implementation. Test question sequence, survey length, translations, skip patterns, programming logic, interviewer instructions, respondent burden, logistics and overall data quality."),
        tags$p(tags$strong("Cognitive interviews ask: "), "“Are respondents understanding questions as intended?” They use trained qualitative researchers to examine selected questions and refine wording and validity."),
        tags$p(tags$strong("Pilot testing asks: "), "“Can this survey be implemented smoothly?” The quantitative survey team tests the whole instrument under field conditions, including length, skip patterns, programming, instructions, respondent burden and logistics."),
        tags$p("Pilot the refined instrument before full-scale implementation, then finalize the survey tool and field procedures."),
        tags$p(class = "ca-source", "Source: supplied toolkit (2026), section 6.3 and Table 6.2.")
      )
    )
  )
}
