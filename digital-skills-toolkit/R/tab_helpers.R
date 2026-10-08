# Shared CSS and helper functions for all content tab pages

tab_shared_css <- tags$style(HTML("
  .ca-page{max-width:1100px;margin:auto;padding:32px 24px 60px;color:#263e50}
  .ca-header{border-left:5px solid #e8a800;padding:12px 24px;background:#f3f7fa}
  .ca-header h2{color:#0d3b5e;margin-top:8px}
  .ca-section{margin-top:30px;line-height:1.7}
  .ca-section h3{color:#0d3b5e}
  .ca-step{border:1px solid #dce3e8;border-radius:4px;margin:10px 0;background:#fff}
  .ca-step summary{padding:16px;cursor:pointer;font-weight:600;color:#0d3b5e}
  .ca-step summary:focus-visible{outline:3px solid #e8a800}
  .ca-step-body{padding:0 20px 16px}
  .ca-note{background:#fff8e4;padding:16px 20px;border-left:3px solid #e8a800}
  .ca-source{font-size:12px;color:#607381;margin-top:14px}
  .ca-page li{margin-bottom:6px}

  /* Before/after question cards */
  .ca-qa-pair{margin:18px 0}
  .ca-qa-original,.ca-qa-revised{padding:12px 16px;border-radius:4px;margin-bottom:6px}
  .ca-qa-original{background:#fff4f0;border-left:4px solid #c0392b}
  .ca-qa-revised{background:#f0f9f4;border-left:4px solid #1a7a4a}
  .ca-qa-label{font-size:11px;font-weight:700;letter-spacing:.06em;text-transform:uppercase;margin-bottom:6px}
  .ca-qa-original .ca-qa-label{color:#c0392b}
  .ca-qa-revised .ca-qa-label{color:#1a7a4a}
  .ca-qa-original p,.ca-qa-revised p{margin:0;font-style:italic}

  /* Process flow */
  .ca-flow{display:flex;flex-wrap:wrap;align-items:center;gap:6px;margin:16px 0}
  .ca-flow-step{background:#0d3b5e;color:#fff;padding:6px 14px;border-radius:20px;font-size:13px;font-weight:600;white-space:nowrap}
  .ca-flow-arrow{color:#607381;font-size:18px;line-height:1}

  /* Numbered step badges — only when summary contains a badge span */
  .ca-step summary{display:flex;align-items:center;gap:12px}
  .ca-step-num{display:inline-flex;align-items:center;justify-content:center;min-width:26px;height:26px;border-radius:50%;background:#0d3b5e;color:#fff;font-size:12px;font-weight:700;flex-shrink:0;line-height:1}

  /* Wording example cards (always-visible, no details) */
  .ca-example{border:1px solid #dce3e8;border-radius:4px;margin:14px 0;background:#fff}
  .ca-example-head{padding:12px 16px;background:#f3f7fa;border-bottom:1px solid #dce3e8;font-weight:600;color:#0d3b5e;font-size:14px}
  .ca-example-body{padding:12px 16px}
  .ca-example-issue{font-size:13px;color:#607381;margin-bottom:10px}
"))

tab_section <- function(title = NULL, ...) {
  if (is.null(title)) {
    tags$section(class = "ca-section", ...)
  } else {
    tags$section(class = "ca-section", tags$h3(title), ...)
  }
}

tab_step <- function(title, ...) {
  tags$details(class = "ca-step",
    tags$summary(title), div(class = "ca-step-body", ...))
}

tab_source_note <- function(reference) {
  tags$p(class = "ca-source",
    paste("Source: Digital Skills Measurement Toolkit (2026) —", reference))
}

tab_trow <- function(label, detail) {
  tags$tr(tags$th(scope = "row", label), tags$td(detail))
}
