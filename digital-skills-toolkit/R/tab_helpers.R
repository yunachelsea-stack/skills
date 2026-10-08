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
"))

tab_section <- function(title, ...) {
  tags$section(class = "ca-section", tags$h3(title), ...)
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
