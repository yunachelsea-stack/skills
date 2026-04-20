library(openxlsx)

# Infer XLSForm type from question text
detect_type <- function(question, response_options) {
  if (is.na(response_options) || trimws(response_options) == "") return("text")
  if (grepl("select all|tick all|all that apply", question, ignore.case = TRUE)) {
    return("select_multiple")
  }
  "select_one"
}

# Parse "1. Label; 2. Label" or slash-separated options into a choices data.frame
parse_choices <- function(opts, qid) {
  if (is.na(opts) || trimws(opts) == "") return(NULL)

  parts <- trimws(strsplit(opts, ";")[[1]])

  if (all(grepl("^[0-9]+\\.", parts))) {
    return(data.frame(
      list_name = qid,
      name      = sub("^([0-9]+)\\..*", "\\1", parts),
      label     = trimws(sub("^[0-9]+\\.", "", parts)),
      stringsAsFactors = FALSE
    ))
  }

  # Fallback: slash-separated
  parts2 <- trimws(strsplit(opts, "/")[[1]])
  parts2 <- parts2[nchar(parts2) > 0]
  data.frame(
    list_name = qid,
    name      = as.character(seq_along(parts2)),
    label     = parts2,
    stringsAsFactors = FALSE
  )
}

export_xlsform <- function(items_df, filepath) {
  types <- mapply(detect_type, items_df$question, items_df$response_options)

  survey_df <- data.frame(
    type      = ifelse(
      types %in% c("select_one", "select_multiple"),
      paste(types, items_df$id),
      types
    ),
    name      = items_df$id,
    label     = items_df$question,
    relevance = ifelse(is.na(items_df$relevance), "", items_df$relevance),
    required  = "",
    stringsAsFactors = FALSE
  )

  choices_list <- mapply(parse_choices, items_df$response_options, items_df$id,
                         SIMPLIFY = FALSE)
  choices_rows <- do.call(rbind, Filter(Negate(is.null), choices_list))
  if (is.null(choices_rows)) {
    choices_rows <- data.frame(list_name = character(), name = character(),
                               label = character())
  }

  wb <- createWorkbook()
  addWorksheet(wb, "survey")
  addWorksheet(wb, "choices")
  writeData(wb, "survey",  survey_df)
  writeData(wb, "choices", choices_rows)
  saveWorkbook(wb, filepath, overwrite = TRUE)
}
