library(openxlsx)

export_xlsform <- function(items_df, filepath) {
  survey_rows <- data.frame(
    type     = ifelse(items_df$type == "select_one",
                      paste("select_one", items_df$id),
                      items_df$type),
    name     = items_df$id,
    label    = items_df$question,
    required = "",
    stringsAsFactors = FALSE
  )

  sel_items <- items_df[items_df$type == "select_one", ]
  choices_rows <- do.call(rbind, lapply(seq_len(nrow(sel_items)), function(i) {
    data.frame(
      list_name = sel_items$id[i],
      name      = c("1", "2"),
      label     = c("Yes", "No"),
      stringsAsFactors = FALSE
    )
  }))

  wb <- createWorkbook()
  addWorksheet(wb, "survey")
  addWorksheet(wb, "choices")
  writeData(wb, "survey",  survey_rows)
  writeData(wb, "choices", choices_rows)
  saveWorkbook(wb, filepath, overwrite = TRUE)
}
