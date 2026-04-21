library(openxlsx)
library(officer)

# ── XLSForm Excel export ────────────────────────────────────────────────────
detect_type <- function(question, response_options) {
  if (is.na(response_options) || trimws(response_options) == "") return("text")
  if (grepl("select all|tick all|all that apply", question, ignore.case = TRUE)) {
    return("select_multiple")
  }
  "select_one"
}

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
    type      = ifelse(types %in% c("select_one", "select_multiple"),
                       paste(types, items_df$id), types),
    name      = items_df$id,
    label     = items_df$question,
    relevance = ifelse(is.na(items_df$relevance), "", items_df$relevance),
    required  = "",
    stringsAsFactors = FALSE
  )
  choices_list <- mapply(parse_choices, items_df$response_options, items_df$id,
                         SIMPLIFY = FALSE)
  choices_rows <- do.call(rbind, Filter(Negate(is.null), choices_list))
  if (is.null(choices_rows))
    choices_rows <- data.frame(list_name = character(), name = character(),
                               label = character())
  wb <- createWorkbook()
  addWorksheet(wb, "survey")
  addWorksheet(wb, "choices")
  writeData(wb, "survey",  survey_df)
  writeData(wb, "choices", choices_rows)
  saveWorkbook(wb, filepath, overwrite = TRUE)
}

# ── Word survey export ────────────────────────────────────────────────────
export_word <- function(items_df, filepath) {
  doc <- read_docx()

  # Text style definitions
  q_text_prop  <- fp_text(font.size = 11)
  opt_prop     <- fp_text(font.size = 10, color = "#888888", italic = TRUE)
  skip_prop    <- fp_text(font.size = 9,  color = "#b36200", italic = TRUE)
  opt_par      <- fp_par(padding.left = 300)

  doc <- body_add_par(doc, "Digital Skills Survey", style = "Title")

  modules <- unique(items_df$module)

  for (mod in modules) {
    mod_df <- items_df[items_df$module == mod, ]
    doc <- body_add_par(doc, mod, style = "heading 1")

    domains <- unique(mod_df$competency_domain)
    for (dom in domains) {
      dom_df <- mod_df[mod_df$competency_domain == dom, ]
      doc <- body_add_par(doc, dom, style = "heading 2")

      skills <- unique(dom_df$skill_area)
      for (sk in skills) {
        sk_df <- dom_df[dom_df$skill_area == sk, ]
        doc <- body_add_par(doc, sk, style = "heading 3")

        for (i in seq_len(nrow(sk_df))) {
          row <- sk_df[i, ]
          num <- sub("_.*$", "", row$id)

          # Skip logic note (before the question)
          if (!is.na(row$relevance) && trimws(row$relevance) != "") {
            doc <- body_add_fpar(doc,
              fpar(ftext(paste0("[Ask if: ", row$relevance, "]"),
                         prop = skip_prop),
                   fp_p = opt_par)
            )
          }

          # Question text
          doc <- body_add_fpar(doc,
            fpar(ftext(paste0(num, ". ", row$question), prop = q_text_prop))
          )

          # Response options
          if (!is.na(row$response_options) && trimws(row$response_options) != "") {
            doc <- body_add_fpar(doc,
              fpar(ftext(row$response_options, prop = opt_prop), fp_p = opt_par)
            )
          }
        }
      }
    }
  }

  print(doc, target = filepath)
}
