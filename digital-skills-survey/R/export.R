library(openxlsx)
library(officer)

# ── XLSForm Excel export ────────────────────────────────────────────────────
detect_type <- function(question, response_options) {
  if (is.na(response_options) || trimws(response_options) == "") return("text")
  if (grepl("select all|tick all|all that apply", question, ignore.case = TRUE))
    return("select_multiple")
  "select_one"
}

is_yes_no <- function(opts) {
  if (is.na(opts) || trimws(opts) == "") return(FALSE)
  parts  <- trimws(strsplit(opts, ";")[[1]])
  labels <- trimws(sub("^[0-9]+\\.", "", parts))
  length(labels) == 2 && tolower(labels[1]) == "yes" && tolower(labels[2]) == "no"
}

# Strip leading code segments (e.g. IDL1_, DU_HEA1_, CHW_IDL1_) from IDs.
# make.unique() resolves any remaining duplicates by appending _1, _2, etc.
strip_id <- function(ids) {
  make.unique(sub("^([A-Z][A-Z0-9a-z]*_)+", "", ids), sep = "_")
}

# Translate relevance expressions to ODK syntax:
#   original IDs → ${stripped_id}, == → =
translate_relevance <- function(rel_vec, id_map) {
  sapply(rel_vec, function(rel) {
    if (is.na(rel) || trimws(rel) == "") return("")
    result <- rel
    for (orig in names(id_map)[order(-nchar(names(id_map)))]) {
      result <- gsub(paste0("(?<![A-Za-z0-9_])", orig, "(?![A-Za-z0-9_])"),
                     paste0("${", id_map[[orig]], "}"), result, perl = TRUE)
    }
    gsub("==", "=", result, fixed = TRUE)
  }, USE.NAMES = FALSE)
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
  types        <- mapply(detect_type, items_df$question, items_df$response_options)
  yn_flags     <- mapply(is_yes_no,   items_df$response_options)
  stripped_ids <- strip_id(items_df$id)
  id_map       <- setNames(stripped_ids, items_df$id)
  list_names   <- ifelse(yn_flags, "yesno", stripped_ids)

  survey_df <- data.frame(
    type      = ifelse(types %in% c("select_one", "select_multiple"),
                       paste(types, list_names), types),
    name      = stripped_ids,
    label     = items_df$question,
    relevance = translate_relevance(items_df$relevance, id_map),
    stringsAsFactors = FALSE
  )

  # One shared yes_no list; individual lists only for non-yes/no questions
  yes_no_rows  <- data.frame(list_name = "yesno", name = c("1", "0"),
                              label = c("Yes", "No"), stringsAsFactors = FALSE)
  choices_list <- mapply(function(opts, qid, yn) if (yn) NULL else parse_choices(opts, qid),
                         items_df$response_options, stripped_ids, yn_flags,
                         SIMPLIFY = FALSE)
  other_rows   <- do.call(rbind, Filter(Negate(is.null), choices_list))
  choices_rows <- if (is.null(other_rows)) yes_no_rows else rbind(yes_no_rows, other_rows)

  wb <- createWorkbook()
  addWorksheet(wb, "survey")
  addWorksheet(wb, "choices")
  writeData(wb, "survey",  survey_df)
  writeData(wb, "choices", choices_rows)
  saveWorkbook(wb, filepath, overwrite = TRUE)
}

# ── View Excel export (human-readable, one sheet per module) ─────────────
export_view_xlsx <- function(items_df, filepath) {
  wb <- createWorkbook()

  header_style <- createStyle(fontColour = "#ffffff", fgFill = "#003366",
                               textDecoration = "bold", halign = "left",
                               wrapText = TRUE)
  wrap_style   <- createStyle(wrapText = TRUE, valign = "top")
  bold_style   <- createStyle(textDecoration = "bold", wrapText = TRUE,
                               valign = "top")

  for (mod in unique(items_df$module)) {
    mod_df     <- items_df[items_df$module == mod, ]
    sheet_name <- substr(gsub("[\\[\\]\\*\\?:/\\\\]", "", mod), 1, 31)
    addWorksheet(wb, sheet_name)

    view_df <- data.frame(
      `No.`              = mod_df$number,
      `Question`         = mod_df$question,
      `Response Options` = ifelse(is.na(mod_df$response_options), "",
                                  gsub(";\\s*", "\n", mod_df$response_options)),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    writeData(wb, sheet_name, view_df)
    addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
    addStyle(wb, sheet_name, wrap_style,   rows = seq_len(nrow(view_df)) + 1,
             cols = 1:3, gridExpand = TRUE)

    # Bold core questions
    core_rows <- which(mod_df$recommended_core) + 1
    if (length(core_rows))
      addStyle(wb, sheet_name, bold_style, rows = core_rows, cols = 1:3,
               gridExpand = TRUE)

    setColWidths(wb, sheet_name, cols = 1, widths = 8)
    setColWidths(wb, sheet_name, cols = 2, widths = 60)
    setColWidths(wb, sheet_name, cols = 3, widths = 40)
  }

  saveWorkbook(wb, filepath, overwrite = TRUE)
}

# ── Word survey export ────────────────────────────────────────────────────
export_word <- function(items_df, filepath) {
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)

  doc <- read_docx()
  avail_styles <- styles_info(doc)$style_name
  title_style  <- if ("Title" %in% avail_styles) "Title" else "heading 1"
  opt_style    <- if ("List Bullet" %in% avail_styles) "List Bullet" else "Normal"

  # Document title — not a heading, so Word won't number it
  doc <- body_add_par(doc, "Digital Skills Survey", style = title_style)

  modules <- unique(items_df$module)
  for (mod in modules) {
    mod_df <- items_df[items_df$module == mod, ]
    # Plain module name — Word's heading 1 outline numbering handles the number
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

          if (!is.na(row$relevance) && trimws(row$relevance) != "")
            doc <- body_add_par(doc,
              paste0("[Ask if: ", row$relevance, "]"), style = "Normal")

          doc <- body_add_par(doc,
            paste0(num, ". ", row$question), style = "Normal")

          if (!is.na(row$response_options) && trimws(row$response_options) != "") {
            opts <- trimws(strsplit(row$response_options, ";")[[1]])
            for (opt in opts)
              doc <- body_add_par(doc, opt, style = opt_style)
          }

          doc <- body_add_par(doc, "", style = "Normal")
        }
      }
    }
  }

  print(doc, target = tmp)
  file.copy(tmp, filepath, overwrite = TRUE)
}
