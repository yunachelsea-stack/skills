packages <- c("shiny", "DT", "dplyr", "readr", "openxlsx", "officer")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
port <- as.integer(Sys.getenv("PORT", "3000"))
if (is.na(port)) stop("PORT must be an integer")
shiny::runApp(".", host = "0.0.0.0", port = port, launch.browser = FALSE)
