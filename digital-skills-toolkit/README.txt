Digital Skills Toolkit — R Shiny source

Includes Survey Builder and resource tabs 2–6.
Keep the R/ and data/ folders alongside app.R.

Install dependencies once in R:
install.packages(c("shiny", "DT", "dplyr", "readr", "openxlsx", "officer"))

Set the R working directory to this extracted folder, then run:
shiny::runApp(".")

Optional verification:
source("check.R")

run.R is an alternative server launcher; it defaults to port 3000.
