if (!("shiny" %in% installed.packages()[,"Package"])) {
  install.packages("shiny")
}
shiny::runGitHub("shiny-X13SA","ftvalentini")
