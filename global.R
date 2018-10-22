pacs <- c("shiny",
          "shinythemes",
          "dplyr",
          "readxl",
          "XLConnect",
          "seasonal",
          "x13binary",
          "magrittr",
          "tidyr")
newpacs <- pacs[!(pacs %in% installed.packages()[,"Package"])]
if (length(newpacs)>0) install.packages(newpacs)
for (i in pacs) library(i,character.only=T)

"%+%" <- function(a,b) paste0(a,b)

# RUN ONLY ONCE:
# Sys.setenv(X13_PATH = "C:/WinX13/x13as")
# write('Sys.setenv(X13_PATH = "C:/WinX13/x13as")', file = "~/.Rprofile", append = TRUE)
