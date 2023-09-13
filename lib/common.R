# Load in required libraries
library(fs)
library(rprojroot)
library(sets)
library(poster)
library(shiny)
library(optparse)
library(hms)
library(glue)
library(humaniformat)
library(wru)
library(tidycensus)
library(furrr)
library(rlang)
library(tidyselect)
library(functional)
library(magrittr)
library(lubridate)
library(vctrs)
library(openssl)
library(broom)
library(tidyverse)

# Get the root dierctory
ROOT <- path(find_root(has_file(".gitignore")))

# Source required local libraries
source(path(ROOT, "lib", "utils.R"))
source(path(ROOT, "lib", "standards.R"))

# Set a future plan to avoid blowing out all the cores.
plan(multicore(workers=max(availableCores() %/% 2, 1)))
