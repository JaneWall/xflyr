## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE, warning = FALSE----------------------------------
library(xflyr)
#library(tidyverse)

## ----get_data-----------------------------------------------------------------
pilot_data <- xc_summary()
head(pilot_data)
nick_data <- pilot_data$nickname
head(nick_data)

