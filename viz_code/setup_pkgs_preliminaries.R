# Loading libraries -------------------------------------------------------
library(tidyverse)
library(showtext)
library(readr)
library(janitor)
library(patchwork)
library(ggtext)
library(systemfonts)
library(treemapify)
library(paletteer)

# Setting up custom fonts
cus_fnt <- match_font("Roboto Condensed")$path
cus_fnt <- system_fonts()$family[system_fonts()$path == cus_fnt]


register_variant(
  name = "cabinet_grotesk",
  family = "Cabinet Grotesk",
  features = font_feature(letters = "stylistic")
)