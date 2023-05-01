library(tidyverse)
library(Linkplots)
library(mice)
library(mgcv)

source("functions_kaggle.R", encoding = "UTF8")

# Thème LinkPact
theme_LinkPact() |> theme_set() # le theme est utilise pour tous les graphiques futurs

# Couleurs du thème
lkp_blue  <- grDevices::rgb(0, 34, 93, maxColorValue = 255)  # Bleu LinkPact
lkp_green <- grDevices::rgb(0, 136, 81, maxColorValue = 255) # Vert LinkPact
lkp_magenta <- grDevices::rgb(148, 0, 113, maxColorValue = 255) # Magenta LinkPact
lkp_orange <- grDevices::rgb(237, 127, 16, maxColorValue = 255) # Orange LinkPact
lkp_lightblue <- grDevices::rgb(0, 113, 148, maxColorValue = 255) # Gris LinkPact
lkp_grey <- grDevices::rgb(140, 142, 145, maxColorValue = 255) # Gris LinkPact

lkp_colors <- c(lkp_lightblue, lkp_green, lkp_magenta, lkp_orange, lkp_blue, lkp_grey)

# Options de mise en page
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "",
                      rows.print = 20,
                      fig.align = "center", 
                      fig.width = 10, 
                      fig.asp = 0.618)

# Taille des animations
options(gganimate.dev_args = list(width = 0.9 * 960, height = 0.9 * 0.618 * 960))
options(knitr.kable.NA = "")

# Mise en forme des tables
output_format <- function() {
  
  if (is_html_output()) return("html")
  if (is_latex_output()) return("latex")
  return("other")
}

kable_LinkPact <- function(..., format = output_format()) {
  
  format |> 
    switch(html = kable_LinkPact_html,
           latex = kable_LinkPact_latex,
           kable) |> 
    do.call(args = list(...))
}

kable_LinkPact2 <- function(...) kable_LinkPact(..., dims = 2)

kable_LinkPact_html <- function(df, caption = "", digits = 2, dims = 1, font_size = NULL, ...) {
  
  out <- df |> kableExtra::kbl(caption = caption,
                   digits = digits,
                   format.args = list(big.mark = " ", dec = ","),
                   table.attr = paste0("class=\"t", dims,"\""),
                   ...) |> 
    kableExtra::row_spec(0, extra_css = "text-align: center; font-weight: bold;")
  
  if (dims == 2) out <- out |> 
      kableExtra::column_spec(1, extra_css = "text-align: center; font-weight: bold;")
  
  out |>
    kableExtra::kable_styling(c("striped", "condensed"), full_width = FALSE, font_size = font_size) |>
    kableExtra::scroll_box(box_css = "max-height: 40em;",
               extra_css = "overflow-x: auto; overflow-y: auto;")
}

kable_LinkPact_latex <- function(df, caption = NULL, digits = 2, dims = 1, font_size = NULL, ...) {
  
  if (is.null(caption)) {
    out <- df |> kableExtra::kbl(digits = digits,
                     format.args = list(big.mark = " ", dec = ","),
                     table.envir = "table",
                     ...)
  } else {
    out <- df |> kableExtra::kbl(caption = caption,
                     digits = digits,
                     format.args = list(big.mark = " ", dec = ","),
                     ...)
  }
  
  if (dims == 2) {
    out |> 
      kableExtra::row_spec(0, bold = TRUE, color = "white", background = lkp_green, align = c("|c|", rep("c|", ncol(df)))) |> 
      kableExtra::column_spec(1, border_left = TRUE, bold = TRUE, color = "white", background = lkp_green) |> 
      kableExtra::column_spec(ncol(df) + 1, border_right = TRUE)
  } else {
    out |> 
      kableExtra::row_spec(0, bold = TRUE, color = "white", background = lkp_green, align = c("|c|", rep("c|", ncol(df) - 1))) |> 
      kableExtra::column_spec(1, border_left = TRUE) |> 
      kableExtra::column_spec(ncol(df), border_right = TRUE)
  } |> 
    kableExtra::kable_styling(c("striped", "condensed"), full_width = FALSE, font_size = font_size)
}