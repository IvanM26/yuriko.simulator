bold_percentage <- function(value) {
  shiny::strong(scales::label_percent(accuracy = 0.01)(value))
}
