bold_percentage <- function(value) {
  shiny::strong(scales::label_percent(accuracy = 0.01)(value))
}

render_card <- function(decklist, input, card_index, card_height = 250) {
  shiny::renderUI({
    img_src <- decklist |>
      dplyr::filter(card_name_scryfall == input[[glue::glue("card{ card_index }")]]) |> 
      dplyr::pull(img_src)
    
    shiny::tags$img(
      src = img_src,
      alt = glue::glue("card{ card_index }"),
      crossorigin = "anonymous",
      height = card_height
    )
  })
}
