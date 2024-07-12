parse_txt_file <- function(filepath) {

  # Read cards into a character vector
  cards <- readLines(filepath, warn = FALSE)

  # TODO: Error handler when the file is not structured correctly

  # Find the index of the last card in the main deck
  index_last_card_in_main_deck <- which(cards == "")[[1]] - 1

  # Remove all cards that are not part of the main deck
  cards <- cards[seq_len(index_last_card_in_main_deck)]

  cards_as_tibble <- cards |> 
    # Convert to tibble
    tibble::tibble() |> 
    # Separate into n_cards and card_name columns
    tidyr::separate(
      col = "cards",
      into = c("n_cards", "card_name"),
      sep = " ",
      extra = "merge",
      convert = TRUE
    )

  # Check that the main deck has 99 cards
  if (sum(cards_as_tibble$n_cards) != 99) {
    cli::cli_abort("Main deck should have 99 cards")
  }

  return(cards_as_tibble)

}

add_scryfall_data <- function(card_data) {

  card_data |> 
    dplyr::pull(card_name) |> 
    purrr::map(.f = function(card_name) {
      
      # Wait between requests to the API
      Sys.sleep(0.1)

      # API endpoint and query parameters
      url <- "https://api.scryfall.com/cards/named"
      query_params <- list(exact = card_name)
      
      # Make GET request to Scryfall API
      response <- httr::GET(url, query = query_params)
      
      # Parse JSON content
      card_data <- response |>
        httr::content(as = "text") |> 
        jsonlite::fromJSON(flatten = TRUE)

      if (card_data$layout %in% c("split", "flip", "adventure", "normal")) {
        out <- tibble::tibble(
          layout = card_data$layout,
          name = card_data$name,
          cmc = card_data$cmc,
          colors = paste0(card_data$colors, collapse = ""),
          mana_cost = card_data$mana_cost,
          type = card_data$type_line,
          produced_mana = paste0(card_data$produced_mana, collapse = ""),
          img_src = card_data$image_uris$png
        )
      }

      if (card_data$layout %in% c("transform")) {
        out <- tibble::tibble(
          layout = card_data$layout,
          name = card_data$name,
          cmc = card_data$cmc,
          colors = paste0(card_data$colors, collapse = ""),
          mana_cost = card_data$mana_cost,
          type = card_data$type_line,
          produced_mana = paste0(card_data$produced_mana, collapse = ""),
          img_src = card_data$card_faces$image_uris.png[[1]]
        )
      }

      if (card_data$layout == "modal_dfc") {
        out <- tibble::tibble(
          layout = card_data$layout,
          name = card_data$name,
          cmc = card_data$cmc,
          colors = paste0(card_data$card_faces$colors[[1]], collapse = ""),
          mana_cost = paste0(card_data$card_faces$mana_cost, collapse = " // "),
          type = paste0(card_data$card_faces$type_line, collapse = " // "),
          produced_mana = paste0(card_data$produced_mana, collapse = ""),
          img_src = card_data$card_faces$image_uris.png[[1]]
        )
      }
      
      # TODO: add logic for "prototype" layout
      
      message(card_name)
      
      out

    }) |> 
    purrr::list_rbind()

}

add_custom_attributes <- function(card_data) {
  # TODO
}
