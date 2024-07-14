#' @export
process_card_data <- function(filepath, run_checks = TRUE) {
  parse_txt_file(filepath, run_checks) |> 
    add_scryfall_data() |> 
    add_custom_attributes()
}

parse_txt_file <- function(filepath, run_checks) {

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
  
  if (run_checks) {

    # Check that the main deck has 99 cards
    if (sum(cards_as_tibble$n_cards) != 99) {
      cli::cli_abort("Main deck should have 99 cards")
    }

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
          card_name = card_name,
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
          card_name = card_name,
          layout = card_data$layout,
          name = card_data$name,
          cmc = card_data$cmc,
          colors = paste0(card_data$card_faces$colors[[1]], collapse = ""),
          mana_cost = paste0(card_data$card_faces$mana_cost[[1]], collapse = ""),
          type = card_data$type_line,
          produced_mana = paste0(card_data$produced_mana, collapse = ""),
          img_src = card_data$card_faces$image_uris.png[[1]]
        )
      }

      if (card_data$layout == "modal_dfc") {
        out <- tibble::tibble(
          card_name = card_name,
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

  card_data |>
    edit_scryfall_data() |> 
    dplyr::mutate(

      enabler = is_enabler(cmc, type),

      # Enabler cmc
      enabler_0 = is_enabler_0(enabler, mana_cost),
      enabler_1 = is_enabler_1(enabler, cmc, mana_cost),      
      enabler_2 = is_enabler_2(enabler, cmc),
      enabler_3 = is_enabler_3(enabler, cmc),

      # Enabler mana cost (non-mdfc)
      enabler_c = is_enabler_c(enabler_1, mana_cost),
      enabler_u = is_enabler_u(enabler_1, mana_cost),
      enabler_b = is_enabler_b(enabler_1, mana_cost),

      enabler_1u = is_enabler_1u(enabler_2, mana_cost),
      enabler_1b = is_enabler_1b(enabler_2, mana_cost),
      enabler_uu = is_enabler_uu(enabler_2, mana_cost),
      enabler_bb = is_enabler_bb(enabler_2, mana_cost),
      enabler_ub = is_enabler_ub(enabler_2, mana_cost),

      enabler_2u = is_enabler_2u(layout, enabler_3, mana_cost),
      enabler_2b = is_enabler_2b(layout, enabler_3, mana_cost),

      # Enabler mana cost (mdfc)
      mdfc_enabler_2u = is_mdfc_enabler_2u(layout, enabler_3, mana_cost),
      mdfc_enabler_2b = is_mdfc_enabler_2b(layout, enabler_3, mana_cost),

      # Card color
      color_u = is_color_u(mana_cost),
      color_b = is_color_b(mana_cost),

      # Lands (non-mdfc)
      land = is_land(type),
      color_land = is_color_land(land, produced_mana),
      land_u = is_land_u(color_land, produced_mana),
      land_b = is_land_b(color_land, produced_mana),

      # Lands (mdfc)
      mdfc_land = is_mdfc_land(layout, type),
      mdfc_land_u = is_mdfc_land_u(mdfc_land, produced_mana),
      mdfc_land_b = is_mdfc_land_b(mdfc_land, produced_mana),

      # Cards that require additional logic
      chrome_mox = card_name == "Chrome Mox",
      dark_ritual = card_name == "Dark Ritual",
      lotus_petal = card_name == "Lotus Petal",
      mana_crypt = card_name == "Mana Crypt",
      mox_diamond = card_name == "Mox Diamond"

    )
}

edit_scryfall_data <- function(card_data) {

  # For simplicity, we assume that we are playing first
  # which means Gemstone Caverns produces only colorless mana
  if ("Gemstone Caverns" %in% card_data$name) {
    card_data <- card_data |> 
      dplyr::mutate(
        produced_mana = ifelse(
          test = name == "Gemstone Caverns",
          yes = "C",
          no = produced_mana
        )
      )
  }
  
  # With Demonic Tutor we can tutor an Enabler with MV = 0
  # So we can consider Demonic Tutor as an Enabler that costs 1B
  # (for which logic was already defined)
  if ("Demonic Tutor" %in% card_data$name) {
    card_data <- card_data |> 
      dplyr::mutate(
        type = ifelse(
          test = name == "Demonic Tutor",
          yes = "Creature // Sorcery",
          no = type
        )
      )
  }
  
  card_data

}
