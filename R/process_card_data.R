#' @export
process_card_data <- function(decklist_source, source_path, use_httr) {
  parse_decklist(decklist_source, source_path) |> 
    add_scryfall_data(use_httr) |> 
    add_custom_attributes()
}

parse_decklist <- function(decklist_source, source_path) {
  
  if (decklist_source == "moxfield_url") {
    
    moxfield_json <- source_path |> 
      stringr::str_replace("https://www.moxfield.com/decks", "https://api.moxfield.com/v2/decks/all") |> 
      jsonlite::fromJSON(flatten = TRUE)
    
    tibble::tibble(
      card_name_source = names(moxfield_json$mainboard) |> sort()
    )
    
  } else if (decklist_source == "file") {
    
    # Read cards into a character vector
    cards <- readLines(source_path, warn = FALSE)
    
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
        into = c("n_cards", "card_name_source"),
        sep = " ",
        extra = "merge",
        convert = TRUE
      )

  }
  
}

add_scryfall_data <- function(card_data, use_httr) {

  # Initialize an empty list to store the data
  scryfall_data <- list()

  # Extract card names from the input data
  card_names <- card_data |>
    dplyr::pull(card_name_source)

  # Initialize progress bar
  cli::cli_progress_bar("Processing Decklist...", total = nrow(card_data), format = paste0("ETA:{cli::pb_eta}"))
  
  # Iterate over each card
  for (card_index in seq_along(card_names)) {
    card_name_source <- card_names[card_index]

    # Wait between requests to the API
    Sys.sleep(0.1)

    if (use_httr) {

      # API endpoint and query parameters
      url <- "https://api.scryfall.com/cards/named"
      query_params <- list(exact = card_name_source)

      # Make GET request to Scryfall API
      response <- httr::GET(url, query = query_params)

      # Parse JSON content
      card_info <- response |>
        httr::content(as = "text") |> 
        jsonlite::fromJSON(flatten = TRUE)
      
    } else {

      # httr does not work with shinylive
      temp_card_json <- tempfile(pattern = "card", fileext = ".json")

      # Format card name for URL
      card_name_for_url <- card_name_source |> 
        stringr::str_replace_all(" ", "%20") |> 
        stringr::str_replace_all("/", "%20")

      url <- glue::glue("https://api.scryfall.com/cards/named?exact={ card_name_for_url }")

      # Download the JSON file
      download.file(url, temp_card_json)

      # Parse JSON content
      card_info <- temp_card_json |> 
        jsonlite::fromJSON(flatten = TRUE)

    }

    # Process card data based on layout type
    if (card_info$layout %in% c("split", "flip", "adventure", "normal")) {
      card_index_scryfall_data <- tibble::tibble(
        card_name_source = card_name_source,
        layout = card_info$layout,
        card_name_scryfall = card_info$name,
        cmc = card_info$cmc,
        mana_cost = card_info$mana_cost,
        type = card_info$type_line,
        produced_mana = paste0(card_info$produced_mana, collapse = ""),
        img_src = card_info$image_uris$png
      )
    }

    if (card_info$layout %in% c("transform")) {
      card_index_scryfall_data <- tibble::tibble(
        card_name_source = card_name_source,
        layout = card_info$layout,
        card_name_scryfall = card_info$name,
        cmc = card_info$cmc,
        mana_cost = paste0(card_info$card_faces$mana_cost[[1]], collapse = ""),
        type = card_info$type_line,
        produced_mana = paste0(card_info$produced_mana, collapse = ""),
        img_src = card_info$card_faces$image_uris.png[[1]]
      )
    }

    if (card_info$layout == "modal_dfc") {
      card_index_scryfall_data <- tibble::tibble(
        card_name_source = card_name_source,
        layout = card_info$layout,
        card_name_scryfall = card_info$name,
        cmc = card_info$cmc,
        mana_cost = paste0(card_info$card_faces$mana_cost, collapse = " // "),
        type = paste0(card_info$card_faces$type_line, collapse = " // "),
        produced_mana = paste0(card_info$produced_mana, collapse = ""),
        img_src = card_info$card_faces$image_uris.png[[1]]
      )
    }

    # TODO: add logic for "prototype" layout

    # Add the processed card data to the list
    scryfall_data[[card_index]] <- card_index_scryfall_data

    # Update the progress bar
    cli::cli_progress_update()

  }

  # Combine all individual card dataframes into one
  scryfall_data <- dplyr::bind_rows(scryfall_data)

  # Mark progress as complete
  cli::cli_progress_done()

  return(scryfall_data)

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

      enabler_cc = is_enabler_cc(enabler_2, mana_cost),
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
      chrome_mox = card_name_scryfall == "Chrome Mox",
      dark_ritual = card_name_scryfall == "Dark Ritual",
      lotus_petal = card_name_scryfall == "Lotus Petal",
      mana_crypt = card_name_scryfall == "Mana Crypt",
      mox_diamond = card_name_scryfall == "Mox Diamond"

    )
}

edit_scryfall_data <- function(card_data) {

  # For simplicity, we assume that we are playing first
  # which means Gemstone Caverns produces only colorless mana
  if ("Gemstone Caverns" %in% card_data$card_name_scryfall) {
    card_data <- card_data |> 
      dplyr::mutate(
        produced_mana = ifelse(
          test = card_name_scryfall == "Gemstone Caverns",
          yes = "C",
          no = produced_mana
        )
      )
  }
  
  # With Demonic Tutor we can tutor an Enabler with MV = 0
  # So we can consider Demonic Tutor as an Enabler that costs 1B
  # (for which logic was already defined)
  if ("Demonic Tutor" %in% card_data$card_name_scryfall) {
    card_data <- card_data |> 
      dplyr::mutate(
        type = ifelse(
          test = card_name_scryfall == "Demonic Tutor",
          yes = "Creature // Sorcery",
          no = type
        )
      )
  }
  
  card_data

}
