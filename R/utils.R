#' @export
draw_seven <- function(decklist, fixed_cards = NULL){
  if (is.null(fixed_cards)) {

    decklist |>
      dplyr::slice_sample(n = 7)

  } else {
    n_fixed_cards <- length(fixed_cards)
    
    hand <- decklist |>
      dplyr::filter(!card_name %in% fixed_cards) |>
      dplyr::slice_sample(n = 7 - n_fixed_cards)
    
    hand |>
      dplyr::bind_rows(decklist |> dplyr::filter(card_name %in% fixed_cards))
  }

}

#' @export
get_hand <- function(decklist, cards){
  decklist |>
    dplyr::filter(card_name %in% cards)
}
