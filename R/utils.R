#' @export
draw_seven <- function(decklist){
  decklist %>%
    dplyr::slice_sample(n = 7)
}

#' @export
get_hand <- function(decklist, cards){
  decklist %>%
    dplyr::filter(card_name %in% cards)
}
