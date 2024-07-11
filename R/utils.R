#' @export
draw_seven <- function(decklist, must_have = NULL){
  if (is.null(must_have)) {

    decklist %>%
      dplyr::slice_sample(n = 7)

  } else {
    n_must_have <- length(must_have)
    
    hand <- decklist %>%
      dplyr::filter(!card_name %in% must_have) %>%
      dplyr::slice_sample(n = 7 - n_must_have)
    
    hand %>%
      dplyr::bind_rows(decklist %>% dplyr::filter(card_name %in% must_have))
  }

}

#' @export
get_hand <- function(decklist, cards){
  decklist %>%
    dplyr::filter(card_name %in% cards)
}
