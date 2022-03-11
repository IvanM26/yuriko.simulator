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

#' @export
can_cast_enabler_t1 <- function(opening_hand){
  produce_u <- max(opening_hand$produce_u)
  produce_b <- max(opening_hand$produce_b)

  b_cards <- sum(opening_hand$color_b)
  u_cards <- sum(opening_hand$color_u)

  land_cards <- sum(opening_hand$is_land)
  has_land_back_u <- max(opening_hand$is_land_back_u)
  has_land_back_b <- max(opening_hand$is_land_back_b)

  has_chrome_mox <- "Chrome Mox" %in% opening_hand$carta
  has_mana_crypt <- "Mana Crypt" %in% opening_hand$carta
  has_mox_diamond <- "Mox Diamond" %in% opening_hand$carta
  has_lotus_petal <- "Lotus Petal" %in% opening_hand$carta
  has_dark_ritual <- "Dark Ritual" %in% opening_hand$carta

  has_enabler_0 <- max(opening_hand$enabler_0) == 1
  has_enabler_1 <- max(opening_hand$enabler_1) == 1
  has_enabler_2 <- max(opening_hand$enabler_2) == 1
  has_enabler_3 <- max(opening_hand$enabler_3) == 1

  # opening hand has an enabler with mv = 0
  if (has_enabler_0) return(TRUE)

  # opening hand has an enabler with mv = 1 and produces its color
  # have to cast it without lotus petal
  if (has_enabler_1){
    enablers <- opening_hand %>%
      dplyr::filter(enabler_1 == 1)

    enabler_req_u <- max(enablers$color_u)
    enabler_req_b <- max(enablers$color_b)
    colorless_enabler <- max(enablers$color_c)

    # colorless enabler
    if (colorless_enabler & (produce_u | produce_b)) return(TRUE)
    if (colorless_enabler & has_mana_crypt) return(TRUE)

    if (enabler_req_u & produce_u) return(TRUE) # u enabler
    if (enabler_req_b & produce_b) return(TRUE) # b enabler

    if (has_chrome_mox){
      if (enabler_req_b & b_cards > 1) return(TRUE)
      if (enabler_req_u & u_cards > 1) return(TRUE)
      if (colorless_enabler & (b_cards + u_cards > 0)) return(TRUE)
    }

    if (has_mox_diamond){
      if (land_cards >= 1) return(TRUE)
    }

  }
  # opening hand has an enabler with mv = 2 and produces its color
  if (has_enabler_2){
    enablers <- opening_hand %>%
      dplyr::filter(enabler_2 == 1)

    enabler_req_u <- max(enablers$color_u)
    enabler_req_b <- max(enablers$color_b)

    if (has_mana_crypt){
      if (enabler_req_u & produce_u) return(TRUE) # u enabler
      if (enabler_req_b & produce_b) return(TRUE) # b enabler

      if (has_chrome_mox){
        if (enabler_req_b & b_cards > 1) return(TRUE)
        if (enabler_req_u & u_cards > 1) return(TRUE)
      }

      if (has_mox_diamond){
        if (land_cards >= 1) return(TRUE)
      }

      if (has_lotus_petal) return(TRUE)

    }

    if (has_chrome_mox){
      if (enabler_req_b & b_cards > 1 & land_cards >= 1) return(TRUE)
      if (enabler_req_u & u_cards > 1 & land_cards >= 1) return(TRUE)

      if (enabler_req_b & has_land_back_b & (b_cards > 2 | u_cards > 0)) return(TRUE)
      if (enabler_req_b & has_land_back_u & b_cards > 2) return(TRUE)

      if (enabler_req_u & has_land_back_u & (u_cards > 2 | b_cards > 0)) return(TRUE)
      if (enabler_req_u & has_land_back_b & u_cards > 2) return(TRUE)

      if (enabler_req_b & u_cards > 0 & produce_b) return(TRUE)
      if (enabler_req_u & b_cards > 1 & produce_u) return(TRUE)

      if (has_mox_diamond){
        if (enabler_req_b & (b_cards > 1 | u_cards > 0) & land_cards >= 1) return(TRUE)
        if (enabler_req_u & (u_cards > 1 | b_cards > 0) & land_cards >= 1) return(TRUE)
      }

      if (has_lotus_petal){
        if (enabler_req_b & (b_cards > 1 | u_cards > 0)) return(TRUE)
        if (enabler_req_u & (u_cards > 1 | b_cards > 0)) return(TRUE)
      }
    }

    if (has_mox_diamond){
      if (land_cards >= 2) return(TRUE)
    }

    if (has_lotus_petal){
      if (land_cards >= 1) return(TRUE)
    }

    if (has_dark_ritual){
      if (produce_b) return(TRUE)
      if (has_chrome_mox){
        if (b_cards > 2) return(TRUE)
      }
    }
  }

  # opening hand has an enabler with mv = 3 and produces its color
  if (has_enabler_3){
    enablers <- opening_hand %>%
      dplyr::filter(enabler_3 == 1)

    enabler_req_u <- max(enablers$color_u)
    enabler_req_b <- max(enablers$color_b)

    if (has_mana_crypt){
      if (enabler_req_u & produce_u) return(TRUE) # u enabler
      if (enabler_req_b & produce_b) return(TRUE) # b enabler
    }
  }

  return(FALSE)
}

#' @export
can_produce_ub <- function(opening_hand){
  land_cards <- sum(opening_hand$is_land)
  produce_color <- sum(opening_hand$produce_color)
  produce_u <- max(opening_hand$produce_u)
  produce_b <- max(opening_hand$produce_b)
  has_land_back_u <- max(opening_hand$is_land_back_u)
  has_land_back_b <- max(opening_hand$is_land_back_b)

  b_cards <- sum(opening_hand$color_b)
  u_cards <- sum(opening_hand$color_u)

  has_mox_diamond <- "Mox Diamond" %in% opening_hand$carta
  has_chrome_mox <- "Chrome Mox" %in% opening_hand$carta
  has_lotus_petal <- "Lotus Petal" %in% opening_hand$carta
  has_dark_ritual <- "Dark Ritual" %in% opening_hand$carta

  if (produce_color >= 2 & produce_u & produce_b) return(TRUE)

  if (has_mox_diamond){
    if (land_cards >= 2 & (produce_u | produce_b)) return(TRUE)
    if (land_cards == 1 & (has_land_back_u | has_land_back_b)) return(TRUE)

  }


  if (has_chrome_mox){
    if (produce_u & b_cards > 0) return(TRUE)
    if (produce_b & u_cards > 0) return(TRUE)

    if ((b_cards > 0 | u_cards > 0)){
      if (land_cards >= 1 & has_mox_diamond) return(TRUE)
      if (has_lotus_petal){
        if (produce_u | produce_b) return(TRUE)
      }
    }
  }

  # special cases where we will / will not consider having ub

  if (has_lotus_petal){
    has_enabler_0 <- max(opening_hand$enabler_0) == 1
    has_enabler_1 <- max(opening_hand$enabler_1) == 1
    has_enabler_2 <- max(opening_hand$enabler_2) == 1
    has_enabler_3 <- max(opening_hand$enabler_3) == 1

    if (has_enabler_0){
      if (produce_u | produce_b) return(TRUE)
    }

    if (has_enabler_1){
      enablers <- opening_hand %>%
        dplyr::filter(enabler_1 == 1)

      enabler_req_u <- max(enablers$color_u)
      enabler_req_b <- max(enablers$color_b)
      colorless_enabler <- max(enablers$color_c)

      if (colorless_enabler & (produce_u | produce_b)) return(TRUE)

      if (enabler_req_u & produce_u) return(TRUE)
      if (enabler_req_b & produce_b) return(TRUE)

      if (has_chrome_mox){
        if (enabler_req_b & b_cards > 1) return(TRUE)
        if (enabler_req_u & u_cards > 1) return(TRUE)
        if (colorless_enabler & (b_cards + u_cards > 0)) return(TRUE)
      }

    }

    if (has_enabler_2){
      if (has_dark_ritual){
        if (produce_b) return(TRUE)
        if (has_chrome_mox){
          if (b_cards > 2) return(TRUE)
        }
      }
    }

    if (has_enabler_3){
      if (produce_u | produce_b) return(TRUE)
    }

  }

  return(FALSE)
}

#' @export
keep_hand <- function(opening_hand){
  if (can_cast_enabler_t1(opening_hand) & can_produce_ub(opening_hand)) TRUE
  else FALSE
}
