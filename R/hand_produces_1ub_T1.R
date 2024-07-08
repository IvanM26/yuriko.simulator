hand_produces_1ub_T1 <- function(hand_metrics) {
  if (hand_metrics$dark_ritual){
    if (hand_metrics$lotus_petal){
      if (hand_metrics$n_color_lands + hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      if (hand_metrics$mox_diamond & hand_metrics$n_lands >= 1) return(TRUE)
      if (hand_metrics$chrome_mox & hand_metrics$n_cards_u + hand_metrics$n_cards_b >= 2) return(TRUE)
    }
    if (hand_metrics$chrome_mox){
      if (hand_metrics$n_lands_b + hand_metrics$n_mdfc_lands_b >= 1){
        if (hand_metrics$n_cards_u >= 1) return(TRUE)
      }
      if (hand_metrics$n_lands_u + hand_metrics$n_mdfc_lands_u >= 1){
        if (hand_metrics$n_cards_b >= 2) return(TRUE)
      }
      if (hand_metrics$mox_diamond){
        if (hand_metrics$n_lands >= 1 & hand_metrics$n_cards_u + hand_metrics$n_cards_b >= 2) return(TRUE)
      }
    }
  }
  
  if (hand_metrics$lotus_petal){
    if (hand_metrics$chrome_mox){
      if (hand_metrics$n_lands >= 1 & hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 1) return(TRUE)
      if (hand_metrics$n_mdfc_lands >= 1 & hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 2) return(TRUE)
    }
    if (hand_metrics$mox_diamond){
      if (hand_metrics$n_lands >= 2) return(TRUE)
      if (hand_metrics$n_lands == 1 & hand_metrics$mana_crypt) return(TRUE)
    }
  }
  
  if (hand_metrics$chrome_mox){
    if (hand_metrics$n_lands >= 2){
      if (hand_metrics$mox_diamond){
        if (hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 1) return(TRUE)
      }
      if (hand_metrics$mana_crypt){
        if (hand_metrics$n_lands_b >= 1 & hand_metrics$n_cards_u >= 1) return(TRUE)
        if (hand_metrics$n_lands_u >= 1 & hand_metrics$n_cards_b >= 1) return(TRUE)
      }
    }
    if (hand_metrics$n_lands == 1){
      if (hand_metrics$mox_diamond){
        if (hand_metrics$n_mdfc_lands_b >= 1 & hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 2) return(TRUE)
        if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 2) return(TRUE)
        if (hand_metrics$mana_crypt & hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 1) return(TRUE)
      }
      if (hand_metrics$mana_crypt){
        if (hand_metrics$n_lands_b >= 1 & hand_metrics$n_cards_u >= 1) return(TRUE)
        if (hand_metrics$n_lands_u >= 1 & hand_metrics$n_cards_b >= 1) return(TRUE)
      }
    }
    if (hand_metrics$n_lands == 0){
      if (hand_metrics$n_mdfc_lands >= 1){
        if (hand_metrics$mana_crypt){
          if (hand_metrics$n_mdfc_lands_b >= 1 & hand_metrics$n_cards_u >= 1) return(TRUE)
          if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$n_cards_b >= 1) return(TRUE)
        }
      }
    }
  }
  
  if (hand_metrics$mana_crypt){
    if (hand_metrics$lotus_petal){
      if (hand_metrics$n_color_lands >= 1) return(TRUE)
      if (hand_metrics$chrome_mox & hand_metrics$n_cards_b + hand_metrics$n_cards_u >= 1) return(TRUE)
    }
    if (hand_metrics$mox_diamond){
      if (hand_metrics$n_lands >= 2 & hand_metrics$n_color_lands >= 1) return(TRUE)
    }
  }
  
  return(FALSE)
}
