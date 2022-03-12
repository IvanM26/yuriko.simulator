hand_produces_ub <- function(hand_metrics){
  if (hand_metrics$n_lands >= 2){
    
    if (hand_metrics$n_color_lands >= 2){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (hand_metrics$lotus_petal) return(TRUE)
      if ((hand_metrics$n_lands_u >= 1) & (hand_metrics$n_lands_b >= 1)) return(TRUE)
      if (hand_metrics$n_lands_b == 0){
        if (hand_metrics$chrome_mox & (hand_metrics$n_cards_b >= 1)) return(TRUE)
        if (hand_metrics$n_mdfc_lands_b >= 1) return(TRUE)
      }
      if (hand_metrics$n_lands_u == 0){
        if (hand_metrics$chrome_mox & (hand_metrics$n_cards_u >= 1)) return(TRUE)
        if (hand_metrics$n_mdfc_lands_u >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 1){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (hand_metrics$lotus_petal) return(TRUE)
      if (hand_metrics$n_lands_u == 1 & hand_metrics$n_lands_b == 1 & hand_metrics$chrome_mox & (hand_metrics$n_cards_u >= 1 | hand_metrics$n_cards_b >= 1)) return(TRUE)
      if (hand_metrics$n_lands_b == 0){
        if (hand_metrics$chrome_mox & hand_metrics$n_cards_b >= 1) return(TRUE)
        if (hand_metrics$n_mdfc_lands_b >= 1) return(TRUE)
      }
      if (hand_metrics$n_lands_u == 0){
        if (hand_metrics$chrome_mox & hand_metrics$n_cards_u >= 1) return(TRUE)
        if (hand_metrics$n_mdfc_lands_u >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 0){
      if (hand_metrics$n_mdfc_lands == 0){
        if (hand_metrics$mox_diamond & hand_metrics$chrome_mox & (hand_metrics$n_cards_u >= 1 | hand_metrics$n_cards_b >= 1)) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands == 1){
        if (hand_metrics$mox_diamond) return(TRUE)
        if (hand_metrics$lotus_petal) return(TRUE)
        if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_b >= 1) return(TRUE)
        if (hand_metrics$n_mdfc_lands_b >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_u >= 1) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands == 2){
        if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$n_mdfc_lands_b >= 1) return(TRUE)
      }
    }
  }
  
  if (hand_metrics$n_lands == 1){
    
    if (hand_metrics$n_color_lands == 1){
      if (hand_metrics$lotus_petal) return(TRUE)
      if (hand_metrics$n_lands_u == 1 & hand_metrics$n_lands_b == 1 & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      if (hand_metrics$n_lands_b == 1){
        if (hand_metrics$chrome_mox & hand_metrics$n_cards_u >= 1) return(TRUE)
        if (hand_metrics$n_mdfc_lands_u >= 1) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (hand_metrics$n_lands_u == 1){
        if (hand_metrics$chrome_mox & hand_metrics$n_cards_b >= 1) return(TRUE)
        if (hand_metrics$n_mdfc_lands_b >= 1) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 0){
      if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$n_mdfc_lands_b >= 1) return(TRUE)
      if (hand_metrics$mox_diamond){
        if (hand_metrics$chrome_mox & (hand_metrics$n_cards_u >= 1 | hand_metrics$n_cards_b >= 1)) return(TRUE)
        if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_b >= 1) return(TRUE)
      if (hand_metrics$n_mdfc_lands_b >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_u >= 1) return(TRUE)
      if (hand_metrics$lotus_petal & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
    }
  }
  
  if (hand_metrics$n_lands == 0){
    if (hand_metrics$lotus_petal & hand_metrics$chrome_mox & (hand_metrics$n_cards_u >= 1 | hand_metrics$n_cards_b >= 1)) return(TRUE)
    if (hand_metrics$lotus_petal & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
    if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_b >= 1) return(TRUE)
    if (hand_metrics$n_mdfc_lands_b >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_u >= 1) return(TRUE)
  }
  
  return(FALSE)
}