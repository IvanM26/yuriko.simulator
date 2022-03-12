hand_produces_ub_with_constraint <- function(hand_metrics, constraint){
  if (constraint == "U ON T1"){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_u
    n_mdfc_lands_constrain_color <- hand_metrics$n_mdfc_lands_u
    n_cards_constraint_color     <- hand_metrics$n_cards_u
    
    n_lands_other_color      <- hand_metrics$n_lands_b
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_b
    n_cards_other_color      <- hand_metrics$n_cards_b
    
    
  } else if (constraint == "B ON T1"){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_b
    n_mdfc_lands_constrain_color <- hand_metrics$n_mdfc_lands_b
    n_cards_constraint_color     <- hand_metrics$n_cards_b
    
    n_lands_other_color      <- hand_metrics$n_lands_u
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_u
    n_cards_other_color      <- hand_metrics$n_cards_u
    
  }
  
  if (hand_metrics$n_lands >= 2){
    
    if (hand_metrics$n_color_lands >= 2){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (hand_metrics$lotus_petal){
        if (n_lands_constraint_color     >= 1) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
      }
      if ((n_lands_constraint_color >= 1) & (n_lands_other_color >= 1)) return(TRUE)
      if (n_lands_other_color == 0){
        if (hand_metrics$chrome_mox & (n_cards_other_color >= 1)) return(TRUE)
        if (n_mdfc_lands_other_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 0){
        if (hand_metrics$chrome_mox & (n_cards_constraint_color >= 2)) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 1){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (hand_metrics$lotus_petal){
        if (n_lands_constraint_color >= 1) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 1 & n_lands_other_color == 1 & hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
      if (n_lands_other_color == 0){
        if (hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
        if (n_mdfc_lands_other_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 0){
        if (hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 0){
      if (hand_metrics$n_mdfc_lands == 0){
        if (hand_metrics$mox_diamond & hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands == 1){
        if (hand_metrics$mox_diamond) return(TRUE)
        if (hand_metrics$lotus_petal & n_mdfc_lands_constrain_color >= 1) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1 & hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
        if (n_mdfc_lands_other_color >= 1 & hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands == 2){
        if (n_mdfc_lands_constrain_color >= 1 & n_mdfc_lands_other_color >= 1) return(TRUE)
      }
    }
  }
  
  if (hand_metrics$n_lands == 1){
    
    if (hand_metrics$n_color_lands == 1){
      if (hand_metrics$lotus_petal){
        if (n_lands_constraint_color >= 1) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 1 & n_lands_other_color == 1 & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      if (n_lands_other_color == 1){
        if (hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 1){
        if (hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
        if (n_mdfc_lands_other_color >= 1) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 0){
      if (n_mdfc_lands_constrain_color >= 1 & n_mdfc_lands_other_color >= 1) return(TRUE)
      if (hand_metrics$mox_diamond){
        if (hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
        if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (n_mdfc_lands_constrain_color >= 1 & hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
      if (n_mdfc_lands_other_color >= 1 & hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
      if (hand_metrics$lotus_petal & n_mdfc_lands_constrain_color >= 1) return(TRUE)
    }
  }
  
  if (hand_metrics$n_lands == 0){
    if (hand_metrics$lotus_petal & hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
    if (hand_metrics$lotus_petal & n_mdfc_lands_constrain_color >= 1) return(TRUE)
    if (n_mdfc_lands_constrain_color >= 1 & hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
    if (n_mdfc_lands_other_color >= 1 & hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
  }
  
  return(FALSE)
}