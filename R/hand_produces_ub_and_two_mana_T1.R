hand_produces_ub_and_two_mana_T1 <- function(hand_metrics, constraint){
  
  if (constraint %in% c("U ON T1", "UU ON T1")){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_u
    n_mdfc_lands_constrain_color <- hand_metrics$n_mdfc_lands_u
    n_cards_constraint_color     <- hand_metrics$n_cards_u
    
    n_lands_other_color      <- hand_metrics$n_lands_b
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_b
    n_cards_other_color      <- hand_metrics$n_cards_b
    
    
  } else if (constraint %in% c("B ON T1", "BB ON T1")){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_b
    n_mdfc_lands_constrain_color <- hand_metrics$n_mdfc_lands_b
    n_cards_constraint_color     <- hand_metrics$n_cards_b
    
    n_lands_other_color      <- hand_metrics$n_lands_u
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_u
    n_cards_other_color      <- hand_metrics$n_cards_u
    
  }
  
  if (hand_metrics$lotus_petal){
    if (hand_produces_ub(hand_metrics, use_lotus_petal = FALSE)) return(TRUE)
  }
  
  if (hand_metrics$chrome_mox){
    if (n_cards_constraint_color >= 2){
      if (hand_produces_ub(hand_metrics, use_lotus_petal = FALSE)) return(TRUE)
    }
    
    if (n_cards_other_color >= 1 & !(constraint %in% c("UU ON T1", "BB ON T1"))){
      if (hand_produces_ub_with_constraint(hand_metrics, constraint, use_lotus_petal = FALSE)) return(TRUE)
    }
    
  }

  if (hand_metrics$mox_diamond){
    if (hand_metrics$n_lands >= 2){
      if (hand_metrics$n_color_lands >= 1) return(TRUE)
      if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
    }
    if (hand_metrics$n_lands == 1){
      if (constraint %in% c("U ON T1", "B ON T1")) {
        if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (constraint %in% c("UU ON T1", "BB ON T1")) {
        if (n_mdfc_lands_constrain_color >= 1) return(TRUE)
      }
    }
  }
  
  return(FALSE)
  
}