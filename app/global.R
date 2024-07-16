
# ---- Start of hand_produces_1ub_T1.R ----

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

# ---- End of hand_produces_1ub_T1.R ----


# ---- Start of hand_produces_ub.R ----

hand_produces_ub <- function(hand_metrics, use_lotus_petal = TRUE){
  if (hand_metrics$n_lands >= 2){
    
    if (hand_metrics$n_color_lands >= 2){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (use_lotus_petal & hand_metrics$lotus_petal) return(TRUE)
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
      if (use_lotus_petal & hand_metrics$lotus_petal) return(TRUE)
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
        if (use_lotus_petal & hand_metrics$lotus_petal) return(TRUE)
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
      if (use_lotus_petal & hand_metrics$lotus_petal) return(TRUE)
      if (hand_metrics$n_lands_u == 1 & hand_metrics$n_lands_b == 1 & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      if (hand_metrics$n_lands_b == 1){
        if (hand_metrics$chrome_mox){
          if (hand_metrics$n_cards_u >= 1) return(TRUE)
          if (hand_metrics$n_cards_b >= 1 & hand_metrics$mox_diamond) return(TRUE)
        }
        
        if (hand_metrics$n_mdfc_lands_u >= 1) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (hand_metrics$n_lands_u == 1){
        if (hand_metrics$chrome_mox){
          if (hand_metrics$n_cards_b >= 1) return(TRUE)
          if (hand_metrics$n_cards_u >= 1 & hand_metrics$mox_diamond) return(TRUE)
        }
        
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
      if (use_lotus_petal & hand_metrics$lotus_petal & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
    }
  }
  
  if (hand_metrics$n_lands == 0){
    if (use_lotus_petal & hand_metrics$lotus_petal & hand_metrics$chrome_mox & (hand_metrics$n_cards_u >= 1 | hand_metrics$n_cards_b >= 1)) return(TRUE)
    if (use_lotus_petal & hand_metrics$lotus_petal & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
    if (hand_metrics$n_mdfc_lands_u >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_b >= 1) return(TRUE)
    if (hand_metrics$n_mdfc_lands_b >= 1 & hand_metrics$chrome_mox & hand_metrics$n_cards_u >= 1) return(TRUE)
  }
  
  return(FALSE)
}

# ---- End of hand_produces_ub.R ----


# ---- Start of hand_produces_ub_and_three_mana_T1.R ----

hand_produces_ub_and_three_mana_T1 <- function(hand_metrics, constraint){
  
  if (constraint == "U ON T1"){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_u
    n_mdfc_lands_constraint_color <- hand_metrics$n_mdfc_lands_u
    n_cards_constraint_color     <- hand_metrics$n_cards_u
    
    n_lands_other_color      <- hand_metrics$n_lands_b
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_b
    n_cards_other_color      <- hand_metrics$n_cards_b
    
    
  } else if (constraint == "B ON T1"){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_b
    n_mdfc_lands_constraint_color <- hand_metrics$n_mdfc_lands_b
    n_cards_constraint_color     <- hand_metrics$n_cards_b
    
    n_lands_other_color      <- hand_metrics$n_lands_u
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_u
    n_cards_other_color      <- hand_metrics$n_cards_u
    
  }
  
  if (hand_metrics$lotus_petal){
    if (hand_metrics$chrome_mox){
      if (n_cards_constraint_color >= 2){
        if (hand_produces_ub(hand_metrics, use_lotus_petal = FALSE)) return(TRUE)
      }
      
      if (n_cards_other_color >= 1){
        if (hand_produces_ub_with_constraint(hand_metrics, constraint, use_lotus_petal = FALSE)) return(TRUE)
      }
      
    }
    
    if (hand_metrics$mox_diamond){
      if (hand_metrics$n_lands >= 2){
        if (hand_metrics$n_color_lands >= 1) return(TRUE)
        if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (hand_metrics$n_lands == 1){
        if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
    }
  }
  
  if (hand_metrics$chrome_mox){
    if (n_cards_constraint_color >= 2 | n_cards_other_color >= 1){
      if (hand_metrics$mox_diamond){
        if (hand_metrics$n_lands >= 2) return(TRUE)
        if (hand_metrics$n_lands == 1 & hand_metrics$n_mdfc_lands >= 1) return(TRUE) 
      } 
    }
  }
  
  if (hand_metrics$dark_ritual){
    if (hand_produces_ub_and_two_mana_T1(hand_metrics, "B ON T1")) return(TRUE)
  }

  return(FALSE)
}

# ---- End of hand_produces_ub_and_three_mana_T1.R ----


# ---- Start of hand_produces_ub_and_two_mana_T1.R ----

hand_produces_ub_and_two_mana_T1 <- function(hand_metrics, constraint){
  
  if (constraint %in% c("U ON T1", "UU ON T1")){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_u
    n_mdfc_lands_constraint_color <- hand_metrics$n_mdfc_lands_u
    n_cards_constraint_color     <- hand_metrics$n_cards_u
    
    n_lands_other_color      <- hand_metrics$n_lands_b
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_b
    n_cards_other_color      <- hand_metrics$n_cards_b
    
    
  } else if (constraint %in% c("B ON T1", "BB ON T1")){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_b
    n_mdfc_lands_constraint_color <- hand_metrics$n_mdfc_lands_b
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
        if (n_mdfc_lands_constraint_color >= 1) return(TRUE)
      }
    }
  }
  
  return(FALSE)
  
}

# ---- End of hand_produces_ub_and_two_mana_T1.R ----


# ---- Start of hand_produces_ub_with_constraint.R ----

hand_produces_ub_with_constraint <- function(hand_metrics, constraint, use_lotus_petal = TRUE, mdfc_enabler = FALSE){
  if (constraint == "U ON T1"){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_u
    n_mdfc_lands_constraint_color <- hand_metrics$n_mdfc_lands_u
    n_cards_constraint_color     <- hand_metrics$n_cards_u

    n_lands_other_color      <- hand_metrics$n_lands_b
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_b
    n_cards_other_color      <- hand_metrics$n_cards_b

  } else if (constraint == "B ON T1"){
    
    n_lands_constraint_color     <- hand_metrics$n_lands_b
    n_mdfc_lands_constraint_color <- hand_metrics$n_mdfc_lands_b
    n_cards_constraint_color     <- hand_metrics$n_cards_b

    n_lands_other_color      <- hand_metrics$n_lands_u
    n_mdfc_lands_other_color <- hand_metrics$n_mdfc_lands_u
    n_cards_other_color      <- hand_metrics$n_cards_u

  }
  
  if (hand_metrics$n_lands >= 2){
    
    if (hand_metrics$n_color_lands >= 2){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (use_lotus_petal & hand_metrics$lotus_petal){
        if (n_lands_constraint_color     >= 1) return(TRUE)
        if (n_mdfc_lands_constraint_color >= 1) return(TRUE)
      }
      if ((n_lands_constraint_color >= 1) & (n_lands_other_color >= 1)) return(TRUE)
      if (n_lands_other_color == 0){
        if (hand_metrics$chrome_mox & (n_cards_other_color >= 1)) return(TRUE)
        if (n_mdfc_lands_other_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 0){
        if (hand_metrics$chrome_mox & (n_cards_constraint_color >= 2)) return(TRUE)
        if (n_mdfc_lands_constraint_color >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 1){
      if (hand_metrics$mox_diamond) return(TRUE)
      if (use_lotus_petal & hand_metrics$lotus_petal){
        if (n_lands_constraint_color >= 1) return(TRUE)
        if (n_mdfc_lands_constraint_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 1 & n_lands_other_color == 1 & hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
      if (n_lands_other_color == 0){
        if (hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
        if (n_mdfc_lands_other_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 0){
        if (hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
        if (n_mdfc_lands_constraint_color >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 0){
      if (hand_metrics$n_mdfc_lands == 0){
        if (hand_metrics$mox_diamond & hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands == 1){
        if (hand_metrics$mox_diamond) return(TRUE)
        if (use_lotus_petal & hand_metrics$lotus_petal & n_mdfc_lands_constraint_color >= 1) return(TRUE)
        if (n_mdfc_lands_constraint_color >= 1 & hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
        if (n_mdfc_lands_other_color >= 1 & hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
      }
      if (hand_metrics$n_mdfc_lands == 2){
        if (n_mdfc_lands_constraint_color >= 1 & n_mdfc_lands_other_color >= 1) return(TRUE)
      }
    }
  }
  
  if (hand_metrics$n_lands == 1){

    if (hand_metrics$n_color_lands == 1){
      if (use_lotus_petal & hand_metrics$lotus_petal){
        if (n_lands_constraint_color >= 1) return(TRUE)
        if (n_mdfc_lands_constraint_color >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 1 & n_lands_other_color == 1 & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      if (n_lands_other_color == 1){
        if (hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
        if (!mdfc_enabler & n_mdfc_lands_constraint_color >= 1) return(TRUE)
        if (mdfc_enabler & n_mdfc_lands_constraint_color >= 2) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (n_lands_constraint_color == 1){
        if (hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
        if (n_mdfc_lands_other_color >= 1) return(TRUE)
        if (hand_metrics$mox_diamond & hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
    }
    
    if (hand_metrics$n_color_lands == 0){
      if (n_mdfc_lands_constraint_color >= 1 & n_mdfc_lands_other_color >= 1) return(TRUE)
      if (hand_metrics$mox_diamond){
        if (hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
        if (hand_metrics$n_mdfc_lands >= 1) return(TRUE)
      }
      if (n_mdfc_lands_constraint_color >= 1 & hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
      if (n_mdfc_lands_other_color >= 1 & hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
      if (use_lotus_petal & hand_metrics$lotus_petal & n_mdfc_lands_constraint_color >= 1) return(TRUE)
    }
  }
  
  if (hand_metrics$n_lands == 0){
    if (use_lotus_petal & hand_metrics$lotus_petal & hand_metrics$chrome_mox & (n_cards_constraint_color >= 2 | n_cards_other_color >= 1)) return(TRUE)
    if (use_lotus_petal & hand_metrics$lotus_petal & n_mdfc_lands_constraint_color >= 1) return(TRUE)
    if (n_mdfc_lands_constraint_color >= 1 & hand_metrics$chrome_mox & n_cards_other_color >= 1) return(TRUE)
    if (n_mdfc_lands_other_color >= 1 & hand_metrics$chrome_mox & n_cards_constraint_color >= 2) return(TRUE)
  }
  
  return(FALSE)
}

# ---- End of hand_produces_ub_with_constraint.R ----


# ---- Start of is_yuriko_on_T2.R ----

#' @export
is_yuriko_on_T2 <- function(opening_hand){

  # TODO: Add parameter to indicate whether we are playing first
  # TODO: Add logic to account for Gemstone Caverns when we are not playing first

  hand_metrics <- summarise_hand(opening_hand)

  if (hand_metrics$n_enablers >= 1){
    
    # ENABLER 0 ####
    if (hand_metrics$n_enablers_0 >= 1){
      if (hand_produces_ub(hand_metrics)) return(TRUE)
    }
    
    # ENABLER 1 ####
    if (hand_metrics$n_enablers_1 >= 1){
      
      # > COLORLESS ####
      if (hand_metrics$n_enablers_c >= 1){
        if (hand_produces_ub(hand_metrics)) return(TRUE)
      }
      
      # > BLUE ####
      if (hand_metrics$n_enablers_u >= 1){
        if (hand_produces_ub_with_constraint(hand_metrics, "U ON T1")) return(TRUE)
      }
      
      # > BLACK ####
      if (hand_metrics$n_enablers_b >= 1){
        if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
      }
      
    }
    
    # ENABLER 2 ####
    if (hand_metrics$n_enablers_2 >= 1){
      
      # > 1U ####
      if (hand_metrics$n_enablers_1u >= 1){
        if (hand_metrics$mana_crypt){
          if (hand_produces_ub_with_constraint(hand_metrics, "U ON T1")) return(TRUE)
        }
        
        if (hand_produces_ub_and_two_mana_T1(hand_metrics, "U ON T1")) return(TRUE)
        
      }
      
      # > 1B ####
      if (hand_metrics$n_enablers_1b >= 1){
        if (hand_metrics$mana_crypt){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
        }
        
        if (hand_produces_ub_and_two_mana_T1(hand_metrics, "B ON T1")) return(TRUE)
        
        if (hand_metrics$dark_ritual){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
        }
      }
      
      # > UU ####
      if (hand_metrics$n_enablers_uu >= 1){
        if (hand_produces_ub_and_two_mana_T1(hand_metrics, "UU ON T1")) return(TRUE)
      }
      
      # > BB ####
      if (hand_metrics$n_enablers_bb >= 1){
        if (hand_produces_ub_and_two_mana_T1(hand_metrics, "BB ON T1")) return(TRUE)
        
        if (hand_metrics$dark_ritual){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
        }
      }
      
      # > UB ####
      if (hand_metrics$n_enablers_ub >= 1){
        if (
          hand_produces_ub_and_two_mana_T1(hand_metrics, "B ON T1") |
          hand_produces_ub_and_two_mana_T1(hand_metrics, "U ON T1")
          ) return(TRUE)
      }
      
      
    }
    # ENABLER 3 ####
    if (hand_metrics$n_enablers_3 >= 1){
      
      # > 2U ####
      if (hand_metrics$n_enablers_2u >= 1){
        if (hand_metrics$mana_crypt){
          if (hand_produces_ub_with_constraint(hand_metrics, "U ON T1")) return(TRUE)
        }
        
        if(hand_produces_ub_and_three_mana_T1(hand_metrics, "U ON T1")) return(TRUE)
        
        
      }
      
      # > MDFC 2U ####
      if (hand_metrics$n_mdfc_enablers_2u >= 1){
        if (hand_metrics$mana_crypt){
          if (hand_produces_ub_with_constraint(hand_metrics, "U ON T1", mdfc_enabler = TRUE)) return(TRUE)
        }
        
        if(hand_produces_ub_and_three_mana_T1(hand_metrics, "U ON T1")) return(TRUE)
        
        
      }
      
      # > 2B ####
      if (hand_metrics$n_enablers_2b >= 1){
        
        if (hand_metrics$mana_crypt){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
        }
        
        if (hand_metrics$dark_ritual){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
        }
        
        if(hand_produces_ub_and_three_mana_T1(hand_metrics, "B ON T1")) return(TRUE)
        
      }
      
      # > MDFC 2B ####
      if (hand_metrics$n_mdfc_enablers_2b >= 1){
        if (hand_metrics$mana_crypt){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1", mdfc_enabler = TRUE)) return(TRUE)
        }
        
        if (hand_metrics$dark_ritual){
          if (hand_produces_ub_with_constraint(hand_metrics, "B ON T1")) return(TRUE)
        }
        
        if(hand_produces_ub_and_three_mana_T1(hand_metrics, "B ON T1")) return(TRUE)
        
      }

    }
  }

  # YURIKO T1 ####
  if (hand_produces_1ub_T1(hand_metrics)) return(TRUE)

  return(FALSE)
}

# ---- End of is_yuriko_on_T2.R ----


# ---- Start of process_card_data.R ----

#' @export
process_card_data <- function(filepath, run_checks = TRUE, use_httr = TRUE, waiter = NULL) {
  parse_txt_file(filepath, run_checks) |> 
    add_scryfall_data(use_httr, waiter) |> 
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

add_scryfall_data <- function(card_data, use_httr = TRUE, waiter = NULL) {

  card_data |> 
    dplyr::pull(card_name) |> 
    purrr::imap(.f = function(card_name, card_index) {

      # Wait between requests to the API
      Sys.sleep(0.1)
      
      if (use_httr) {

        # API endpoint and query parameters
        url <- "https://api.scryfall.com/cards/named"
        query_params <- list(exact = card_name)
        
        # Make GET request to Scryfall API
        response <- httr::GET(url, query = query_params)
        
        card_info <- response |>
          httr::content(as = "text") |> 
          jsonlite::fromJSON(flatten = TRUE)

      } else {

        # httr does not work with shinylive
        temp_card_json <- tempfile(pattern = "card", fileext = ".json")

        card_name_for_url <- card_name |> 
          stringr::str_replace_all(" ", "%20") |> 
          stringr::str_replace_all("/", "%20")

        url <- glue::glue("https://api.scryfall.com/cards/named?exact={ card_name_for_url }")

        # API endpoint and query parameters
        download.file(url, temp_card_json)

        # Parse JSON content
        card_info <- temp_card_json |> 
          jsonlite::fromJSON(flatten = TRUE)

      }

      if (card_info$layout %in% c("split", "flip", "adventure", "normal")) {
        out <- tibble::tibble(
          card_name = card_name,
          layout = card_info$layout,
          name = card_info$name,
          cmc = card_info$cmc,
          colors = paste0(card_info$colors, collapse = ""),
          mana_cost = card_info$mana_cost,
          type = card_info$type_line,
          produced_mana = paste0(card_info$produced_mana, collapse = ""),
          img_src = card_info$image_uris$png
        )
      }

      if (card_info$layout %in% c("transform")) {
        out <- tibble::tibble(
          card_name = card_name,
          layout = card_info$layout,
          name = card_info$name,
          cmc = card_info$cmc,
          colors = paste0(card_info$card_faces$colors[[1]], collapse = ""),
          mana_cost = paste0(card_info$card_faces$mana_cost[[1]], collapse = ""),
          type = card_info$type_line,
          produced_mana = paste0(card_info$produced_mana, collapse = ""),
          img_src = card_info$card_faces$image_uris.png[[1]]
        )
      }

      if (card_info$layout == "modal_dfc") {
        out <- tibble::tibble(
          card_name = card_name,
          layout = card_info$layout,
          name = card_info$name,
          cmc = card_info$cmc,
          colors = paste0(card_info$card_faces$colors[[1]], collapse = ""),
          mana_cost = paste0(card_info$card_faces$mana_cost, collapse = " // "),
          type = paste0(card_info$card_faces$type_line, collapse = " // "),
          produced_mana = paste0(card_info$produced_mana, collapse = ""),
          img_src = card_info$card_faces$image_uris.png[[1]]
        )
      }
      
      # TODO: add logic for "prototype" layout
      
      message <- glue::glue("Processing card { card_index } of { nrow(card_data) }: { card_name }")
      
      if (!is.null(waiter)) {
        waiter$update(
          shiny::tagList(
            waiter::spin_fading_circles(),
            message
          )
        )
      } else {
        cli::cli_inform(message)
      }
      
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

# ---- End of process_card_data.R ----


# ---- Start of summarise_hand.R ----

summarise_hand <- function(opening_hand){
  list(
    "n_lands"        = opening_hand$land        |> sum(),
    "n_color_lands"  = opening_hand$color_land  |> sum(),
    "n_lands_u"      = opening_hand$land_u      |> sum(),
    "n_lands_b"      = opening_hand$land_b      |> sum(),
    "n_mdfc_lands"   = opening_hand$mdfc_land   |> sum(),
    "n_mdfc_lands_u" = opening_hand$mdfc_land_u |> sum(),
    "n_mdfc_lands_b" = opening_hand$mdfc_land_b |> sum(),
    
    "n_enablers"   = opening_hand$enabler   |> sum(),
    "n_enablers_0" = opening_hand$enabler_0 |> sum(),
    "n_enablers_1" = opening_hand$enabler_1 |> sum(),
    "n_enablers_2" = opening_hand$enabler_2 |> sum(),
    "n_enablers_3" = opening_hand$enabler_3 |> sum(),

    "n_enablers_c" = opening_hand$enabler_c |> sum(),
    "n_enablers_u" = opening_hand$enabler_u |> sum(),
    "n_enablers_b" = opening_hand$enabler_b |> sum(),

    "n_enablers_1u" = opening_hand$enabler_1u |> sum(),
    "n_enablers_1b" = opening_hand$enabler_1b |> sum(),
    "n_enablers_uu" = opening_hand$enabler_uu |> sum(),
    "n_enablers_bb" = opening_hand$enabler_bb |> sum(),
    "n_enablers_ub" = opening_hand$enabler_ub |> sum(),
    
    "n_enablers_2u" = opening_hand$enabler_2u |> sum(),
    "n_enablers_2b" = opening_hand$enabler_2b |> sum(),
    
    "n_mdfc_enablers_2u" = opening_hand$mdfc_enabler_2u |> sum(),
    "n_mdfc_enablers_2b" = opening_hand$mdfc_enabler_2b |> sum(),

    "n_cards_u" = opening_hand$color_u |> sum(),
    "n_cards_b" = opening_hand$color_b |> sum(),

    "chrome_mox"  = "Chrome Mox"  %in% opening_hand$card_name,
    "dark_ritual" = "Dark Ritual" %in% opening_hand$card_name,
    "lotus_petal" = "Lotus Petal" %in% opening_hand$card_name,
    "mana_crypt"  = "Mana Crypt"  %in% opening_hand$card_name,
    "mox_diamond" = "Mox Diamond" %in% opening_hand$card_name
  )
}

# ---- End of summarise_hand.R ----


# ---- Start of utils-add_custom_attributes.R ----

# ENABLERS ####

# TODO: add logic for enablers that use phyrexian mana

is_enabler <- function(cmc, type) {
  cmc <= 3 & grepl("Creature", type)
}

# > CMC = 0 ####
is_enabler_0 <- function(is_enabler, mana_cost) {
  is_enabler & mana_cost == "{0}"
}

# > CMC = 1 ####
is_enabler_1 <- function(is_enabler, cmc, mana_cost) {
  is_enabler & (cmc == 1 | mana_cost == "{X}")
}

is_enabler_c <- function(is_enabler_1, mana_cost) {
  is_enabler_1 & mana_cost %in% c("{1}", "{X}")
}

is_enabler_u <- function(is_enabler_1, mana_cost) {
  is_enabler_1 & grepl("{U}", mana_cost, fixed = TRUE)
}

is_enabler_b <- function(is_enabler_1, mana_cost) {
  is_enabler_1 & grepl("{B}", mana_cost, fixed = TRUE)
}

# > CMC = 2 ####
is_enabler_2 <- function(is_enabler, cmc) {
  is_enabler & cmc == 2
}

is_enabler_1u <- function(is_enabler_2, mana_cost) {
  is_enabler_2 & grepl("{1}{U}", mana_cost, fixed = TRUE)
}

is_enabler_1b <- function(is_enabler_2, mana_cost) {
  is_enabler_2 & grepl("{1}{B}", mana_cost, fixed = TRUE)
}

is_enabler_uu <- function(is_enabler_2, mana_cost) {
  is_enabler_2 & grepl("{U}{U}", mana_cost, fixed = TRUE)
}

is_enabler_bb <- function(is_enabler_2, mana_cost) {
  is_enabler_2 & grepl("{B}{B}", mana_cost, fixed = TRUE)
}

is_enabler_ub <- function(is_enabler_2, mana_cost) {
  is_enabler_2 & grepl("{U}{B}", mana_cost, fixed = TRUE)
}

# > CMC = 3 ####
is_enabler_3 <- function(is_enabler, cmc) {
  is_enabler & cmc == 3
}

is_enabler_2u <- function(layout, is_enabler_3, mana_cost) {
  layout != "modal_dfc" & is_enabler_3 & grepl("{2}{U}", mana_cost, fixed = TRUE)
}

is_enabler_2b <- function(layout, is_enabler_3, mana_cost) {
  layout != "modal_dfc" & is_enabler_3 & grepl("{2}{B}", mana_cost, fixed = TRUE)
}

is_mdfc_enabler_2u <- function(layout, is_enabler_3, mana_cost) {
  layout == "modal_dfc" & is_enabler_3 & grepl("{2}{U}", mana_cost, fixed = TRUE)
}

is_mdfc_enabler_2b <- function(layout, is_enabler_3, mana_cost) {
  layout == "modal_dfc" & is_enabler_3 & grepl("{2}{B}", mana_cost, fixed = TRUE)
}

# COLOR IDENTITY ####
is_color_u <- function(colors) {
  grepl("U", colors)
}

is_color_b <- function(colors) {
  grepl("B", colors)
}

# LANDS ####
is_land <- function(type) {
  # mdfc cards that have a land in both card faces are included
  grepl("Land", type) & !grepl("(Creature)|(Instant)|(Sorcery)", type)
}

is_color_land <- function(is_land, produced_mana) {
  # Even though Fetchlands don't produce mana, they are considered color lands
  is_land & (grepl("U|B", produced_mana) | produced_mana == "")
}

is_land_u <- function(is_color_land, produced_mana) {
  is_color_land & (grepl("U", produced_mana) | produced_mana == "")
}

is_land_b <- function(is_color_land, produced_mana) {
  is_color_land & (grepl("B", produced_mana) | produced_mana == "")
}

# MDFC LANDS ####
is_mdfc_land <- function(layout, type) {
  # mdfc cards that have a land in both card faces are excluded
  layout == "modal_dfc" & grepl("Land", type) & grepl("(Creature)|(Instant)|(Sorcery)", type)
}

is_mdfc_land_u <- function(is_mdfc_land, produced_mana) {
  is_mdfc_land & grepl("U", produced_mana)
}

is_mdfc_land_b <- function(is_mdfc_land, produced_mana) {
  is_mdfc_land & grepl("B", produced_mana)
}

# ---- End of utils-add_custom_attributes.R ----


# ---- Start of utils.R ----

#' @export
draw_seven <- function(decklist, must_have = NULL){
  if (is.null(must_have)) {

    decklist |>
      dplyr::slice_sample(n = 7)

  } else {
    n_must_have <- length(must_have)
    
    hand <- decklist |>
      dplyr::filter(!card_name %in% must_have) |>
      dplyr::slice_sample(n = 7 - n_must_have)
    
    hand |>
      dplyr::bind_rows(decklist |> dplyr::filter(card_name %in% must_have))
  }

}

#' @export
get_hand <- function(decklist, cards){
  decklist |>
    dplyr::filter(card_name %in% cards)
}

# ---- End of utils.R ----


