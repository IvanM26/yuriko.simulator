#' @export
is_yuriko_on_T2 <- function(opening_hand){

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
    }
  }
  
  # YURIKO T1 ####
  if (hand_produces_ub_and_three_mana_T1(hand_metrics, "B ON T1") |
      hand_produces_ub_and_three_mana_T1(hand_metrics, "U ON T1")) return(TRUE)
  
  if (hand_metrics$mana_crypt){
    if (
      hand_produces_ub_and_two_mana_T1(hand_metrics, "B ON T1") |
      hand_produces_ub_and_two_mana_T1(hand_metrics, "U ON T1")
    ) return(TRUE)
  }
  
  return(FALSE)
  }