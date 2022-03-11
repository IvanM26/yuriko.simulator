is_yuriko_on_T2 <- function(opening_hand){
  # amount of lands
  n_lands <- opening_hand$is_land %>% sum()
  
  # amount of lands that produce at least one color
  n_color_lands <- opening_hand$is_color_land %>% sum()
  
  # amount of lands that produce u
  n_lands_u <- opening_hand$is_land_u %>% sum()
  
  # amount of lands that produce b
  n_lands_b <- opening_hand$is_land_b %>% sum()
  
  # amount of mdfc lands
  n_mdfc_lands <- opening_hand$is_mdfc_land %>% sum()
  
  # amount of mdfc lands that produce u
  n_mdfc_lands_u <- opening_hand$is_mdfc_land_u %>% sum()
  
  # amount of mdfc lands that produce b
  n_mdfc_lands_b <- opening_hand$is_mdfc_land_b %>% sum()
  
  # amount of enablers
  n_enablers <- opening_hand$enabler %>% sum()
  
  # amount of enablers with mv = 0
  n_enablers_0 <- opening_hand$enabler_0 %>% sum()

  # amount of enablers with mv = 1
  n_enablers_1 <- opening_hand$enabler_1 %>% sum()
  
  # amount of enablers with mv = 2
  n_enablers_2 <- opening_hand$enabler_2 %>% sum()
  
  # amount of enablers with mv = 3
  n_enablers_3 <- opening_hand$enabler_3 %>% sum()
  
  # amount of colorless enablers with mv = 1
  n_enablers_c <- opening_hand$enabler_c %>% sum()
  
  # amount of u enablers with mv = 1
  n_enablers_u <- opening_hand$enabler_u %>% sum()
  
  # amount of b enablers with mv = 1
  n_enablers_b <- opening_hand$enabler_b %>% sum()
  
  # amount of 1u enablers with mv = 2
  n_enablers_1u <- opening_hand$enabler_1u %>% sum()
  
  # amount of 1b enablers with mv = 2
  n_enablers_1b <- opening_hand$enabler_1b %>% sum()
  
  # amount of 2u enablers with mv = 3
  n_enablers_2u <- opening_hand$enabler_2u %>% sum()
  
  # amount of 2b enablers with mv = 3
  n_enablers_2b <- opening_hand$enabler_2b %>% sum()
  
  # amount of cards with u identity
  n_cards_u <- opening_hand$color_u %>% sum()
  
  # amount of cards with b identity
  n_cards_b <- opening_hand$color_b %>% sum()
  
  chrome_mox <- "Chrome Mox" %in% opening_hand$card_name
  dark_ritual <- "Dark Ritual" %in% opening_hand$card_name
  lotus_petal <- "Lotus Petal" %in% opening_hand$card_name
  mana_crypt <- "Mana Crypt" %in% opening_hand$card_name
  mox_diamond <- "Mox Diamond" %in% opening_hand$card_name
  
  if (n_enablers >= 1){
    # ENABLER 0 ####
    if (n_enablers_0 >= 1){
      
      if (n_lands >= 2){
        
        if (n_color_lands >= 2){
          if (mox_diamond) return(TRUE)
          if (lotus_petal) return(TRUE)
          if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
          if (n_lands_b == 0){
            if (chrome_mox & (n_cards_b >= 1)) return(TRUE)
            if (n_mdfc_lands_b >= 1) return(TRUE)
          }
          if (n_lands_u == 0){
            if (chrome_mox & (n_cards_u >= 1)) return(TRUE)
            if (n_mdfc_lands_u >= 1) return(TRUE)
          }
        }
        
        if (n_color_lands == 1){
          if (mox_diamond) return(TRUE)
          if (lotus_petal) return(TRUE)
          if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
          if (n_lands_b == 0){
            if (chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1) return(TRUE)
          }
          if (n_lands_u == 0){
            if (chrome_mox & n_cards_u >= 1) return(TRUE)
            if (n_mdfc_lands_u >= 1) return(TRUE)
          }
        }
        
        if (n_color_lands == 0){
          if (n_mdfc_lands == 0){
            if (mox_diamond & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
          }
          if (n_mdfc_lands == 1){
            if (mox_diamond) return(TRUE)
            if (lotus_petal) return(TRUE)
            if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
          }
          if (n_mdfc_lands == 2){
            if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
          }
        }
      }
      
      if (n_lands == 1){
        
        if (n_color_lands == 1){
          if (lotus_petal) return(TRUE)
          if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
          if (n_lands_b == 1){
            if (chrome_mox & n_cards_u >= 1) return(TRUE)
            if (n_mdfc_lands_u >= 1) return(TRUE)
            if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
          }
          if (n_lands_u == 1){
            if (chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1) return(TRUE)
            if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
          }
        }
        
        if (n_color_lands == 0){
          if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
          if (mox_diamond){
            if (chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
            if (n_mdfc_lands >= 1) return(TRUE)
          }
          if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
          if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
          if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
        }
      }
      
      if (n_lands == 0){
        if (lotus_petal & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
        if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
        if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
        if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
      }
    }
    
    # ENABLER 1 ####
    if (n_enablers_1 >= 1){
      
      # > COLORLESS ####
      
      if (n_enablers_c >= 1){
        # SAME AS ENABLER 0
        if (n_lands >= 2){
          
          if (n_color_lands >= 2){
            if (mox_diamond) return(TRUE)
            if (lotus_petal) return(TRUE)
            if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
            if (n_lands_b == 0){
              if (chrome_mox & (n_cards_b >= 1)) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 0){
              if (chrome_mox & (n_cards_u >= 1)) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 1){
            if (mox_diamond) return(TRUE)
            if (lotus_petal) return(TRUE)
            if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
            if (n_lands_b == 0){
              if (chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 0){
              if (chrome_mox & n_cards_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 0){
            if (n_mdfc_lands == 0){
              if (mox_diamond & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
            }
            if (n_mdfc_lands == 1){
              if (mox_diamond) return(TRUE)
              if (lotus_petal) return(TRUE)
              if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
            }
            if (n_mdfc_lands == 2){
              if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
            }
          }
        }
        
        if (n_lands == 1){
          
          if (n_color_lands == 1){
            if (lotus_petal) return(TRUE)
            if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
            if (n_lands_b == 1){
              if (chrome_mox & n_cards_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
              if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
            }
            if (n_lands_u == 1){
              if (chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
              if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 0){
            if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
            if (mox_diamond){
              if (chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
              if (n_mdfc_lands >= 1) return(TRUE)
            }
            if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
            if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
          }
        }
        
        if (n_lands == 0){
          if (lotus_petal & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
          if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
          if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
          if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
        }
        
      }
      
      # > BLUE ####
      if (n_enablers_u >= 1){
        if (n_lands >= 2){
          
          if (n_color_lands >= 2){
            if (mox_diamond) return(TRUE)
            if (lotus_petal){
              if (n_lands_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
            if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
            if (n_lands_b == 0){
              if (chrome_mox & (n_cards_b >= 1)) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 0){
              if (chrome_mox & (n_cards_u >= 2)) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 1){
            if (mox_diamond) return(TRUE)
            if (lotus_petal){
              if (n_lands_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
            if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
            if (n_lands_b == 0){
              if (chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 0){
              if (chrome_mox & n_cards_u >= 2) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 0){
            if (n_mdfc_lands == 0){
              if (mox_diamond & chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
            }
            if (n_mdfc_lands == 1){
              if (mox_diamond) return(TRUE)
              if (lotus_petal & n_mdfc_lands_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 2) return(TRUE)
            }
            if (n_mdfc_lands == 2){
              if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
            }
          }
        }
        
        if (n_lands == 1){
          
          if (n_color_lands == 1){
            if (lotus_petal){
              if (n_lands_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
            if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
            if (n_lands_b == 1){
              if (chrome_mox & n_cards_u >= 2) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
              if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
            }
            if (n_lands_u == 1){
              if (chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
              if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 0){
            if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
            if (mox_diamond){
              if (chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
              if (n_mdfc_lands >= 1) return(TRUE)
            }
            if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 2) return(TRUE)
            if (lotus_petal & n_mdfc_lands_u >= 1) return(TRUE)
          }
        }
        
        if (n_lands == 0){
          if (lotus_petal & chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
          if (lotus_petal & n_mdfc_lands_u >= 1) return(TRUE)
          if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
          if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 2) return(TRUE)
        }
        
      }
      
      # > BLACK ####
      if (n_enablers_b >= 1){
        # SIMILAR TO BLUE ENABLER
        if (n_lands >= 2){
          
          if (n_color_lands >= 2){
            if (mox_diamond) return(TRUE)
            if (lotus_petal){
              if (n_lands_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
            if (n_lands_b == 0){
              if (chrome_mox & (n_cards_b >= 2)) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 0){
              if (chrome_mox & (n_cards_u >= 1)) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 1){
            if (mox_diamond) return(TRUE)
            if (lotus_petal){
              if (n_lands_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 2)) return(TRUE)
            if (n_lands_b == 0){
              if (chrome_mox & n_cards_b >= 2) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 0){
              if (chrome_mox & n_cards_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 0){
            if (n_mdfc_lands == 0){
              if (mox_diamond & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 2)) return(TRUE)
            }
            if (n_mdfc_lands == 1){
              if (mox_diamond) return(TRUE)
              if (lotus_petal & n_mdfc_lands_b >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 2) return(TRUE)
              if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
            }
            if (n_mdfc_lands == 2){
              if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
            }
          }
        }
        
        if (n_lands == 1){
          
          if (n_color_lands == 1){
            if (lotus_petal){
              if (n_lands_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
            }
            if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
            if (n_lands_b == 1){
              if (chrome_mox & n_cards_u >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1) return(TRUE)
              if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
            }
            if (n_lands_u == 1){
              if (chrome_mox & n_cards_b >= 2) return(TRUE)
              if (n_mdfc_lands_b >= 1) return(TRUE)
              if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
            }
          }
          
          if (n_color_lands == 0){
            if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
            if (mox_diamond){
              if (chrome_mox & (n_cards_u >= 1 | n_cards_b >= 2)) return(TRUE)
              if (n_mdfc_lands >= 1) return(TRUE)
            }
            if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 2) return(TRUE)
            if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
            if (lotus_petal & n_mdfc_lands_b >= 1) return(TRUE)
          }
        }
        
        if (n_lands == 0){
          if (lotus_petal & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 2)) return(TRUE)
          if (lotus_petal & n_mdfc_lands_b >= 1) return(TRUE)
          if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 2) return(TRUE)
          if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
        }
        
      }
      
    }
    # ENABLER 2 ####
    if (n_enablers_2 >= 1){
      
      # > 1U ####
      if (n_enablers_1u >= 1){
        if (mana_crypt){
          # SAME AS BLUE ENABLER
          if (n_lands >= 2){
            
            if (n_color_lands >= 2){
              if (mox_diamond) return(TRUE)
              if (lotus_petal){
                if (n_lands_u >= 1) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
              if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
              if (n_lands_b == 0){
                if (chrome_mox & (n_cards_b >= 1)) return(TRUE)
                if (n_mdfc_lands_b >= 1) return(TRUE)
              }
              if (n_lands_u == 0){
                if (chrome_mox & (n_cards_u >= 2)) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
            }
            
            if (n_color_lands == 1){
              if (mox_diamond) return(TRUE)
              if (lotus_petal){
                if (n_lands_u >= 1) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
              if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
              if (n_lands_b == 0){
                if (chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1) return(TRUE)
              }
              if (n_lands_u == 0){
                if (chrome_mox & n_cards_u >= 2) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
            }
            
            if (n_color_lands == 0){
              if (n_mdfc_lands == 0){
                if (mox_diamond & chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
              }
              if (n_mdfc_lands == 1){
                if (mox_diamond) return(TRUE)
                if (lotus_petal & n_mdfc_lands_u >= 1) return(TRUE)
                if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 2) return(TRUE)
              }
              if (n_mdfc_lands == 2){
                if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
              }
            }
          }
          
          if (n_lands == 1){
            
            if (n_color_lands == 1){
              if (lotus_petal){
                if (n_lands_u >= 1) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
              if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
              if (n_lands_b == 1){
                if (chrome_mox & n_cards_u >= 2) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
                if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
              }
              if (n_lands_u == 1){
                if (chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1) return(TRUE)
                if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
              }
            }
            
            if (n_color_lands == 0){
              if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
              if (mox_diamond){
                if (chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
                if (n_mdfc_lands >= 1) return(TRUE)
              }
              if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 2) return(TRUE)
              if (lotus_petal & n_mdfc_lands_u >= 1) return(TRUE)
            }
          }
          
          if (n_lands == 0){
            if (lotus_petal & chrome_mox & (n_cards_u >= 2 | n_cards_b >= 1)) return(TRUE)
            if (lotus_petal & n_mdfc_lands_u >= 1) return(TRUE)
            if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 2) return(TRUE)
          }
        }
        
        if (lotus_petal){
          # SAME AS ENABLER 0
          if (n_lands >= 2){
            
            if (n_color_lands >= 2){
              if (mox_diamond) return(TRUE)
              if (lotus_petal) return(TRUE)
              if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
              if (n_lands_b == 0){
                if (chrome_mox & (n_cards_b >= 1)) return(TRUE)
                if (n_mdfc_lands_b >= 1) return(TRUE)
              }
              if (n_lands_u == 0){
                if (chrome_mox & (n_cards_u >= 1)) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
            }
            
            if (n_color_lands == 1){
              if (mox_diamond) return(TRUE)
              if (lotus_petal) return(TRUE)
              if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
              if (n_lands_b == 0){
                if (chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1) return(TRUE)
              }
              if (n_lands_u == 0){
                if (chrome_mox & n_cards_u >= 1) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
              }
            }
            
            if (n_color_lands == 0){
              if (n_mdfc_lands == 0){
                if (mox_diamond & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
              }
              if (n_mdfc_lands == 1){
                if (mox_diamond) return(TRUE)
                if (lotus_petal) return(TRUE)
                if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
              }
              if (n_mdfc_lands == 2){
                if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
              }
            }
          }
          
          if (n_lands == 1){
            
            if (n_color_lands == 1){
              if (lotus_petal) return(TRUE)
              if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
              if (n_lands_b == 1){
                if (chrome_mox & n_cards_u >= 1) return(TRUE)
                if (n_mdfc_lands_u >= 1) return(TRUE)
                if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
              }
              if (n_lands_u == 1){
                if (chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1) return(TRUE)
                if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
              }
            }
            
            if (n_color_lands == 0){
              if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
              if (mox_diamond){
                if (chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
                if (n_mdfc_lands >= 1) return(TRUE)
              }
              if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
              if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
            }
          }
          
          if (n_lands == 0){
            if (lotus_petal & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
            if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
            if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
            if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
          }
          
        }
        
        if (chrome_mox){
          if (n_cards_u >= 2){
            # SAME AS ENABLER 0
            if (n_lands >= 2){
              
              if (n_color_lands >= 2){
                if (mox_diamond) return(TRUE)
                if (lotus_petal) return(TRUE)
                if ((n_lands_u >= 1) & (n_lands_b >= 1)) return(TRUE)
                if (n_lands_b == 0){
                  if (chrome_mox & (n_cards_b >= 1)) return(TRUE)
                  if (n_mdfc_lands_b >= 1) return(TRUE)
                }
                if (n_lands_u == 0){
                  if (chrome_mox & (n_cards_u >= 1)) return(TRUE)
                  if (n_mdfc_lands_u >= 1) return(TRUE)
                }
              }
              
              if (n_color_lands == 1){
                if (mox_diamond) return(TRUE)
                if (lotus_petal) return(TRUE)
                if (n_lands_u == 1 & n_lands_b == 1 & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
                if (n_lands_b == 0){
                  if (chrome_mox & n_cards_b >= 1) return(TRUE)
                  if (n_mdfc_lands_b >= 1) return(TRUE)
                }
                if (n_lands_u == 0){
                  if (chrome_mox & n_cards_u >= 1) return(TRUE)
                  if (n_mdfc_lands_u >= 1) return(TRUE)
                }
              }
              
              if (n_color_lands == 0){
                if (n_mdfc_lands == 0){
                  if (mox_diamond & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
                }
                if (n_mdfc_lands == 1){
                  if (mox_diamond) return(TRUE)
                  if (lotus_petal) return(TRUE)
                  if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
                  if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
                }
                if (n_mdfc_lands == 2){
                  if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
                }
              }
            }
            
            if (n_lands == 1){
              
              if (n_color_lands == 1){
                if (lotus_petal) return(TRUE)
                if (n_lands_u == 1 & n_lands_b == 1 & n_mdfc_lands >= 1) return(TRUE)
                if (n_lands_b == 1){
                  if (chrome_mox & n_cards_u >= 1) return(TRUE)
                  if (n_mdfc_lands_u >= 1) return(TRUE)
                  if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
                }
                if (n_lands_u == 1){
                  if (chrome_mox & n_cards_b >= 1) return(TRUE)
                  if (n_mdfc_lands_b >= 1) return(TRUE)
                  if (mox_diamond & n_mdfc_lands >= 1) return(TRUE)
                }
              }
              
              if (n_color_lands == 0){
                if (n_mdfc_lands_u >= 1 & n_mdfc_lands_b >= 1) return(TRUE)
                if (mox_diamond){
                  if (chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
                  if (n_mdfc_lands >= 1) return(TRUE)
                }
                if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
                if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
                if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
              }
            }
            
            if (n_lands == 0){
              if (lotus_petal & chrome_mox & (n_cards_u >= 1 | n_cards_b >= 1)) return(TRUE)
              if (lotus_petal & n_mdfc_lands >= 1) return(TRUE)
              if (n_mdfc_lands_u >= 1 & chrome_mox & n_cards_b >= 1) return(TRUE)
              if (n_mdfc_lands_b >= 1 & chrome_mox & n_cards_u >= 1) return(TRUE)
            }
          }
        }
        
        if (mox_diamond){
          
        }
        
        
        
      }
      
      
      
      
    }
    
  }
  
  
  
  return(FALSE)
  }