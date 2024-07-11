summarise_hand <- function(opening_hand){
  list(
    "n_lands"        = opening_hand$land        %>% sum(),
    "n_color_lands"  = opening_hand$color_land  %>% sum(),
    "n_lands_u"      = opening_hand$land_u      %>% sum(),
    "n_lands_b"      = opening_hand$land_b      %>% sum(),
    "n_mdfc_lands"   = opening_hand$mdfc_land   %>% sum(),
    "n_mdfc_lands_u" = opening_hand$mdfc_land_u %>% sum(),
    "n_mdfc_lands_b" = opening_hand$mdfc_land_b %>% sum(),
    
    "n_enablers"   = opening_hand$enabler   %>% sum(),
    "n_enablers_0" = opening_hand$enabler_0 %>% sum(),
    "n_enablers_1" = opening_hand$enabler_1 %>% sum(),
    "n_enablers_2" = opening_hand$enabler_2 %>% sum(),
    "n_enablers_3" = opening_hand$enabler_3 %>% sum(),

    "n_enablers_c" = opening_hand$enabler_c %>% sum(),
    "n_enablers_u" = opening_hand$enabler_u %>% sum(),
    "n_enablers_b" = opening_hand$enabler_b %>% sum(),

    "n_enablers_1u" = opening_hand$enabler_1u %>% sum(),
    "n_enablers_1b" = opening_hand$enabler_1b %>% sum(),
    "n_enablers_uu" = opening_hand$enabler_uu %>% sum(),
    "n_enablers_bb" = opening_hand$enabler_bb %>% sum(),
    "n_enablers_ub" = opening_hand$enabler_ub %>% sum(),
    
    "n_enablers_2u" = opening_hand$enabler_2u %>% sum(),
    "n_enablers_2b" = opening_hand$enabler_2b %>% sum(),

    "n_cards_u" = opening_hand$color_u %>% sum(),
    "n_cards_b" = opening_hand$color_b %>% sum(),

    "chrome_mox"  = "Chrome Mox"  %in% opening_hand$card_name,
    "dark_ritual" = "Dark Ritual" %in% opening_hand$card_name,
    "lotus_petal" = "Lotus Petal" %in% opening_hand$card_name,
    "mana_crypt"  = "Mana Crypt"  %in% opening_hand$card_name,
    "mox_diamond" = "Mox Diamond" %in% opening_hand$card_name
  )
}