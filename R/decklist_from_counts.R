decklist_from_counts <- function(group_counts) {
  
  tibble::tibble(
    group = rep(names(group_counts), times = unlist(group_counts))
  ) |> 
    dplyr::left_join(get_group_attributes(), by = "group")
  
}

get_group_attributes <- function() {
  tibble::tribble(
    ~group           , ~layout    , ~card_name_scryfall   , ~name        , ~cmc, ~mana_cost , ~type             , ~produced_mana,
    "enabler_0"      , "normal"   , ""                    , ""           ,    0, "{0}"      , "Creature"        , ""            ,
    "enabler_b"      , "normal"   , ""                    , ""           ,    1, "{B}"      , "Creature"        , ""            ,
    "enabler_u"      , "normal"   , ""                    , ""           ,    1, "{U}"      , "Creature"        , ""            ,
    "enabler_c"      , "normal"   , ""                    , ""           ,    1, "{1}"      , "Creature"        , ""            ,
    "enabler_bb"     , "normal"   , ""                    , ""           ,    2, "{B}{B}"   , "Creature"        , ""            ,
    "enabler_uu"     , "normal"   , ""                    , ""           ,    2, "{U}{U}"   , "Creature"        , ""            ,
    "enabler_ub"     , "normal"   , ""                    , ""           ,    2, "{U}{B}"   , "Creature"        , ""            ,
    "enabler_1b"     , "normal"   , ""                    , ""           ,    2, "{1}{B}"   , "Creature"        , ""            ,
    "enabler_1u"     , "normal"   , ""                    , ""           ,    2, "{1}{U}"   , "Creature"        , ""            ,
    "enabler_cc"     , "normal"   , ""                    , ""           ,    2, "{2}"      , "Creature"        , ""            ,
    "enabler_1bb"    , "normal"   , ""                    , ""           ,    3, "{1}{B}{B}", "Creature"        , ""            ,
    "enabler_2b"     , "normal"   , ""                    , ""           ,    3, "{2}{B}"   , "Creature"        , ""            ,
    "enabler_2u"     , "normal"   , ""                    , ""           ,    3, "{2}{U}"   , "Creature"        , ""            ,
    "mdfc_enabler_2b", "modal_dfc", ""                    , ""           ,    3, "{2}{B}"   , "Creature // Land", "B"           ,
    "mdfc_enabler_2u", "modal_dfc", ""                    , ""           ,    3, "{2}{U}"   , "Creature // Land", "U"           ,
    "land_b"         , "normal"   , ""                    , ""           ,    9, ""         , "Land"            , "B"           ,
    "land_u"         , "normal"   , ""                    , ""           ,    9, ""         , "Land"            , "U"           ,
    "land_c"         , "normal"   , ""                    , ""           ,    9, ""         , "Land"            , "C"           ,
    "land_ub"        , "normal"   , ""                    , ""           ,    9, ""         , "Land"            , "BU"          ,
    "mdfc_land_b"    , "modal_dfc", ""                    , ""           ,    9, "{B}"      , "Instant // Land" , "B"           ,
    "mdfc_land_u"    , "modal_dfc", ""                    , ""           ,    9, "{U}"      , "Instant // Land" , "U"           ,
    "other_b"        , "normal"   , ""                    , ""           ,    9, "{B}"      , ""                , ""            ,
    "other_u"        , "normal"   , ""                    , ""           ,    9, "{U}"      , ""                , ""            ,
    "other_c"        , "normal"   , ""                    , ""           ,    9, ""         , ""                , ""            ,
    "other_ub"       , "normal"   , ""                    , ""           ,    9, "{U}{B}"   , ""                , ""            ,
    "dark_ritual"    , "normal"   , "Dark Ritual"         , "Dark Ritual",    9, "{B}"      , ""                , ""            ,
    "chrome_mox"     , "normal"   , "Chrome Mox"          , "Chrome Mox" ,    9, ""         , ""                , ""            ,
    "lotus_petal"    , "normal"   , "Lotus Petal"         , "Lotus Petal",    9, ""         , ""                , ""            ,
    "mana_crypt"     , "normal"   , "Mana Crypt"          , "Mana Crypt" ,    9, ""         , ""                , ""            ,
    "mox_diamond"    , "normal"   , "Mox Diamond"         , "Mox Diamond",    9, ""         , ""                , ""            
  ) |> 
    add_custom_attributes() |> 
    dplyr::select(
      -layout,
      -name,
      -cmc,
      -mana_cost,
      -type,
      -produced_mana
    )
}
