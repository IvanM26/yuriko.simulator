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
