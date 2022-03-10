filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath) %>%
  dplyr::filter(in_deck == 1)

# ENABLER WITH MV 0 ####
hand <- get_hand(decklist, c("Ornithopter",
                              "Island1",
                              "Island2",
                              "Island3",
                              "Island4",
                              "Island5",
                              "Island6"))

test_that("detects enabler with mv 0", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# ENABLER WITH MV 1 ####
hand <- get_hand(decklist, c("Sage of Epityr",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("detects u enabler with u land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Sage of Epityr",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects u enabler with b land", {
  expect_equal(can_cast_enabler_t1(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("detects colorless enabler with u land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects colorless enabler with b land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Underground River",
                             "Force of Will",
                             "Vampiric Tutor",
                             "Mystical Tutor",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects colorless enabler with ub land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("detects b enabler with u land", {
  expect_equal(can_cast_enabler_t1(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Underground Sea",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5"))

test_that("detects b enabler with ub land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Chrome Mox",
                             "Snuff Out",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5"))

test_that("detects b enabler with chrome mox on", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Chrome Mox",
                             "Force of Will",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5"))

test_that("detects b enabler with chrome mox off", {
  expect_equal(can_cast_enabler_t1(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mothdust Changeling",
                             "Chrome Mox",
                             "Force of Will",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4"))

test_that("detects u enabler with chrome mox on", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mothdust Changeling",
                             "Mox Diamond",
                             "Force of Will",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4"))

test_that("detects u enabler with mox diamond on", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast",
                             "Mox Diamond",
                             "Force of Will",
                             "Sea Gate Restoration",
                             "Island1",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects b enabler with mox diamond on with u land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Mox Diamond",
                             "Force of Will",
                             "Sea Gate Restoration",
                             "Island1",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects colorless enabler with mox diamond on", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Mana Crypt",
                             "Force of Will",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects colorless enabler with mana crypt", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Chrome Mox",
                             "Force of Will",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects colorless enabler with chrome mox on and no lands", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Mox Diamond",
                             "Gemstone Caverns",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects colorless enabler with mox diamond and colorless land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Thousand-Faced Shadow",
                             "Swamp1",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects castable colorless enabler with uncastable u enabler", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Stonecoil Serpent",
                             "Force of Will",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects uncastable colorless enabler", {
  expect_equal(can_cast_enabler_t1(hand), FALSE)
})

# ENABLER WITH MV 2 ####
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Mana Crypt",
                             "Agadeem's Awakening",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with b land and mana crypt", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Moon-Circuit Hacker",
                             "Mana Crypt",
                             "Island1",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("mv2 - detects u enabler with u land and mana crypt", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Moon-Circuit Hacker",
                             "Mana Crypt",
                             "Chrome Mox",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("mv2 - detects u enabler with mana crypt and chrome mox", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Chrome Mox",
                             "Sea Gate Restoration",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with land and chrome mox on b", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Chrome Mox",
                             "Agadeem's Awakening",
                             "Force of Will",
                             "Mox Amber",
                             "Mist-Syndicate Naga",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with land and chrome mox on u", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Chrome Mox",
                             "Sea Gate Restoration",
                             "Mist-Syndicate Naga",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects uncastable b enabler with u land and chrome mox on u", {
  expect_equal(can_cast_enabler_t1(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Mox Diamond",
                             "Island1",
                             "Mana Crypt",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with u land, mana crypt and mox diamond", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Lotus Petal",
                             "Island1",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with land and lotus petal", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Lotus Petal",
                             "Chrome Mox",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with chrome mox and lotus petal", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Dark Ritual",
                             "Swamp1",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with dark ritual and produce b with land", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Dark Ritual",
                             "Chrome Mox",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with dark ritual and produce b with chrome mox", {
  expect_equal(can_cast_enabler_t1(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Dark Ritual",
                             "Chrome Mox",
                             "Brainstorm",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("mv2 - detects b enabler with dark ritual and produce b with chrome mox", {
  expect_equal(can_cast_enabler_t1(hand), FALSE)
})
