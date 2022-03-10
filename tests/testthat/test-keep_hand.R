filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath) %>%
  dplyr::filter(in_deck == 1)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Ornithopter",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("with enabler with mv 0 but can't produce ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Sage of Epityr",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("with u enabler but can't produce ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("with colorless enabler but can't produce ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Swamp1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Force of Will"))

test_that("can produce ub but no enabler", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Underground Sea",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5"))

test_that("b enabler and ub", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Chrome Mox",
                             "Snuff Out",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5"))

test_that("b enabler and ub with chrome mox on", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Fourth Bridge Prowler",
                             "Chrome Mox",
                             "Force of Will",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5"))

test_that("b enabler with chrome mox off", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mothdust Changeling",
                             "Chrome Mox",
                             "Force of Will",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4"))

test_that("u enabler with chrome mox on", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mothdust Changeling",
                             "Mox Diamond",
                             "Force of Will",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4"))

test_that("u enabler with mox diamond on", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast",
                             "Mox Diamond",
                             "Force of Will",
                             "Sea Gate Restoration",
                             "Island1",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("b enabler with mox diamond on with u land", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Mox Diamond",
                             "Force of Will",
                             "Sea Gate Restoration",
                             "Island1",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("colorless enabler with mox diamond on", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Mana Crypt",
                             "Force of Will",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("colorless enabler with mana crypt but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Chrome Mox",
                             "Force of Will",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("colorless enabler with chrome mox on but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Mox Diamond",
                             "Gemstone Caverns",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("colorless enabler with mox diamond and colorless land but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Universal Automaton",
                             "Thousand-Faced Shadow",
                             "Swamp1",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("detects castable colorless enabler with uncastable u enabler but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Mana Crypt",
                             "Agadeem's Awakening",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with b land and mana crypt but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Moon-Circuit Hacker",
                             "Mana Crypt",
                             "Island1",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Swamp1"))

test_that("u enabler mv2 with ub and mana crypt", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Moon-Circuit Hacker",
                             "Mana Crypt",
                             "Chrome Mox",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Island1",
                             "Fierce Guardianship"))

test_that("u enabler mv2 with mana crypt and chrome mox and ub", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Chrome Mox",
                             "Sea Gate Restoration",
                             "Deadly Rollick",
                             "Mox Amber",
                             "Snuff Out",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with land and chrome mox on b", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Chrome Mox",
                             "Sea Gate Restoration",
                             "Mist-Syndicate Naga",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("uncastable b enabler mv2 with u land and chrome mox on u", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Mox Diamond",
                             "Island1",
                             "Mana Crypt",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with u land, mana crypt and mox diamond but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Lotus Petal",
                             "Island1",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with land and lotus petal but no ub when casted", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Lotus Petal",
                             "Chrome Mox",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with chrome mox and lotus petal but no ub when casted", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Dark Ritual",
                             "Swamp1",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with dark ritual and produce b with land but no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast",
                             "Island1",
                             "Lotus Petal",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv1 with no ub", {
  expect_equal(keep_hand(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast",
                             "Swamp1",
                             "Lotus Petal",
                             "Snuff Out",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv1 with ub using lotus petal", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Swamp1",
                             "Lotus Petal",
                             "Island1",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with ub", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Swamp1",
                             "Lotus Petal",
                             "Dark Ritual",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv2 with ub and dark ritual", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mistblade Shinobi",
                             "Island1",
                             "Mana Crypt",
                             "Swamp1",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv3 with ub", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mistblade Shinobi",
                             "Island1",
                             "Mana Crypt",
                             "Lotus Petal",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv3 with ub using lotus petal", {
  expect_equal(keep_hand(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Mistblade Shinobi",
                             "Island1",
                             "Swamp1",
                             "Lotus Petal",
                             "Mox Amber",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("b enabler mv3 with ub using lotus petal but no mana crypt", {
  expect_equal(keep_hand(hand), FALSE)
})
