filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath) %>%
  dplyr::filter(in_deck == 1)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Island1",
                             "Island2",
                             "Island3",
                             "Island4",
                             "Island5",
                             "Island6"))

test_that("no b", {
  expect_equal(can_produce_ub(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Swamp1",
                             "Swamp2",
                             "Swamp3",
                             "Swamp4",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("no u", {
  expect_equal(can_produce_ub(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Underground Sea",
                             "Mox Amber",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("ub without second land", {
  expect_equal(can_produce_ub(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Island1",
                             "Swamp1",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("ub with island and swamp", {
  expect_equal(can_produce_ub(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Scalding Tarn",
                             "Swamp1",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("ub with swamp and fetch", {
  expect_equal(can_produce_ub(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Verdant Catacombs",
                             "Island1",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("ub with island and fetch", {
  expect_equal(can_produce_ub(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Verdant Catacombs",
                             "Sea Gate Restoration",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("ub with land in back and fetch", {
  expect_equal(can_produce_ub(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Island1",
                             "Sea Gate Restoration",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("u with land in back and island", {
  expect_equal(can_produce_ub(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Chrome Mox",
                             "Sea Gate Restoration",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("land in back and chrome mox on", {
  expect_equal(can_produce_ub(hand), TRUE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Underground Sea",
                             "Gemstone Caverns",
                             "Gingerbrute",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("ub land and colorless land", {
  expect_equal(can_produce_ub(hand), FALSE)
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Inkrise Infiltrator",
                             "Underground Sea",
                             "Gemstone Caverns",
                             "Mox Diamond",
                             "Sage of Epityr",
                             "Force of Will",
                             "Fierce Guardianship"))

test_that("land that produce u and/or b and mox diamond with any other land", {
  expect_equal(can_produce_ub(hand), TRUE)
})
