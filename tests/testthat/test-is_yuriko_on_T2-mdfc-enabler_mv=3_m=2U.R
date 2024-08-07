# MDFC ENABLER MV 3 ####
# > 2U ENABLER ####
hand <- get_hand(testdata, c("Hydroelectric Specimen // Hydroelectric Laboratory", "Mana Crypt", "Island", "Swamp"))
test_that("2u mdfc-enabler + mana crypt + land u + land b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Hydroelectric Specimen // Hydroelectric Laboratory", "Mana Crypt", "Island", "Otawara, Soaring City"))
test_that("2u mdfc-enabler + mana crypt but no ub", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Hydroelectric Specimen // Hydroelectric Laboratory", "Mana Crypt", "Swamp"))
test_that("2u mdfc-enabler + mana crypt with ub but no u to cast enabler", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Hydroelectric Specimen // Hydroelectric Laboratory", "Mana Crypt", "Sink into Stupor // Soporific Springs"))
test_that("2u mdfc-enabler + mana crypt + mdfc land u but no b", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Hydroelectric Specimen // Hydroelectric Laboratory", "Mana Crypt", "Sink into Stupor // Soporific Springs", "Swamp"))
test_that("2u mdfc-enabler + mana crypt + mdfc u + land b", 
          {expect_true(is_yuriko_on_T2(hand))})
