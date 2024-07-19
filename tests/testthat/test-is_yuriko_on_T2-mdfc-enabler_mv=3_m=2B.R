# MDFC ENABLER MV 3 ####
# > 2B ENABLER ####
hand <- get_hand(testdata, c("Boggart Trawler", "Mana Crypt", "Island", "Swamp"))
test_that("2b mdfc-enabler + mana crypt + land u + land b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler", "Mana Crypt", "Swamp", "Takenuma, Abandoned Mire"))
test_that("2b mdfc-enabler + mana crypt but no ub", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler", "Mana Crypt", "Island"))
test_that("2b mdfc-enabler + mana crypt with ub but no b to cast enabler", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler", "Mana Crypt", "Agadeem's Awakening"))
test_that("2b mdfc-enabler + mana crypt + mdfc land b but no u", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler", "Mana Crypt", "Agadeem's Awakening", "Island"))
test_that("2b mdfc-enabler + mana crypt + mdfc b + land u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler", "Mana Crypt", "Scalding Tarn"))
test_that("2b mdfc-enabler + mana crypt + but no lands", 
          {expect_false(is_yuriko_on_T2(hand))})
