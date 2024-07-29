# ENABLER MV 2 ####
# > UU ENABLER ####
hand <- get_hand(testdata, c("Thassa's Oracle", "Chrome Mox", "Force of Will", "Island", "Swamp"))
test_that("enabler 2 uu + chrome mox on u t1 + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Chrome Mox", "Snuff Out", "Island", "Swamp"))
test_that("enabler 2 uu + chrome mox on + ub on t2 but no uu t1", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Lotus Petal", "Island", "Otawara, Soaring City"))
test_that("enabler 2 uu + lotus petal but no ub t2", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Lotus Petal", "Island", "Swamp"))
test_that("enabler 2 uu + lotus petal + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Mox Diamond", "Island", "Swamp"))
test_that("enabler 2 uu + mox diamond + 2 lands", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Mox Diamond", "Swamp"))
test_that("enabler 2 uu + mox diamond but 1 land", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Mox Diamond", "Swamp", "Sink into Stupor // Soporific Springs"))
test_that("enabler 2 uu + mox diamond + 1 lands + u mdfc", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Mox Diamond", "Swamp", "Boggart Trawler // Boggart Bog"))
test_that("enabler 2 uu + mox diamond + 1 land + mdfc but b mdfc", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Thassa's Oracle", "Mox Diamond", "Island", "Boggart Trawler // Boggart Bog"))
test_that("enabler 2 uu + mox diamond + 1 land + mdfc but b mdfc", 
          {expect_false(is_yuriko_on_T2(hand))})
