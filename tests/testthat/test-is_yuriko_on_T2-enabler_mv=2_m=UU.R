filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath)

# ENABLER MV 2 ####
# > UU ENABLER ####
hand <- get_hand(decklist, c("Thassa's Oracle", "Chrome Mox", "Force of Will", "Island1", "Swamp1"))
test_that("enabler 2 uu + chrome mox on u t1 + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Chrome Mox", "Snuff Out", "Island1", "Swamp1"))
test_that("enabler 2 uu + chrome mox on + ub on t2 but no uu t1", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Lotus Petal", "Island1", "Island2"))
test_that("enabler 2 uu + lotus petal but no ub t2", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Lotus Petal", "Island1", "Swamp1"))
test_that("enabler 2 uu + lotus petal + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Mox Diamond", "Island1", "Swamp1"))
test_that("enabler 2 uu + mox diamond + 2 lands", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Mox Diamond", "Swamp1"))
test_that("enabler 2 uu + mox diamond but 1 land", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Mox Diamond", "Swamp1", "Sink into Stupor"))
test_that("enabler 2 uu + mox diamond + 1 lands + u mdfc", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Mox Diamond", "Swamp1", "Boggart Trawler"))
test_that("enabler 2 uu + mox diamond + 1 land + mdfc but b mdfc", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Thassa's Oracle", "Mox Diamond", "Island1", "Boggart Trawler"))
test_that("enabler 2 uu + mox diamond + 1 land + mdfc but b mdfc", 
          {expect_false(is_yuriko_on_T2(hand))})