filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath)

# ENABLER MV 2 ####
# > BB ENABLER ####
hand <- get_hand(decklist, c("Dauthi Voidwalker", "Chrome Mox", "Snuff Out", "Island", "Swamp"))
test_that("enabler 2 bb + chrome mox on + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Chrome Mox", "Force of Will", "Island", "Swamp"))
test_that("enabler 2 bb + chrome mox on + ub on t2 but no bb t1", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Lotus Petal", "Swamp", "Takenuma, Abandoned Mire"))
test_that("enabler 2 bb + lotus petal but no ub t2", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Lotus Petal", "Island", "Swamp"))
test_that("enabler 2 bb + lotus petal + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Mox Diamond", "Island", "Swamp"))
test_that("enabler 2 bb + mox diamond + 2 lands", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Mox Diamond", "Swamp"))
test_that("enabler 2 bb + mox diamond but 1 land", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Mox Diamond", "Swamp", "Boggart Trawler"))
test_that("enabler 2 bb + mox diamond + 1 lands + b mdfc", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Mox Diamond", "Swamp", "Sink into Stupor"))
test_that("enabler 2 bb + mox diamond + 1 land + mdfc but u mdfc", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Mox Diamond", "Island", "Sink into Stupor"))
test_that("enabler 2 bb + mox diamond + 1 land + mdfc but u mdfc", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Dark Ritual", "Island", "Boggart Trawler"))
test_that("enabler 2 bb + dark ritual + b mdfc", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Dark Ritual", "Swamp", "Sink into Stupor"))
test_that("enabler 2 bb + dark ritual + u mdfc", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Dark Ritual", "Swamp", "Boggart Trawler"))
test_that("enabler 2 bb + dark ritual but no ub", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Dark Ritual", "Island", "Sink into Stupor"))
test_that("enabler 2 bb + dark ritual but no ub", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dauthi Voidwalker", "Dark Ritual", "Island", "Scalding Tarn"))
test_that("enabler 2 bb + dark ritual + ub", 
          {expect_true(is_yuriko_on_T2(hand))})
