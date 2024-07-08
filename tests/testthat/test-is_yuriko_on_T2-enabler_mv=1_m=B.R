filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath)

# ENABLER MV 1 ####
# > BLACK ENABLER ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Island2","Mox Diamond"))
test_that("enabler 1 b + 2 lands that produce any color and mox diamond", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Island2","Lotus Petal"))
test_that("no b t1", 
          {expect_false(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Swamp1"))
test_that("enabler 1 b + 2 lands that produce ub", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Island2", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 2 lands that produce u and chrome mox on b", {
  expect_true(is_yuriko_on_T2(hand))
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Island2", "Agadeem's Awakening"))
test_that("enabler 1 b + 2 lands that produce u and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1","Swamp2", "Chrome Mox", "Force of Will"))
test_that("enabler 1 b + 2 lands that produce b and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1","Swamp2", "Sea Gate Restoration"))
test_that("enabler 1 b + 2 lands that produce b and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Gemstone Caverns", "Mox Diamond"))
test_that("enabler 1 b + 2 lands where only one produce color and mox diamond", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Gemstone Caverns", "Lotus Petal"))
test_that("no b t1", 
          {expect_false(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Underground Sea","Gemstone Caverns", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 2 lands where one produce colorless and the other ub and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Gemstone Caverns", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 2 lands where one produce colorless and the other u and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Gemstone Caverns", "Agadeem's Awakening"))
test_that("enabler 1 b + 2 lands where one produce colorless and the other u and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1","Gemstone Caverns", "Chrome Mox", "Force of Will"))
test_that("enabler 1 b + 2 lands where one produce colorless and the other b and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1","Gemstone Caverns", "Sea Gate Restoration"))
test_that("enabler 1 b + 2 lands where one produce colorless and the other b and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Mutavault","Gemstone Caverns", "Mox Diamond", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 2 lands where both produce colorless and mox diamond and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Mutavault","Gemstone Caverns", "Sea Gate Restoration", "Mox Diamond"))
test_that("enabler 1 b + 2 lands where both produce colorless and mox diamond and mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Mutavault","Gemstone Caverns", "Sea Gate Restoration", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 2 lands where both produce colorless and mdfc u land and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Mutavault","Gemstone Caverns", "Agadeem's Awakening", "Chrome Mox", "Force of Will"))
test_that("enabler 1 b + 2 lands where both produce colorless and mdfc b land and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Mutavault","Gemstone Caverns", "Agadeem's Awakening", "Sea Gate Restoration"))
test_that("enabler 1 b + 2 lands where both produce colorless and mdfc u land and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Lotus Petal"))
test_that("no b t1", 
          {expect_false(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Underground Sea","Agadeem's Awakening"))
test_that("enabler 1 b + 1 land that produce ub and mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1", "Chrome Mox", "Force of Will"))
test_that("enabler 1 b + 1 land that produce b and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1", "Sea Gate Restoration"))
test_that("enabler 1 b + 1 land that produce b and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1", "Mox Diamond", "Agadeem's Awakening"))
test_that("enabler 1 b + 1 land that produce b and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 1 land that produce u and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1", "Agadeem's Awakening"))
test_that("enabler 1 b + 1 land that produce u and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1", "Mox Diamond", "Sea Gate Restoration"))
test_that("enabler 1 b + 1 land that produce u and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Gemstone Caverns", "Agadeem's Awakening", "Sea Gate Restoration"))
test_that("enabler 1 b + 1 land that produce colorless and mdfc u and mdfc b lands", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Gemstone Caverns", "Mox Diamond", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 1 land that produce colorless and mox diamond and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Gemstone Caverns", "Mox Diamond", "Agadeem's Awakening"))
test_that("enabler 1 b + 1 land that produce colorless and mox diamond and any mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Gemstone Caverns", "Sea Gate Restoration", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + 1 land that produce colorless and mdfc u land and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Gemstone Caverns", "Agadeem's Awakening", "Chrome Mox", "Force of Will"))
test_that("enabler 1 b + 1 land that produce colorless and mdfc b land and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Lotus Petal", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + no lands and lotus petal and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Sea Gate Restoration", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 b + no lands and mdfc u land and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Agadeem's Awakening", "Chrome Mox", "Force of Will"))
test_that("enabler 1 b + no lands and mdfc b land and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Island1","Island2", "Chrome Mox", "Force of Will"))
test_that("no card to pitch b", {
  expect_false(is_yuriko_on_T2(hand))
})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Mutavault","Gemstone Caverns", "Agadeem's Awakening", "Lotus Petal"))
test_that("enabler 1 b + mdfc b land and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1","Lotus Petal"))
test_that("enabler 1 b + land b and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Swamp1", "Chrome Mox", "Snuff Out"))
test_that("no u", 
          {expect_false(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Lotus Petal", "Agadeem's Awakening"))
test_that("enabler 1 b + no lands and mdfc b land and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Gemstone Caverns", "Agadeem's Awakening", "Lotus Petal"))
test_that("enabler 1 b + no color lands and mdfc b land and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
hand <- get_hand(decklist, c("Changeling Outcast","Lotus Petal", "Agadeem's Awakening", "Swamp1", "Swamp2"))
test_that("enabler 1 b + b lands and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})