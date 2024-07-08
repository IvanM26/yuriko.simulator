filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath)

# T1 YURIKO ####
hand <- get_hand(decklist, c("Mana Crypt", "Island1", "Chrome Mox", "Snuff Out"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Swamp1", "Chrome Mox", "Snuff Out"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Mutavault", "Chrome Mox", "Snuff Out"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Scalding Tarn", "Chrome Mox", "Gingerbrute"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Scalding Tarn", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Swamp1", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Island1", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Mutavault", "Chrome Mox", "Snuff Out", "Mox Diamond"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Boggart Trawler", "Mana Crypt", "Island1", "Chrome Mox"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Boggart Trawler", "Mana Crypt", "Island1", "Mox Diamond"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Lotus Petal", "Chrome Mox", "Boggart Trawler"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Lotus Petal", "Chrome Mox", "Sink into Stupor"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Boggart Trawler", "Island"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Boggart Trawler", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Island1", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Sink into Stupor", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Mutavault", "Lotus Petal"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Mutavault", "Lotus Petal", "Mox Diamond"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Chrome Mox", "Lotus Petal", "Cursed Totem"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Chrome Mox", "Lotus Petal", "Force of Will"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Chrome Mox", "Lotus Petal", "Snuff Out"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Chrome Mox", "Lotus Petal", "Sink into Stupor"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Lotus Petal"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Dark Ritual", "Chrome Mox"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Dark Ritual", "Chrome Mox", "Swamp1"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Mana Crypt", "Island1", "Chrome Mox", "Swamp1"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Dark Ritual", "Chrome Mox", "Agadeem's Awakening", "Sink into Stupor"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Lotus Petal", "Chrome Mox", "Agadeem's Awakening", "Sink into Stupor"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})
