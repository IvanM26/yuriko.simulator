# T1 YURIKO ####
hand <- get_hand(testdata, c("Mana Crypt", "Island", "Chrome Mox", "Snuff Out"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Swamp", "Chrome Mox", "Snuff Out"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Mutavault", "Chrome Mox", "Snuff Out"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Scalding Tarn", "Chrome Mox", "Universal Automaton"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Scalding Tarn", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Swamp", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Island", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Mutavault", "Chrome Mox", "Snuff Out", "Mox Diamond"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler // Boggart Bog", "Mana Crypt", "Island", "Chrome Mox"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Boggart Trawler // Boggart Bog", "Mana Crypt", "Island", "Mox Diamond"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Lotus Petal", "Chrome Mox", "Boggart Trawler // Boggart Bog"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Lotus Petal", "Chrome Mox", "Sink into Stupor // Soporific Springs"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Boggart Trawler // Boggart Bog", "Island"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Boggart Trawler // Boggart Bog", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Island", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Sink into Stupor // Soporific Springs", "Lotus Petal"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Mutavault", "Lotus Petal"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Mutavault", "Lotus Petal", "Mox Diamond"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Chrome Mox", "Lotus Petal", "Cursed Totem"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Chrome Mox", "Lotus Petal", "Force of Will"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Chrome Mox", "Lotus Petal", "Snuff Out"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Chrome Mox", "Lotus Petal", "Sink into Stupor // Soporific Springs"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Lotus Petal"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Dark Ritual", "Chrome Mox"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Dark Ritual", "Chrome Mox", "Swamp"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Mana Crypt", "Island", "Chrome Mox", "Swamp"))
test_that("no 1ub on t1",
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Dark Ritual", "Chrome Mox", "Agadeem's Awakening // Agadeem, the Undercrypt", "Sink into Stupor // Soporific Springs"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Lotus Petal", "Chrome Mox", "Agadeem's Awakening // Agadeem, the Undercrypt", "Sink into Stupor // Soporific Springs"))
test_that("at least 1ub on t1",
          {expect_true(is_yuriko_on_T2(hand))})
