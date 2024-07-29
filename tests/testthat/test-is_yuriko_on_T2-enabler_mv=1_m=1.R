# ENABLER MV 1 ####
# > COLORLESS ENABLER ####

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Otawara, Soaring City", "Mox Diamond"))
test_that("enabler 1 colorless + 2 lands that produce any color and mox diamond", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Otawara, Soaring City", "Lotus Petal"))
test_that("enabler 1 colorless + 2 lands that produce any color and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Swamp"))
test_that("enabler 1 colorless + 2 lands that produce ub", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Otawara, Soaring City", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 2 lands that produce u and chrome mox on b",
          { expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Otawara, Soaring City", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + 2 lands that produce u and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Takenuma, Abandoned Mire", "Chrome Mox", "Force of Will"))
test_that("enabler 1 colorless + 2 lands that produce b and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Takenuma, Abandoned Mire", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 colorless + 2 lands that produce b and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Gemstone Caverns", "Mox Diamond"))
test_that("enabler 1 colorless + 2 lands where only one produce color and mox diamond", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Gemstone Caverns", "Lotus Petal"))
test_that("enabler 1 colorless + 2 lands where only one produce color and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Underground Sea", "Gemstone Caverns", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 2 lands where one produce colorless and the other ub and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Gemstone Caverns", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 2 lands where one produce colorless and the other u and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + 2 lands where one produce colorless and the other u and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Gemstone Caverns", "Chrome Mox", "Force of Will"))
test_that("enabler 1 colorless + 2 lands where one produce colorless and the other b and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Gemstone Caverns", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 colorless + 2 lands where one produce colorless and the other b and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Mutavault", "Gemstone Caverns", "Mox Diamond", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 2 lands where both produce colorless and mox diamond and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Mutavault", "Gemstone Caverns", "Sink into Stupor // Soporific Springs", "Mox Diamond"))
test_that("enabler 1 colorless + 2 lands where both produce colorless and mox diamond and mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Mutavault", "Gemstone Caverns", "Sink into Stupor // Soporific Springs", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 2 lands where both produce colorless and mdfc u land and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Mutavault", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt", "Chrome Mox", "Force of Will"))
test_that("enabler 1 colorless + 2 lands where both produce colorless and mdfc b land and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Mutavault", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt", "Lotus Petal"))
test_that("enabler 1 colorless + 2 lands where both produce colorless and mdfc land and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Mutavault", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 colorless + 2 lands where both produce colorless and mdfc u land and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Lotus Petal"))
test_that("enabler 1 colorless + 1 land that produce any color and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Underground Sea", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + 1 land that produce ub and mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Chrome Mox", "Force of Will"))
test_that("enabler 1 colorless + 1 land that produce b and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 colorless + 1 land that produce b and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Swamp", "Mox Diamond", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + 1 land that produce b and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 1 land that produce u and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + 1 land that produce u and mdfc b land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Island", "Mox Diamond", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 colorless + 1 land that produce u and mdfc u land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 colorless + 1 land that produce colorless and mdfc u and mdfc b lands", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Mox Diamond", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 1 land that produce colorless and mox diamond and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Mox Diamond", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + 1 land that produce colorless and mox diamond and any mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Sink into Stupor // Soporific Springs", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + 1 land that produce colorless and mdfc u land and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt", "Chrome Mox", "Force of Will"))
test_that("enabler 1 colorless + 1 land that produce colorless and mdfc b land and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Agadeem's Awakening // Agadeem, the Undercrypt", "Lotus Petal"))
test_that("enabler 1 colorless + 1 land that produce colorless and any mdfc land and lotus petal", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Lotus Petal", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + no lands and lotus petal and chrome mox on any color", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Lotus Petal", "Agadeem's Awakening // Agadeem, the Undercrypt"))
test_that("enabler 1 colorless + no lands and lotus petal and any mdfc land", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Sink into Stupor // Soporific Springs", "Chrome Mox", "Snuff Out"))
test_that("enabler 1 colorless + no lands and mdfc u land and chrome mox on b", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Agadeem's Awakening // Agadeem, the Undercrypt", "Chrome Mox", "Force of Will"))
test_that("enabler 1 colorless + no lands and mdfc b land and chrome mox on u", 
          {expect_true(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Gemstone Caverns", "Sink into Stupor // Soporific Springs", "Chrome Mox", "Force of Will"))
test_that("no ub on t2", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(testdata, c("Universal Automaton", "Agadeem's Awakening // Agadeem, the Undercrypt", "Sink into Stupor // Soporific Springs"))
test_that("enabler 1 + no lands but mdfc b and mdfc u", 
          {expect_true(is_yuriko_on_T2(hand))})
