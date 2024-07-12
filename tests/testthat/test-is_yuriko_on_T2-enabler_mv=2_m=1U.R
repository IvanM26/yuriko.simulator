filepath <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath)

# ENABLER MV 2 ####
hand <- get_hand(decklist, c("Moon-Circuit Hacker", "Chrome Mox", "Island", "Swamp"))
test_that("no enabler t1", 
          {expect_false(is_yuriko_on_T2(hand))})

hand <- get_hand(decklist, c("Moon-Circuit Hacker", "Chrome Mox", "Force of Will", "Island", "Swamp"))
test_that("enabler 2 1u + chrome mox on", 
          {expect_true(is_yuriko_on_T2(hand))})
