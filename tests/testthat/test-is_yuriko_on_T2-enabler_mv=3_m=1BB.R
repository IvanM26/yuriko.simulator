# ENABLER MV 3 ####
# > 1BB ENABLER ####
hand <- get_hand(testdata, c("Nashi, Moon Sage's Scion", "Dark Ritual", "Island", "Swamp"))
test_that("enabler 3 1bb + dark ritual + ub on t2", 
          {expect_true(is_yuriko_on_T2(hand))})
