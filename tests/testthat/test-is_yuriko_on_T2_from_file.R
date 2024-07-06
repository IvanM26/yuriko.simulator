filepath_decklist <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")
filepath_tests <- system.file("extdata", "test_keep.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath_decklist)

tests <- readxl::read_xlsx(filepath_tests)

for (i in 1:nrow(tests)){
  card_list <- c(tests$card1[[i]],tests$card2[[i]],tests$card3[[i]],
                 tests$card4[[i]],tests$card5[[i]],tests$card6[[i]],
                 tests$card7[[i]])
  
  hand <- get_hand(decklist, card_list)
  
  if (tests$keep[[i]]){
    test_that(paste(tests$id[[i]], "should keep hand:\n", paste(card_list, collapse = "\n")), {
      expect_true(is_yuriko_on_T2(hand))
    }) 
  } else {
    test_that(paste(tests$id[[i]],"should mulligan:\n", paste(card_list, collapse = "\n")), {
      expect_false(is_yuriko_on_T2(hand))
    }) 
  }
}
