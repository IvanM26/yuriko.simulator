## code to prepare `testdata` dataset goes here
cards_for_unit_test_url <- "https://api.moxfield.com/v2/decks/all/2NvCVuI39UWwBbF5bLJAsQ/download?exportId=b4d3f9a5-2f75-4f1f-ae7f-b8987607d4e4&arenaOnly=false"

testdata <- cards_for_unit_test_url |> 
  parse_txt_file(run_checks = FALSE) |>
  add_scryfall_data() |>
  add_custom_attributes()

usethis::use_data(testdata, overwrite = TRUE)
