## code to prepare `testdata` dataset goes here
testdata <- process_card_data(
  decklist_source = "moxfield_url",
  source_path = "https://www.moxfield.com/decks/2NvCVuI39UWwBbF5bLJAsQ",
  use_httr = TRUE
)

usethis::use_data(testdata, overwrite = TRUE)
