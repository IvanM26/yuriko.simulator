library(shiny)
library(dplyr)
library(readxl)
library(writexl)

filepath_decklist <- system.file("extdata", "decklist.xlsx", package = "yuriko.simulator")
filepath_tests <- system.file("extdata", "test_keep.xlsx", package = "yuriko.simulator")

decklist <- readxl::read_xlsx(filepath_decklist) %>% dplyr::filter(in_deck == 1)

ui <- fluidPage(
  actionButton("keep", "Keep"),
  actionButton("mull", "Mulligan"),
  fluidRow(
    column(1, imageOutput("card1")),
    column(1, imageOutput("card2")) ,
    column(1, imageOutput("card3")) ,
    column(1, imageOutput("card4")) ,
    column(1, imageOutput("card5")) ,
    column(1, imageOutput("card6")) ,
    column(1, imageOutput("card7")) 
  )
)

server <- function(input, output, session) {
  
  hand <- reactiveVal(draw_seven(decklist))
  
  tests <- reactiveFileReader(intervalMillis = 100,
                              session = NULL,
                              filePath = filepath_tests, 
                              readFunc = read_xlsx)

  output$card1 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[1]], ".png")),
         height = 300)
  }, deleteFile = FALSE)
  
  output$card2 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[2]], ".png")),
         height = 300)
  }, deleteFile = FALSE)
  
  output$card3 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[3]], ".png")),
         height = 300)
  }, deleteFile = FALSE)
  
  output$card4 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[4]], ".png")),
         height = 300)
  }, deleteFile = FALSE)
  
  output$card5 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[5]], ".png")),
         height = 300)
  }, deleteFile = FALSE)
  
  output$card6 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[6]], ".png")),
         height = 300)
  }, deleteFile = FALSE)
  
  output$card7 <-  renderImage({
    list(src = file.path("img", paste0(hand()$card_name[[7]], ".png")),
         height = 300)
  }, deleteFile = FALSE)

  observeEvent(input$keep, {
    new_row <- tibble(
      card1 = hand()$card_name[[1]],
      card2 = hand()$card_name[[2]],
      card3 = hand()$card_name[[3]],
      card4 = hand()$card_name[[4]],
      card5 = hand()$card_name[[5]],
      card6 = hand()$card_name[[6]],
      card7 = hand()$card_name[[7]],
      keep = "TRUE"
    )
    
    tests <- bind_rows(tests(), new_row)
    write_xlsx(tests, filepath_tests)
    
    hand(draw_seven(decklist))
  })
  
  observeEvent(input$mull, {
    new_row <- tibble(
      card1 = hand()$card_name[[1]],
      card2 = hand()$card_name[[2]],
      card3 = hand()$card_name[[3]],
      card4 = hand()$card_name[[4]],
      card5 = hand()$card_name[[5]],
      card6 = hand()$card_name[[6]],
      card7 = hand()$card_name[[7]],
      keep = "FALSE"
    )
    
    tests <- bind_rows(tests(), new_row)
    write_xlsx(tests, filepath_tests)
    
    hand(draw_seven(decklist))
  })
  
}

shinyApp(ui, server)
