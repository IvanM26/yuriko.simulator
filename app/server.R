function(input, output, session) {

  w <- waiter::Waiter$new()
  
  decklist <- shiny::eventReactive(input$upload_decklist, {
    w$show()
    out <- process_card_data(input$upload_decklist$datapath, use_httr = FALSE, waiter = w)
    waiter::waiter_hide()
    out
  })
  
  output$table_decklist <- reactable::renderReactable({
    decklist() |> 
      dplyr::select(card_name) |> 
      reactable::reactable()
  })
  
  shiny::observeEvent(decklist(), {
    shiny::updateSelectInput(session = session, inputId = "card1", choices = decklist()$card_name, selected = decklist()$card_name[[1]])
    shiny::updateSelectInput(session = session, inputId = "card2", choices = decklist()$card_name, selected = decklist()$card_name[[2]])
    shiny::updateSelectInput(session = session, inputId = "card3", choices = decklist()$card_name, selected = decklist()$card_name[[3]])
    shiny::updateSelectInput(session = session, inputId = "card4", choices = decklist()$card_name, selected = decklist()$card_name[[4]])
    shiny::updateSelectInput(session = session, inputId = "card5", choices = decklist()$card_name, selected = decklist()$card_name[[5]])
    shiny::updateSelectInput(session = session, inputId = "card6", choices = decklist()$card_name, selected = decklist()$card_name[[6]])
    shiny::updateSelectInput(session = session, inputId = "card7", choices = decklist()$card_name, selected = decklist()$card_name[[7]])
  })
  
  shiny::observeEvent(input$random_hand, {
    random_hand <- draw_seven(decklist())
    shiny::updateSelectInput(session = session, inputId = "card1", selected = random_hand$card_name[[1]])
    shiny::updateSelectInput(session = session, inputId = "card3", selected = random_hand$card_name[[2]])
    shiny::updateSelectInput(session = session, inputId = "card4", selected = random_hand$card_name[[3]])
    shiny::updateSelectInput(session = session, inputId = "card2", selected = random_hand$card_name[[4]])
    shiny::updateSelectInput(session = session, inputId = "card5", selected = random_hand$card_name[[5]])
    shiny::updateSelectInput(session = session, inputId = "card6", selected = random_hand$card_name[[6]])
    shiny::updateSelectInput(session = session, inputId = "card7", selected = random_hand$card_name[[7]])
  })
  
  output$image1 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card1) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card1", crossorigin = "anonymous", height = 300)
  })

  output$image2 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card2) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card2", crossorigin = "anonymous", height = 300)
  })

  output$image3 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card3) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card3", crossorigin = "anonymous", height = 300)
  })

  output$image4 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card4) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card4", crossorigin = "anonymous", height = 300)
  })

  output$image5 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card5) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card5", crossorigin = "anonymous", height = 300)
  })

  output$image6 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card6) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card6", crossorigin = "anonymous", height = 300)
  })

  output$image7 <- shiny::renderUI({
    img_src <- decklist() |> dplyr::filter(card_name == input$card7) |> dplyr::pull(img_src)
    shiny::tags$img(src = img_src, alt = "card7", crossorigin = "anonymous", height = 300)
  })
  
  test_hand <- shiny::reactive({
    decklist() |> 
      dplyr::filter(card_name %in% c(input$card1, input$card2, input$card3, input$card4, input$card5, input$card6, input$card7))
  })
  
  output$test_hand_output <- shiny::renderUI({
    if (is_yuriko_on_T2(test_hand())) {
      shiny::div(
        class = "alert alert-success",
        "Yuriko triggers on turn two"
      )
    } else {
      shiny::div(
        class = "alert alert-danger",
        "Yuriko does not trigger on turn two"
      )
    }
  })
  
}