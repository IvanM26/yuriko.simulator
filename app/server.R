function(input, output, session) {

  # Remove delay to show progress bar
  options(cli.progress_show_after = 0)

  decklist <- shiny::eventReactive(input$upload_decklist, {
    
    input_source_path <- switch (input$decklist_source,
      moxfield_url = input$user_moxfield_url,
      file = input$user_file$datapath
    )
    
    process_card_data(
      decklist_source = input$decklist_source,
      source_path = input_source_path,
      use_httr = FALSE # Change to TRUE in DEV
    )

  })
  
  output$table_decklist <- reactable::renderReactable({
    decklist() |> 
      dplyr::select(card_name_scryfall) |> 
      reactable::reactable()
  })
  
  shiny::observeEvent(decklist(), {
    shiny::updateSelectInput(
      session = session,
      inputId = "fixed_cards",
      choices = decklist()$card_name_scryfall,
      selected = NULL
    )
  })

  prob_from_decklist <- shiny::eventReactive(input$simulate_from_decklist, {
    run_simulation(decklist(), fixed_cards = input$fixed_cards, n_sim = input$n_sim)
  })
  
  simulation_summary_from_decklist_html <- shiny::eventReactive(prob_from_decklist(), {

    prob_first_two_hands <- 1 - (1 - prob_from_decklist()) ^ 2
    prob_first_three_hands <- 1 - (1 - prob_from_decklist()) ^ 3
    prob_first_four_hands <- 1 - (1 - prob_from_decklist()) ^ 4
    
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(shiny::HTML(glue::glue("{ shiny::strong( scales::label_comma()(input$n_sim) ) } random 7-card hands were drawn from { shiny::strong('Your Decklist') }"))),
        if (!is.null(input$fixed_cards)) shiny::tags$li(shiny::HTML(glue::glue("The following cards were fixed to appear in all hands: { paste0(input$fixed_cards, collapse = ', ')}"))),
        shiny::tags$li(shiny::HTML(glue::glue("{ bold_percentage(prob_from_decklist()) } of the hands drawn had enough cards to trigger Yuriko on turn two")))
      ),
      if (is.null(input$fixed_cards)) {
        shiny::div(
          "Based on that value, it is expected that enough cards to trigger Yuriko on turn two will be present in:",
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML(glue::glue("one of the first two hands drawn in { bold_percentage(prob_first_two_hands) } of matches"))),
            shiny::tags$li(shiny::HTML(glue::glue("one of the first three hands drawn in { bold_percentage(prob_first_three_hands) }  of matches"))),
            shiny::tags$li(shiny::HTML(glue::glue("one of the first four hands drawn in { bold_percentage(prob_first_four_hands) }  of matches"))),
          ),
          "Then, assuming a \"free\" mulligan we can say that:",
          shiny::tags$ul(
            shiny::tags$li(shiny::HTML(glue::glue("in { bold_percentage(prob_first_two_hands) } of matches a { shiny::strong('7-card') } hand will have enough cards to trigger Yuriko on turn two (i.e. considering at most one mulligan)"))),
            shiny::tags$li(shiny::HTML(glue::glue("in { bold_percentage(prob_first_three_hands) } of matches a hand with { shiny::strong('at least 6 cards') } will have enough cards to trigger Yuriko on turn two (i.e. considering at most two mulligans)"))),
            shiny::tags$li(shiny::HTML(glue::glue("in { bold_percentage(prob_first_four_hands) } of matches a hand with { shiny::strong('at least 5 cards') } will have enough cards to trigger Yuriko on turn two (i.e. considering at most three mulligans)"))),
          )
        )
        
      }
    )

  })
  
  output$simulation_summary_from_decklist <- shiny::renderUI({
    simulation_summary_from_decklist_html()
  })

  shiny::observeEvent(decklist(), {
    shiny::updateSelectInput(session = session, inputId = "card1", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[1]])
    shiny::updateSelectInput(session = session, inputId = "card2", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[2]])
    shiny::updateSelectInput(session = session, inputId = "card3", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[3]])
    shiny::updateSelectInput(session = session, inputId = "card4", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[4]])
    shiny::updateSelectInput(session = session, inputId = "card5", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[5]])
    shiny::updateSelectInput(session = session, inputId = "card6", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[6]])
    shiny::updateSelectInput(session = session, inputId = "card7", choices = decklist()$card_name_scryfall, selected = decklist()$card_name_scryfall[[7]])
  })
  
  shiny::observeEvent(input$random_hand, {
    random_hand <- draw_seven(decklist())
    shiny::updateSelectInput(session = session, inputId = "card1", selected = random_hand$card_name_scryfall[[1]])
    shiny::updateSelectInput(session = session, inputId = "card3", selected = random_hand$card_name_scryfall[[2]])
    shiny::updateSelectInput(session = session, inputId = "card4", selected = random_hand$card_name_scryfall[[3]])
    shiny::updateSelectInput(session = session, inputId = "card2", selected = random_hand$card_name_scryfall[[4]])
    shiny::updateSelectInput(session = session, inputId = "card5", selected = random_hand$card_name_scryfall[[5]])
    shiny::updateSelectInput(session = session, inputId = "card6", selected = random_hand$card_name_scryfall[[6]])
    shiny::updateSelectInput(session = session, inputId = "card7", selected = random_hand$card_name_scryfall[[7]])
  })
  
  output$image1 <- render_card(decklist(), input, card_index = 1)
  output$image2 <- render_card(decklist(), input, card_index = 2)
  output$image3 <- render_card(decklist(), input, card_index = 3)
  output$image4 <- render_card(decklist(), input, card_index = 4)
  output$image5 <- render_card(decklist(), input, card_index = 5)
  output$image6 <- render_card(decklist(), input, card_index = 6)
  output$image7 <- render_card(decklist(), input, card_index = 7)
  
  test_hand <- shiny::reactive({
    decklist() |> 
      dplyr::filter(card_name_scryfall %in% c(input$card1, input$card2, input$card3, input$card4, input$card5, input$card6, input$card7))
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