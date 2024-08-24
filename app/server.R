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
  
  n_enablers_mv_1 <- shiny::reactive({
    input$enablers_c + 
      input$enablers_u + 
      input$enablers_b
  })
  
  n_enablers_mv_2 <- shiny::reactive({
    input$enablers_cc +
      input$enablers_1u +
      input$enablers_1b +
      input$enablers_uu +
      input$enablers_bb +
      input$enablers_ub    
  })
  
  n_enablers_mv_3 <- shiny::reactive({
    input$enablers_2u +
      input$enablers_2b 
  })
  
  n_mdfc_enablers_mv_3 <- shiny::reactive({
    input$mdfc_enablers_2u +
      input$mdfc_enablers_2b
  })
  
  n_enablers <- shiny::reactive({
    input$enablers_0 +
      n_enablers_mv_1() +
      n_enablers_mv_2() +
      n_enablers_mv_3() +
      n_mdfc_enablers_mv_3()
  })
  
  n_lands <- shiny::reactive({
    input$lands_c +
      input$lands_u +
      input$lands_b +
      input$lands_ub
  })
  
  # Don't include mdfc enablers
  n_mdfc_lands <- shiny::reactive({
    input$mdfc_lands_b +
      input$mdfc_lands_u
  })

  n_others <- shiny::reactive({
    input$others_c +
      input$others_u +
      input$others_b +
      input$others_ub
  })
  
  n_fast_mana <- shiny::reactive({
    input$include_dark_ritual +
      input$include_chrome_mox +
      input$include_lotus_petal +
      input$include_mana_crypt +
      input$include_mox_diamond
  })
  
  n_cards_in_deck <- shiny::reactive({
    n_enablers() +
      n_lands() +
      n_mdfc_lands() +
      n_others() +
      n_fast_mana()
  })

  output$box_value_n_cards_in_deck <- shiny::renderText({
    n_cards_in_deck()
  })
  
  n_missing_excess <- shiny::reactive({
    99 - n_cards_in_deck()
  })
  
  output$box_title_missing_excess <- shiny::renderText({
    if (n_missing_excess() >= 0) {
      "Missing Cards"
    } else {
      "Excess Cards"
    }
  })
  
  output$box_value_n_missing_excess <- shiny::renderText({
    abs(n_missing_excess())
  })
  
  shiny::observeEvent(n_missing_excess(), {
    if (n_missing_excess() == 0) {
      shinyjs::removeClass(id = "box_n_missing_excess", class = "bg-danger")
      shinyjs::addClass(id = "box_n_missing_excess",class = "bg-success")
    } else if (n_missing_excess() != 0){
      shinyjs::removeClass(id = "box_n_missing_excess", class = "bg-success")
      shinyjs::addClass(id = "box_n_missing_excess",class = "bg-danger")
    }
  })
  
  output$box_value_n_enablers <- shiny::renderText({
    n_enablers()
  })
  
  output$box_title_n_enablers_tooltip <- shiny::renderUI({
    shiny::tagList(
      shiny::p("Distributed as follows:"),
      shiny::tags$ul(
        shiny::tags$li(glue::glue("MV 0: { input$enablers_0 }")),
        shiny::tags$li(glue::glue("MV 1: { n_enablers_mv_1() }")),
        shiny::tags$li(glue::glue("MV 2: { n_enablers_mv_2() }")),
        shiny::tags$li(glue::glue("MV 3: { n_enablers_mv_3() + n_mdfc_enablers_mv_3() }"))
      ),
      shiny::p("MDFC Enablers are included.")
    )
  })
  
  output$box_value_n_lands <- shiny::renderText({
    n_lands()
  })
  
  output$box_title_n_lands_tooltip <- shiny::renderUI({
    shiny::tagList(
      shiny::p("Distributed as follows:"),
      shiny::tags$ul(
        shiny::tags$li(glue::glue("Only Produce C: { input$lands_c }")),
        shiny::tags$li(glue::glue("Only Produce U: { input$lands_u } ({ input$lands_u + input$mdfc_lands_u + input$mdfc_enablers_2u } including MDFC)")),
        shiny::tags$li(glue::glue("Only Produce B: { input$lands_b } ({ input$lands_b + input$mdfc_lands_b + input$mdfc_enablers_2b } including MDFC)")),
        shiny::tags$li(glue::glue("Produce UB: { input$lands_ub }. Fetchlands are included here.")),
      )
    )
  })
  
  output$box_value_n_lands_including_mdfc <- shiny::renderText({
    n_lands() +
      n_mdfc_lands() +
      n_mdfc_enablers_mv_3()
  })
  
  output$box_title_fast_mana <- shiny::renderUI({
    
    if (n_fast_mana() == 0) {
      "Fast Mana"
    } else {
      
      shiny::span(
        "Fast Mana",
        bslib::tooltip(
          trigger = bsicons::bs_icon("info-circle"),
          shiny::tagList(
            shiny::p("Selected cards:"),
            shiny::tags$ul(
              if (input$include_dark_ritual) shiny::tags$li("Dark Ritual"),
              if (input$include_chrome_mox) shiny::tags$li("Chrome Mox"),
              if (input$include_lotus_petal) shiny::tags$li("Lotus Petal"),
              if (input$include_mana_crypt) shiny::tags$li("Mana Crypt"),
              if (input$include_mox_diamond) shiny::tags$li("Mox Diamond")
            )
          ),
          options = list(customClass = "left-and-full-width")
        )
      )
    }
  })
  
  output$box_value_n_fast_mana <- shiny::renderText({
    n_fast_mana()
  })
  
  prob_from_counts <- shiny::eventReactive(input$simulate_from_counts, {
    decklist <- decklist_from_counts(
      list(
        "enabler_0"       = input$enablers_0,
        "enabler_b"       = input$enablers_b,
        "enabler_u"       = input$enablers_u,
        "enabler_c"       = input$enablers_c,
        "enabler_bb"      = input$enablers_bb,
        "enabler_uu"      = input$enablers_uu,
        "enabler_ub"      = input$enablers_ub,
        "enabler_1b"      = input$enablers_1b,
        "enabler_1u"      = input$enablers_1u,
        "enabler_2b"      = input$enablers_2b,
        "enabler_2u"      = input$enablers_2u,
        "mdfc_enabler_2b" = input$mdfc_enablers_2b,
        "mdfc_enabler_2u" = input$mdfc_enablers_2u,
        "land_b"          = input$lands_b,
        "land_u"          = input$lands_u,
        "land_c"          = input$lands_c,
        "land_ub"         = input$lands_ub,
        "mdfc_land_b"     = input$mdfc_lands_b,
        "mdfc_land_u"     = input$mdfc_lands_u,
        "other_b"         = input$others_b,
        "other_u"         = input$others_u,
        "other_c"         = input$others_c,
        "other_ub"        = input$others_ub,
        "dark_ritual"     = input$include_dark_ritual,
        "chrome_mox"      = input$include_chrome_mox,
        "lotus_petal"     = input$include_lotus_petal,
        "mana_crypt"      = input$include_mana_crypt,
        "mox_diamond"     = input$include_mox_diamond
      )
    )
    
    run_simulation(decklist, n_sim = input$n_sim_from_counts)
  })
  
  simulation_summary_from_counts_html <- shiny::eventReactive(prob_from_counts(), {
    
    prob_first_two_hands <- 1 - (1 - prob_from_counts()) ^ 2
    prob_first_three_hands <- 1 - (1 - prob_from_counts()) ^ 3
    prob_first_four_hands <- 1 - (1 - prob_from_counts()) ^ 4
    
    shiny::div(
      shiny::tags$ul(
        shiny::tags$li(shiny::HTML(glue::glue("{ shiny::strong( scales::label_comma()(input$n_sim_from_counts) ) } random 7-card hands were drawn from { shiny::strong('Custom Decklist') }"))),
        shiny::tags$li(shiny::HTML(glue::glue("{ bold_percentage(prob_from_counts()) } of the hands drawn had enough cards to trigger Yuriko on turn two")))
      ),
      
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
      
      
    )
    
  })
  
  output$simulation_summary_from_counts <- shiny::renderUI({
    simulation_summary_from_counts_html()
  })

}