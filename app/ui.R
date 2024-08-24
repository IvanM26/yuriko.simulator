shiny::tagList(
  shiny::tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  shinyjs::useShinyjs(),
  bslib::page_navbar(
    title = "Yuriko Simulator",
    fillable = c("Your Decklist"),
    bslib::nav_panel(
      title = "Your Decklist",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 520,
          title = "Upload Decklist",
          shiny::selectInput(
            inputId = "decklist_source",
            label = "Select Source",
            choices = c(
              # "Moxfield URL" = "moxfield_url", # Doesn't work in shinylive
              "File" = "file"
            )
          ),
          shiny::div(
            shiny::conditionalPanel(
              condition = "input.decklist_source === 'moxfield_url'",
              shiny::textInput(
                inputId = "user_moxfield_url",
                label = "Paste Moxfield URL",
                placeholder = "Expected format: https://www.moxfield.com/decks/DECK_ID",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              condition = "input.decklist_source === 'file'",
              shiny::fileInput(
                inputId = "user_file",
                label = "Select File",
                width = "100%"
              )
            )
          ),
          shiny::actionButton(
            inputId = "upload_decklist",
            label = "Upload"
          )
        ),
        bslib::navset_card_tab(
          bslib::nav_panel(
            title = "Decklist Stats",
          ),
          bslib::nav_panel(
            title = "Simulate",
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                width = 400,
                shiny::numericInput(
                  inputId = "n_sim",
                  label = "Number of Hands to Draw",
                  min = 1000,
                  value = 1000,
                  step = 1
                ),
                shiny::selectizeInput(
                  inputId = "fixed_cards",
                  label = bslib::tooltip(
                    trigger = list(
                      "Select Fixed Card(s) for All Hands",
                      bsicons::bs_icon("info-circle")
                    ),
                    "Selected card(s) will be present in all simulated hands drawn"
                  ),
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 7
                  )
                ) |> 
                  shiny::tagAppendAttributes(class = "fixed-cards"),
                shiny::actionButton(
                  inputId = "simulate_from_decklist",
                  label = "Run Simulation"
                )
              ),
              bslib::card(
                bslib::card_header("Simulation Results"),
                shiny::uiOutput("simulation_summary_from_decklist")
              )
            )
          ),
          bslib::nav_panel(
            title = "Test Hand",
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                width = 400,
                shiny::actionButton(
                  inputId = "random_hand",
                  label = "Draw Random Hand"
                ),
                "Cards in Hand:",
                shiny::selectInput(inputId = "card1", label = NULL, choices = NULL),
                shiny::selectInput(inputId = "card2", label = NULL, choices = NULL),
                shiny::selectInput(inputId = "card3", label = NULL, choices = NULL),
                shiny::selectInput(inputId = "card4", label = NULL, choices = NULL),
                shiny::selectInput(inputId = "card5", label = NULL, choices = NULL),
                shiny::selectInput(inputId = "card6", label = NULL, choices = NULL),
                shiny::selectInput(inputId = "card7", label = NULL, choices = NULL)
              ),
              bslib::card(
                bslib::card_header("Your Hand"),
                bslib::layout_columns(
                  col_widths = c(
                    -2, 2, 2, 2, 2, -2, # 4 cards in row one
                    -3, 2, 2, 2, -3   # 3 cards in row two
                  ),
                  shiny::uiOutput("image1", inline = TRUE),
                  shiny::uiOutput("image2", inline = TRUE),
                  shiny::uiOutput("image3", inline = TRUE),
                  shiny::uiOutput("image4", inline = TRUE),
                  shiny::uiOutput("image5", inline = TRUE),
                  shiny::uiOutput("image6", inline = TRUE),
                  shiny::uiOutput("image7", inline = TRUE)
                )
              ),
              shiny::uiOutput("test_hand_output")
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Group Counts",
      bslib::card(
        bslib::card_header("Decklist Composition Summary"),
        bslib::layout_columns(
          bslib::value_box(
            title = "Cards in Deck",
            value = shiny::textOutput(outputId = "box_value_n_cards_in_deck")
          ),
          bslib::value_box(
            id = "box_n_missing_excess",
            title = shiny::textOutput(outputId = "box_title_missing_excess"),
            value = shiny::textOutput(outputId = "box_value_n_missing_excess"),
            class = "text-light"
          ),
          bslib::value_box(
            title = shiny::span(
              "Enablers",
              bslib::tooltip(
                trigger = bsicons::bs_icon("info-circle"),
                shiny::htmlOutput(outputId = "box_title_n_enablers_tooltip"),
                options = list(customClass = "left-and-full-width")
              )
            ),
            value = shiny::textOutput(outputId = "box_value_n_enablers")
          ),
          bslib::value_box(
            title = shiny::span(
              "Lands",
              bslib::tooltip(
                trigger = bsicons::bs_icon("info-circle"),
                shiny::htmlOutput(outputId = "box_title_n_lands_tooltip"),
                options = list(customClass = "left-and-full-width")
              )
            ),
            value = shiny::textOutput(outputId = "box_value_n_lands"),
            shiny::span(shiny::em("(", shiny::textOutput(outputId = "box_value_n_lands_including_mdfc", inline = TRUE), "including MDFC )"))
          ),
          bslib::value_box(
            title = shiny::htmlOutput(outputId = "box_title_fast_mana"),
            value = shiny::textOutput(outputId = "box_value_n_fast_mana")
          )
        )
      ),
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = "Allocate Cards",
          bslib::card(
            bslib::card_header(
              bslib::tooltip(
                trigger = list(
                  "Non-MDFC Enablers",
                  bsicons::bs_icon("info-circle")
                ),
                "An Enabler is a Creature you cast on Turn 1 and then use to Ninjutsu Yuriko onto the battlefield on Turn 2. MDFC Enablers (such as Boggart Trawler) are allocated in a different section."
              )
            ),
            bslib::layout_columns(
              custom_autonumericInput(
                inputId = "enablers_0",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {0}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Ornithopter"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_c",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {1} or {X}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Universal Automaton; Stonecoil Serpent"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_u",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {U} or {X}{U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  shiny::HTML("e.g. Mothdust Changeling;<br>Ingenious Prodigy")
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_b",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Changeling Outcast"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(3, 3, 3, -3),
              custom_autonumericInput(
                inputId = "enablers_cc",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {2}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Metallic Mimic"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_1u",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {1}{U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Moon-Circuit Hacker"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_1b",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {1}{B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Inkrise Infiltrator"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(3, 3, 3, -3),
              custom_autonumericInput(
                inputId = "enablers_uu",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {U}{U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Thassa's Oracle"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_bb",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {B}{B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Nether Traitor"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_ub",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {U}{B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Satoru, the Infiltrator"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(3, 3, -3, -3),
              custom_autonumericInput(
                inputId = "enablers_2u",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {2}{U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Mist-Syndicate Naga"
                )
              ),
              custom_autonumericInput(
                inputId = "enablers_2b",
                label = bslib::tooltip(
                  trigger = list(
                    "Mana Cost {2}{B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Opposition Agent"
                )
              )
            )
          ),
          bslib::layout_columns(
            bslib::card(
              bslib::card_header("Lands"),
              custom_autonumericInput(
                inputId = "lands_c",
                label = bslib::tooltip(
                  trigger = list(
                    "Only Produce {C}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Mutavault"
                )
              ),
              custom_autonumericInput(
                inputId = "lands_u",
                label = bslib::tooltip(
                  trigger = list(
                    "Only Produce {U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  shiny::HTML("e.g. Island;<br>Otawara, the Soaring City")
                )
              ),
              custom_autonumericInput(
                inputId = "lands_b",
                label = bslib::tooltip(
                  trigger = list(
                    "Only Produce {B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  shiny::HTML("e.g. Swamp;<br>Takenuma, Abandoned Mire"),
                  options = list(customClass = "full-width")
                )
              ),
              custom_autonumericInput(
                inputId = "lands_ub",
                label = bslib::tooltip(
                  trigger = list(
                    "Produce {U} or {B} (or Fetchlands)",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Underground Sea; Polluted Delta"
                )
              )
            ),
            bslib::card(
              bslib::card_header("MDFC Cards"),
              custom_autonumericInput(
                inputId = "mdfc_enablers_2u",
                label = bslib::tooltip(
                  trigger = list(
                    "MDFC Enabler with Mana Cost {2}{U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Hydroelectric Specimen"
                )
              ),
              custom_autonumericInput(
                inputId = "mdfc_enablers_2b",
                label = bslib::tooltip(
                  trigger = list(
                    "MDFC Enabler with Mana Cost {2}{B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Boggart Trawler"
                )
              ),
              custom_autonumericInput(
                inputId = "mdfc_lands_b",
                label = bslib::tooltip(
                  trigger = list(
                    "Non-Enabler MDFC that Only Produce {B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Agadeem's Awakening"
                )
              ),
              custom_autonumericInput(
                inputId = "mdfc_lands_u",
                label = bslib::tooltip(
                  trigger = list(
                    "Non-Enabler MDFC that Only Produce {U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Sea Gate Restoration"
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                bslib::tooltip(
                  trigger = list(
                    "Fast Mana",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Select the cards that you want to include in the deck."
                )
              ),
              shiny::checkboxInput(
                inputId = "include_dark_ritual",
                label = "Dark Ritual"
              ),
              shiny::checkboxInput(
                inputId = "include_chrome_mox",
                label = "Chrome Mox"
              ),
              shiny::checkboxInput(
                inputId = "include_lotus_petal",
                label = "Lotus Petal"
              ),
              shiny::checkboxInput(
                inputId = "include_mana_crypt",
                label = "Mana Crypt"
              ),
              shiny::checkboxInput(
                inputId = "include_mox_diamond",
                label = "Mox Diamond"
              )
            ),
            bslib::card(
              bslib::card_header(
                bslib::tooltip(
                  trigger = list(
                    "Other Cards",
                    bsicons::bs_icon("info-circle")
                  ),
                  "Allocate missing cards based on their color(s). This will only impact the simulation when Chrome Mox is in the deck."
                )
              ),
              custom_autonumericInput(
                inputId = "others_c",
                label = bslib::tooltip(
                  trigger = list(
                    "Colorless",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Cursed Totem"
                )
              ),
              custom_autonumericInput(
                inputId = "others_u",
                label = bslib::tooltip(
                  trigger = list(
                    "Mono {U}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Force of Will"
                )
              ),
              custom_autonumericInput(
                inputId = "others_b",
                label = bslib::tooltip(
                  trigger = list(
                    "Mono {B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Snuff Out"
                )
              ),
              custom_autonumericInput(
                inputId = "others_ub",
                label = bslib::tooltip(
                  trigger = list(
                    "{U}{B}",
                    bsicons::bs_icon("info-circle")
                  ),
                  "e.g. Fallen Shinobi; Consign//Oblivion"
                )
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Simulate",
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              width = 400,
              shiny::numericInput(
                inputId = "n_sim_from_counts",
                label = "Number of Hands to Draw",
                min = 1000,
                value = 1000,
                step = 1
              ),
              shiny::actionButton(
                inputId = "simulate_from_counts",
                label = "Run Simulation"
              )
            ),
            bslib::card(
              bslib::card_header("Simulation Results"),
              shiny::uiOutput("simulation_summary_from_counts")
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "About"
    )
  )
)
