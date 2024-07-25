#' @export
run_simulation <- function(decklist, fixed_cards = NULL, seed = 2611, n_sim) {
  # Set seet for reproducibility
  set.seed(seed)
  
  # Initialize count of yuriko triggers on turn two
  n_yuriko_triggers_on_turn_two <- 0
  
  # Initialize progress bar
  cli::cli_progress_bar("Running Simulation...", total = n_sim)
  
  # Simulate...
  for (i in 1:n_sim) {
    
    hand <- draw_seven(decklist, fixed_cards)
    
    if (is_yuriko_on_T2(hand)) n_yuriko_triggers_on_turn_two = n_yuriko_triggers_on_turn_two + 1
    
    cli::cli_progress_update()
    
  }
  
  cli::cli_progress_done()
  
  prob_yuriko_triggers_on_turn_two <- n_yuriko_triggers_on_turn_two / n_sim
  
  return(prob_yuriko_triggers_on_turn_two)
  
}
