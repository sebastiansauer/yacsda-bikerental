

add_id <- function(d) {
  
  d %>% 
    mutate(id = 1:n()) %>% 
    select(id, contains("count"), everything())
}







correct_for_no_func <- function(d, var) {
  
  args <- as.character(match.call())

  stopifnot(last(args) %in% names(d))

  d %>%
    mutate({{var}} := case_when(
      func == "No" ~ 0,
      TRUE ~ {{var}})) %>% 
    select(id, contains("count"), contains("pred"), contains("func"), everything())

}


plot_results <- function(d) {
  
  d %>%
    filter(.metric == "rmse") %>% 
    ggdotplotstats(x = .estimate, y = id,
                   ylab = "workflow/model",
                   xlab = "performance (rmse)",
                   title = "Comparison of model performance")
}


