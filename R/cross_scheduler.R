#' schedule your SGA crosses
#' @import readr
#' @import tidyr
#' @import dplyr
#' @export
#'
#' @param duration path to a csv file specifying, on each row: each step
#' of the crossing procedure and the number of days between one given step
#' and the first liquid culture launching step.
#' @param crosses path to a txt file (tab delimited) specifying the id
#' (has to be unique) of the cross, the queries in the cross (separated by commas)
#' , and the date (YYYY-MM-DD) at which liquid cultures were launched before
#' making lawns.

cross_scheduler <- function (duration, crosses, write = T) {

  duration <- read_csv("duration.csv")

  crosses <- read_delim("crosses.txt", delim = "\t")

  step <- duration %>%
    select(selection_step) %>%
    t() %>%
    c()

  time <- duration %>%
    select(duration) %>%
    t() %>%
    c()

  id <- crosses %>%
    select(cross_id)


  schedule <- crosses %>%
    cbind(
      matrix( time,
              ncol = step %>% length(),
              nrow = id %>% length(),
              dimnames = list(NULL,step)
      ) %>%
        as.data.frame()
    )

  schedule <-
    schedule %>%
    mutate_each(funs( date = as.Date(. + as.Date(date_culture))), Lawns:Screen) %>%
    #mutate_each(funs( ifelse(as.Date(.) == Sys.Date() | as.Date(.) == Sys.Date() + 1, as.character(.), NA )), Lawns:Screen) %>%
    mutate_each( funs( as.Date(.)) , Lawns:Screen) %>%
    gather(key = procedure, value = date, date_culture:Screen, -cross_id, -queries, -date_culture ) %>%
    filter( (date == Sys.Date()) | (date == Sys.Date() + 1))

}

