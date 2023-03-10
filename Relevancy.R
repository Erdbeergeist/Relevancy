#' cumulative_gain
#'
#' @param relevancy_function A function returning a relevancy score object that implements the sum operation
#' @param documents A list of documents to be scored
#'
#' @return The cumulative gain
#' @export
cumulative_gain <- function(relevancy_function, documents){
  return(sum(map(relevancy_function, documents)))
}

#' discounted_cumulative_gain
#'
#' @param relevancy_function A function returning a relevancy score object that implements the sum operation
#' @param documents A list of documents to be scored 
#'
#' @return The discounted cumulative gain
#' @export
discounted_cumulative_gain <- function(relevancy_function, documents){
  return(map2(map(relevancy_function, documents), seq(1, length(documents)), ~.x/log2(.y + 1)))
}

#' discounted_cumulative_gain_alternate
#'
#' @param relevancy_function A function returning a relevancy score object that implements the sum operation
#' @param documents A list of documents to be scored 
#'
#' @return Alternative version of the discounted cumulative gain
#' @export
discounted_cumulative_gain_alternate <- function(relevancy_function, documents){
  return(map2(map(relevancy_function, documents), seq(1, length(documents)), ~(2^(.x) - 1)/log2(.y + 1)))
}