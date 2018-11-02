#' @name isolation_forest
#' @title Grow an isolation forest
#' @description Implements isolation forest  from Isolation-Based Anomaly
#'   Detection (<doi:10.1145/2133360.2133363>)
#' @details This function internally uses \code{\link[ranger]{ranger}}.
#'   Arguments may be passed to it except these: dependent.variable.name, data,
#'   mtry, min.node.size, splitrule, num.random.splits,
#'   respect.unordered.factors. Pass 'seed' argument for reproducibility
#' @param dataset Dataframe with column names
#' @param ... Optional arguments passed to \code{\link[ranger]{ranger}}
#' @return An object(list) of class 'solitude'. The isolationforest is in the
#'   'forest' slot
#' @examples
#' mo <- isolation_forest(iris, seed = 1)
#' @export
isolation_forest <- function(dataset, ...){

  columnNames  <- colnames(dataset)
  responseName <- columnNames[[1]]
  while(deparse(substitute(responseName)) %in% columnNames){
    responseName <- sample(c(letters, LETTERS), 20, replace = TRUE)
  }
  dataset[[deparse(substitute(responseName))]] <- 1:nrow(dataset)

  # build a isolation forest
  iso <- ranger::ranger(
    dependent.variable.name     = deparse(substitute(responseName))
    , data                      = dataset
    , mtry                      = 1
    , min.node.size             = 1
    , splitrule                 = "extratrees"
    , num.random.splits         = 1
    , respect.unordered.factors = "partition"
    , ...
  )

  return(structure(list(forest = iso), class = "solitude"))
}
