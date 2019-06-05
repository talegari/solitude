#' @name isolationForest
#' @aliases isolation_forest
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
#' mo <- isolationForest(iris, seed = 1)
#' @export
isolationForest <- function(dataset, ...){

  columnNames  <- colnames(dataset)
  responseName <- columnNames[[1]]
  while(deparse(substitute(responseName)) %in% columnNames){
    responseName <- sample(c(letters, LETTERS), 20, replace = TRUE)
  }

  arguments <- list(...)
  arg_names <- names(arguments)

  fixed_names <- c("dependent.variable.name"
                   , "data"
                   , "mtry"
                   , "min.node.size"
                   , "splitrule"
                   , "num.random.splits"
                   )
  if(any(fixed_names %in% arg_names)){
    stop(
      paste0("These optional arguments of ranger::ranger should not be specified: "
             , toString(fixed_names)
             )
      )
  }

  if("seed" %in% names(arguments)){
    seed <- arguments[["seed"]]
  } else {
    seed <- sample.int(1e5, 1)
  }

  if(!("replace" %in% arg_names)){
    arguments[["replace"]] <- FALSE
  }

  if(!("sample.fraction" %in% arg_names)){
    arguments[["sample.fraction"]] <- 1
  }

  if(!("respect.unordered.factors" %in% arg_names)){
    arguments[["respect.unordered.factors"]] <- "partition"
  }

  set.seed(seed)
  dataset[[deparse(substitute(responseName))]] <- sample.int(nrow(dataset))

  # build a isolation forest
  iso <- fastDoCall(ranger::ranger
    , c(
        list(
          dependent.variable.name     = deparse(substitute(responseName))
          , data                      = dataset
          , mtry                      = 1
          , min.node.size             = 1
          , splitrule                 = "extratrees"
          , num.random.splits         = 1
          )
        , arguments
        )
    )

  return(structure(list(forest = iso), class = "solitude"))
}

#' @export
isolation_forest <- isolationForest