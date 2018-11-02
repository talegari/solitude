#' @name predict.solitude
#' @title Predict method for solitude class
#' @description Predict anomaly score and corrected depth of each observation in
#'   the data using the isolation forest
#' @param object Isolation forest model of class 'solitude'
#' @param data Dataframe to predict on
#' @param type Type of prediction. One among: anomaly_score, depth_corrected
#' @param aggregator Function(as a string, default is 'median') to aggregate the
#'   corrected depths per observation over all trees. This is applicable when
#'   type is anomaly_score
#' @param ... Ignored
#' @details The following types of prediction are supported: \itemize{
#'
#'   \item anomaly_score: The thumb rule says: If the score is closer to 1 for a
#'   some obervations, they are likely outliers. If the score for all
#'   observations hover around 0.5, there might not be outliers at all.
#'
#'   \item depth_corrected: This estimates the depth of the observation by
#'   adding a factor of average length unsuccessful search in binary search
#'   tree.
#'
#'   }
#'
#'   See <doi:10.1145/2133360.2133363> for more details
#' @return Two outputs depending on type argument: \itemize{
#'
#'   \item anomaly_score: A vector(length of number of observations in the data)
#'   of scores. See details for the thumb rule about interpreting them.
#'
#'   \item depth_corrected: A matrix with number of rows equal to number of
#'   observations in the data and number of columns equal to the number of trees
#'   in the model. A value is the estimated depth of an observation in some
#'   tree.
#'
#'   }
#' @examples
#' set.seed(100)
#' index      <- sample.int(150, 75)
#' iris_train <- iris[index, ]
#' iris_test  <- iris[-index, ]
#' mo         <- isolation_forest(iris_train)
#' set.seed(100)
#' index      <- sample.int(150, 100)
#' iris_train <- iris[index, ]
#' iris_test  <- iris[-index, ]
#' mo         <- isolation_forest(iris_train[, 1:4], seed = 101)
#' scores     <- predict(mo, iris_test)
#' summary(scores)
#' with(iris_test
#'      , plot(Sepal.Length
#'             , Sepal.Width
#'             , col = Species
#'             , cex = ifelse(scores > 0.58, 2, 1)
#'             , pch = 20
#'             )
#'      )
#' \dontrun{
#' with(iris_train
#'      , plot(Sepal.Length
#'             , Sepal.Width
#'             , col = Species
#'             , cex = ifelse(predict(mo, iris_train) > 0.6, 2, 1)
#'             , pch = 20
#'             )
#'      )
#' }
#' @importFrom stats predict
#' @importFrom data.table :=
#' @export
predict.solitude <- function(object
                             , data
                             , type       = "anomaly_score"
                             , aggregator = "median"
                             , ...
                             ){

  if(!(type %in% c("anomaly_score", "depth_corrected"))){
    stop("type has to be among: anomaly_score, depth_corrected")
  }

  res <- switch(type
                , depth_corrected = depth_corrected(object, data)
                , anomaly_score   = anomaly_score(object, data, aggregator)
                )

  return(res)
}

#' @name depth_corrected
#' @title depth_corrected
#' @description depth_corrected
#' @param object isolation forest model
#' @param newdata dataframe to predict
depth_corrected <- function(object, newdata){

  num_trees <- object[["forest"]][["num.trees"]]

  tnm <- predict(object[["forest"]]
                 , data = newdata
                 , type = "terminalNodes"
                 )[["predictions"]] + 1L


  get_corrected_depths <- function(x){
    depths <- c(
      depth_terminalNodes(ranger::treeInfo(object[["forest"]], x))
      , average_path_length(table(tnm[, x]))
      )

    tapply(depths, names(depths), sum)
  }

  corrected_depths <-
    do.call(cbind
            , lapply(
              1:num_trees
              , function(x) as.numeric(get_corrected_depths(x)[as.character(tnm[, x])])
            )
    )

  return(corrected_depths)
}

#' @name anomaly_score
#' @title anomaly_score
#' @description anomaly_score
#' @param object isolation forest model
#' @param newdata dataframe to predict
#' @param aggregator aggregator
anomaly_score <- function(object, newdata, aggregator = "median"){

  corrected_depths <- depth_corrected(object, newdata)
  res <- compute_anomaly(apply(corrected_depths
                               , 1
                               , eval(as.symbol(aggregator))
                               )
                         , nrow(newdata)
                         )
  return(res)
}

#' @name depth_terminalNodes
#' @title depth_terminalNodes
#' @description depth_terminalNodes
#' @param treelike A single ranger tree extracted from 'ranger::treeInfo'
depth_terminalNodes <- function(treelike){

  nodeID     <- NULL
  leftChild  <- NULL
  rightChild <- NULL

  data.table::setDT(treelike)
  dropThese <- setdiff(colnames(treelike)
                       , c("nodeID", "leftChild", "rightChild")
  )
  treelike[, c(dropThese) := NULL]
  melted    <- data.table::melt(treelike
                                , id.vars      = "nodeID"
                                , measure.vars = c("leftChild", "rightChild")
  )
  value       <- NULL
  edgeMat     <- as.matrix(melted[!is.na(value), c("nodeID", "value")]) + 1L
  treegraph   <- igraph::graph_from_edgelist(edgeMat)
  tnValues    <- treelike[is.na(leftChild) & is.na(rightChild), nodeID] + 1L
  depths      <- igraph::distances(treegraph
                                   , v    = 1
                                   , to   = tnValues
                                   , mode = "out"
                                   )
  dim(depths) <- NULL
  names(depths) <- tnValues
  return(depths)
}

#' @name average_path_length
#' @title average_path_length
#' @description average_path_length
#' @param n n
average_path_length <- Vectorize(
  function(n){
    ifelse(n == 1, 0, 2 * ( harmonic(n - 1) - ((n - 1)/n) ))
  }
  , vectorize.args = "n"
)

#' @name harmonic_approx
#' @title harmonic_approx
#' @description harmonic_approx
#' @param n n
harmonic_approx <- function(n){ log(n) + 0.577216 }

#' @name harmonic_exact
#' @title harmonic_exact
#' @description harmonic_exact
#' @param n n
harmonic_exact <-
  Vectorize(
    function(n){
      sum(1/seq(1,n))
    }
    , vectorize.args = "n"
    )

#' @name harmonic
#' @title harmonic
#' @description harmonic
#' @param n n
harmonic <- Vectorize(
  function(n){
    if(n < 11){
      harmonic_exact(n)
    } else {
      harmonic_approx(n)
    }
  }
  , vectorize.args = "n"
  )

#' @name compute_anomaly
#' @title compute_anomaly
#' @description compute_anomaly
#' @param pathLength pathLength
#' @param dataSize dataSize
compute_anomaly <- Vectorize(
  function(pathLength, dataSize){
    2^( -( pathLength / average_path_length(dataSize) ) )
  }
  , vectorize.args = "pathLength"
  )
