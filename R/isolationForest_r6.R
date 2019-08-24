#' @name isolationForest
#' @title Fit an Isolation Forest
#' @description 'solitude' class implements the isolation forest method
#'   introduced by paper Isolation based Anomaly Detection (Liu, Ting and Zhou
#'   <doi:10.1145/2133360.2133363>). The extremely randomized trees (extratrees)
#'   required to build the isolation forest is grown using
#'   \code{\link[ranger]{ranger}} function from \pkg{ranger} package.
#' @section Design: \code{$new()} initiates a new 'solitude' object. The
#'   possible arguments are:
#'
#'   \itemize{
#'
#'   \item \code{sample_fraction}: (between 0(exclusive) and 1(inclusive),
#'   default = 1) Fraction of observations in the dataset to used to build the
#'   forest
#'
#'   \item \code{num_trees}: (positive integer, default = 100) Number of trees
#'   to be built in the forest
#'
#'   \item \code{replace}: (boolean) Whether the sample of observations should
#'   be chosen with replacement when sample_fraction < 1
#'
#'   \item \code{seed}: (positive integer) Random seed for the forest
#'
#'   \item \code{nproc}: (NULL or positive integer, default: Use all resources)
#'   Number of parallel threads to be used by ranger
#'
#'   \item \code{respect_unordered_factors}: See respect.unordered.factors
#'   argument in \code{\link[ranger]{ranger}}
#'
#'   }
#'
#'   \code{$fit()} fits a isolation forest for the given dataframe, computes
#'   depths of terminal nodes of each tree and stores the anomaly scores and
#'   average depth values in \code{$scores} object as a data.table
#'
#'   \code{$predict()} returns anomaly scores for a new data as a data.table
#'
#' @section Details:
#'
#'   \itemize{
#'
#'   \item Parallelization: \code{\link[ranger]{ranger}} is parallelized and by
#'   default uses all the resources. This is supported when nproc is set to
#'   NULL. The process of obtaining depths of terminal nodes (which is excuted
#'   with \code{$fit()} is called) may be parallelized separately by setting up
#'   a \pkg{future} backend.
#'
#'   }
#'
#' @examples
#' data("attrition", package = "rsample")
#' set.seed(1)
#' index = sample(ceiling(nrow(attrition) * 0.2))
#' isf = isolationForest$new()  # initiate
#' isf$fit(attrition[index, ])  # fit on 80% data
#' isf$scores                   # obtain anomaly scores
#'
#' # scores closer to 1 might indicate outliers
#' plot(density(isf$scores$anomaly_score))
#' round(head(sort(isf$scores$anomaly_score, dec = TRUE), 20), 2)
#'
#' isf$predict(attrition[-index, ]) # scores for new data
#' @export

isolationForest = R6::R6Class(
  "solitude"
  ,
  # Implementation:
  # 'solitude' class implements the isolation forest method defined by
  # Liu, Fei Tony, Ting, Kai Ming and Zhou, Zhi-Hua.
  # "Isolation-based anomaly detection." ACM Transactions on Knowledge Discovery   # from Data (T KDD). <doi:10.1145/2133360.2133363>
  #
  # Plan:
  # 1. Build 'extratrees' forest using ranger.
  # 2. Obtain terminal node depths by parsing each tree.
  # 3. Compute anomaly scores.
  #
  # Design:
  # 1. 'initialize' method sets the params required for forest growth.
  # 2. Calling 'fit' on some dataset does these things:
  #    - Grow 'extratrees' forest.
  #    - Compute and store the depth of each terminal node in every tree.
  #    - Compute depth or pathlength for each observation.
  #    - Compute anomaly score for each observation and store it in 'scores'.
  # 3. 'predict' method computes anomaly scores on new data.
  #
  public = list(

    sample_fraction = 1
    , num_trees     = 100
    , replace       = TRUE
    , seed          = 101
    , scores        = NULL
    , nproc         = NULL
    ,
    # intialize arguments required for fitting extratrees via ranger
    initialize = function(sample_fraction             = 1
                          , num_trees                 = 100
                          , replace                   = FALSE
                          , seed                      = 101
                          , nproc                     = NULL
                          , respect_unordered_factors = NULL
                          ){

      stopifnot(is.numeric(sample_fraction) && length(sample_fraction) == 1)
      stopifnot(0 < sample_fraction)
      stopifnot(sample_fraction <= 1)
      stopifnot(is_integerish(num_trees) && length(num_trees) == 1)
      stopifnot(0 < num_trees)
      stopifnot(is.logical(replace) && length(replace) == 1)
      stopifnot(is_integerish(seed) && length(seed) == 1)
      stopifnot(is.null(nproc) || is_integerish(nproc))
      stopifnot(is.null(respect_unordered_factors) ||
                  is.character(respect_unordered_factors) ||
                  length(respect_unordered_factors) == 1
                )


      self$sample_fraction = sample_fraction
      self$num_trees       = num_trees
      self$seed            = seed
      self$replace         = replace
    }
    ,
    fit = function(dataset){

      # create a new 'y' column with jumbled 1:n
      columnNames  = colnames(data)
      responseName = columnNames[[1]]
      while(deparse(substitute(responseName)) %in% columnNames){
        responseName = sample(c(letters, LETTERS), 20, replace = TRUE)
      }
      set.seed(self$seed)
      dataset[[deparse(substitute(responseName))]] = sample.int(nrow(dataset))

      # build a extratrees forest
      message("Building Isolation Forest ... ", appendLF = FALSE)
      private$forest = ranger::ranger(
        dependent.variable.name     = deparse(substitute(responseName))
        , data                      = dataset
        , mtry                      = ncol(dataset) - 1L
        , min.node.size             = 1L
        , splitrule                 = "extratrees"
        , num.random.splits         = 1L
        , num.trees                 = self$num_trees
        , replace                   = self$replace
        , sample.fraction           = self$sample_fraction
        , respect.unordered.factors = self$respect_unordered_factors
        , num.threads               = self$nproc
        , seed                      = self$seed
        )
      message("done")

      # compute terminal nodes depth
      message("Computing depth of terminal nodes ... ", appendLF = FALSE)
      private$terminal_nodes_depth = terminalNodesDepth(private$forest)
      message("done")

      # set phi -- sample size used for tree building
      private$phi = self$sample_fraction * nrow(dataset)

      # predict anomaly scores for data used for fitting
      self$scores = self$predict(dataset)
    }
    ,
    predict = function(data){

      tnm = predict(private$forest
                    , data
                    , type        = "terminalNodes"
                    , num.threads = self$nproc
                    )[["predictions"]]

      tnm = data.table::as.data.table(tnm)
      data.table::setnames(tnm, colnames(tnm), as.character(1:ncol(tnm)))
      tnm[, id := .I]
      tnm = data.table::melt(tnm
                             , id.vars         = "id"
                             , variable.name   = "id_tree"
                             , value           = "id_node"
                             , variable.factor = FALSE
                             )
      id_tree = NULL
      id_node = NULL

      tnm[, id_tree := as.integer(id_tree)]
      tnm[, id_node := as.integer(id_node)]

      obs_depth = merge(tnm
                        , private$terminal_nodes_depth
                        , by = c("id_tree", "id_node")
                        )

      average_depth = NULL
      depth         = NULL
      id            = NULL
      anomaly_score = NULL

      scores = obs_depth[, .(average_depth = mean(depth)), by = id][
        order(id)]
      scores[, anomaly_score := private$computeAnomaly(average_depth, private$phi)]

      return(scores)
    }
  )
  ,
  private = list(

    forest                 = NULL
    , terminal_nodes_depth = NULL
    , phi                  = NULL
    ,
    pathLengthNormalizer = function(phi){

      res = 0
      if (phi == 2){
        res = 1
      } else if (phi > 2){
        res = 2 * private$harmonic(phi - 1) - (2 * (phi - 1)/phi)
      }

      return(res)

    }
    ,
    computeAnomaly = function(depth, data_size){

      den = private$pathLengthNormalizer(data_size)
      return(2^( -( depth / den ) ))
    }
    ,
    harmonic = function(n){
      ifelse(n > 11, log(n) + 0.577216, sum(1/seq(1,n)))
    }
  )
)
