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
#'   \item \code{sample_size}: (positive integer, default = 256) Number of
#'   observations in the dataset to used to build a tree in  the forest
#'
#'   \item \code{num_trees}: (positive integer, default = 100) Number of trees
#'   to be built in the forest
#'
#'   \item \code{replace}: (boolean, default = FALSE) Whether the sample of
#'   observations should be chosen with replacement when sample_size is less
#'   than the number of observations in the dataset
#'
#'   \item \code{seed}: (positive integer, default = 101) Random seed for the
#'   forest
#'
#'   \item \code{nproc}: (NULL or a positive integer, default: NULL, means use
#'   all resources) Number of parallel threads to be used by ranger
#'
#'   \item \code{respect_unordered_factors}: (string, default: "partition")See
#'   respect.unordered.factors argument in \code{\link[ranger]{ranger}}
#'
#'   \item \code{max_depth}: (positive number, default:
#'   ceiling(log2(sample_size))) See max.depth argument in
#'   \code{\link[ranger]{ranger}}
#'
#'   }
#'
#'   \code{$fit()} fits a isolation forest for the given dataframe or sparse matrix, computes
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
#' \dontrun{
#' library("solitude")
#' library("tidyverse")
#' library("mlbench")
#'
#' data(PimaIndiansDiabetes)
#' PimaIndiansDiabetes = as_tibble(PimaIndiansDiabetes)
#' PimaIndiansDiabetes
#'
#' splitter   = PimaIndiansDiabetes %>%
#'   select(-diabetes) %>%
#'   rsample::initial_split(prop = 0.5)
#' pima_train = rsample::training(splitter)
#' pima_test  = rsample::testing(splitter)
#'
#' iso = isolationForest$new()
#' iso$fit(pima_train)
#'
#' scores_train = pima_train %>%
#'   iso$predict() %>%
#'   arrange(desc(anomaly_score))
#'
#' scores_train
#'
#' umap_train = pima_train %>%
#'   scale() %>%
#'   uwot::umap() %>%
#'   setNames(c("V1", "V2")) %>%
#'   as_tibble() %>%
#'   rowid_to_column() %>%
#'   left_join(scores_train, by = c("rowid" = "id"))
#'
#' umap_train
#'
#' umap_train %>%
#'   ggplot(aes(V1, V2)) +
#'   geom_point(aes(size = anomaly_score))
#'
#' scores_test = pima_test %>%
#'   iso$predict() %>%
#'   arrange(desc(anomaly_score))
#'
#' scores_test
#' }
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

    sample_size                 = NULL
    , num_trees                 = NULL
    , replace                   = NULL
    , seed                      = NULL
    , nproc                     = NULL
    , respect_unordered_factors = NULL
    , max_depth                 = NULL
    , forest                    = NULL
    , scores = NULL
    , status = "not_initialized"
    ,
    # intialize arguments required for fitting extratrees via ranger
    initialize = function(sample_size                 = 256
                          , num_trees                 = 100
                          , replace                   = FALSE
                          , seed                      = 101
                          , nproc                     = NULL
                          , respect_unordered_factors = NULL
                          , max_depth                 = ceiling(log2(sample_size))
                          ){

      stopifnot(is_integerish(sample_size) && length(sample_size) == 1)
      stopifnot(0 < sample_size)
      stopifnot(is_integerish(num_trees) && length(num_trees) == 1)
      stopifnot(0 < num_trees)
      stopifnot(is.logical(replace) && length(replace) == 1)
      stopifnot(is_integerish(seed) && length(seed) == 1)
      stopifnot(is.null(nproc) ||
                  (is_integerish(nproc) && length(nproc == 1) && nproc >= 1)
                )
      stopifnot(is.null(respect_unordered_factors) ||
                  (is.character(respect_unordered_factors) &&
                  length(respect_unordered_factors) == 1)
                )
      stopifnot(is_integerish(max_depth) &&
                length(max_depth) == 1 &&
                max_depth > 0
                )

      self$sample_size               = sample_size
      self$num_trees                 = num_trees
      self$replace                   = replace
      self$seed                      = seed
      self$nproc                     = nproc
      self$respect_unordered_factors = respect_unordered_factors
      self$max_depth                 = max_depth
      self$status                    = "not_trained"
    }
    ,
    fit = function(dataset){

      is_sparse = grepl("dg.Matrix", class(dataset)[[1]])
      # check if any rows are duplicated
      if (is_sparse) {
        lgr::lgr$info("sparse dataset detected, skipping duplication check")
      } else if (anyDuplicated(dataset) > 0) {
        lgr::lgr$info("dataset has duplicated rows")
      }

      # create new fit
      if(self$status == "trained"){
        self$status = "not_trained"
        lgr::lgr$info("Retraining ... ")
      }

      # create a new 'y' column with jumbled 1:n
      columnNames  = colnames(dataset)
      nr           = nrow(dataset)

      set.seed(self$seed)
      yy = sample.int(nrow(dataset))

      # deduce sample_fraction
      stopifnot(self$sample_size <= nr)
      private$sample_fraction = self$sample_size/nr

      # build a extratrees forest
      lgr::lgr$info("Building Isolation Forest ... ")
      self$forest = ranger::ranger(
        x = dataset
        , y = yy
        , mtry                      = ncol(dataset) - 1L
        , min.node.size             = 1L
        , splitrule                 = "extratrees"
        , num.random.splits         = 1L
        , num.trees                 = self$num_trees
        , replace                   = self$replace
        , sample.fraction           = private$sample_fraction
        , respect.unordered.factors = self$respect_unordered_factors
        , num.threads               = self$nproc
        , seed                      = self$seed
        , max.depth                 = self$max_depth
        )
      lgr::lgr$info("done")

      # compute terminal nodes depth
      lgr::lgr$info("Computing depth of terminal nodes ... ")
      private$terminal_nodes_depth = terminalNodesDepth(self$forest)
      lgr::lgr$info("done")

      # set phi -- sample size used for tree building
      private$phi = floor(private$sample_fraction * nr)

      # create path length extend dataframe
      tnm = stats::predict(self$forest
                           , dataset
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

      # update train status
      self$status = "trained"
      lgr::lgr$info("Completed growing isolation forest")
    }
    ,
    predict = function(data){

      tnm = stats::predict(self$forest
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

      scores = obs_depth[ , .(average_depth = mean(depth)), by = id][
        order(id)]
      scores[, anomaly_score :=
               private$computeAnomaly(average_depth, private$phi)][]

      return(scores)
    }
  )
  ,
  private = list(

    terminal_nodes_depth       = NULL
    , phi                      = NULL
    , sample_fraction          = NULL
    ,
    pathLengthNormalizer = function(phi){

      res = 0

      if(phi == 2){
        res = 1
      }
      if(phi > 2){
        res = (2 * private$harmonic(phi - 1)) - (2 * (phi - 1)/phi)
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
