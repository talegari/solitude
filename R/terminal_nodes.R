#' @name terminalNodesDepth
#' @title Depth of each terminal node of all trees in a ranger model
#' @description Depth of each terminal node of all trees in a ranger model is
#'   returned as a three column tibble with column names: 'id_tree',
#'   'id_node', 'depth'. Note that root node has the node_id = 0.
#' @param model A ranger model
#' @details This function may be parallelized using a future backend.
#' @return A tibble with three columns: 'id_tree', 'id_node', 'depth'.
#' @examples
#' rf = ranger::ranger(Species ~ ., data = iris, num.trees = 100)
#' terminalNodesDepth(rf)
#' @export
terminalNodesDepth = function(model){

  id_node   = NULL
  depth     = NULL
  num_trees = model[["num.trees"]]

  # per tree node ids and depth
  nodeDepth = function(x){

    out = terminalNodesDepthPerTree(ranger::treeInfo(model, x))
    # return two column df for that tree: id_node and depth
    res = data.table::data.table(id_node = names(out)
                                 , depth = unname(out)
                                 )
    return(res)
  }

  # iterate over trees
  tnd = future.apply::future_lapply(1:num_trees, nodeDepth)
  tnd = data.table::rbindlist(tnd, idcol = "id_tree")

  tnd[ , id_node := as.integer(id_node)]
  tnd[ , depth   := as.integer(depth)]

  return(tnd)
}


#' @name terminalNodesDepthPerTree
#' @title Depth of each terminal node of a single tree in a ranger model
#' @description Depth of each terminal node of a single tree in a ranger model.
#'   Note that root node has the node_id = 0.
#' @param treelike Output of `ranger::treeInfo`
#' @return Named vector of depths where the names are the terminal node IDs
#' @examples
#' \dontrun{
#'   rf = ranger::ranger(Species ~ ., data = iris)
#'   terminalNodesDepthPerTree(ranger::treeInfo(rf, 1))
#' }
terminalNodesDepthPerTree = function(treelike){

  nodeID     = NULL
  leftChild  = NULL
  rightChild = NULL
  terminal   = NULL

  treelike  = data.table::as.data.table(treelike)
  melted    = data.table::melt(
    treelike[, c("nodeID", "leftChild", "rightChild")]
    , id.vars      = "nodeID"
    , measure.vars = c("leftChild", "rightChild")
    )
  value       = NULL
  edgeMat     = as.matrix(melted[!is.na(value), c("nodeID", "value")]) + 1L
  treegraph   = igraph::graph_from_edgelist(edgeMat)
  tnValues    = treelike[terminal == TRUE,][["nodeID"]]
  depths      = igraph::distances(treegraph
                                   , v    = 1
                                   , to   = tnValues + 1L
                                   , mode = "out"
                                   )
  dim(depths)   = NULL
  names(depths) = tnValues

  return(depths)
}
