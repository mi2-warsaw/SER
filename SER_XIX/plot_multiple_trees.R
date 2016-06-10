if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}


xgb.plot.multi.trees <- function(model, feature_names = NULL, features.keep = 5, plot.width = NULL, plot.height = NULL){
  tree.matrix <- xgb.model.dt.tree(feature_names = feature_names, model = model)
  
  # first number of the path represents the tree, then the following numbers are related to the path to follow
  # root init
  root.nodes <- tree.matrix[str_detect(ID, "\\d+-0"), ID]
  tree.matrix[ID %in% root.nodes, abs.node.position:=root.nodes]
  
  precedent.nodes <- root.nodes
  
  while(tree.matrix[,sum(is.na(abs.node.position))] > 0) {
    yes.row.nodes <- tree.matrix[abs.node.position %in% precedent.nodes & !is.na(Yes)]
    no.row.nodes <- tree.matrix[abs.node.position %in% precedent.nodes & !is.na(No)]
    yes.nodes.abs.pos <- yes.row.nodes[, abs.node.position] %>% paste0("_0")
    no.nodes.abs.pos <- no.row.nodes[, abs.node.position] %>% paste0("_1")
    
    tree.matrix[ID %in% yes.row.nodes[, Yes], abs.node.position := yes.nodes.abs.pos]
    tree.matrix[ID %in% no.row.nodes[, No], abs.node.position := no.nodes.abs.pos]
    precedent.nodes <- c(yes.nodes.abs.pos, no.nodes.abs.pos)
  }
  
  tree.matrix[!is.na(Yes),Yes:= paste0(abs.node.position, "_0")]
  tree.matrix[!is.na(No),No:= paste0(abs.node.position, "_1")]
  
  
  
  remove.tree <- . %>% str_replace(pattern = "^\\d+-", replacement = "")
  
  tree.matrix[,`:=`(abs.node.position=remove.tree(abs.node.position), Yes=remove.tree(Yes), No=remove.tree(No))]
  
  nodes.dt <- tree.matrix[,.(Quality = sum(Quality)),by = .(abs.node.position, Feature)][,.(Text =paste0(Feature[1:min(length(Feature), features.keep)], " (", Quality[1:min(length(Quality), features.keep)], ")") %>% paste0(collapse = "\n")), by=abs.node.position]
  edges.dt <- tree.matrix[Feature != "Leaf",.(abs.node.position, Yes)] %>% list(tree.matrix[Feature != "Leaf",.(abs.node.position, No)]) %>% rbindlist() %>% setnames(c("From", "To")) %>% .[,.N,.(From, To)] %>% .[,N:=NULL]
  
  nodes <- DiagrammeR::create_nodes(nodes = nodes.dt[,abs.node.position],
                                    label = nodes.dt[,Text],
                                    style = "filled",
                                    color = "DimGray",
                                    fillcolor= "Beige",
                                    shape = "oval",
                                    fontname = "Helvetica"
  )
  
  edges <- DiagrammeR::create_edges(from = edges.dt[,From],
                                    to = edges.dt[,To],
                                    color = "DimGray", 
                                    arrowsize = "1.5", 
                                    arrowhead = "vee",
                                    fontname = "Helvetica",
                                    rel = "leading_to")
  
  graph <- DiagrammeR::create_graph(nodes_df = nodes,
                                    edges_df = edges,
                                    graph_attrs = "rankdir = LR")
  
  DiagrammeR::render_graph(graph, width = plot.width, height = plot.height)  
}

globalVariables(
  c(
    "Feature", "no.nodes.abs.pos", "ID", "Yes", "No", "Tree", "yes.nodes.abs.pos", "abs.node.position"
  )
)