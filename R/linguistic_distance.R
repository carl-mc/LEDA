############################
# Linguistic distance computation
############################



# Make igraph from languages
#' @name make_ling_tree
#' @title Transform wide language dataset into igraph tree
#' @import igraph
#'    stringr
#'    plyr
#' @keywords internal
make_ling_tree <- function(df, collapse = T){
  # Packages
  # require(igraph)
  # require(stringr)
  # require(plyr)
  
  # Make Graph
  
  # ... unique languages
  df <- unique(df[, c(paste0("level", 1:14), "iso", "dialect")])
  
  # ... add 1 to extended weight for each missing
  iso.weight.expand <- rep(1, nrow(df))
  for(i in c(1:14)){
    iso.weight.expand[is.na(df[,paste0("level", i)]) | df[,paste0("level", i)] == ""] <- 
      iso.weight.expand[is.na(df[,paste0("level", i)]) | df[,paste0("level", i)] == ""] + 1
  }
  
  # ... get and replace missing dialect
  dial.weight.replace <- rep(1, nrow(df))
  dial.weight.replace[is.na(df[,"dialect"]) | df[,"dialect"] == ""] <- 0
  df$dialect[df$dialect %in% c("", NA)] <- df$iso[df$dialect %in% c("", NA)] 
  
  # ... drop missings
  df.ls <- apply(df[, c(paste0("level", 1:14), "iso", "dialect")], 1, function(x){
    c(level0 = "root", x[!x %in% c(NA, "")])
  })
  
  
  # ... check nothing is lost
  stopifnot(all(unlist(lapply(df.ls, function(x){
    names(x) == c(paste0("level", 0:(length(x)-3)), "iso", "dialect")
  }))))
  
  # ... rename so path to root is contained in each node name
  df.ls <- lapply(df.ls, function(x){
    x[1:(length(x))] <- unlist(lapply(1:(length(x)), function(i){paste(x[1:i], collapse = "__")}))
    x
  })
  
  # ... make edges
  edges <- do.call(rbind, lapply(df.ls, function(x){
    data.frame(from = x[1:(length(x) - 1)], 
               to = x[2:(length(x))],
               from_level = names(x[1:(length(x) - 1)]),
               to_level = names(x[2:(length(x))]),
               stringsAsFactors = F
    )
  }))
  edges <- unique(edges)
  rownames(edges) <- NULL
  
  
  # ... make edge data
  edge.df <- data.frame(edges,
                        weight = rep(1, nrow(edges)),
                        weight.expand = rep(1, nrow(edges)))
  edge.df$from_name <- unlist(lapply(strsplit(edge.df$from, "__", fixed = T), function(x){x[length(x)]}))
  edge.df$to_name <- unlist(lapply(strsplit(edge.df$to, "__", fixed = T), function(x){x[length(x)]}))
  
  # ... add expanded weights
  edge.df$weight.expand[edge.df$to_level %in% "iso"] <- 
    plyr::join(edge.df[edge.df$to_level %in% "iso",], 
               unique(data.frame(to_name = df$iso, new = iso.weight.expand)),
               type = "left", by = "to_name", match = "first")$new
  
  # ... add reduced weights
  edge.df$weight[edge.df$from_level %in% "iso"] <- 
    plyr::join(edge.df[edge.df$from_level %in% "iso",], 
               unique(data.frame(from_name = df$iso, new = dial.weight.replace)),
               type = "left", by = "from_name", match = "first")$new
  
  
  # Make Graph
  g = graph.data.frame(edge.df, directed = F)
  
  # Encode vertex level
  V(g)$name_org <- unlist(lapply(strsplit(V(g)$name, "__", fixed = T), function(x){x[length(x)]}))
  V(g)$is.language <- ifelse(V(g)$name_org %in% df$iso & degree(g, V(g), mode="all") > 1, 1, 0)
  V(g)$is.dialect <- ifelse(degree(g, V(g), mode="all") == 1, 1, 0)
  V(g)$iso[V(g)$is.dialect == 1] <- unlist(lapply(strsplit(V(g)$name[V(g)$is.dialect == 1], 
                                                           "__", fixed = T), 
                                                  function(x){x[length(x) - 1]}))
  stopifnot(all(V(g)$iso[V(g)$is.dialect == 1] %in% df$iso)) 
  
  # Encode unique vertex ID
  V(g)$vertex_id <- 1:length(V(g))
  
  # Check all expanded add up to 15
  stopifnot(all(distances(g, 
                          v = "root", 
                          to = which(V(g)$is.language == 1), 
                          mode = c("all"), weights = E(g)$weight.expand) == 15))
  stopifnot(all(distances(g, 
                          v = "root", 
                          to = which(V(g)$is.dialect == 1), 
                          mode = c("all"), weights = E(g)$weight.expand) == 16))
  
  # return
  return(g)
}


# Hekper function to calculate language similarity
#' @name calc_lang_dist
#' @title Calculate linguistic distance
#' @import igraph
#' @keywords internal
calc_lang_dist <- function(ethno.g, vertex.a, vertex.b, expand = F, delta = .5){
  # require(igraph)
  
  ## Distance
  if(expand){
    ## Raw distance between all pairs of languages
    dist.ab <- distances(ethno.g, 
                         v = which(V(ethno.g)$vertex_id %in% vertex.a), 
                         to = which(V(ethno.g)$vertex_id %in% vertex.b), 
                         mode = c("all"), weights = E(ethno.g)$weight.expand)
    
    ## Distance to root
    dist.root.a <- distances(ethno.g, 
                             v = "root", 
                             to = which(V(ethno.g)$vertex_id %in% vertex.a), 
                             mode = c("all"), weights = E(ethno.g)$weight.expand)[1,]
    dist.root.b <- distances(ethno.g, 
                             v = "root", 
                             to = which(V(ethno.g)$vertex_id %in% vertex.b), 
                             mode = c("all"), weights = E(ethno.g)$weight.expand)[1,]
    
    
  } else {
    ## Raw distance between all pairs of expanded languages
    dist.ab <- distances(ethno.g, 
                         v = which(V(ethno.g)$vertex_id %in% vertex.a), 
                         to = which(V(ethno.g)$vertex_id %in% vertex.b), 
                         mode = c("all"), weights = E(ethno.g)$weight)
    
    ## Distance to root
    dist.root.a <- distances(ethno.g, 
                             v = "root", 
                             to = which(V(ethno.g)$vertex_id %in% vertex.a), 
                             mode = c("all"), weights = E(ethno.g)$weight)[1,]
    dist.root.b <- distances(ethno.g, 
                             v = "root", 
                             to = which(V(ethno.g)$vertex_id %in% vertex.b), 
                             mode = c("all"), weights = E(ethno.g)$weight)[1,]
    
  }
  
  ## Name distances
  rownames(dist.ab) <- V(ethno.g)$vertex_id[V(ethno.g)$vertex_id %in% vertex.a]
  colnames(dist.ab) <- V(ethno.g)$vertex_id[V(ethno.g)$vertex_id %in% vertex.b]
  names(dist.root.a) <- V(ethno.g)$vertex_id[V(ethno.g)$vertex_id %in% vertex.a]
  names(dist.root.b) <- V(ethno.g)$vertex_id[V(ethno.g)$vertex_id %in% vertex.b]
  
  # Distance to root to matrix
  dist.root.a <- matrix(rep(dist.root.a, ncol(dist.ab)), nrow = nrow(dist.ab))
  dist.root.b <- matrix(rep(dist.root.b, each = nrow(dist.ab)), nrow = nrow(dist.ab))
  
  # Calculate
  shared <- ((dist.root.a + dist.root.b) - dist.ab) / 2
  dist <- 1- ((2 * shared)/(dist.root.a + dist.root.b))^delta
  
  # Checks
  stopifnot(nrow(dist) == length(unique(vertex.a)))
  stopifnot(ncol(dist) == length(unique(vertex.b)))
  
  # Return
  dist
}


# Compute lingustic distance between all pairs of groups
#' @name ling_dist_df
#' @title Make linguistic distance DataFrame
#' @import igraph
#'    plyr
#' @keywords internal
ling_dist_df <- function(link.a, link.b, link.dict, list.dict, level = c("dialect", "langauge"),
                         by.country = T, delta = .5, expand = FALSE, 
                         agg_fun.a = min, agg_fun.b = min, 
                         ethno.df, ethno.long, ethno.g = NULL){
  # require(plyr)
  # require(igraph)
  
  ## Check
  if(length(level) > 1 | !all(level %in% c("dialect", "language"))){
    stop("level must be either 'dialect' or 'language'.")
  }
  
  ## Make an ethnologue tree
  if(is.null(ethno.g)){
    ethno.g <- make_ling_tree(df  = ethno.df)
  } else {
    stopifnot("igraph" %in% class(ethno.g))
  }
  
  ## Level to numeric
  level.num <- ifelse(level == "language", 16, 17)
  if(level.num == 16){
    lang.vars <- "iso"
  } else {
    lang.vars <- c("iso","dialect")
  }
  
  
  # Prepare linking data
  
  ## Subset link dictionary to right language level
  link.dict <- link.dict[link.dict$link.level == level.num,]
  
  ## Get ethnologue nodes of link.a and link.b
  
  ### Subset ethnolong to all isocodes
  ethno.long <- ethno.long[ethno.long$level == level.num, c("iso3c",lang.vars, "ethno.id")]
  ethno.long$ethno.iso3c <- ethno.long$iso3c
  ethno.long$iso3c <- NULL
  
  ### Join
  df.link.a <- link.dict[link.dict$list.id %in% link.a & 
                           link.dict$link.level == level.num,]
  df.link.b <- link.dict[link.dict$list.id %in% link.b & 
                           link.dict$link.level == level.num,]
  
  ## Join with ethnologue language isocodes
  df.link.a <- join(df.link.a, ethno.long, type = "left", match = "first", 
                    by = c("ethno.id", "ethno.iso3c"))
  df.link.b <- join(df.link.b, ethno.long, type = "left", match = "first", 
                    by = c("ethno.id", "ethno.iso3c"))
  
  
  ## Collapse to unique ethnologue nodes (each can appear in multiple lists)
  df.link.a <- unique(df.link.a[,c("group","list.id", lang.vars)])
  df.link.b <- unique(df.link.b[,c("group","list.id", lang.vars)])
  
  ## Add vertex IDs
  
  ### Make vertex df
  vertex.df <- data.frame(do.call(cbind, lapply(vertex_attr_names(ethno.g), function(v){
    vertex_attr(ethno.g, v)
  })))
  colnames(vertex.df) <- vertex_attr_names(ethno.g)
  
  ### Add
  if(level.num == 16){
    df.link.a <- join(cbind(df.link.a, name_org = df.link.a$iso), 
                      vertex.df[vertex.df$is.language == 1, 
                                c("name_org", "vertex_id")], 
                      type = "left", match = "first",
                      by = c("name_org"))
    df.link.b <- join(cbind(df.link.b, name_org = df.link.b$iso), 
                      vertex.df[vertex.df$is.language == 1, 
                                c("name_org", "vertex_id")], 
                      type = "left", match = "first",
                      by = c("name_org"))
  } else {
    df.link.a <- join(cbind(df.link.a, name_org = df.link.a$dialect), 
                      vertex.df[vertex.df$is.dialect == 1, 
                                c("iso","name_org", "vertex_id")], 
                      type = "left", match = "first",
                      by = c("iso", "name_org"))
    df.link.b <- join(cbind(df.link.b, name_org = df.link.b$dialect), 
                      vertex.df[vertex.df$is.dialect == 1, 
                                c("iso","name_org", "vertex_id")], 
                      type = "left", match = "first",
                      by = c("iso", "name_org"))
  }
  
  ## Add list information
  df.link.a <- join(df.link.a, list.dict[,c("iso3c", "list.id")], 
                    by = "list.id", match = "first", type = "left")
  df.link.b <- join(df.link.b, list.dict[,c("iso3c", "list.id")], 
                    by = "list.id", match = "first", type = "left")
  
  ## Finalize:
  
  ### By country
  if(by.country){
    ## Get all countries
    all.countries <- unique(df.link.a$iso3c)[unique(df.link.a$iso3c) %in% df.link.b$iso3c]
  } else {
    all.countries <- "ALL_COUNTRIES"
  }  
  
  ## Loop over them
  dist.df <- do.call(rbind, lapply(all.countries, function(c){
    # print(c)
    
    ## Subset to country
    if(c == "ALL_COUNTRIES"){
      this.df.link.a <- df.link.a
      this.df.link.b <- df.link.b
    } else {
      this.df.link.a <- df.link.a[df.link.a$iso3c == c, , drop = F]
      this.df.link.b <- df.link.b[df.link.b$iso3c == c, , drop = F]
    }
    
    
    # Drop isocodes if not on graph
    if(any(is.na(this.df.link.a$vertex_id))){
      to.drop <- which(is.na(this.df.link.a$vertex_id))
      # warning(paste("Dropping", length(to.drop), " nodes a in", c))
      this.df.link.a <- this.df.link.a[!is.na(this.df.link.a$vertex_id),]
    }
    if(any(is.na(this.df.link.b$vertex_id))){
      to.drop <- which(is.na(this.df.link.b$vertex_id))
      # warning(paste("Dropping", length(to.drop), " nodes b in", c))
      this.df.link.b <- this.df.link.b[!is.na(this.df.link.b$vertex_id),]
    }
    
    ## Get IDs of unique group x list combinations
    this.df.link.a$grplist.id <- as.numeric(as.factor(paste0(this.df.link.a$group,".", this.df.link.a$list.id)))
    this.df.link.b$grplist.id <- as.numeric(as.factor(paste0(this.df.link.b$group,".", this.df.link.b$list.id)))
    
    # Calculate distance matrix of unique languages
    
    ## ethnologue isocodes
    vertex_id.a <- unique(this.df.link.a$vertex_id)
    vertex_id.b <- unique(this.df.link.b$vertex_id)
    
    ## Calculate distance
    dist.mat <- calc_lang_dist(ethno.g = ethno.g, 
                               vertex.a = vertex_id.a, vertex.b = vertex_id.b , 
                               expand = expand, delta = delta)
    
    # Transfer to groups in A and B and collapse
    
    ## Collapse groups to all
    grps.a <- unique(this.df.link.a[,c("grplist.id", "group", "list.id")])
    colnames(grps.a) <- paste0("a.", colnames(grps.a))
    grps.b <- unique(this.df.link.b[,c("grplist.id", "group", "list.id")])
    colnames(grps.b) <- paste0("b.", colnames(grps.b))
    
    ## Expand to full matrix between all groups in A and B and collapse with agg_fun
    dist.vec <- unlist(lapply(grps.a$a.grplist.id, function(a){
      lapply(grps.b$b.grplist.id, function(b){
        # Retrieve distance matrix for a -- b 
        this.dist.mat <- dist.mat[as.character(this.df.link.a$vertex_id[this.df.link.a$grplist.id == a]), 
                                  as.character(this.df.link.b$vertex_id[this.df.link.b$grplist.id == b]), 
                                  drop = F]
        # Collapse by each language node of a over nodes in b
        a.dist <- apply(this.dist.mat, 1, agg_fun.b)
        
        # Collapse across language nodes of a
        agg_fun.a(a.dist)
      })
    }))
    
    ## To one data.frame
    dist.df <- cbind(grps.a[rep(1:nrow(grps.a), each = nrow(grps.b)), 2:3],
                     grps.b[rep(1:nrow(grps.b), nrow(grps.a)), 2:3],
                     distance = dist.vec)
    
    ## Return by country
    return(dist.df)
  }))
  
  
  
  # Return
  return(dist.df)
}


