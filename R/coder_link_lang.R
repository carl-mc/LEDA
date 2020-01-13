#########################
# Coding Object to link ethnic groups to languages
#########################



######################
# Automatic matching to tree
######################


## Get names of language nodes
#' @name get_ethno_names
#' @title Get names of language nodes
#' @keywords internal
#' @importFrom stats na.omit
get_ethno_names <- function(ethno.df){
  # Cast to long
  name.vec <- unlist(ethno.df[,c("name",paste0("level", c(1:14)))])
  type.vec <- rep(c("org",paste0("L", c(1:14))), each = nrow(ethno.df))
  
  # Make df
  name.df <- na.omit(unique(data.frame(name = name.vec, 
                                       type = type.vec, 
                                       stringsAsFactors = F)))
  name.df <- name.df[nchar(name.df$name) > 1,]
  
  # Return 
  return(name.df)
}


## get alternative language names
get_altnames <- function(ethno.df){
  
  # Alternate natmes vector
  alt.names <- ethno.df$Alternate.names
  if(all(alt.names %in% c("", NA))){return(NULL)}
  
  # Split by comma
  alt.split <- strsplit(alt.names, ",")
  
  # Make new dataframe
  new.df <- unique(data.frame(name = rep(ethno.df$name, 
                                         unlist(lapply(alt.split, length))),
                              alt.name = trimws(unlist(alt.split), which = "both"),
                              stringsAsFactors = F))
  return(new.df)
}

# Get dialects (from full, including alternative names of dialects)
get_dialects <- function(ethno.df){
  # Dialect names vector
  dial.names <- ethno.df$dialect
  dial.full.names <- ethno.df$dialect_full
  if(all(dial.names %in% c("", NA))){return(NULL)}
  
  # Extract from parentheses in full
  dial.names.par <- strsplit(unlist(lapply(regmatches(dial.full.names, 
                                                      gregexpr("(?<=\\().*?(?=\\))",  dial.full.names, perl=T)), 
                                           paste, collapse = ", ")),
                             ",")
  dial.names.ls <- lapply(c(1:length(dial.names)), function(x){
    if(length(dial.names.par[[x]]) > 0){
      c(dial.names[x], dial.names.par[[x]])
    } else {
      dial.names[x]
    }
  })
  
  # Make new dataframe
  new.df <- unique(data.frame(dialect = rep(dial.names, unlist(lapply(dial.names.ls, length))),
                              alt.name = trimws(unlist(dial.names.ls), which = "both"),
                              stringsAsFactors = F))
  new.df <- new.df[!new.df$dialect %in% c(NA,""),, drop = F]
  
  return(new.df)
}

# (function) auto-match all countries
auto_match_all <- function(groups, ethno.df, prev.link.df, threshold = .2,
                           costs = c(insertions = 1,deletions = 1, substitutions = 1)){
 
  # Retrieve language names
  ethno.name.df <- get_ethno_names(ethno.df)
  
  # Get alternate names 
  alt.name.df <- get_altnames(ethno.df)
  
  # Get dialects
  dialect.df <- get_dialects(ethno.df)
  
  # Get previous links
  prev.link.df <- unique(prev.link.df[, c("group","link")])
  prev.link.df <- prev.link.df[!prev.link.df$link %in% c("", NA),]
  
  # Linking 
  link.df <- data.frame(
    group = groups,
    # ... original names:
    auto_link_org = fuzzy_str_match(source = groups, target = ethno.name.df$name, 
                                     target.name = ethno.name.df$name, 
                    target.type = ethno.name.df$type, threshold = threshold,
                    costs = costs),
    
    # ... alternate names:
    auto_link_alt = fuzzy_str_match(source = groups, target = alt.name.df$alt.name, 
                                     target.name = alt.name.df$name, 
                    target.type = rep("org", nrow(alt.name.df)),threshold,
                    costs = costs),
    
    # ... dialect names:
    auto_link_dial = fuzzy_str_match(source = groups, target = dialect.df$dialect, 
                                      target.name = dialect.df$dialect, 
                                      target.type = rep("dial", nrow(dialect.df)),  threshold,
                                      costs = costs),
    
    # ... previous links:
    auto_link_prev = fuzzy_str_match(source = groups, target = prev.link.df$group, 
                                      target.name = prev.link.df$link, 
                                      target.type = rep(NA, nrow(prev.link.df)),  threshold,
                                      costs = costs),
    stringsAsFactors = F
  )
  
  # Clean previous links
  link.df$auto_link_prev <- unlist(lapply(link.df$auto_link_prev, function(x){
    paste(unique(unlist(strsplit(x, "|", fixed = T))), collapse = "|")
  }))
  
  # Return 
  return(link.df)
}




# (function) Fuzzy Matching of string to target
#' @name fuzzy_str_match
#' @title Fuzzy Matching of string to target string
#' @import utils
#' @keywords internal
fuzzy_str_match <- function(source, target, target.name = target, target.type, threshold = .2,
                             costs = c(insertions = 1,deletions = 1, substitutions = 1)){
  # require(utils)

  # Check
  if(length(target) == 0){
    return(rep("", length(source)))
  }
  
  # Levenshtein distance matrix
  dist.mat <- adist(x = source, y = target, costs = costs, 
                    counts = FALSE, fixed = TRUE, 
                    ignore.case = TRUE, useBytes = FALSE) / nchar(source)
  
  # Matches
  matches <- lapply(c(1:nrow(dist.mat)), function(x){which(dist.mat[x,] <= threshold)})
  
  if(length(matches) == length(source)){
    matches <- unlist(lapply(matches, function(x){
      if(length(x) >= 1){
        this.df <- unique(cbind(target.name[x], target.type[x]))
        this.df[,2] <- ifelse(is.na(this.df[,2]),"", paste0(" [", this.df[,2],"]"))
        return(paste(paste0(this.df[,1],  this.df[,2]), collapse = "|"))
      } else {
        return("")
      }
    }))
    stopifnot(length(matches) == length(source))
  } else {
    matches <- rep("", length(source))
  }
  
  # Return
  return(matches)
}


