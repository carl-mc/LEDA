# Check links ###########



#' @name check_lang_links
#' @title Check hand-coded links from ethnic groups to languages
#' @import plyr
#' @keywords internal
check_lang_links <- function(link.df, ethno.long){
  # require(plyr)
  
  # links and ethnologue to long data
  
  # ... ethnologue
  ethno.long$link <- ethno.long$group

  # ... linked data
  link.long <- links_wide2long(link.df)
  link.long <- link.long[!link.long$link %in% c("",NA),]
  link.long$link.id <- c(1:nrow(link.long))
  
  # Check 1: All links exist
  check.1.df <- join(link.long, ethno.long[,!colnames(ethno.long) %in% c("level","iso3c")], 
                     type = "left",by = "link", match = "first" )
  
  if(any(is.na(check.1.df$ethno.id))){
    print("SOME LINKED LANGUAGES DO NOT EXIST IN ETHNOLOGUE")
    if("iso3c" %in% colnames(check.1.df)){
      print(check.1.df[is.na(check.1.df$ethno.id),c("iso3c", "group","link")])
    } else {
      print(check.1.df[is.na(check.1.df$ethno.id),c("group","link")])
    }
    
  }
  
  # Check 2: If level indicated, links exist on the right level
  check.2.df <- join(link.long[!is.na(link.long$level),], 
                     ethno.long[,!colnames(ethno.long) %in% c("iso3c")], 
                     type = "left",by = c("level","link"), match = "first" )
  
  if(any(is.na(check.2.df$ethno.id))){
    print("SOME LINKED LANGUAGES DO NOT EXIST ON THE RIGHT LEVEL IN ETHNOLOGUE")
    if("iso3c" %in% colnames(check.2.df)){
      print(check.2.df[is.na(check.2.df$ethno.id),c("iso3c", "group","link", "level")])
    } else {
      print(check.2.df[is.na(check.2.df$ethno.id),c("group","link", "level")])
    }
    
  }
  
  # Check 3: No duplicate links exist
  
  # ... link
  check.3.df <- link_long_4steps(ethno.long, link.long)
  
  
  # ... check for duplicate entries
  if(any(duplicated(check.3.df$link.id))){
    print("NON-UNIQUE links FOUND:")
    print("(Please specify language iso codes)")
    if("iso3c" %in% colnames(check.3.df)){
      print(check.3.df[duplicated(check.3.df$link.id),c("iso3c","group","link")])
    } else {
      print(check.3.df[duplicated(check.3.df$link.id),c("group","link")])
    }
    
  }
  
  # Return
  return(NULL)
}



## Main function to link groups to language tree

#' @name link_long_4steps
#' @title Main function to link ethnic groups to language tree
#' @import plyr
#' @importFrom gtools smartbind
#' @importFrom stats aggregate.data.frame
#' @keywords internal
link_long_4steps <- function(ethno.long, link.long){
  # require(plyr)
  # require(gtools)
  
  # ... within country link
  if("iso3c" %in% colnames(link.long)){
    iso.split <- strsplit(ethno.long$iso3c, "|", fixed = T)
    in.cow.ethno <- ethno.long[rep(1:nrow(ethno.long), unlist(lapply(iso.split, length))),]
    in.cow.ethno$iso3c <- unlist(iso.split)
    if(any(is.na(link.long$level)) & !all(is.na(link.long$level))){
      link.a.df <- smartbind(cbind(join(link.long[is.na(link.long$level),!colnames(link.long) %in% c("level")], in.cow.ethno, 
                                        type = "left",by = c("link", "iso3c") ), link.type = 1),
                             cbind(join(link.long[!is.na(link.long$level),], in.cow.ethno, 
                                        type = "left",by = c("level","link", "iso3c")), link.type = 2))
    } else if(all(is.na(link.long$level))) {
      link.a.df <- cbind(join(link.long[is.na(link.long$level),!colnames(link.long) %in% c("level")], in.cow.ethno, 
                              type = "left",by = c("link", "iso3c") ), link.type = 1)
    } else {
      link.a.df <- cbind(join(link.long[!is.na(link.long$level),], in.cow.ethno, 
                              type = "left",by = c("level","link", "iso3c")), link.type = 2)
    }
    
    link.a.df <- link.a.df[!is.na(link.a.df$ethno.id),, drop = F]
  } else {
    link.a.df <- NULL
  }
  
  
  # .... outside country link
  out.cow.ethno <- ethno.long[!duplicated(ethno.long[,!colnames(ethno.long) %in% c("iso3c", "ethno.id")]),
                              !colnames(ethno.long) %in% c("iso3c")]
  if(sum(is.na(link.long$level) & !link.long$link.id %in% link.a.df$link.id) > 0){
    link.b3.df <- cbind(join(link.long[is.na(link.long$level) & !link.long$link.id %in% link.a.df$link.id,!colnames(link.long) %in% c("level")], 
                              out.cow.ethno, 
                              type = "left",by = c("link") ), link.type = 3)
  } else {
    link.b3.df <- NULL
  }
  
  if(sum(!is.na(link.long$level) & !link.long$link.id %in% link.a.df$link.id) > 0){
    link.b4.df <- cbind(join(link.long[!is.na(link.long$level) & !link.long$link.id %in% link.a.df$link.id,], 
                              out.cow.ethno, 
                              type = "left",by = c("level","link") ), link.type = 4)
  } else {
    link.b4.df <- NULL
  }
  
  if(!is.null(link.b3.df) & !is.null(link.b4.df)){
    link.b.df <- smartbind(link.b3.df,link.b4.df)
  } else if(is.null(link.b3.df) & !is.null(link.b4.df)){
    link.b.df <- link.b4.df
  } else if(!is.null(link.b3.df) & is.null(link.b4.df)){
    link.b.df <- link.b3.df
  } else {
    link.b.df <- NULL
  }
  
  
  
  # ...combine
  if(!is.null(link.b.df) & !is.null(link.a.df)){
    link.df <- smartbind(link.a.df, link.b.df)
  } else if(is.null(link.b.df) & !is.null(link.a.df)) {
    link.df <- link.a.df
  } else if(!is.null(link.b.df) & is.null(link.a.df)){
    link.df <- link.b.df
  } else {
    stop("Ooops, nothing was matched... Please check your input data.")
  }
  
  
  # ... by link, get highest ethnologue level
  if("iso3c" %in% colnames(link.df)){
    link.df <- join(aggregate.data.frame(list(level = link.df$level), 
                                         by = link.df[,c("link.id","link.type", "iso3c")], 
                                         FUN = min),
                    link.df, by = c("level","link.id", "link.type", "iso3c"), type = "left")
  } else {
    link.df <- join(aggregate.data.frame(list(level = link.df$level), 
                                         by = link.df[,c("link.id","link.type")], 
                                         FUN = min),
                    link.df, by = c("level","link.id", "link.type"), type = "left")
  }
  
  
  # return
  return(link.df)
}


# (function) links: wide --> long
#' @name links_wide2long
#' @title Function to transform handcoded links of ethnic groups to languages from wide to long format.
#' @description Returns data so that each link has its own row, adds a variable for the level of the link
#' @keywords internal
links_wide2long <- function(link.df){
  # Split "link" column
  link.split <- strsplit(link.df$link, split = "|",  fixed = T)
  
  # Data to long
  link.long <- link.df[rep(c(1:nrow(link.df)), unlist(lapply(link.split, length))),colnames(link.df) != "link"] 
  link.long$link <- unlist(link.split)
  
  # Level variable
  link.long$level <- unlist(lapply(regmatches(link.long$link, gregexpr("(?<=\\[).*?(?=\\])", link.long$link, perl=T)),
                                    function(x){ifelse(length(x) != 1, NA, x)}))
  link.long$level <- ifelse(!is.na(link.long$level) & link.long$level == "dial", 17,
                             ifelse(!is.na(link.long$level) & link.long$level == "iso", 16,
                                    ifelse(!is.na(link.long$level) & link.long$level == "org", 15,
                                           ifelse(!is.na(link.long$level) & substr(link.long$level, 1, 1) == "L", 
                                                  gsub("L","",link.long$level), NA))))
  link.long$level <- as.numeric(link.long$level)
  link.long$link <- trimws(gsub("\\s*\\[[^\\]+\\]", "", link.long$link), which = "both")
  
  # Return
  return(link.long)
}


# (function) Widen the link to all lowest-level ethnologue entries
#' @name consistent_lang_link
#' @title Wrapper to link ethnic groups to language tree
#' @import plyr
#' @keywords internal
consistent_lang_link <- function(link.df, ethno.long, list.dict){
  # require(plyr)
  
  # links and ethnologue to long
  
  # ... rename ethnologue columns
  ethno.long$link <- ethno.long$group
  ethno.long$group <- NULL
  
  # ... linked data
  link.long <- links_wide2long(link.df)
  link.long <- link.long[!link.long$link %in% c("",NA),]
  link.long$link.id <- c(1:nrow(link.long))
  
  
  # link with ethnologue (1:1)
  
  # ...combine
  combined.df <-  link_long_4steps(ethno.long, link.long)
  
  # .. drop non-links
  if(any(is.na(combined.df$ethno.id))){
    print("Dropping the following linked groups: ")
    print(unique(combined.df[is.na(combined.df$ethno.id),
                             c("iso3c","group","link")]))
    combined.df <- combined.df[!is.na(combined.df$ethno.id),]
  }
  
  # ... replace  iso-codes  with "names" for unique levels
  combined.df$link[combined.df$level == 16] <- combined.df$name[combined.df$level == 16]
  combined.df$iso[combined.df$level == 16] <- NA
  combined.df$level[combined.df$level == 16] <- 15
  
  # ... check for duplicate links
  stopifnot(sum(duplicated(combined.df$link.id)) == 0)
  
  
  # Expand links to level X ethnologue entries
  levels <- c(paste0("level", c(1:14)), "name", "iso", "dialect")
  links <- do.call(rbind, lapply(c(1:length(levels)), function(level.x){
    # print(level.x)
    # ... initialize
    list.ids <- unique(link.long$list.id)
    minimalist.links <- NULL
    links <-  NULL
    all.ethno.children <- ethno.long[ethno.long$level == level.x,]
    
    # ... disentangle
    for(level in c(level.x:1)){
      # Get links on this level
      these.links <- which(ifelse(combined.df$level > level.x, level.x, combined.df$level) %in% level)
      if(length(these.links) == 0){next}
      
      # Get all x-grand-children of link
      link.children <- join(combined.df[these.links,c(levels[1:level], "link.id", "list.id")], 
                             all.ethno.children,
                             by = levels[1:level], type = "inner")
      
      # Flag forbidden children (those that have been linked already)
      if(!is.null(minimalist.links)){
        link.children <- join(link.children, 
                               cbind(minimalist.links, minimalist = 0), type = "left", by = c("list.id", "ethno.id"))
        link.children$minimalist[is.na(link.children$minimalist)] <- 1
      } else {
        link.children$minimalist <- 1
      }
      
      # Save these links
      links <- rbind(links, link.children)
      minimalist.links <- unique(rbind(minimalist.links, unique(link.children[,c("ethno.id", "list.id")]))) 
    }
    
    # Save linking level
    links$link.level <- level.x
    
    # Return
    return(links)
  }))
  
  # Add iso3c as missing if not coming from above
  if(!"iso3c" %in% colnames(links)){
    links$iso3c <- NA
  }
  
  # Drop duplicated link on level 17 (caused by $extension)
  links <- links[!(duplicated(links[,c("link.id", "list.id", "ethno.id", "link.level")]) & 
                         links$link.level == 17),]
  
  # Checks
  
  # ... no duplicate links within one list across levels
  check.1.df <- unique(links[,c("list.id", "ethno.id","level", "link.level")])
  stopifnot(!any(duplicated(check.1.df[,c("list.id", "ethno.id", "link.level")])))
  
  # # ... any link.id without links? // legacy code. 
  # check.2.df <- combined.df[!combined.df$link.id %in% 
  #                             unique(links$link.id[links$link.level == 17 & links$minimalist == 1]),]
  # if(nrow(check.2.df) > 0){
  #   print("Not linked to any language with minimalist link on level 17")
  #   print(check.2.df[,c("type","group", "link", "level", "iso3c")])
  # }
  # 
  
  # Finalize link table
  link.fin <- join(link.long[,c("group","list.id","link.id")],
                    links[,c("list.id", "link.level","link.id", "ethno.id","iso3c", "minimalist")],
                    type = "left",by = c( "list.id", "link.id"))
  
  # link.fin$link.id <- NULL
  colnames(link.fin)[colnames(link.fin) == "iso3c"] <- "ethno.iso3c"
  
  # Final check
  stopifnot(all(!duplicated(link.fin[,c("list.id", "link.level","link.id", "ethno.id")])))
  
  # Return
  return(link.fin)
}

