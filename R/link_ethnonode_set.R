###########################
# Link via set of shared ethnologue nodes
###########################


# (function) linking two lists

# Usage: 
# link <- link_any(link.a = list.dict$list.id[list.dict$type == "DHS"], 
#                    link.b = list.dict$list.id[list.dict$type == "AMAR"], 
#                    link.level = 17, link.on.country = T, minimalist = 0, 
#                    ethno.same.country = F, link.dict = link.dict, list.dict = list.dict)
#' @name link_any
#' @title Set link, work horse function
#' @import plyr
#' @importFrom stats aggregate.data.frame
#' @keywords internal
#' @note Note that, for coding reasons,
#'   'dialects' are level 17 in our language dictionary.
#'   Levels 15 and 16 are languages, identified via their
#'   name (level 15) and iso code (level 16). 
link_any <- function(link.a, link.b, link.level, 
                      link.on.country = T, 
                      minimalist = F, 
                      ethno.same.country = F, link.dict, list.dict,
                      drop.a.threshold = 0, drop.b.threshold = 0){
  # require(plyr)
  
  ## Recode link level -- see Note
  if(link.level == "language"){
    link.level <- 16
  } else if(link.level == "dialect"){
    link.level <- 17
  } else if(link.level == 16){
    link.level <- 17
  }
  
  # Select data
  # ... subset to level
  link.dict <- link.dict[link.dict$link.level == link.level,]
  
  # ... subset to minimalist?
  if(minimalist){
    link.dict <- link.dict[link.dict$minimalist == 1,]
  }
  
  # ... subset to same-country-linkes
  if(ethno.same.country){
    link.dict <- join(link.dict, list.dict[,c("list.id", "iso3c")],
                       type = "left", by = "list.id")
    link.dict <- link.dict[as.logical(apply(link.dict[,c("iso3c", "ethno.iso3c")], 1, function(x){grepl(x[1], x[2])})),]
    link.dict$iso3c <- NULL
  }
  
  # ... add country of list (if link wished)
  if(link.on.country){
    link.dict <- join(link.dict, list.dict[,c("list.id", "iso3c")],
                       type = "left", by = "list.id")
  }
  
  # ... subset to lists to be linked
  df.link.a <- link.dict[link.dict$list.id %in% link.a & 
                             link.dict$link.level == link.level,]
  df.link.b <- link.dict[link.dict$list.id %in% link.b & 
                             link.dict$link.level == link.level,]
  
  
  # rename columns
  ren.cols <- c("group", "list.id")
  if(!link.on.country){
    ren.cols <- c(ren.cols, "iso3c")
  }
  colnames(df.link.a)[colnames(df.link.a) %in% ren.cols] <- 
    paste0("a.", colnames(df.link.a)[colnames(df.link.a) %in% ren.cols] )
  colnames(df.link.b)[colnames(df.link.b) %in% ren.cols] <- 
    paste0("b.", colnames(df.link.b)[colnames(df.link.b) %in% ren.cols] )
  
  # Join
  drop.cols <- c("iso3n","ethno.iso3c", "link", "link.id", "minimalist")
  if(link.on.country){
    link <- join(df.link.a[,!colnames(df.link.a) %in% drop.cols], 
                  df.link.b[,!colnames(df.link.b) %in% drop.cols],
                  by = c("ethno.id", "link.level", "iso3c"), type = "left")
  } else {
    link <- join(df.link.a[,!colnames(df.link.a) %in% drop.cols], 
                  df.link.b[,!colnames(df.link.b) %in% drop.cols],
                  by = c("ethno.id", "link.level"), type = "left")
  }
  link <- unique(link)
  
  # Subset of thresholds
  
  # ... for each B group
  
  # ... --- linked number of ethno.ids
  ei.link.num <- aggregate.data.frame(list(ei.link.num = rep(1, nrow(link))),
                                       link[,c("a.group", "a.list.id","b.group", "b.list.id")], FUN = sum)
  ei.org.num <- aggregate.data.frame(list(ei.org.num = rep(1, nrow(df.link.b))),
                                     df.link.b[,c("b.group", "b.list.id")], FUN = sum)
  ei.frac <- join(ei.link.num, ei.org.num, type = "left", by = c("b.group", "b.list.id"))
  ei.frac$ei.frac.b <- ei.frac$ei.link.num / ei.frac$ei.org.num
  
  # ... --- join
  link <- join(link,ei.frac[,c("a.group", "a.list.id","b.group", "b.list.id", "ei.frac.b")], type = "left", 
                by = c("a.group", "a.list.id","b.group", "b.list.id"), match = "first")
  
  # ... --- drop groups b if so whished
  if(is.numeric(drop.b.threshold)){
    link[link$ei.frac.b < drop.b.threshold & !is.na(link$ei.frac.b), 
          c("b.group", "b.list.id", "ei.frac.b")] <- NA
  }
  
  # ... for A -- b combination
  
  # ... --- linked number of ethno.ids by dyad
  ei.link.num <- aggregate.data.frame(list(ei.link.num = link$ethno.id),
                                       link[,c("a.group", "a.list.id", "b.group", "b.list.id")], 
                                       FUN = function(x){length(unique(x))})
  ei.org.num <- aggregate.data.frame(list(ei.org.num = rep(1, nrow(df.link.a))),
                                     df.link.a[,c("a.group", "a.list.id")], FUN = sum)
  ei.frac <- join(ei.link.num, ei.org.num, type = "left", by = c("a.group", "a.list.id"))
  ei.frac$ei.frac.a <- ei.frac$ei.link.num / ei.frac$ei.org.num
  
  # ... --- join
  link <- join(link,ei.frac[,c("a.group", "a.list.id", "b.group", "b.list.id", "ei.frac.a")], type = "left", 
                by = c("a.group", "a.list.id", "b.group", "b.list.id"), match = "first")
  
  # ... --- drop groups a if so whished
  if(is.numeric(drop.a.threshold)){
    link[link$ei.frac.a < drop.a.threshold & !is.na(link$ei.frac.a), 
         c("ei.frac.a", "b.group", "b.list.id", "ei.frac.b")] <- NA
  }
  
  # ... for A overall
  
  # ... --- linked number of ethno.ids
  ei.link.num <- aggregate.data.frame(list(ei.link.num = link$ethno.id),
                                       link[,c("a.group", "a.list.id", "b.list.id")], 
                                       FUN = function(x){length(unique(x))})
  ei.org.num <- aggregate.data.frame(list(ei.org.num = rep(1, nrow(df.link.a))),
                                     df.link.a[,c("a.group", "a.list.id")], FUN = sum)
  ei.frac <- join(ei.link.num, ei.org.num, type = "left", by = c("a.group", "a.list.id"))
  ei.frac$ei.frac.alla <- ei.frac$ei.link.num / ei.frac$ei.org.num
  
  # ... --- join
  link <- join(link,ei.frac[,c("a.group", "a.list.id", "b.list.id", "ei.frac.alla")], type = "left", 
                by = c("a.group", "a.list.id", "b.list.id"), match = "first")
  
  # ... for ensemble of B groups
  
  # ... --- linked number of ethno.ids
  ei.link.num <- aggregate.data.frame(list(ei.link.num = link$ethno.id),
                                       link[,c("a.group", "a.list.id", "b.list.id")], 
                                       FUN = function(x){length(unique(x))})
  ei.org.num <- join(df.link.b, unique(link[,c("a.group", "a.list.id", "b.group", "b.list.id")]),
                     by = c("b.group", "b.list.id"), type = "left")
  ei.org.num <- ei.org.num[!is.na(ei.org.num$a.list.id),]
  ei.org.num <- aggregate.data.frame(list(ei.org.num = ei.org.num$ethno.id),
                                     ei.org.num[,c("a.group", "a.list.id", "b.list.id")], 
                                     FUN = function(x){length(unique(x))})
  ei.frac <- join(ei.link.num, ei.org.num, type = "left", by = c("a.group", "a.list.id", "b.list.id"), match = "first")
  ei.frac$ei.frac.allb <- ei.frac$ei.link.num / ei.frac$ei.org.num
  
  # ... --- join
  link <- join(link,ei.frac[,c("a.group", "a.list.id", "b.list.id", "ei.frac.allb")], type = "left", 
                by = c("a.group", "a.list.id", "b.list.id"), match = "first")
  
  # Clean up
  link <- link[, !grepl("iso3c.", colnames(link), fixed = T)]
  
  # return
  return(link)
}


