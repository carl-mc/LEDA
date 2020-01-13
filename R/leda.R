######################
# LEDA Class
# For load full linkage information and linkage methods
######################

#' @title Linking Ethnic Data from Africa
#'
#' @description 
#'    Provides an interface to link ethnic groups from 12 different datasets to each other
#'    and calculate lingustic distances between them.
#'    
#' @section Usage:
#' \preformatted{
#' # Initialize
#' leda.obj <- LEDA$new()
#' 
#' # Apply any LEDA method
#' # leda.obj$method() ## not run.
#' }
#'
#'
#' @details 
#'    The LEDA package contains a full pipeline to link 
#'    ethnic datasets from Africa. The main strength of LEDA
#'    consists in leveraging the structure of the language
#'    tree to provide a flexible link between any two ethnic 
#'    group that are linked to the tree. 
#'    
#'    The package allows lists of ethnic groups to be linked to each 
#'    other using three main linkage types: binary linking based 
#'    on the relations of sets of language nodes associated with
#'    two groups; binary linking based on lingustic distances; 
#'    and a full computation of dyadic linguistic distances.
#'    
#'     Usage of a LEDA object is structured around 
#'     \emph{lists of ethnic groups}. These lists of groups 
#'     stem from the original datasets that have been joined 
#'     to the language tree. Lists are structured by data source,
#'     country, year, or, in the case of survey data, survey rounds. 
#'     Via the language tree, any two lists of ethnic groups
#'     can be linked to each other. 
#'     
#'     For full information on the LEDA project and methodology,
#'     read the \href{https://github.com/carl-mc/LEDA/raw/master/docs/LEDA_paper.pdf}{paper}. 
#'     
#'    When using the LEDA package, please cite:
#'    Müller-Crepon, Carl, Nils-Christian Bormann, and Yannick Pengl (2020). 
#'    \emph{Linking Ethnic Data from Africa}. Unpublished working paper.
#'      
#'
#' @name LEDA
#' @import R6
#'    igraph
#'    plyr
#'    stringr
#' @docType class
#' 
#' @examples
#' 
#' # Initialize linkage object
#' leda.obj <- LEDA$new()
#' 
#' # link Afrobarometer to FRT
#' 
#' ## Based on set relation
#' setlink <- leda.obj$link_set(
#'   lists.a = list(type = c("Afrobarometer"), 
#'   iso3c = "UGA"), 
#'   lists.b = list(type = c("FRT"),iso3c = "UGA"), 
#'   link.level = "dialect",   by.country = TRUE, 
#'   drop.b.threshold = 0, drop.ethno.id = TRUE)
#' head(setlink[,c("a.group","b.group")])
#' 
#' ## Nearest linguistic neighbor
#' mindistlink <- leda.obj$link_minlingdist(
#'  lists.a = list(type = c("Afrobarometer"), iso3c = "UGA"), 
#'  lists.b = list(type = c("FRT"),  iso3c = "UGA"),
#'  level = "dialect",
#'  by.country = TRUE, expand = FALSE,  delta = .5,
#'  agg_fun.a = mean, agg_fun.b = min)
#' head(mindistlink[,c("a.group","b.group")])
#' 
#' ## Within maximum linguistic distance
#' withindistlink <- leda.obj$link_withinlingdist(
#'   lists.a = list(type = c("Afrobarometer"),  iso3c = "UGA"), 
#'   lists.b = list(type = c("FRT"), iso3c = "UGA"),
#'   level = "dialect",
#'   max.distance = .1, by.country = TRUE,
#'   delta = .5, expand = FALSE, 
#'   agg_fun.a = mean, agg_fun.b = min)
#' head(withindistlink[,c("a.group","b.group")])
#' 
#' ## Compute pairwise linguistic distance
#' distance.df <- leda.obj$ling_distance(
#'    lists.a = list(type = c("Afrobarometer"),  iso3c = "UGA"),
#'    lists.b = list(type = c("FRT"), iso3c = "UGA"),
#'    level = "dialect",
#'    by.country = TRUE, delta = .5, 
#'    expand = FALSE,  
#'    agg_fun.a = mean, agg_fun.b = min)
#' head(distance.df[,c("a.group","b.group")])
#' @export
LEDA <- R6Class("LEDA",
                public = list(
                  ## Public data
                  
                  
                  ## Public functions
                  
                  ### Initialize object
                  #' @description
                  #' Initialize a new LEDA object
                  #' @examples
                  #' library(LEDA)
                  #' leda.obj <- LEDA$new()
                  #' 
                  initialize = function(){
                    # Load data
                    private$list.dict = readRDS(system.file("extdata", "input_listdict_df.rds", package = "LEDA"))
                    private$link.dict = readRDS(system.file("extdata", "link_dict_df.rds", package = "LEDA"))
                    private$ethno.df = readRDS(system.file("extdata", "input_ethnologue_df.rds", package = "LEDA"))
                    private$ethno.long = readRDS(system.file("extdata", "input_ethnologue_long.rds", package = "LEDA"))
                    
                    # Add source variable to link dictionary
                    private$link.dict$linksource <- "LEDA"
                    
                  },
                  

                  #' @description Returns a vector with the variables that define
                  #'    lists of ethnic groups in different datasets.
                  #'    
                  #' @return A vector.
                  #' 
                  #' @details The variables coded are the following:
                  #'     
                  #'     \code{cowcode}: Correlates of War code of country
                  #'     
                  #'     \code{iso3c}: 3-letter isocode of country
                  #'     
                  #'     \code{type}: Type of ethnic group dataset. 
                  #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
                  #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
                  #'     
                  #'     \code{marker}: Ethnic marker used in list. 
                  #'     \code{"ethnic group"}: Ethnic group / ethnicity.
                  #'     \code{"language"}: Language.     
                  #'     \code{"mtongue"}: Mother tongue.
                  #'     
                  #'     \code{groupvar} Variable name of ethnic group identifier 
                  #'     in original dataset.
                  #'     
                  #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
                  #'     
                  #'     \code{subround}: Subround of survey (DHS; SIDE)
                  #'     
                  #'     \code{year}: Year (EPR; IPUMS)
                  #'     
                  #'     \code{list.id}: ID of list of ethnic groups.
                  #'     
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Get list parameters
                  #' leda$show_list_parameters()
                  show_list_parameters = function(){
                    colnames(private$list.dict)
                  },
                  
                  
                  #' @description Returns the full dictionary of lists of ethnic groups that
                  #'    are included in the LEDA project. An example of a list is the 
                  #'    IPUMS census data from Ghana in 2000.
                  #' @return A DataFrame.
                  #' 
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Get list dictionaries
                  #' list.dict <- leda$get_list_dict()
                  #' head(list.dict)
                  get_list_dict = function(){
                    return(private$list.dict)
                  },
                  

                  #' @description Returns a subset of the dictionary of lists of ethnic groups that
                  #'    are included in the LEDA project. An example of a list is the 
                  #'    IPUMS census data from Ghana in 2000.
                  #'    
                  #' @param param_list List of parameter values to subset list dictionary. 
                  #'     The following fields are allowed: 
                  #'     
                  #'     \code{cowcode}: Correlates of War code of country
                  #'     
                  #'     \code{iso3c}: 3-letter isocode of country
                  #'     
                  #'     \code{type}: Type of ethnic group dataset. 
                  #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
                  #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
                  #'     
                  #'     \code{marker}: Ethnic marker used in list. 
                  #'     \code{"ethnic group"}: Ethnic group / ethnicity.
                  #'     \code{"language"}: Language.     
                  #'     \code{"mtongue"}: Mother tongue.
                  #'     
                  #'     \code{groupvar} Variable name of ethnic group identifier 
                  #'     in original dataset.
                  #'     
                  #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
                  #'     
                  #'     \code{subround}: Subround of survey (DHS; SIDE)
                  #'     
                  #'     \code{year}: Year (EPR; IPUMS)
                  #'     
                  #'     \code{list.id}: ID of list of ethnic groups.
                  #'     
                  #'     These are parameters are also returned by method 
                  #'     \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.
                  #'     
                  #' @return A DataFrame.
                  #' 
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Get list data for Afrobarometers in Uganda
                  #' leda$get_list_dict_subset(param_list = 
                  #'      list(type = "Afrobarometer", iso3c = c("UGA","KEN")))
                  get_list_dict_subset = function(param_list = list()){
                    ## Check 
                    stopifnot(all(names(param_list) %in% colnames(private$list.dict)))
                    
                    ## Return full or empty data frame if NULL or nothing selected
                    if(is.null(param_list)){
                      return(private$list.dict)
                    } else if(length(param_list) == 0){
                      return(private$list.dict[c(), , drop = F])
                    }
                    
                    ## Subset list directory
                    return.list.dict <- private$list.dict
                    for(p in seq_along(param_list)){
                      return.list.dict <- return.list.dict[return.list.dict[, names(param_list)[p]] %in% param_list[[p]],]
                    }
                    
                    ## Return
                    return(return.list.dict)
                  },

                  #' @description Returns the a subset of the IDs of lists of ethnic groups that
                  #'    are included in the LEDA project.
                  #'    
                  #' @param param_list List of parameter values to subset IDs from list dictionary. 
                  #'     The following fields are allowed: 
                  #'     
                  #'     \code{cowcode}: Correlates of War code of country
                  #'     
                  #'     \code{iso3c}: 3-letter isocode of country
                  #'     
                  #'     \code{type}: Type of ethnic group dataset. 
                  #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
                  #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
                  #'     
                  #'     \code{marker}: Ethnic marker used in list. 
                  #'     \code{"ethnic group"}: Ethnic group / ethnicity.
                  #'     \code{"language"}: Language.     
                  #'     \code{"mtongue"}: Mother tongue.
                  #'     
                  #'     \code{groupvar} Variable name of ethnic group identifier 
                  #'     in original dataset.
                  #'     
                  #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
                  #'     
                  #'     \code{subround}: Subround of survey (DHS; SIDE)
                  #'     
                  #'     \code{year}: Year (EPR; IPUMS)
                  #'     
                  #'     \code{list.id}: ID of list of ethnic groups.
                  #'     
                  #'     These are parameters are also returned by method 
                  #'     \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.
                  #'     
                  #' @return A vector of list.ids
                  #' 
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Get list IDs for Afrobarometers in Uganda
                  #' leda$get_list_ids(param_list = 
                  #'    list(type = "Afrobarometer", iso3c = c("UGA","KEN")))
                  get_list_ids = function(param_list = list()){
                    if(is.null(param_list)){
                      self$get_list_dict()$list.id
                    } else {
                      self$get_list_dict_subset(param_list)$list.id
                    }
                    
                  },
                  
                  
                  #' @description Returns a link table of ethnic groups contained in lists \emph{A} and \emph{B},
                  #'   based on their set relation. At the baseline, groups \emph{a} to \emph{b} are linked to each other
                  #'   as soon as they share any language node at the level of the language tree specified by \code{link_level}. 
                  #'   Links are provided between all lists in \emph{A} with every list in \emph{B} separately.
                  #'      
                  #'   The returned DataFrame contains at least one row per group \emph{a} 
                  #'   that has been linked to the ethnologue language tree. 
                  #'   If \emph{a} is not linked to any group \emph{b},
                  #'   the columns that contains linked groups \emph{b} are 
                  #'   set to missing. The returned DataFrame contains multiple 
                  #'   rows per group \emph{a} if \emph{a} is linked to multiple groups \emph{b}.
                  #'          
                  #' @param lists.a Vector of lists \emph{A}, identified via their 
                  #'   list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.
                  #'   Or a list of parameters that specify lists \emph{A}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.. 
                  #' 
                  #' @param lists.b Vector of lists \emph{B}, identified via their 
                  #'   list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.
                  #'   Or a list of parameters that specify lists \emph{B}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.. 
                  #'   
                  #' @param link.level Level on the linguistic tree on 
                  #'   which ethnic groups are linked to each other. 
                  #'   Must be one of: \code{1:16},
                  #'   or \code{'language'}, or \code{'dialect'}. \code{'language'} 
                  #'   corresponds to level 15, 
                  #'   or \code{'dialect'} corresponds to level 16. 
                  #' 
                  #' @param by.country Flag for linking lists only within 
                  #'   the same country (\code{by.country = TRUE}), 
                  #'   or also across countries (\code{by.country = FALSE}).
                  #'   Defaults to \code{TRUE} to avoid accidental 
                  #'   computation of a huge number of links.
                  #'   
                  #' @param drop.a.threshold Maximum share of language nodes 
                  #'   associated with \emph{a} that have to be 
                  #'  associated with group \emph{b} for a link to be dropped. 
                  #'  
                  #' @param drop.b.threshold Maximum share of language nodes 
                  #'   associated with \emph{b} that have to be 
                  #'  associated with group \emph{a} for a link to be dropped. 
                  #'  
                  #' @param drop.ethno.id Drop all ethnologue language IDs 
                  #'   that are used to link group \emph{a} with \emph{b}. If
                  #'  \code{FALSE}, the returned DataFrame has as many rows 
                  #'  per link between \emph{a} and \emph{b} as there are 
                  #'  language nodes the two groups share. 
                  #' 
                  #' 
                  #' @param add_listmetadata Adds metadate of lists 
                  #'  \emph{A} and \emph{B} to the output. Defaults
                  #'  to \code{TRUE}.
                  #' 
                  #' @return A DataFrame. Columns include the names and identifiers of 
                  #'   groups \emph{a} and \emph{b} and the \code{link.level} used for the 
                  #'   link. If \code{expand = TRUE}, the column \code{ethno.id} stores 
                  #'   the ID of each Ethnologue node used for a link. Columns
                  #'   \code{ei.frac.a}  and \code{ei.frac.b} store the fraction of language
                  #'   nodes of \emph{a} and \emph{b} covered by a link. 
                  #'   \code{ei.frac.alla} containes the fraction of nodes of \emph{a} covered by all
                  #'   groups \emph{b} linked to \emph{a}. \code{ei.frac.allb} contains the 
                  #'   fraction of the nodes of all groups \emph{b} covered the nodes of \emph{a}. 
                  #'   This information can be used to further finetune a link. 
                  #' 
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda.obj <- LEDA$new()
                  #' 
                  #' # link Afrobarometer to FRT
                  #' setlink <- leda.obj$link_set(lists.a = list(type = "Afrobarometer", 
                  #'                                           iso3c = c("UGA","KEN")),
                  #'                            lists.b = list(type = "FRT", 
                  #'                                           iso3c = c("UGA","KEN")), 
                  #'                            link.level = "dialect",  
                  #'                            by.country = TRUE, 
                  #'                            drop.a.threshold = 0, 
                  #'                            drop.b.threshold = 0, 
                  #'                            drop.ethno.id = TRUE, 
                  #'                            add_listmetadata = TRUE)
                  #' head(setlink)
                  link_set = function(lists.a, lists.b, 
                                      link.level, 
                                      by.country = T, 
                                      drop.a.threshold = 0, 
                                      drop.b.threshold = 0, 
                                      drop.ethno.id = T,
                                      add_listmetadata = T){
                    ## Get list IDs if link.a / link.b lists of selection parameters
                    
                    ### Side A
                    if(is.list(lists.a)){
                      lists.a <- self$get_list_ids(param_list = lists.a)
                    } else {
                      stopifnot(all(lists.a %in% private$list.dict$list.id))
                    }
                    if(length(lists.a) == 0){
                      stop("Your lists.a parameters do not identify any list of ethnic groups. ")
                    }
                    
                    ### Side B
                    if(is.list(lists.b)){
                      lists.b <- self$get_list_ids(param_list = lists.b)
                    } else {
                      stopifnot(all(lists.b %in% private$list.dict$list.id))
                    }
                    if(length(lists.b) == 0){
                      stop("Your lists.b parameters do not identify any list of ethnic groups. ")
                    }
                    
                    ## Check all arguments
                    fun_param_ls <- ls()
                    names(fun_param_ls) <- fun_param_ls
                    fun_param_ls <- lapply(fun_param_ls, get, envir = environment())
                    private$check_fun_parameters(fun_param_ls)
                    
                    ## link
                    link.df <- link_any(link.a = lists.a, link.b = lists.b, 
                                        link.level= link.level, 
                                        link.on.country = by.country, 
                                        minimalist = F, ## This is a legacy hook. 
                                        ethno.same.country = F, ## This is a legacy hook.
                                        link.dict = private$link.dict,
                                        list.dict = private$list.dict,
                                        drop.a.threshold = drop.a.threshold,
                                        drop.b.threshold = drop.b.threshold)
                    
                    ## Code link level to level
                    link.df$link.level <- link.level
                    
                   
                    ## Drop ethnologue ID
                    if(drop.ethno.id){
                      ## Drop ethno.id
                      link.df <- unique(link.df[, !colnames(link.df) %in% c("ethno.id")])
                      
                      ## drop unnecessary missings
                      link.df <- link.df[!((duplicated(link.df[,c("a.list.id", "a.group")]) | 
                                              duplicated(link.df[,c("a.list.id", "a.group")], fromLast = T)) &
                                                       is.na(link.df$b.group)),]
                      
                    }
                    
                    ## Fill and check
                    link.df <- private$fill_link(link.df, lists.a)
                    
                    ## Add list metadata
                    if(add_listmetadata){
                      link.df <- private$add_listmetadata(link.df)
                    }
                    
                    ## Return
                    return(link.df)
                  },
                  

                  
                  #' @description Returns a table of linguistic distances between all ethnic groups 
                  #'   contained in lists \emph{A} and \emph{B}. The 
                  #'   linguistic distance between two languages \eqn{L_1} and \eqn{L_2}
                  #'   is computed as
                  #'   
                  #'   \deqn{ 1 - ((d(L_1,R) + d(L_2,R) - d(L_1,L_2)) / (d(L_1,R) + d(L_2,R)))^{\delta} }
                  #'   
                  #'   where \eqn{d(L_i,R)} is the length of path from a
                  #'   language to the tree's origin and 
                  #'   \eqn{d(L_1,L_2)} is the length of 
                  #'   the shortest path from the first to the second 
                  #'   language. \eqn{\delta} is an exponent to discount 
                  #'   short distances on the tree. 
                  #'   
                  #'   Because we oftentimes link ethnic groups 
                  #'   \emph{a} and \emph{b} to several languages, we have to aggregate 
                  #'   the resulting distance matrix, for example by taking the minimum 
                  #'   distance between all languages \eqn{L_a} in group \eqn{a} to 
                  #'   all languages \eqn{L_b} associated with group \eqn{b}. 
                  #'   
                  #'   
                  #'   
                  #' @param lists.a Vector of lists \emph{A}, identified via their 
                  #'   list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.
                  #'   Or a list of parameters that specify lists \emph{A}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.. 
                  #' 
                  #' @param lists.b Vector of lists \emph{B}, identified via
                  #'   their list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.
                  #'   Or a list of parameters that specify lists \emph{B}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.. 
                  #'    
                  #' @param level Level on the linguistic tree from
                  #'   which distances are computed. 
                  #'   Must be \code{'language'} or \code{'dialect'}. 
                  #'   \code{'language'} corresponds to level 15, 
                  #'   or \code{'dialect'} corresponds to level 16. 
                  #'   
                  #' @param by.country Flag for computing distances
                  #'   only between groups in the   
                  #'   same country (\code{by.country = TRUE}), 
                  #'   or also across countries (\code{by.country = FALSE}). 
                  #'   Defaults to \code{TRUE} to avoid accidental 
                  #'   computation of a huge number of distances.
                  #'   
                  #' @param expand Expand the language tree so that all 
                  #'   languages are located on level 15 or not. 
                  #'   If \code{FALSE} languages are located on their 
                  #'   original position in the linguistic tree, 
                  #'   which can be considerably closer to the root of 
                  #'   the tree. Defaults to \code{FALSE} for 
                  #'   reasons explained in the paper.
                  #'   
                  #' @param delta Delta parameter used to discount 
                  #'   short distances on the language tree. 
                  #'   See \href{#method-ling_distance}{\code{LEDA$ling_distance()}}. for details.
                  #'   
                  #' @param agg_fun.a Function used aggregate linguistic distances 
                  #'   across the nodes associated with group \emph{a} to group \emph{b} 
                  #'   in the (common) cases where \emph{a} is associated with multiple
                  #'   language nodes. Defaults to \code{min}. 
                  #'   
                  #' @param agg_fun.b Function used aggregate linguistic distances 
                  #'   across the nodes associated with group \emph{b} to each node
                  #'   associated with group \emph{a}. 
                  #'   in the (common) cases where \emph{b} is associated with multiple
                  #'   language nodes. Defaults to \code{min}. 
                  #' 
                  #' @param add_listmetadata Adds metadate of lists 
                  #'  \emph{A} and \emph{B} to the output. Defaults
                  #'  to \code{TRUE}.
                  #'   
                  #' @return A DataFrame. Columns include the names and identifiers of 
                  #'   groups \emph{a} and \emph{b}. Column \code{distance} stores
                  #'   the linguistic distance between groups \emph{a} and \emph{b}.
                  #'   
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda.obj <- LEDA$new()
                  #' 
                  #' # link Afrobarometer to FRT
                  #' ling.distance <- leda.obj$ling_distance(
                  #'    lists.a = list(type = c("Afrobarometer"),  iso3c = "UGA"),
                  #'    lists.b = list(type = c("FRT"), iso3c = "UGA"),
                  #'    level = "dialect",
                  #'    by.country = TRUE, delta = .5, 
                  #'    expand = FALSE,  
                  #'    agg_fun.a = mean, agg_fun.b = min)
                  #' head(ling.distance)
                  ling_distance = function(lists.a, lists.b, by.country = T, 
                                           delta = .5, expand = TRUE, 
                                           level = c("dialect", "language"),
                                           agg_fun.a = min, agg_fun.b = min,
                                           add_listmetadata = T){
                    ## Get list IDs if link.a / link.b lists of selection parameters
                    
                    ### Side A
                    if(is.list(lists.a)){
                      lists.a <- self$get_list_ids(param_list = lists.a)
                    } else {
                      stopifnot(all(lists.a %in% private$list.dict$list.id))
                    }
                    if(length(lists.a) == 0){
                      stop("Your lists.a parameters do not identify any list of ethnic groups. ")
                    }
                    
                    ### Side B
                    if(is.list(lists.b)){
                      lists.b <- self$get_list_ids(param_list = lists.b)
                    } else {
                      stopifnot(all(lists.b %in% private$list.dict$list.id))
                    }
                    if(length(lists.b) == 0){
                      stop("Your lists.b parameters do not identify any list of ethnic groups. ")
                    }
                    
                    ## Check all arguments
                    fun_param_ls <- ls()
                    names(fun_param_ls) <- fun_param_ls
                    fun_param_ls <- lapply(fun_param_ls, get, envir = environment())
                    private$check_fun_parameters(fun_param_ls)
                    
                    ## Make language graph if does not exist
                    if(is.null(private$ethno.g)){
                      private$ethno.g <- private$make_ling_tree()
                    }
                    
                    ## Compute linguistic distance
                    ling.dist <- ling_dist_df(link.a = lists.a, link.b = lists.b, 
                                              link.dict = private$link.dict, 
                                              list.dict = private$list.dict, 
                                              by.country = by.country, 
                                              level = level,
                                              delta = delta, expand = expand, 
                                              agg_fun.a = agg_fun.a, 
                                              agg_fun.b = agg_fun.b,
                                              ethno.df = private$ethno.df, ethno.long = private$ethno.long,
                                              ethno.g = private$ethno.g)

                    
                    ## Add list metadata
                    if(add_listmetadata){
                      ling.dist <- private$add_listmetadata(ling.dist)
                    }
                    
                    ## Return
                    return(ling.dist)
                  },
                  

                  
                  #' @description Returns a link table of ethnic groups contained in lists \emph{A} and \emph{B}. 
                  #'   Each for each list of ethnic groups in \emph{A} and \emph{B}, each group \emph{a} is linked to 
                  #'   all groups \emph{b} within a linguistic distance specified by \code{max.distance}. 
                  #'   Note that group \emph{a} can be therefore linked to several groups \emph{b}. 
                  #'   Links are provided between all lists in \emph{A} with every list in \emph{B} separately.
                  #'   
                  #'   The returned DataFrame contains at least one row per group \emph{a} 
                  #'   that has been linked to the ethnologue language tree. 
                  #'   If \emph{a} is not linked to any group \emph{b},
                  #'   the columns that contains linked groups \emph{b} are 
                  #'   set to missing. The returned DataFrame contains multiple 
                  #'   rows per group \emph{a} if \emph{a} is linked to multiple groups \emph{b}.
                  #'   
                  #'   
                  #' @param lists.a Vector of lists \emph{A}, identified via their 
                  #'   list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.. 
                  #'   Or a list of parameters that specify lists \emph{A}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.. 
                  #' 
                  #' @param lists.b Vector of lists \emph{B}, identified via
                  #'   their list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.. 
                  #'   Or a list of parameters that specify lists \emph{B}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.. 
                  #'      
                  #' @param level Level on the linguistic tree from
                  #'   which distances are computed. 
                  #'   Must be \code{'language'} or \code{'dialect'}. 
                  #'   \code{'language'} corresponds to level 15, 
                  #'   or \code{'dialect'} corresponds to level 16. 
                  #'   
                  #' @param by.country Flag for linking lists only within  
                  #'   the same country (\code{by.country = TRUE}), 
                  #'   or also across countries (\code{by.country = FALSE}). 
                  #'   Defaults to \code{TRUE} to avoid accidental 
                  #'   computation of a huge number of links.
                  #'   
                  #' @param max.distance Maximum linguistic distance. 
                  #'   All pairs of groups \emph{a} and \emph{b} with a
                  #'   distance smaller or equal \code{max.distance}
                  #'   are linked to each other.
                  #'   
                  #' @param expand Expand the language tree so that all 
                  #'   languages are located on level 15 or not. 
                  #'   If \code{FALSE} languages are located on their 
                  #'   original position in the linguistic tree, 
                  #'   which can be considerably closer to the root of 
                  #'   the tree. Defaults to \code{FALSE} for 
                  #'   reasons explained in the paper.
                  #'   
                  #' @param delta Delta parameter used to discount 
                  #'   short distances on the language tree. 
                  #'   Affect the links returned. 
                  #'   See \href{#method-ling_distance}{\code{LEDA$ling_distance()}}. for details.
                  #'   
                  #' @param agg_fun.a Function used aggregate linguistic distances 
                  #'   across the nodes associated with group \emph{a} to group \emph{b} 
                  #'   in the (common) cases where \emph{a} is associated with multiple
                  #'   language nodes. Defaults to \code{min}. 
                  #'   
                  #' @param agg_fun.b Function used aggregate linguistic distances 
                  #'   across the nodes associated with group \emph{b} to each node
                  #'   associated with group \emph{a}. 
                  #'   in the (common) cases where \emph{b} is associated with multiple
                  #'   language nodes. Defaults to \code{min}. 
                  #' 
                  #' @param add_listmetadata Adds metadate of lists 
                  #'  \emph{A} and \emph{B} to the output. Defaults
                  #'  to \code{TRUE}.
                  #'  
                  #' @return A DataFrame. Columns include the names and identifiers of 
                  #'   groups \emph{a} and \emph{b}. Column \code{distance} stores
                  #'   the linguistic distance between groups \emph{a} and \emph{b}.
                  #' 
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda.obj <- LEDA$new()
                  #' 
                  #' # link Afrobarometer to FRT
                  #' link.withindist <- leda.obj$link_withinlingdist(
                  #'   lists.a = list(type = c("Afrobarometer"),  iso3c = "UGA"), 
                  #'   lists.b = list(type = c("FRT"), iso3c = "UGA"),
                  #'   level = "dialect",
                  #'   max.distance = .1, by.country = TRUE,
                  #'   delta = .5, expand = FALSE, 
                  #'   agg_fun.a = mean, agg_fun.b = min)
                  #' head(link.withindist)
                  link_withinlingdist = function(lists.a, lists.b, max.distance,
                                                 by.country = T,
                                                 level = c("dialect", "language"),
                                                 delta = .5, expand = FALSE, 
                                                 agg_fun.a = min, agg_fun.b = min,
                                                 add_listmetadata = T){
                    
                    ## Calculate liguistic distance
                    dist.df <- self$ling_distance(lists.a, lists.b, 
                                                  by.country = by.country,
                                                  level = level,
                                                  delta = delta, expand = expand, 
                                                  agg_fun.a = agg_fun.a, 
                                                  agg_fun.b = agg_fun.b,
                                                  add_listmetadata = F)
                    
                    ## Get linkes below threshold distance
                    link.df <- dist.df[dist.df$distance <= max.distance,]
                    
                    ## Fill and check
                    link.df <- private$fill_link(link.df, lists.a)
                    
                    ## Add list metadata
                    if(add_listmetadata){
                      link.df <- private$add_listmetadata(link.df)
                    }
                    
                    ## Return
                    return(link.df)
                  },
                  

                  
                  #' @description Returns a link table of ethnic groups contained in lists \emph{A} and \emph{B}. 
                  #'   Each for each list of ethnic groups in \emph{A} and \emph{B}, each group \emph{a} is linked to its closest 
                  #'   linguistic neighbour \emph{b}. Note that group \emph{a} can be linked to several groups \emph{b} if they are
                  #'   equidistant to \emph{a}. 
                  #'   Links are provided between all lists in \emph{A} with every list in \emph{B} separately.
                  #'   
                  #'   The returned DataFrame contains at least one row per group \emph{a} 
                  #'   that has been linked to the ethnologue language tree. 
                  #'   If \emph{a} is not linked to any group \emph{b},
                  #'   the columns that contains linked groups \emph{b} are 
                  #'   set to missing. The returned DataFrame contains multiple 
                  #'   rows per group \emph{a} if \emph{a} is linked to multiple groups \emph{b}.
                  #'   
                  #' @param lists.a Vector of lists \emph{A}, identified via their 
                  #'   list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.. 
                  #'   Or a list of parameters that specify lists \emph{A}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.
                  #' 
                  #' @param lists.b Vector of lists \emph{B}, identified via
                  #'   their list.id returned by method \href{#method-get_list_ids}{\code{LEDA$get_list_ids()}}.. 
                  #'   Or a list of parameters that specify lists \emph{B}. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.
                  #'      
                  #' @param level Level on the linguistic tree from
                  #'   which distances are computed. 
                  #'   Must be \code{'language'} or \code{'dialect'}. 
                  #'   \code{'language'} corresponds to level 15, 
                  #'   or \code{'dialect'} corresponds to level 16. 
                  #'   
                  #' @param by.country Flag for linking lists only within  
                  #'   the same country (\code{by.country = TRUE}), 
                  #'   or also across countries (\code{by.country = FALSE}). 
                  #'   Defaults to \code{TRUE} to avoid accidental 
                  #'   computation of a huge number of links.
                  #'   
                  #' @param expand Expand the language tree so that all 
                  #'   languages are located on level 15 or not. 
                  #'   If \code{FALSE} languages are located on their 
                  #'   original position in the linguistic tree, 
                  #'   which can be considerably closer to the root of 
                  #'   the tree. Defaults to \code{FALSE} for 
                  #'   reasons explained in the paper.
                  #'   
                  #' @param delta Delta parameter used to discount 
                  #'   short distances on the language tree. 
                  #'   Does not affect the links, only the absolute
                  #'   linguistic distance associated with them 
                  #'   (but not their rank).
                  #'   See \href{#method-ling_distance}{\code{LEDA$ling_distance()}}. for details.
                  #'   
                  #' @param agg_fun.a Function used aggregate linguistic distances 
                  #'   across the nodes associated with group \emph{a} to group \emph{b} 
                  #'   in the (common) cases where \emph{a} is associated with multiple
                  #'   language nodes. Defaults to \code{min}. 
                  #'   
                  #' @param agg_fun.b Function used aggregate linguistic distances 
                  #'   across the nodes associated with group \emph{b} to each node
                  #'   associated with group \emph{a}. 
                  #'   in the (common) cases where \emph{b} is associated with multiple
                  #'   language nodes. Defaults to \code{min}. 
                  #' 
                  #' @param add_listmetadata Adds metadate of lists 
                  #'  \emph{A} and \emph{B} to the output. Defaults
                  #'  to \code{TRUE}.
                  #' 
                  #' @return A DataFrame. Columns include the names and identifiers of 
                  #'   groups \emph{a} and \emph{b}. Column \code{distance} stores
                  #'   the linguistic distance between groups \emph{a} and \emph{b}.
                  #' 
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda.obj <- LEDA$new()
                  #' 
                  #' # link Afrobarometer to FRT
                  #' link.mindist <- leda.obj$link_minlingdist(
                  #'  lists.a = list(type = c("Afrobarometer"), iso3c = "UGA"), 
                  #'  lists.b = list(type = c("FRT"),  iso3c = "UGA"),
                  #'  level = "dialect",
                  #'  by.country = TRUE, expand = FALSE,  delta = .5,
                  #'  agg_fun.a = mean, agg_fun.b = min)
                  #'  
                  #' head(link.mindist)
                  link_minlingdist = function(lists.a, lists.b,
                                              level = c("dialect", "language"),
                                              by.country = T,
                                              expand = FALSE, 
                                              delta = .5,
                                              agg_fun.a = min, 
                                              agg_fun.b = min,
                                              add_listmetadata = T){
                    
                    ## Calculate liguistic distance
                    dist.df <- self$ling_distance(lists.a, lists.b, 
                                                  by.country = by.country,
                                                  delta = delta, ## can be constant since does not affect ranking
                                                  expand = expand,
                                                  level = level,
                                                  agg_fun.a = agg_fun.a, 
                                                  agg_fun.b = agg_fun.b,
                                                  add_listmetadata = F)
                    
                    ## Get best link by group a
                    agg.dist.df.a <- aggregate.data.frame(dist.df[, "distance", drop = F],
                                                          dist.df[, c("a.group", "a.list.id")],
                                                          FUN = min)
                    agg.dist.df.a$link <- 1
                    
                    ## Join with distance
                    link.df <- join(dist.df, agg.dist.df.a, by =  c("a.group", "a.list.id", "distance"),
                                    type = "left", match = "all")
                    
                    ## Subset
                    link.df <- link.df[link.df$link == 1 & !is.na(link.df$link),, drop = F]
                    link.df$link <- NULL
                    
                    ## Fill and check
                    link.df <- private$fill_link(link.df, lists.a)
                    
                    ## Add list metadata
                    if(add_listmetadata){
                      link.df <- private$add_listmetadata(link.df)
                    }
                    
                    
                    ## Return
                    return(link.df)
                    
                  },
                  

                  
                  #' @description Retrieve the raw coding of links 
                  #'    between ethnic groups
                  #'    contained in group lists specified by \code{param_list} 
                  #'    to the language tree.  The function returns the raw data
                  #'    the LEDA object is based upon. 
                  #'    
                  #' @param param_list List of parameter values to subset lists of 
                  #'     ethnic groups.  
                  #'     The following fields are allowed: 
                  #'     
                  #'     \code{cowcode}: Correlates of War code of country
                  #'     
                  #'     \code{iso3c}: 3-letter isocode of country
                  #'     
                  #'     \code{type}: Type of ethnic group dataset. 
                  #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
                  #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
                  #'     
                  #'     \code{marker}: Ethnic marker used in list. 
                  #'     \code{"ethnic group"}: Ethnic group / ethnicity.
                  #'     \code{"language"}: Language.     
                  #'     \code{"mtongue"}: Mother tongue.
                  #'     
                  #'     \code{groupvar} Variable name of ethnic group identifier 
                  #'     in original dataset.
                  #'     
                  #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
                  #'     
                  #'     \code{subround}: Subround of survey (DHS; SIDE)
                  #'     
                  #'     \code{year}: Year (EPR; IPUMS)
                  #'     
                  #'     \code{list.id}: ID of list of ethnic groups.
                  #'     
                  #'     These are parameters are also returned by method 
                  #'     \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}.
                  #'    
                  #' @examples
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Get list parameters
                  #' leda$get_raw_ethnolinks(param_list = 
                  #'    list(type = "Afrobarometer", iso3c = "UGA"))
                  get_raw_ethnolinks = function(param_list){
                    
                    ## Get ethno links
                    private$load_raw_ethnolinks(param_list)
                  },
                  
                  #' @description Get types of group lists in LEDA object
                  #' 
                  #' @examples
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Get list parameters
                  #' leda$get_types()
                  get_types = function(){
                    types <- unique(self$get_list_dict()$type)
                    types[order(types)]
                  },

                  #' @description Based on an input of ethnic group
                  #'   names, the function returns a link table between 
                  #'   the groups and automatically found likely matches
                  #'   on the language tree. These non-authorative 
                  #'   'suggestions' are 
                  #'   identified via a fuzzy string match of input
                  #'   group names with (1) the names of nodes on the 
                  #'   langauge tree, including dialects and alternative
                  #'   names, as well as (2) the names of groups that
                  #'   have been previously linked to the tree as
                  #'   contained in the respective LEDA object. 
                  #'   
                  #' 
                  #' @param group.df A \code{DataFrame} that contains the
                  #'   names of ethnic groups to be linked to the language
                  #'   tree, as well as any other (meta)data whished to retain. 
                  #'   
                  #' @param groupvar String containing the name of the 
                  #'   column in \code{group.df} that contains the 
                  #'   names of ethnic groups. 
                  #'   
                  #'      
                  #' @param by.country Logical determining whether ethnic groups
                  #'   names should be string matched separately within and outside the
                  #'   country they belong. Setting the parameter \code{TRUE}
                  #'   leads to more plausible matches, but requires a
                  #'   column \code{iso3c} in \code{group.df}. Column \code{iso3c}
                  #'   has to contain valid 3-letter iso codes of African 
                  #'   countries. See e.g. the R-package \code{countrycode}. 
                  #'   
                  #' @param return Logical determining whether the reulting 
                  #'   link table shall be returned.
                  #'   
                  #' @param save.path String of the path to which the reulting
                  #'   link table is stored, as a .csv file. If \code{NULL} (the default),
                  #'   nothing is stored.  
                  #'   
                  #' @param overwrite Logical determining whether a previously
                  #'   existing file located by \code{save.path} is overwritten. 
                  #'   
                  #' @param prev_link_param_list Parameters that determine the 
                  #'   subset of previous links between ethnic group lists and
                  #'   the language tree to automatically retrieve link suggestions from. 
                  #'   If \code{NULL} (the default), all available lists are used. 
                  #'   See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}. for details.
                  #'   
                  #' @param levenshtein.threshold Threshold Levenshtein string 
                  #'   distance below which a fuzzy string match is returned.
                  #'   
                  #' @param levenshtein.costs Vector of costs used to compute 
                  #'   Levenshtein string distance. See \code{utils::adist} for details. 
                  #'   
                  #'
                  #' 
                  #' 
                  #' @details 
                  #' 
                  #'   The automatic links should not be regarded as authorative,
                  #'   but merely as a help to facilitate the coding, 
                  #'   which proceeds outside the LEDA environment from
                  #'   this point onwards. The final table returned by the
                  #'   function has a column called \code{link}, which is 
                  #'   to be filled by the user, using the automatically 
                  #'   generated suggestions
                  #'   and secondary data sources. Once this coding is completed,
                  #'   the table can be added to the LEDA object with the method
                  #'   \href{#method-get_tree_links}{\code{LEDA$add_tree_links()}}..
                  #' 
                  #' @return 
                  #'   A \code{DataFrame} of the same height as \code{group.df}.
                  #'   In addition to the columns of \code{group.df}, 
                  #'   it contains the following columns:
                  #'   
                  #'   \code{auto_link_org}: Language tree nodes 
                  #'     (levels 1:14; languages, i.e. level 15) 
                  #'     matched via their original (org) name. 
                  #'   
                  #'   \code{auto_link_alt}: Languages (level 15) matched
                  #'     via their alternative name.
                  #'   
                  #'   \code{auto_link_dial}: Dialects matched via their
                  #'     name or alternative name. 
                  #'   
                  #'   \code{auto_link_prev}: Language tree nodes 
                  #'     found via a fuzzy string match of input groups
                  #'     to the groups previously linked to the language 
                  #'     tree (potentially subsetted by \code{prev_link_param_list}).
                  #'   
                  #'   \code{auto_link_foreign}: If \code{by.country}, 
                  #'     same four fields as above for langauges from
                  #'     'foreign' countries, but pasted into one
                  #'     single string. 
                  #'   
                  #'   \code{link} Empty column for the final link, 
                  #'     to be filled by the user.
                  #'     
                  #'   \code{comment} Empty column for comments on the final link, 
                  #'     to be filled by the user.
                  #'     
                  #'   \code{source} Empty column for the source of the final link, 
                  #'     to be filled by the user.
                  #'     
                  #'   
                  #'   Multiple matches are combined by pasting the languague names
                  #'   separated by a '|'. The matched language nodes' tree level 
                  #'   is indicated behind its name in '[]', with L1:L14 denoting 
                  #'   super-language levels, 'lang' denoting languages, and 'dial'
                  #'   denoting dialects. This coding format should be maintained 
                  #'   when filling the column \code{link} with the final link of groups
                  #'   to the language tree. 
                  #'   
                  #'   
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Make or load some dataset of ethnic groups
                  #' new.groups.df <- data.frame(group_name = 
                  #'   c("Asante", "Grusi", "Akan"),iso3c = c("GHA"),
                  #'   type = "My Survey in Ghana",
                  #'   marker = "ethnic group",
                  #'   stringsAsFactors = FALSE)
                  #' 
                  #' 
                  #' # Prepare a new link table 
                  #' newlink.df <- leda$prepare_newlink_table(
                  #'   group.df = new.groups.df, 
                  #'   groupvar = "group_name",
                  #'   by.country = TRUE, 
                  #'   return = TRUE, save.path =  NULL, 
                  #'   overwrite = TRUE, prev_link_param_list = NULL,
                  #'   levenshtein.threshold = .2,
                  #'   levenshtein.costs = 
                  #'   c(insertions = 1,deletions = 1, substitutions = 1))
                  #' 
                  #' 
                  prepare_newlink_table = function(group.df,
                                                    groupvar, 
                                                    by.country = FALSE, 
                                                    return = TRUE, 
                                                    save.path = NULL, overwrite = FALSE,
                                                    prev_link_param_list = NULL,
                                                    levenshtein.threshold = .2,
                                                    levenshtein.costs = c(insertions = 1,deletions = 1, substitutions = 1)){
                    # Check iso3c
                    if(by.country & !"iso3c" %in% colnames(group.df)){
                      stop("group.df has no idvar called 'iso3c' --  this is the 3-letter country ISO code. Cannot link by country.")
                    }
                    
                    

                    # Get langauge matches
                    if(by.country){
                      link.df <- do.call(rbind, lapply(unique(group.df$iso3c), function(c){
                        ## Own Country
                        
                        ### Prepare previous link parameters
                        this.param_list <- prev_link_param_list
                        if(any(names(this.param_list) == "iso3c")){
                          this.param_list$iso3c <- this.param_list$iso3c[this.param_list$iso3c == c]
                        } else {
                          this.param_list$iso3c <- c
                        }
                        
                        ### Link
                        own.link.df <-  auto_match_all(groups = group.df$group[group.df$iso3c == c], 
                                                       ethno.df = private$ethno.df[private$ethno.df$iso3c == c, , drop = F], 
                                                       prev.link.df = self$get_raw_ethnolinks(param_list = this.param_list), 
                                                       threshold = levenshtein.threshold,
                                                       costs = levenshtein.costs)
                        
                        ## Foreign countries
                        
                        ### Prepare previous link parameters
                        this.param_list <- prev_link_param_list
                        if(any(names(this.param_list) == "iso3c")){
                          this.param_list$iso3c <- this.param_list$iso3c[this.param_list$iso3c != c]
                        } else {
                          this.param_list$iso3c <- unique(private$list.dict$iso3c)
                          this.param_list$iso3c <- this.param_list$iso3c[this.param_list$iso3c != c]
                        }
                        
                        ### Link
                        for.link.df <-  auto_match_all(groups = group.df$group[group.df$iso3c == c], 
                                                       ethno.df = private$ethno.df[private$ethno.df$iso3c != c, , drop = F], 
                                                       prev.link.df = self$get_raw_ethnolinks(param_list = this.param_list), 
                                                       threshold = levenshtein.threshold,
                                                       costs = levenshtein.costs)
                        
                        ### Collapse
                        for.link.df$auto_link_foreign <- paste0("Org: ", for.link.df$auto_link_org,
                                                                "|--|Alt: ", for.link.df$auto_link_alt,
                                                                "|--|Dial: ", for.link.df$auto_link_dial,
                                                                "|--|Prev: ", for.link.df$auto_link_prev)
                        
                        ### Add to own country link
                        fin.link.df <- cbind(own.link.df, auto_link_foreign = for.link.df$auto_link_foreign)
                        
                        ## return
                        return(fin.link.df)
                      }))
                    } else {
                      # Link groups to language tree and previous links
                      link.df <- auto_match_all(groups = group.df$group, 
                                                ethno.df = private$ethno.df, 
                                                prev.link.df = self$get_raw_ethnolinks(param_list = prev_link_param_list), 
                                                threshold = levenshtein.threshold,
                                                costs = levenshtein.costs)
                    }
                    
                    # Add columns for link, comment, and source
                    link.df[,c("link","comment","source")] <- as.character(NA)
                    
                    # Join with original data
                    link.df <- cbind(group.df, 
                                     link.df)
                    
                    # Drop potentially duplicated group variable
                    link.df <- link.df[,!duplicated(colnames(link.df)) | 
                                         colnames(link.df) != "group"]
                    
                    # Save
                    if(!is.null(save.path)){
                      if(file.exists(save.path) & !overwrite){
                        # Save nothing
                        warning(paste(save.path, "already exists. Nothing is saved."))
                      } else {
                        if(tools::file_ext(save.path) == "csv"){
                          # Throw warning if exists
                          if(file.exists(save.path)){
                            warning(paste("Overwriting", save.path, ""))
                          }
                          
                          # Save
                          write.csv(link.df, file = save.path, row.names = F, na = "")
                        } else if(!tools::file_ext(save.path) == ".csv"){
                          stop("save.path must have a .csv extension.")
                        }
                      }
                      
                    }
                    
                    # Return
                    if(return){
                      return(link.df)
                    }
                  }, 
                  


                  #' @description 
                  #'   Function to add a table that links a list of ethnic groups 
                  #'   with nodes on the language tree to the LEDA object.
                  #'   
                  #' 
                  #' @param tree.link.df 
                  #'   Table that contains te links between ethnic groups and 
                  #'   the langauge tree. It must contain the following variables:
                  #'   
                  #'   \code{group} Names of ethnic groups, of type "character". 
                  #'   
                  #'   \code{link} Names of language nodes linked to ethnic 
                  #'     group, of type "character". Multiple nodes linked to the 
                  #'     same ethnic group must be separated by a '|'. Names
                  #'     of language nodes must be contained in the Ethnologue 
                  #'     database 13. A node's level is specified in [], the node must 
                  #'     exist on that level. Level specifiers follow this form: L1:L14 denoting 
                  #'     super-language levels, 'lang' denoting languages, 'dial'
                  #'     denoting dialects, and iso denoting language ISO-codes. 
                  #'     E.g. "Akan [L9]", "Asante [dial]", or "aka [iso]". If no level 
                  #'     is specified and multiple langauge tree nodes share the same name,
                  #'     the one closest to the node is chosen. 
                  #'     Note that the safest way to avoid confusion is to provide iso-codes.
                  #'     
                  #'  \code{iso3c} (not required) Country identifier as 3-letter ISO code. 
                  #'     If provided, the algorithm gives preference to nodes of the
                  #'     language tree in the same country in cases where multiple nodes
                  #'     share the same name given by \code{link}. 
                  #'     
                  #'  \code{idvars} Additional variables that identify lists of ethnic groups in your
                  #'     data. See below. 
                  #'     
                  #' @param idvars Variables that identify lists of ethnic groups in your
                  #'     data. These should contain a list \code{type} (e.g. "My survey"),
                  #'     and be typically nested within countries and years. 
                  #'     See \href{#method-show_list_parameters}{\code{LEDA$show_list_parameters()}}. for the variables that 
                  #'     identify ethnic group lists in the LEDA dataset. 
                  #'     
                  #'     The unique combinations of \code{idvars} values in \code{group.df}
                  #'     are used to create new entries in the LEDA object's dictionary of 
                  #'     group lists \code{leda.obj$list.dict}.
                  #'     
                  #' @param type String that contains the 'type' of the group list to add,
                  #'   for example 'My survey'. Must not be one of the types already 
                  #'   in the LEDA project.
                  #'    
                  #' @details 
                  #'   The function links the input ethnic groups to the linguistic tree 
                  #'   contained in the package and thereby updates the LEDA object. 
                  #'   Once this has been done, the added group can be linked to all 
                  #'   ethnic group lists contained in the LEDA object. See examples below. 
                  #'   
                  #' @examples 
                  #' # Initialize linkage object
                  #' leda <- LEDA$new()
                  #' 
                  #' # Make toy link dataset
                  #' new.groups.df <- data.frame(
                  #'    group = c("Asante", "Mossi"), ## Ethnic group names
                  #'    link = c("Asante [dial]","Moore [org]"), ## Language nodes
                  #'    marker = "Ethnic self identification",
                  #'    iso3c = c("GHA", "BFA"), ## Countries
                  #'    stringsAsFactors = FALSE ## Everything as character
                  #'    )
                  #' 
                  #' # Add to LEDA
                  #' leda$add_tree_links(tree.link.df = new.groups.df, 
                  #'        idvars = c("iso3c", "type", "marker"),
                  #'        type = "My data")
                  #'        
                  #' # Use the new link
                  #' setlink <- leda$link_set(lists.a = list(type = c("My data")), 
                  #'     lists.b = list(type = c("Afrobarometer"), 
                  #'     round = 4, marker = "language",
                  #'     iso3c = c("GHA","BFA")), 
                  #'     link.level = 15, by.country = FALSE, 
                  #'     drop.b.threshold = 0, drop.ethno.id = TRUE)
                  #'     
                  #' head(setlink[, c("a.group", "b.group", "a.type", "b.type", "a.list.id", "b.list.id")])
                  add_tree_links = function(tree.link.df, idvars, type){
                    
                    # Check type
                    if(type %in% self$get_types()){
                      stop(paste("`type` must not be none of the types already in the LEDA object."))
                    }
                    
                    # Add type
                    tree.link.df$type <- as.character(type)
                    idvars <- unique(c(idvars, "type"))
                    
                    ## Check existence of all language nodes
                    check_lang_links(link.df = tree.link.df, ethno.long = private$ethno.long)
                    
                    ## Add new list entries to dictionary
                    
                    ### Old number of lists
                    old.dict.nrow <- nrow(private$list.dict)
                    
                    ### unique new lists
                    unique.input.lists <- unique(tree.link.df[, idvars, drop = F])
                    private$list.dict <- join(private$list.dict, unique.input.lists, 
                                           by = colnames(unique.input.lists)[colnames(unique.input.lists) %in% 
                                                                               colnames(private$list.dict)],
                                           type = "full", match = "first")
                    
                    ### Add list.ids where missing
                    private$list.dict$list.id[is.na(private$list.dict$list.id)] <-
                      seq_len(sum(is.na(private$list.dict$list.id))) + max(private$list.dict$list.id, na.rm = T)
                    
                    ### Print message
                    print(paste("Added", nrow(private$list.dict) - old.dict.nrow, "lists to list dictionary"))
                    
                    ### Add list.id to tree.link.df
                    tree.link.df <- join(tree.link.df, private$list.dict, 
                                         by = idvars,
                                         type = "left", match = "first")
                    
                    ### Drop iso3c if not in original idvars
                    if(!"iso3c" %in% idvars){
                      tree.link.df$iso3c <- NULL
                    }
                    
                    ## Link
                    dict.new <- consistent_lang_link(link.df = tree.link.df, ethno.long = private$ethno.long)
                    dict.new$linksource <- Sys.info()["user"]
                    
                    ## Check
                    stopifnot(all(!is.na(dict.new$list.id)))
                    stopifnot(all(colnames(private$link.dict) %in% colnames(dict.new)))
                    
                    
                    ## Add to link.dict
                    old.nrow <- nrow(private$link.dict)
                    private$link.dict <- rbind(private$link.dict,
                                            dict.new[,colnames(private$link.dict)])
                    
                    ## Delete duplicates
                    private$link.dict <- unique(private$link.dict)
                    
                    ## Add raw links
                    for(type in unique(tree.link.df$type)){
                      private$new.raw[[type]] <- tree.link.df[tree.link.df$type == type,]
                    }
                    
                    ## Print
                    if(nrow(private$link.dict) > old.nrow){
                      print("Added new entries to link dictionary.")
                    }
                    
                    ## Check
                    stopifnot(all(!is.na(private$link.dict$list.id)))
                    stopifnot(all(!is.na(private$list.dict$list.id)))
                    
                  }
                ),
                private = list(
                  
                  ## Private data objects
                  
                  ## field list.dict Dictionary of lists of ethnic groups
                  list.dict = NULL,
                  
                  ## field link.dict Dictionary of links of ethnic groups to ethnologue nodes
                  link.dict = NULL,
                  
                  ## field ethno.df Wide-format language data
                  ethno.df = NULL,
                  
                  ## field ethno.long Long-format language data
                  ethno.long = NULL,
                  
                  ## field ethno.g Language tree
                  ethno.g = NULL,
                  
                  ## field ethnologue.version Ethnologue version used
                  ethnologue.version = "16th edition",
                  
                  ## field new raw data
                  new.raw = list(),
                  
                  
                  ## Private functions
                  
                  ### Make language tree
                  make_ling_tree = function(){
                    # Make graph
                    private$ethno.g <- make_ling_tree(private$ethno.df)
                  },
                  
                  ### Check function parameters
                  check_fun_parameters = function(fun_param_ls){
                    
                    # Add errors
                    for(p in seq_along(fun_param_ls)){
                     
                      if(names(fun_param_ls)[p] %in% c("lists.a","lists.b")){
                        if(any(!fun_param_ls[[p]] %in% private$list.dict$list.id)){
                          stop(paste("All", names(fun_param_ls)[p], " must be in list dictionary."))
                        }
                        if(length(fun_param_ls[[p]]) == 0){
                          stop(paste("", names(fun_param_ls)[p], " does not identify a single list of groups."))
                        }
                      } else if(names(fun_param_ls)[p] == "link.level"){
                        if(any(!fun_param_ls[[p]] %in% 1:16 & !fun_param_ls[[p]] %in% c("language", "dialect"))){
                          stop(paste(names(fun_param_ls)[p], " must be in 1:16, or 'language' or 'dialect'."))
                        }
                      } else if(names(fun_param_ls)[p] %in% c("by.country", "minimalist", "expand",
                                                              "ethno.same.country", "drop.ethno.id",
                                                              "add_listmetadata")){
                        if(any(!is.logical(fun_param_ls[[p]]))){
                          stop(paste(names(fun_param_ls)[p], " must be in TRUE, FALSE"))
                        }
                      } else if(names(fun_param_ls)[p] == "drop.b.threshold"){
                        if(fun_param_ls[[p]] > 1 | fun_param_ls[[p]] < 0){
                          stop(paste(names(fun_param_ls)[p], " must be in [0,1]"))
                        }
                      }  else if(names(fun_param_ls)[p] == "param_list"){
                        if(!is.list(fun_param_ls[[p]]) | !is.null(fun_param_ls[[p]])){
                          stop(paste(names(fun_param_ls)[p], " must be either NULL or a list of parameters"))
                        }
                        if(!all(names(param_list) %in% colnames(private$list.dict))){
                          stop(paste(names(fun_param_ls)[p], " must only contain elements named after columns in the list dictionary."))
                        }
                      } 
                    }
                    
                    
                  },
                  
                  ### Get all link levels currently available
                  get_link_levels = function(){
                    unique(private$link.dict$link.level)
                  },
                  
                  ### Get highest ethnologue ID
                  get_highest_ethno_id = function(){
                    max(c(private$link.dict$ethno.id, private$ethno.long$ethno.id))
                  },
                  
                  ### Check id.vars used for adding data
                  check_add_ids = function(id.vars, side = NULL, link.df){
                    ## Check naming
                    if(!is.null(side)){
                      if(!all(substr(id.vars, 1, 2) == paste0(side, "."))){
                        stop(paste0("Please name ", side, ".idvars suing a ", side, ". prefix."))
                      }
                    }
                    
                    ## check whether they uniquely identify lists that are already in data
                    unique.input.lists <- unique(link.df[, id.vars, drop = F])
                    if(!is.null(side)){
                      colnames(unique.input.lists) <- substr(id.vars, 3, nchar(id.vars))
                    }
                    unique.input.lists <- unique.input.lists[unique.input.lists$type %in% private$list.dict$type,, drop = F]
                    joined.input.lists <- join(unique.input.lists, private$list.dict,
                                               by = colnames(unique.input.lists)[colnames(unique.input.lists) %in% 
                                                                                   colnames(private$list.dict)],
                                               type = "left", match = "all")
                    if(nrow(joined.input.lists) > nrow(unique.input.lists) |
                       any(duplicated(joined.input.lists$list.id))){
                      if(!is.null(side)){
                        stop(paste0( side, ".idvars do not uniquely identify lists that exist in list dictionary"))
                      } else {
                        stop(paste0("idvars do not uniquely identify lists that exist in list dictionary"))
                      }
                      
                    }
                  },
                  
                  
                  ### Add A groups linked to ethnologue without link in B
                  fill_link = function(link.df, lists.a){
                    # require(plyr)
                    
                    ## Check
                    if(is.list(lists.a)){
                      lists.a <- self$get_list_ids(param_list = lists.a)
                    } else {
                      stopifnot(all(lists.a %in% self$get_list_ids(NULL)))
                    }
                    stopifnot(all(is.numeric(lists.a)))
                    
                    ## Get all groups a from link.dict
                    all.a.groups <- private$link.dict[private$link.dict$link.level == 1 &
                                                     private$link.dict$list.id %in% lists.a,, drop = F]
                    all.a.groups <- unique(all.a.groups[, c("group", "list.id")])
                    colnames(all.a.groups) <- paste0("a.", colnames(all.a.groups))
                    
                    ## Join with link.df
                    link.df <- join(all.a.groups, link.df, type = "left",
                                    match = "all", by = colnames(all.a.groups))
                    
                    ## Return
                    return(link.df)
                  },
                  
                  ### Add list metadata to linkes
                  add_listmetadata = function(link.df){
                    # require(plyr)
                    
                    # Add metadata for A and B lists
                    for(i in c( "a.", "b.")){
                      ## Copy list.dict
                      this.list.dict <- private$list.dict
                      
                      ## rename columns
                      colnames(this.list.dict) <- paste0(i, colnames(this.list.dict))
                      
                      ## subset to list.ids in A or B
                      this.list.dict <- this.list.dict[this.list.dict[, paste0(i, "list.id")] %in%
                                                         link.df[, paste0(i, "list.id")],, drop = F]
                      
                      ## Join with link.df
                      link.df <- join(link.df, this.list.dict, by = paste0(i, "list.id"),
                                      type = "left", match = "first")
                    }
                    
                    # Sort link.df
                    sort.vars <- c(paste0("a.", colnames(private$list.dict)),
                                   paste0("b.", colnames(private$list.dict)))
                    link.df <- link.df[, c(sort.vars, colnames(link.df)[!colnames(link.df) %in% sort.vars])]
                    
                    # Return
                    return(link.df)
                  },
                  
                  ### Load raw links to ethnologue
                  load_raw_ethnolinks = function(param_list){
                    ## Get list dictionary
                    this.list.dict <- self$get_list_dict_subset(param_list = param_list)
                    
                    ## Get raw data
                    all.raw <- readRDS(system.file("extdata", "input_orgdata_ls.rds", package = "LEDA"))
                    
                    ## Load all links
                    all.links <- readRDS(system.file("extdata", "linked_raw_ls.rds", package = "LEDA"))
                    
                    ## Prepare output
                    raw.link.out <- list()
                    for(type in unique(this.list.dict$type)){
                      ### Get type-specific raw data
                      if(type %in% names(all.raw)){
                        
                        ### Get raw data
                        this.raw <- all.raw[[type]]
                        
                        ### Subset with parameters
                        join.vars <- colnames(this.raw)[colnames(this.raw) %in% colnames(this.list.dict)]
                        this.raw <- join(this.raw[, c(join.vars, "group")], 
                                         this.list.dict[this.list.dict$type == type,, drop = F], 
                                         by = join.vars,
                                         type = "inner", match = "all")
                        ### Join with links
                        this.out <- join(this.raw, 
                                         all.links[[type]][, c("iso3c", "group", "link")], 
                                         by = c("iso3c", "group"),
                                         type = "left", match = "first")
                      } else if(type %in% names(private$new.raw)){
                        ## Get linked table
                        this.out <- private$new.raw[[type]]
                        
                        ## Assign missings to missing idvars
                        this.out[,colnames(this.list.dict)[!colnames(this.list.dict) %in% colnames(this.out)]] <- NA
                        
                      } else {
                        stop(paste("Type", type, "does not exist in LEDA or newly coded original data."))
                      }

                      ### Save
                      raw.link.out[[type]] <- this.out[, c(colnames(this.list.dict), "group","link")]
                    }
                    
                    # Collapse to single dataframe
                    raw.link.out <- do.call(rbind, raw.link.out)
                    
                    # Return
                    return(raw.link.out)
                  }
                  
                  
                )
)




######################
# Utility functions
######################

# # String cleaning
# string_clean <- function(group){
#   require(stringr)
#   require(gsubfn)
#   require(tools)
#   
#   # String handling #########
#   data <- data.frame(group = group, stringsAsFactors = F)
#   
#   # ... pre-transformed è/é/etc.
#   weird <- unique(data$group[grepl("<",data$group)])
#   unwanted <- list("<82>" = "e", "<e9>" = "e", "<f4>"="o", "<e7>"="c","<8b>"="i","<ef>"="i")
#   clean <- gsubfn(paste(names(unwanted),collapse='|'), unwanted, weird)
#   repl <- data.frame(group=weird,clean=clean, stringsAsFactors = F)
#   data <- join(data,repl,by="group",type="left")
#   data$group[!is.na(data$clean)] <- data$clean[!is.na(data$clean)]
#   data$clean <- NULL
#   
#   # ... string encoding
#   data$group <- iconv(data$group,to="ASCII//TRANSLIT")
#   
#   # ... capitalization
#   caps <- data.frame(group=unique(data$group), caps=toTitleCase(tolower(unique(data$group))), stringsAsFactors = F)
#   data <- join(data,caps,by="group",type="left")
#   data$group <- iconv(data$caps,to="ASCII//TRANSLIT")
#   data$caps <- NULL
#   
#   # ... encode missings
#   data$group[data$group=="NA"] <-NA
#   
#   # return
#   return(data$group)
# }
