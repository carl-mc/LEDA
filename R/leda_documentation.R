#' 
#' #' 
#' #' @name LEDA
#' #' 
#' #' @titel Linking Ethnic Data from Africa
#' #' 
#' #' @description
#' #' Provides an interface to link ethnic groups from 12 different datasets to each other
#' #' and calculate lingustic distances between them.
#' #' 

#' NULL
#' 
#' 
#' ###############################
#' # Documentation: Linking Functions
#' ###############################
#' 
#' #' @name LEDA.link_set
#' #' 
#' #' @title Links ethnic groups based on set-relations
#' #' 
#' #' @description Returns a link table of ethnic groups contained in lists \emph{A} and \emph{B},
#' #'   based on their set relation. At the baseline, groups \emph{a} to \emph{b} are linked to each other
#' #'   as soon as they share any language node at the level of the language tree specified by \code{link_level}. 
#' #'   Links are provided between all lists in \emph{A} with every list in \emph{B} separately.
#' #'      
#' #'   The returned DataFrame contains at least one row per group \emph{a} 
#' #'   that has been linked to the ethnologue language tree. 
#' #'   If \emph{a} is not linked to any group \emph{b},
#' #'   the columns that contains linked groups \emph{b} are 
#' #'   set to missing. The returned DataFrame contains multiple 
#' #'   rows per group \emph{a} if \emph{a} is linked to multiple groups \emph{b}.
#' #'   
#' #' @usage leda.obj$link_set(lists.a, lists.b, 
#' #'          link.level, by.country = TRUE, 
#' #'          minimalist = ifelse(link.level == 17, T, F), 
#' #'          ethno.same.country = FALSE, 
#' #'          drop.b.threshold = 0, drop.ethno.id = TRUE, 
#' #'          add_listmetadata = TRUE)
#' #'          
#' #' @param lists.a Vector of lists \emph{A}, identified via their 
#' #'   list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{A}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #' 
#' #' @param lists.b Vector of lists \emph{B}, identified via their 
#' #'   list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{B}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #'   
#' #' @param link.level Level on the linguistic tree on 
#' #'   which ethnic groups are linked to each other. 
#' #' 
#' #' @param by.country Flag for linking lists only within 
#' #'   the same country (\code{by.country = TRUE}), 
#' #'   or also across countries (\code{by.country = FALSE}).
#' #'   Defaults to \code{TRUE} to avoid accidental 
#' #'   computation of a huge number of links.
#' #'   
#' #' @param minimalist Minimalist linking. Write documentation. Partly legacy. 
#' #' 
#' #' @param ethno.same.country Flag for linking lists 
#' #'   only via languages enumerated by 
#' #'   Ethnologue in the same country 
#' #'   (\code{ethno.same.country = TRUE}), 
#' #'   or also in foreign countries (\code{ethno.same.country = FALSE}). 
#' #'   If a group \emph{a} can be linked to 
#' #'   any group \emph{b} via a same-country-language, this link takes 
#' #'   precendence and no further links are searched. 
#' #'   
#' #' @param drop.b.threshold Minimum share of language nodes 
#' #'   associated with \emph{b} that have to be 
#' #'  associated with group \emph{a} for a link to be established. 
#' #'  
#' #' @param drop.ethno.id Drop all ethnologue language IDs 
#' #'   that are used to link group \emph{a} with \emph{b}. If
#' #'  \code{FALSE}, the returned DataFrame has as many rows 
#' #'  per link between \emph{a} and \emph{b} as there are 
#' #'  language nodes the two groups share. 
#' #' 
#' #' 
#' #' @param add_listmetadata Adds metadate of lists 
#' #'  \emph{A} and \emph{B} to the output. Defaults
#' #'  to \code{TRUE}.
#' #' 
#' #' @return A DataFrame.
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda.obj <- LEDA$new()
#' #' 
#' #' # link Afrobarometer to FRT
#' #' setlink <- leda.obj$link_set(lists.a = list(type = "Afrobarometer", 
#' #'                                           iso3c = c("UGA","KEN")), ## link data from Uganda and Kenya
#' #'                            lists.b = list(type = "FRT", 
#' #'                                           iso3c = c("UGA","KEN")), ## link data from Uganda and Kenya
#' #'                            link.level = "dialect",  ## Link via dialects
#' #'                            by.country = TRUE, ## Natch by country
#' #'                            minimalist = FALSE, ## Don't link minimalistically
#' #'                            ethno.same.country = FALSE, ## Use foreign-country ethnologue nodes
#' #'                            drop.b.threshold = 0, ## Don't use a threshold on linking quality
#' #'                            drop.ethno.id = TRUE, ## Return collapsed version of the linked data
#' #'                            add_listmetadata = TRUE) ## Add list metadata
#' #' head(setlink)
#' NULL
#' 
#' 
#' 
#' 
#' #' @name LEDA.link_minlingdist
#' #' 
#' #' @title Links closest linguistic neighbours
#' #' 
#' #' @description Returns a link table of ethnic groups contained in lists \emph{A} and \emph{B}. 
#' #'   Each for each list of ethnic groups in \emph{A} and \emph{B}, each group \emph{a} is linked to its closest 
#' #'   linguistic neighbour \emph{b}. Note that group \emph{a} can be linked to several groups \emph{b} if they are
#' #'   equidistant to \emph{a}. 
#' #'   Links are provided between all lists in \emph{A} with every list in \emph{B} separately.
#' #'   
#' #'   The returned DataFrame contains at least one row per group \emph{a} 
#' #'   that has been linked to the ethnologue language tree. 
#' #'   If \emph{a} is not linked to any group \emph{b},
#' #'   the columns that contains linked groups \emph{b} are 
#' #'   set to missing. The returned DataFrame contains multiple 
#' #'   rows per group \emph{a} if \emph{a} is linked to multiple groups \emph{b}.
#' #'      
#' #' @usage leda.obj$link_minlingdist(lists.a, lists.b, 
#' #'             level = c("language", "dialect"),
#' #'             by.country = TRUE, expand = FALSE, 
#' #'             delta = .5, 
#' #'             agg_fun.a = min, agg_fun.b = min,
#' #'             add_listmetadata = TRUE)
#' #'   
#' #' @param lists.a Vector of lists \emph{A}, identified via their 
#' #'   list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{A}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #' 
#' #' @param lists.b Vector of lists \emph{B}, identified via
#' #'   their list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{B}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #'   
#' #' @param by.country Flag for linking lists only within  
#' #'   the same country (\code{by.country = TRUE}), 
#' #'   or also across countries (\code{by.country = FALSE}). 
#' #'   Defaults to \code{TRUE} to avoid accidental 
#' #'   computation of a huge number of links.
#' #'   
#' #' @param expand Expand the language tree so that all 
#' #'   languages are located on level 15 or not. 
#' #'   If \code{FALSE} languages are located on their 
#' #'   original position in the linguistic tree, 
#' #'   which can be considerably closer to the root of 
#' #'   the tree. Defaults to \code{FALSE} for 
#' #'   reasons explained in the paper.
#' #'   
#' #' @param delta Delta parameter used to discount 
#' #'   short distances on the language tree. 
#' #'   Does not affect the links, only the absolute
#' #'   linguistic distance associated with them 
#' #'   (but not their rank).
#' #'   See \code{\link{LEDA.ling_distance}} for details.
#' #'   
#' #' @param agg_fun Function used aggregate linguistic distances 
#' #'   between each group \emph{a} and \emph{b} 
#' #'   in the (common) cases where one or both of them are 
#' #'   associated with multiple 
#' #'   languages. Defaults to \code{min}. 
#' #'   
#' #' 
#' #' @param add_listmetadata Adds metadate of lists 
#' #'  \emph{A} and \emph{B} to the output. Defaults
#' #'  to \code{TRUE}.
#' #' 
#' #' @return A DataFrame.
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda.obj <- LEDA$new()
#' #' 
#' #' # link Afrobarometer to FRT
#' #' link.mindist <- leda.obj$link_minlingdist(
#' #'  lists.a = list(type = c("Afrobarometer"), iso3c = "UGA"), 
#' #'  lists.b = list(type = c("FRT"),  iso3c = "UGA"),
#' #'  level = "dialect",
#' #'  by.country = TRUE, expand = FALSE,  delta = .5,
#' #'  agg_fun.a = mean, agg_fun.b = min)
#' #'  
#' #' head(link.mindist)
#' NULL
#' 
#' 
#' 
#' #' @name LEDA.link_withinlingdist
#' #' 
#' #' @title Links within linguistic distance
#' #' 
#' #' @description Returns a link table of ethnic groups contained in lists \emph{A} and \emph{B}. 
#' #'   Each for each list of ethnic groups in \emph{A} and \emph{B}, each group \emph{a} is linked to 
#' #'   all groups \emph{b} within a linguistic distance specified by \code{max.distance}. 
#' #'   Note that group \emph{a} can be therefore linked to several groups \emph{b}. 
#' #'   Links are provided between all lists in \emph{A} with every list in \emph{B} separately.
#' #'   
#' #'   The returned DataFrame contains at least one row per group \emph{a} 
#' #'   that has been linked to the ethnologue language tree. 
#' #'   If \emph{a} is not linked to any group \emph{b},
#' #'   the columns that contains linked groups \emph{b} are 
#' #'   set to missing. The returned DataFrame contains multiple 
#' #'   rows per group \emph{a} if \emph{a} is linked to multiple groups \emph{b}.
#' #'   
#' #'   
#' #' @usage leda.obj$link_withinlingdist(lists.a, lists.b, 
#' #'             max.distance,
#' #'             level = c("language", "dialect"),
#' #'             by.country = TRUE, expand = FALSE, 
#' #'             delta = .5, 
#' #'             agg_fun.a = min, agg_fun.b = min,
#' #'             add_listmetadata = TRUE)
#' #'   
#' #' @param lists.a Vector of lists \emph{A}, identified via their 
#' #'   list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{A}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #' 
#' #' @param lists.b Vector of lists \emph{B}, identified via
#' #'   their list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{B}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #'   
#' #' @param by.country Flag for linking lists only within  
#' #'   the same country (\code{by.country = TRUE}), 
#' #'   or also across countries (\code{by.country = FALSE}). 
#' #'   Defaults to \code{TRUE} to avoid accidental 
#' #'   computation of a huge number of links.
#' #'   
#' #' @param max.distance Maximum linguistic distance. 
#' #'   All pairs of groups \emph{a} and \emph{b} with a
#' #'   distance smaller or equal \code{max.distance}
#' #'   are linked to each other.
#' #'   
#' #' @param expand Expand the language tree so that all 
#' #'   languages are located on level 15 or not. 
#' #'   If \code{FALSE} languages are located on their 
#' #'   original position in the linguistic tree, 
#' #'   which can be considerably closer to the root of 
#' #'   the tree. Defaults to \code{FALSE} for 
#' #'   reasons explained in the paper.
#' #'   
#' #' @param delta Delta parameter used to discount 
#' #'   short distances on the language tree. 
#' #'   Affect the links returned. 
#' #'   See \code{\link{LEDA.ling_distance}} for details.
#' #'   
#' #' @param agg_fun Function used aggregate linguistic distances 
#' #'   between each group \emph{a} and \emph{b} 
#' #'   in the (common) cases where one or both of them are 
#' #'   associated with multiple 
#' #'   languages. Defaults to \code{min}. 
#' #'  
#' #' @param add_listmetadata Adds metadate of lists 
#' #'  \emph{A} and \emph{B} to the output. Defaults
#' #'  to \code{TRUE}.
#' #'  
#' #' @return A DataFrame.
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda.obj <- LEDA$new()
#' #' 
#' #' # link Afrobarometer to FRT
#' #' link.withindist <- leda.obj$link_withinlingdist(
#' #'   lists.a = list(type = c("Afrobarometer"),  iso3c = "UGA"), 
#' #'   lists.b = list(type = c("FRT"), iso3c = "UGA"),
#' #'   level = "dialect",
#' #'   max.distance = .1, by.country = TRUE,
#' #'   delta = .5, expand = FALSE, 
#' #'   agg_fun.a = mean, agg_fun.b = min)
#' #' head(link.withindist)
#' NULL
#' 
#' 
#' 
#' 
#' 
#' ###############################
#' # Documentation: Linguistic distance function
#' ###############################
#' 
#' #' @name LEDA.ling_distance
#' #' 
#' #' @title Compute linguistic distances
#' #' 
#' #' @description Returns a table of linguistic distances between all ethnic groups 
#' #'   contained in lists \emph{A} and \emph{B}. The 
#' #'   linguistic distance between two languages \eqn{L_1} and \eqn{L_2}
#' #'   is computed as
#' #'   
#' #'   \deqn{ 1 - ((d(L_1,R) + d(L_2,R) - d(L_1,L_2)) / (d(L_1,R) + d(L_2,R)))^{\delta} }
#' #'   
#' #'   where \eqn{d(L_i,R)} is the length of path from a
#' #'   language to the tree's origin and 
#' #'   \eqn{d(L_1,L_2)} is the length of 
#' #'   the shortest path from the first to the second 
#' #'   language. \eqn{\delta} is an exponent to discount 
#' #'   short distances on the tree. 
#' #'   
#' #'   Because we oftentimes link ethnic groups 
#' #'   \emph{a} and \emph{b} to several languages, we have to aggregate 
#' #'   the resulting distance matrix, for example by taking the minimum 
#' #'   distance between all languages \eqn{L_a} in group \eqn{a} to 
#' #'   all languages \eqn{L_b} associated with group \eqn{b}. 
#' #'   
#' #'   
#' #'   
#' #' @usage leda.obj$ling_distance(lists.a, lists.b, 
#' #'              by.country = TRUE, 
#' #'              level = c("language", "dialect"),
#' #'              delta = .5, expand = TRUE, 
#' #'              agg_fun.a = min, agg_fun.b = min,
#' #'              add_listmetadata = TRUE)
#' #'   
#' #' @param lists.a Vector of lists \emph{A}, identified via their 
#' #'   list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{A}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #' 
#' #' @param lists.b Vector of lists \emph{B}, identified via
#' #'   their list.id returned by method \code{\link{LEDA.get_list_ids}}. 
#' #'   Or a list of parameters that specify lists \emph{B}. 
#' #'   See \code{\link{LEDA.list_parameters}}. 
#' #'   
#' #' @param by.country Flag for computing distances
#' #'   only between groups in the   
#' #'   same country (\code{by.country = TRUE}), 
#' #'   or also across countries (\code{by.country = FALSE}). 
#' #'   Defaults to \code{TRUE} to avoid accidental 
#' #'   computation of a huge number of distances.
#' #'   
#' #' @param expand Expand the language tree so that all 
#' #'   languages are located on level 15 or not. 
#' #'   If \code{FALSE} languages are located on their 
#' #'   original position in the linguistic tree, 
#' #'   which can be considerably closer to the root of 
#' #'   the tree. Defaults to \code{FALSE} for 
#' #'   reasons explained in the paper.
#' #'   
#' #' @param delta Delta parameter used to discount 
#' #'   short distances on the language tree. 
#' #'   See \code{\link{LEDA.ling_distance}} for details.
#' #'   
#' #' @param agg_fun Function used aggregate linguistic distances 
#' #'   between each group \emph{a} and \emph{b} 
#' #'   in the (common) cases where one or both of them are 
#' #'   associated with multiple 
#' #'   languages. Defaults to \code{min}. 
#' #' 
#' #' @param add_listmetadata Adds metadate of lists 
#' #'  \emph{A} and \emph{B} to the output. Defaults
#' #'  to \code{TRUE}.
#' #'   
#' #' @return A DataFrame.
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda.obj <- LEDA$new()
#' #' 
#' #' # link Afrobarometer to FRT
#' #' ling.distance <- leda.obj$ling_distance(
#' #'    lists.a = list(type = c("Afrobarometer"),  iso3c = "UGA"),
#' #'    lists.b = list(type = c("FRT"), iso3c = "UGA"),
#' #'    level = "dialect",
#' #'    by.country = TRUE, delta = .5, 
#' #'    expand = FALSE,  
#' #'    agg_fun.a = mean, agg_fun.b = min)
#' #' head(ling.distance)
#' NULL
#' 
#' 
#' 
#' 
#' 
#' 
#' ###############################
#' # Documentation: Coding functions
#' ###############################
#' 
#' 
#' #' @name LEDA.prepare_newlink_table
#' #' 
#' #' @title Prepare a table to link a new set
#' #'    of ethnic groups to the langauge tree
#' #'    
#' #'    
#' #' @usage leda.obj$prepare_newlink_table(
#' #'    group.df, idvars, groupvar, 
#' #'    by.country = FALSE, return = TRUE, 
#' #'    save.path = NULL, overwrite = FALSE, 
#' #'    prev_link_param_list = NULL, 
#' #'    levenshtein.threshold = .2, 
#' #'    levenshtein.costs = c(insertions = 1,
#' #'    deletions = 1, substitutions = 1))
#' #' 
#' #' @description Based on an input of ethnic group
#' #'   names, the function returns a link table between 
#' #'   the groups and automatically found likely matches
#' #'   on the language tree. These non-authorative 
#' #'   'suggestions' are 
#' #'   identified via a fuzzy string match of input
#' #'   group names with (1) the names of nodes on the 
#' #'   langauge tree, including dialects and alternative
#' #'   names, as well as (2) the names of groups that
#' #'   have been previously linked to the tree as
#' #'   contained in the respective LEDA object. 
#' #'   
#' #' 
#' #' @param group.df A \code{DataFrame} that contains the
#' #'   names of ethnic groups to be linked to the language
#' #'   tree, as well as any other (meta)data whished to retain. 
#' #'   
#' #' @param groupvar String containing the name of the 
#' #'   column in \code{group.df} that contains the 
#' #'   names of ethnic groups. 
#' #'   
#' #' @param by.country Logical determining whether ethnic groups
#' #'   names should be string matched separately within and outside the
#' #'   country they belong. Setting the parameter \code{TRUE}
#' #'   leads to more plausible matches, but requires a
#' #'   column \code{iso3c} in \code{group.df}. Column \code{iso3c}
#' #'   has to contain valid 3-letter iso codes of African 
#' #'   countries. See e.g. the R-package \code{countrycode}. 
#' #'   
#' #' @param return Logical determining whether the reulting 
#' #'   link table shall be returned.
#' #'   
#' #' @param save.path String of the path to which the reulting
#' #'   link table is stored, as a .csv file. If \code{NULL} (the default),
#' #'   nothing is stored.  
#' #'   
#' #' @param overwrite Logical determining whether a previously
#' #'   existing file located by \code{save.path} is overwritten. 
#' #'   
#' #' @param prev_link_param_list Parameters that determine the 
#' #'   subset of previous links between ethnic group lists and
#' #'   the language tree to automatically retrieve link suggestions from. 
#' #'   If \code{NULL} (the default), all available lists are used. 
#' #'   See \code{\link{LEDA.list_parameters}} for details.
#' #'   
#' #' @param levenshtein.threshold Threshold Levenshtein string 
#' #'   distance below which a fuzzy string match is returned.
#' #'   
#' #' @param levenshtein.costs Vector of costs used to compute 
#' #'   Levenshtein string distance. See \code{utils::adist} for details. 
#' #' 
#' #' 
#' #' @details 
#' #' 
#' #'   The automatic links should not be regarded as authorative,
#' #'   but merely as a help to facilitate the coding, 
#' #'   which proceeds outside the LEDA environment from
#' #'   this point onwards. The final table returned by the
#' #'   function has a column called \code{link}, which is 
#' #'   to be filled by the user, using the automatically 
#' #'   generated suggestions
#' #'   and secondary data sources. Once this coding is completed,
#' #'   the table can be added to the LEDA object with the method
#' #'   \code{\link{LEDA.add_tree_links}}.
#' #' 
#' #' @return 
#' #'   A \code{DataFrame} of the same height as \code{group.df}.
#' #'   In addition to the columns of \code{group.df}, 
#' #'   it contains the following columns:
#' #'   
#' #'   \code{auto_link_org}: Language tree nodes 
#' #'     (levels 1:14; languages, i.e. level 15) 
#' #'     matched via their original (org) name. 
#' #'   
#' #'   \code{auto_link_alt}: Languages (level 15) matched
#' #'     via their alternative name.
#' #'   
#' #'   \code{auto_link_dial}: Dialects matched via their
#' #'     name or alternative name. 
#' #'   
#' #'   \code{auto_link_prev}: Language tree nodes 
#' #'     found via a fuzzy string match of input groups
#' #'     to the groups previously linked to the language 
#' #'     tree (potentially subsetted by \code{prev_link_param_list}).
#' #'   
#' #'   \code{auto_link_foreign}: If \code{by.country}, 
#' #'     same four fields as above for langauges from
#' #'     'foreign' countries, but pasted into one
#' #'     single string. 
#' #'   
#' #'   \code{link} Empty column for the final link, 
#' #'     to be filled by the user.
#' #'     
#' #'   \code{comment} Empty column for comments on the final link, 
#' #'     to be filled by the user.
#' #'     
#' #'   \code{source} Empty column for the source of the final link, 
#' #'     to be filled by the user.
#' #'     
#' #'   
#' #'   Multiple matches are combined by pasting the languague names
#' #'   separated by a '|'. The matched language nodes' tree level 
#' #'   is indicated behind its name in '[]', with L1:L14 denoting 
#' #'   super-language levels, 'lang' denoting languages, and 'dial'
#' #'   denoting dialects. This coding format should be maintained 
#' #'   when filling the column \code{link} with the final link of groups
#' #'   to the language tree. 
#' #'   
#' #'   
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Make or load some dataset of ethnic groups
#' #' new.groups.df <- data.frame(group_name = 
#' #'   c("Asante", "Grusi", "Akan"),iso3c = c("GHA"),
#' #'   type = "My Survey in Ghana",
#' #'   marker = "ethnic group",
#' #'   stringsAsFactors = FALSE)
#' #' 
#' #' 
#' #' # Prepare a new link table 
#' #' newlink.df <- leda$prepare_newlink_table(
#' #'   group.df = new.groups.df, 
#' #'   groupvar = "group_name",
#' #'   by.country = TRUE, 
#' #'   return = TRUE, save.path =  NULL, 
#' #'   overwrite = TRUE, prev_link_param_list = NULL,
#' #'   levenshtein.threshold = .2,
#' #'   levenshtein.costs = 
#' #'   c(insertions = 1,deletions = 1, substitutions = 1))
#' #' 
#' #' 
#' NULL
#' 
#' 
#' 
#' #' @name LEDA.add_tree_links
#' #'
#' #' @title Add a set of ethnic groups linked the
#' #'   langauge tree to LEDA
#' #'    
#' #'    
#' #' @usage leda.obj$add_tree_links(tree.link.df, idvars)
#' #' 
#' #' @description 
#' #'   Function to add a table that links a list of ethnic groups 
#' #'   with nodes on the language tree to the LEDA object.
#' #'   
#' #' 
#' #' @param tree.link.df 
#' #'   Table that contains te links between ethnic groups and 
#' #'   the langauge tree. It must contain the following variables:
#' #'   
#' #'   \code{group} Names of ethnic groups, of type "character". 
#' #'   
#' #'   \code{link} Names of language nodes linked to ethnic 
#' #'     group, of type "character". Multiple nodes linked to the 
#' #'     same ethnic group must be separated by a '|'. Names
#' #'     of language nodes must be contained in the Ethnologue 
#' #'     database 13. A node's level is specified in [], the node must 
#' #'     exist on that level. Level specifiers follow this form: L1:L14 denoting 
#' #'     super-language levels, 'lang' denoting languages, 'dial'
#' #'     denoting dialects, and iso denoting language ISO-codes. 
#' #'     E.g. "Akan [L9]", "Asante [dial]", or "aka [iso]". If no level 
#' #'     is specified and multiple langauge tree nodes share the same name,
#' #'     the one closest to the node is chosen. 
#' #'     Note that the safest way to avoid confusion is to provide iso-codes.
#' #'     
#' #'  \code{iso3c} (not required) Country identifier as 3-letter ISO code. 
#' #'     If provided, the algorithm gives preference to nodes of the
#' #'     language tree in the same country in cases where multiple nodes
#' #'     share the same name given by \code{link}. 
#' #'     
#' #'  \code{idvars} Variables that identify lists of ethnic groups in your
#' #'     data. These should contain a list \code{type} (e.g. "My survey"),
#' #'     and be typically nested within countries and years. 
#' #'     See \code{\link{LEDA.list_parameters}} for the variables that 
#' #'     identify ethnic group lists in the LEDA dataset. 
#' #'     
#' #'     The unique combinations of \code{idvars} values in \code{group.df}
#' #'     are used to create new entries in the LEDA object's dictionary of 
#' #'     group lists \code{leda.obj$list.dict}.
#' #'     
#' #' @details 
#' #'   The function links the input ethnic groups to the linguistic tree 
#' #'   contained in the package and thereby updates the LEDA object. 
#' #'   Once this has been done, the added group can be linked to all 
#' #'   ethnic group lists contained in the LEDA object. See examples below. 
#' #'   
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Make toy link dataset
#' #' new.groups.df <- data.frame(
#' #'    group = c("Asante", "Mossi"), ## Ethnic group names
#' #'    link = c("Asante [dial]","Moore [org]"), ## Language nodes
#' #'    type = "My data",
#' #'    marker = "Ethnic self identification",
#' #'    iso3c = c("GHA", "BFA"), ## Countries
#' #'    stringsAsFactors = FALSE ## Everything as character
#' #'    )
#' #' 
#' #' # Add to LEDA
#' #' leda$add_tree_links(tree.link.df = new.groups.df, 
#' #'        idvars = c("iso3c", "type", "marker"))
#' #'        
#' #' # Use the new link
#' #' setlink <- leda$link_set(lists.a = list(type = c("My data")), 
#' #'     lists.b = list(type = c("Afrobarometer"), 
#' #'       round = 4, marker = "language",
#' #'       iso3c = c("GHA","BFA")), 
#' #'     link.level = 15, by.country = FALSE, 
#' #'     minimalist = FALSE, ethno.same.country = FALSE, 
#' #'     drop.b.threshold = 0, drop.ethno.id = TRUE)
#' #'     
#' #' head(setlink[, c("a.group", "b.group", "a.type", "b.type", "a.list.id", "b.list.id")])
#' 
#' NULL
#' 
#' 
#' ###############################
#' # Documentation: Helper Functions
#' ###############################
#' 
#' #' @name LEDA.show_list_parameters
#' #' 
#' #' @title Parameters that identify lists of ethnic groups
#' #' 
#' #' @description Returns a vector with the variables that define
#' #'    lists of ethnic groups in different datasets.
#' #'    
#' #' @return A vector.
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Get list parameters
#' #' leda$show_list_parameters()
#' NULL
#' 
#' #' @name LEDA.get_list_dict
#' #' 
#' #' @title Get dictionary of all available lists of ethnic groups
#' #' 
#' #' @usage leda.obj$get_list_dict()
#' #' 
#' #' @description Returns the full dictionary of lists of ethnic groups that
#' #'    are included in the LEDA project. An example of a list is the 
#' #'    IPUMS census data from Ghana in 2000.
#' #' @return A DataFrame.
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Get list dictionaries
#' #' list.dict <- leda$get_list_dict()
#' #' head(list.dict)
#' NULL
#' 
#' #' @name LEDA.get_list_dict_subset
#' #' 
#' #' @title Subset dictionary of lists of ethnic groups
#' #' 
#' #' @usage leda.obj$get_list_dict_subset(param_list)
#' #' 
#' #' @description Returns a subset of the dictionary of lists of ethnic groups that
#' #'    are included in the LEDA project. An example of a list is the 
#' #'    IPUMS census data from Ghana in 2000.
#' #'    
#' #' @param param_list List of parameter values to subset list dictionary. 
#' #'     The following fields are allowed: 
#' #'     
#' #'     \code{cowcode}: Correlates of War code of country
#' #'     
#' #'     \code{iso3c}: 3-letter isocode of country
#' #'     
#' #'     \code{type}: Type of ethnic group dataset. 
#' #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
#' #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
#' #'     
#' #'     \code{marker}: Ethnic marker used in list. 
#' #'     \code{"ethnic group"}: Ethnic group / ethnicity.
#' #'     \code{"language"}: Language.     
#' #'     \code{"mtongue"}: Mother tongue.
#' #'     
#' #'     \code{groupvar} Variable name of ethnic group identifier 
#' #'     in original dataset.
#' #'     
#' #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
#' #'     
#' #'     \code{subround}: Subround of survey (DHS; SIDE)
#' #'     
#' #'     \code{year}: Year (EPR; IPUMS)
#' #'     
#' #'     \code{list.id}: ID of list of ethnic groups.
#' #'     
#' #'     These are parameters are also returned by method 
#' #'     \code{\link{LEDA.show_list_parameters}}.
#' #'     
#' #' @return A DataFrame.
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Get list data for Afrobarometers in Uganda
#' #' leda$get_list_dict_subset(param_list = list(type = "Afrobarometer", iso3c = c("UGA","KEN")))
#' NULL
#' 
#' #' @name LEDA.get_list_ids
#' #' 
#' #' @title Subset IDs of lists of ethnic groups
#' #' 
#' #' @usage leda.obj$get_list_ids(param_list)
#' #' 
#' #' @description Returns the a subset of the IDs of lists of ethnic groups that
#' #'    are included in the LEDA project.
#' #'    
#' #' @param param_list List of parameter values to subset IDs from list dictionary. 
#' #'     The following fields are allowed: 
#' #'     
#' #'     \code{cowcode}: Correlates of War code of country
#' #'     
#' #'     \code{iso3c}: 3-letter isocode of country
#' #'     
#' #'     \code{type}: Type of ethnic group dataset. 
#' #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
#' #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
#' #'     
#' #'     \code{marker}: Ethnic marker used in list. 
#' #'     \code{"ethnic group"}: Ethnic group / ethnicity.
#' #'     \code{"language"}: Language.     
#' #'     \code{"mtongue"}: Mother tongue.
#' #'     
#' #'     \code{groupvar} Variable name of ethnic group identifier 
#' #'     in original dataset.
#' #'     
#' #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
#' #'     
#' #'     \code{subround}: Subround of survey (DHS; SIDE)
#' #'     
#' #'     \code{year}: Year (EPR; IPUMS)
#' #'     
#' #'     \code{list.id}: ID of list of ethnic groups.
#' #'     
#' #'     These are parameters are also returned by method 
#' #'     \code{\link{LEDA.show_list_parameters}}.
#' #'     
#' #' @return A vector of list.ids
#' #' 
#' #' @examples 
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Get list IDs for Afrobarometers in Uganda
#' #' leda$get_list_ids(param_list = list(type = "Afrobarometer", iso3c = c("UGA","KEN")))
#' NULL
#' 
#' #' @name LEDA.list_parameters
#' #' 
#' #' @title Variables that define ethnic group lists
#' #' 
#' #' @description 
#' #'     LEDA includes a diverse set of datasets with data on ethnic groups.
#' #'     Each of these datasets is indexed in the 'list dictionary', 
#' #'     accessible via the method \code{$get_list_dict}. Within the 
#' #'     dictionary, each list of ethnic groups is uniquely identified
#' #'     with a \code{list.id}, as well as a number of 
#' #'     list characteristics: 
#' #'     
#' #'     \code{cowcode}: Correlates of War code of country (all lists)
#' #'     
#' #'     \code{iso3c}: 3-letter isocode of country (all lists)
#' #'     
#' #'     \code{type}: Type of ethnic group dataset. 
#' #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
#' #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
#' #'     
#' #'     \code{marker}: Ethnic marker used in list. 
#' #'     \code{"ethnic group"}: Ethnic group / ethnicity.
#' #'     \code{"language"}: Language.     
#' #'     \code{"mtongue"}: Mother tongue.
#' #'     
#' #'     \code{groupvar} Variable name of ethnic group identifier 
#' #'     in original dataset.
#' #'     
#' #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
#' #'     
#' #'     \code{subround}: Subround of survey (DHS; SIDE)
#' #'     
#' #'     \code{year}: Year (EPR; IPUMS)
#' #'     
#' #'     
#' #'     These are parameters are also returned by method 
#' #'     \code{\link{LEDA.show_list_parameters}}.
#' #'     
#' #'     Note that each list is only identified with a subset of the 
#' #'     characteristics enumerated above. The respective fields are noted above.
#' #'     and relate directly to the
#' #'     type and source of the respective data. 
#' NULL
#' 
#' 
#' #' @name LEDA.get_raw_ethnolinks
#' #' @title Retrieve original links of groups to
#' #'    language nodes
#' #'    
#' #' @usage leda.obj$get_raw_ethnolinks(param_list)
#' #'    
#' #' @description Retrieve the raw coding of links 
#' #'    between ethnic groups
#' #'    contained in group lists specified by \code{param_list} 
#' #'    to the language tree.  The function returns the raw data
#' #'    the LEDA object is based upon. 
#' #'    
#' #' @param param_list List of parameter values to subset lists of 
#' #'     ethnic groups.  
#' #'     The following fields are allowed: 
#' #'     
#' #'     \code{cowcode}: Correlates of War code of country
#' #'     
#' #'     \code{iso3c}: 3-letter isocode of country
#' #'     
#' #'     \code{type}: Type of ethnic group dataset. 
#' #'     One of: \code{c("AMAR", "DHS", "SIDE", "EPR", "Fearon", "FRT", 
#' #'     "GREG", "Murdock_Map", "IPUMS", "Afrobarometer", "WLMS", "PREG")}
#' #'     
#' #'     \code{marker}: Ethnic marker used in list. 
#' #'     \code{"ethnic group"}: Ethnic group / ethnicity.
#' #'     \code{"language"}: Language.     
#' #'     \code{"mtongue"}: Mother tongue.
#' #'     
#' #'     \code{groupvar} Variable name of ethnic group identifier 
#' #'     in original dataset.
#' #'     
#' #'     \code{round}: Round of survey (DHS; SIDE; Afrobarometer)
#' #'     
#' #'     \code{subround}: Subround of survey (DHS; SIDE)
#' #'     
#' #'     \code{year}: Year (EPR; IPUMS)
#' #'     
#' #'     \code{list.id}: ID of list of ethnic groups.
#' #'     
#' #'     These are parameters are also returned by method 
#' #'     \code{\link{LEDA.show_list_parameters}}.
#' #'    
#' #' @examples
#' #' # Initialize linkage object
#' #' leda <- LEDA$new()
#' #' 
#' #' # Get list parameters
#' #' leda$get_raw_ethnolinks(param_list = 
#' #'    list(type = "Afrobarometer", iso3c = "UGA"))
#' NULL