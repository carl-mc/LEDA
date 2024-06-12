The LEDA package contains a full pipeline to link ethnic datasets from
Africa. The main strength of LEDA consists in leveraging the structure
of the language tree to provide a flexible link between any two ethnic
group that are linked to the tree.

The package allows lists of ethnic groups to be linked to each other
using three main linkage types: binary linking based on the relations of
sets of language nodes associated with two groups; binary linking based
on lingustic distances; and a full computation of dyadic linguistic
distances.

Usage of a LEDA object is structured around *lists of ethnic groups*.
These lists of groups stem from the original datasets that have been
joined to the language tree. Lists are structured by data source,
country, year, or, in the case of survey data, survey rounds. Via the
language tree, any two lists of ethnic groups can be linked to each
other.

For full information on the LEDA project and methodology, read the
[paper](https://github.com/carl-mc/LEDA/raw/master/docs/LEDA_paper.pdf).

When using the LEDA package, please cite: Müller-Crepon, Carl, Yannick
Pengl, and Nils-Christian Bormann (2020). *Linking Ethnic Data from
Africa*. Journal of Peace Research, 59(3), 425–435.

## Updates

*12. June 2024*: Added links to Afrobarometer Rounds 7 and 8. Thanks to
[Vladimir Chlouba](https://www.vladimirchlouba.com/) for the help with
coding these (he coded the ethnicity variables, Carl Müller-Crepon added
the languages)!

## Installation

You can directly download and install the LEDA package from GitHub.

    library(devtools)
    install_github(repo = "carl-mc/LEDA")

## Initialize linking object

The LEDA package is programmed in an object oriented manner. Once you
initialize a LEDA-object, methods are applied directly to the object and
either change the object or return the results of a query. See the
documentation of the R-package
[R6](https://cran.r-project.org/web/packages/R6/R6.pdf) for details.

### Create LEDA objects

    library(LEDA)
    leda <- LEDA$new()

### Help files

Because all functionalities of the LEDA package are methods of LEDA
objects, all documentation can be accessed by calling `?LEDA`.

### Datasets included in LEDA

To get a first overview of the possibilities coming with LEDA, start
querying the ‘list dictionary’, which contains all metadata of all lists
of ethnic groups that the LEDA project links to the Ethnologue language
tree. Lists are identified by their country, the type of dataset
(e.g. EPR, Afrobarometer, DHS), the variable that identifies ethnic
groups in that dataset, the type of ethnic marker (language, ethnic
group, mother tongue), as well as year or survey-round identifiers where
appropriate.

    # Retrieve dataset dictionary
    list.dict <- leda$get_list_dict()

    # Show first entries
    head(list.dict)

    ##   iso3c       marker round cowcode type list.id groupvar year subround variable
    ## 1   GNB ethnic group    NA     404 AMAR       1    Group   NA       NA     <NA>
    ## 2   GMB ethnic group    NA     420 AMAR       2    Group   NA       NA     <NA>
    ## 3   MLI ethnic group    NA     432 AMAR       3    Group   NA       NA     <NA>
    ## 4   SEN ethnic group    NA     433 AMAR       4    Group   NA       NA     <NA>
    ## 5   BEN ethnic group    NA     434 AMAR       5    Group   NA       NA     <NA>
    ## 6   MRT ethnic group    NA     435 AMAR       6    Group   NA       NA     <NA>

    # All data types
    unique(list.dict$type)

    ##  [1] "AMAR"          "DHS"           "SIDE"          "EPR"          
    ##  [5] "Fearon"        "FRT"           "GREG"          "Murdock_Map"  
    ##  [9] "IPUMS"         "Afrobarometer" "WLMS"          "PREG"

## Link data sets

Once familiar with the lists of ethnic groups that are part of the LEDA
object, we can proceed to link the groups contained in any two lists of
groups to each other. The LEDA object includes three methods to link
lists of ethnic groups to each other, each of them described below.

### Link via set relations

We can first link lists *A* to lists *B* by analyzing the set of nodes
on the language tree that groups *a* and *b* share. In the example
below, we link two groups to each other as soon as they are associated
with at least one common dialect on the language tree
(`link.level = "dialect"`). As one specifies link levels closer to the
root of the language tree, i.e. by setting `link.level = "language"` or
`link.level = 5` (language tree level 5 of 16), the number of groups *b*
linked to *a* increases and links become less precise.

The lists entered for parameters `lists.a` and `lists.b` offer a
flexible way to select the lists of ethnic groups that are linked to
each other. Note that you can enter any parameter combination that
identifies at least one list of ethnic groups, but potentially many. The
latter is helpful if you want to, for example, link all Afrobarometer
surveys to the Ethnic Power Relations (EPR) data. It is generally (but
not always) sensible to only link lists of ethnic groups within the same
country borders by setting `by.country = T`.

    ## Link all Afrobarometer groups (rounds 1-5) in Uganda to the FRT data.
    setlink <- leda$link_set(lists.a = list(type = c("Afrobarometer"),  
                                            iso3c = c("UGA"),
                                            round = 8, marker = "language"),
                                    lists.b = list(type = c("FRT"),
                                                   iso3c = c("UGA")),
                                    link.level = "dialect",
                                    by.country = T,
                                    drop.a.threshold = 0,
                                    drop.b.threshold = 0,
                                    drop.ethno.id = T)

    ## Have a look
    head(setlink[, c("a.group", "b.group", "a.type", "b.type")])

    ##      a.group b.group        a.type b.type
    ## 1    Luganda   Ganda Afrobarometer    FRT
    ## 2 Runyankole  Ankole Afrobarometer    FRT
    ## 3    Lugbara Lugbara Afrobarometer    FRT
    ## 4    English    <NA> Afrobarometer   <NA>
    ## 5     Lusoga    Soga Afrobarometer    FRT
    ## 6   Lukhonzo   Konjo Afrobarometer    FRT

One can further refine the link by constraining the arguments
`drop.a.threshold` and `drop.b.threshold` that control the shares of
common languages associated with groups *a* and *b* for a link to be
realized. For eaxample, setting `drop.a.threshold = .5` ensures that in
each link the language nodes of group *b* cover more than 50 percent of
the language nodes associated with *a*. Conversely, setting
`drop.b.threshold = .5` will ensure that in each pair of linked group
*a* and *b*, group *a* covers more than 50 percent of the language nodes
of *b*. More complex set relations can be implemented by setting the
thresholds to 0 and switching `drop.ethno.id = FALSE`. The returned link
table will then have multiple rows per linked pair of groups *a* and
*b*, each coming with the ID of the language node they share.

### Link via linguistic distances

We can also make direct use of the language tree and link groups in
lists *A* and *B* on the basis of their linguistic distances to each
other. To do so, LEDA calculates linguistic distances first and then
subsets the distance matrix to return the links queried by the user.

#### Compute linguistic distance between groups

The algorithm computes the full linguistic distance matrix between
groups in lists *A* and *B*. Via the parameter `level`, users can
specify whether they want links to be based on distances between ethnic
groups’ `"language"` or `"dialect`. As before, it is sensible to not
link lists across country borders by setting `by.country = T`.

The linguistic distance between two languages or dialects
*L*<sub>1</sub> and *L*<sub>2</sub> is computed as :

1 − ((*d*(*L*<sub>1</sub>,*R*)+*d*(*L*<sub>2</sub>,*R*)−*d*(*L*<sub>1</sub>,*L*<sub>2</sub>))/(*d*(*L*<sub>1</sub>,*R*)+*d*(*L*<sub>2</sub>,*R*))))<sup>*δ*</sup>

where *d*(*L*<sub>*i*</sub>,*R*) is the length of path from a language
to the tree’s origin and *d*(*L*<sub>1</sub>,*L*<sub>2</sub>) is the
length of the shortest path from the first to the second language. *δ*
is an exponent to discount short distances on the tree, reflected in the
parameter `delta` below. Lastly, there are two ways to locate languages
and dialects on the language tree. In the first, languages that are
immediate children of a node that is located at level 4 of the language
tree remain at their original level 5 (`expand = FALSE`). In the second
way, the tree is expanded, and all languages are located on level 15 and
all dialects on level 16. This expansion of the tree naturally changes
computed linguistc distances.

Because ethnic groups are often linked to multiple languages or
dialects, there can be multiple linguistic distances between any group
*a* and *b*. `agg_fun.a` and `agg_fun.b` control the aggregation of
these distances. `agg_fun.a` determines for any language node in *a* how
its distances to nodes of *b* are aggregated. `agg_fun.b` controls how
the resulting distances between nodes in *a* and group *b* are
aggregated to arrive at a single distance between *a* and *b*.

    ## Compute distances
    distance.df <- leda$ling_distance(lists.a = list(type = c("Afrobarometer"),
                                                     iso3c = "UGA",
                                                     round = 4, marker = "language"),
                                      lists.b = list(type = c("FRT"), iso3c = "UGA"),
                                      level = "dialect", by.country = T,
                                      delta = .5, expand = FALSE,
                                      agg_fun.a = min, agg_fun.b = min)

    ## Have a look
    head(distance.df[, c("a.group", "b.group", "a.type", "b.type", "distance")])

    ##                       a.group b.group        a.type b.type  distance
    ## Afrobarometer.94698    Acholi  Acholi Afrobarometer    FRT 0.0000000
    ## Afrobarometer.94698.1  Acholi    Alur Afrobarometer    FRT 0.1471971
    ## Afrobarometer.94698.2  Acholi  Ankole Afrobarometer    FRT 1.0000000
    ## Afrobarometer.94698.3  Acholi   Ganda Afrobarometer    FRT 1.0000000
    ## Afrobarometer.94698.4  Acholi    Gisu Afrobarometer    FRT 1.0000000
    ## Afrobarometer.94698.5  Acholi   Gwere Afrobarometer    FRT 1.0000000

### Link to closest linguistic neighbours

Based on the linguistic distances computed as discussed above, users can
query, for every group *a* in lists *A* and for every list *B*, the
closest linguistic neighbor *b*. Note that more than one nearest
linguistic neighbor is returned wherever two or more closest groups *b*
have the exact same lingusitic to *a*.

    mindistlink <- leda$link_minlingdist(lists.a = list(type = c("Afrobarometer"),  
                                                        iso3c = "UGA",
                                                        round = 4, marker = "language"),
                                         lists.b = list(type = c("FRT"), iso3c = "UGA"),
                                         level = "dialect",
                                         by.country = T,
                                         expand = FALSE,
                                         delta = .5,
                                         agg_fun.a = min, agg_fun.b = min)

    ## Have a look
    head(mindistlink[, c("a.group", "b.group", "a.type", "b.type", "distance")])

    ##      a.group b.group        a.type b.type  distance
    ## 1     Acholi  Acholi Afrobarometer    FRT 0.0000000
    ## 2       Alur    Alur Afrobarometer    FRT 0.0000000
    ## 3      Ateso    Teso Afrobarometer    FRT 0.0000000
    ## 4 Japhadhola Padhola Afrobarometer    FRT 0.0000000
    ## 5      Kakwa   Kakwa Afrobarometer    FRT 0.0000000
    ## 6  Kiswahili   Gwere Afrobarometer    FRT 0.1659423

### Link within linguistic distance

Instead of focusing on nearest linguistic neighbors only, users can also
query, for every group *a* in lists *A* and for every list *B*, those
groups *b* that fall within a specified distance `max.distance` of group
*a*.

    withindistlink <- leda$link_withinlingdist(lists.a = list(type = c("Afrobarometer"),  
                                                              iso3c = "UGA",
                                                              round = 4, marker = "language"),
                                               lists.b = list(type = c("FRT"), iso3c = "UGA"),
                                               level = "dialect", max.distance = .1,
                                               by.country = T,
                                               delta = .5, expand = FALSE,
                                               agg_fun.a = min, agg_fun.b = min)

    ## Have a look
    head(withindistlink[, c("a.group", "b.group", "a.type", "b.type", "distance")])

    ##      a.group b.group        a.type b.type  distance
    ## 1     Acholi  Acholi Afrobarometer    FRT 0.0000000
    ## 2     Acholi   Lango Afrobarometer    FRT 0.0741799
    ## 3       Alur    Alur Afrobarometer    FRT 0.0000000
    ## 4      Ateso    Teso Afrobarometer    FRT 0.0000000
    ## 5 Japhadhola Padhola Afrobarometer    FRT 0.0000000
    ## 6      Kakwa   Kakwa Afrobarometer    FRT 0.0000000

## Inspect coding of the ethnic group &lt;–&gt; language link

Sometimes, one might want to inspect the origins of a link between to
groups. LEDA allows that by giving access to the entire raw data that
underlies each match. You can query the link between any list of groups
and the language tree with the following method.

The resulting table contains one column `link` that contains the
language tree nodes linked to any group. Note that in cases of multiple
links, they are separated by a ‘|’. In most cases, the level of a node
on the language tree is indicated in squared brackets behind the nodes
name. L1 to L14 indicate super-languages, ‘lang’ denotes languages,
‘iso’ language isocodes, and ‘dial’ refers to dialects.

    ## Query raw link data
    raw_ethno_links <- leda$get_raw_ethnolinks(param_list = list(type = "Afrobarometer",
                                                                 round = 4,
                                                                 marker = "language",
                                                                 iso3c = "UGA"))

    ## Have a look
    head(raw_ethno_links[, c("type","group", "link")])

    ##                          type      group          link
    ## Afrobarometer.1 Afrobarometer     Acholi  Acholi [org]
    ## Afrobarometer.2 Afrobarometer       Alur     Alur [L9]
    ## Afrobarometer.3 Afrobarometer      Ateso     Teso [L7]
    ## Afrobarometer.4 Afrobarometer Japhadhola   Adhola [L7]
    ## Afrobarometer.5 Afrobarometer      Kakwa   Kakwa [org]
    ## Afrobarometer.6 Afrobarometer  Kiswahili Swahili [org]

## Add new links from groups to language tree

Having gained familiarity with the available ethnic links and methods,
users can go a step further and link new lists of ethnic groups to the
language tree. Doing so allows to link the new list of ethnic groups to
every other list of ethnic groups covered by LEDA or independently added
before.

### Prepare new links between ethnic groups and the tree

First, one has to hand-code the link between ethnic groups and the
language tree. However, this may be less tedious than it sounds. Via the
method `LEDA$prepare_newlink_table()` one can access automatically
generated suggestions to which language node(s) a particular group may
link. These suggestions are generated via a fuzzy string match of a
group’s name to the names of (1) language nodes themselves, and (2) the
names of ethnic groups already matched to the language tree. Thus, with
every additional list of ethnic groups added to the data, linking new
ones to the language tree becomes easier.

Once generated as shown below, the link table should be saved and the
final links between ethnic groups and language nodes established by
hand. I.e., users have to fill in the column `link`, using the
information from the automatically generated suggestions, as well as
secondary sources.

    ## Make or load some dataset of ethnic groups
    new.groups.df <- data.frame(group_name = c("Alur", "Iteso", "Kakwa"),
                                iso3c = c("UGA"),
                                marker = "ethnic group",
                                stringsAsFactors = F)


    ## Prepare a new link table
    ##   This table contains suggested links between each ethnic group
    ##   and language nodes. The columns "link", "comment", and "source"
    ##   have to be filled by hand and correspond to the final link to
    ##   a set of language nodes (separated by '|'), comments on the link,
    ##   and a source (if required).
    newlink.df <- leda$prepare_newlink_table(group.df = new.groups.df,
                               groupvar = "group_name",
                               by.country = TRUE,
                               return = TRUE,
                               save.path =  NULL, overwrite = T,
                               prev_link_param_list = NULL,
                               levenshtein.threshold = .2,
                               levenshtein.costs = c(insertions = 1,deletions = 1, substitutions = 1))

    newlink.df

    ##   group_name iso3c       marker group        auto_link_org auto_link_alt
    ## 1       Alur   UGA ethnic group  Alur                                   
    ## 2      Iteso   UGA ethnic group Iteso Teso [org]|Teso [L7]    Teso [org]
    ## 3      Kakwa   UGA ethnic group Kakwa          Kakwa [org]   Kakwa [org]
    ##   auto_link_dial       auto_link_prev
    ## 1                           Alur [L9]
    ## 2                Teso [org]|Teso [L7]
    ## 3                         Kakwa [org]
    ##                                                                                                                                                                                                  auto_link_foreign
    ## 1                                                                                                                                                  Org: Alur [org]|Alur [L9]|--|Alt: |--|Dial: |--|Prev: Alur [L9]
    ## 2                                                                                                                                                                      Org: |--|Alt: |--|Dial: |--|Prev: Teso [L7]
    ## 3 Org: Akwa [org]|Kabwa [org]|--|Alt: Kako [org]|Kwa' [org]|Teke-Kukuya [org]|Avikam [org]|--|Dial: Kakia [dial]|Dakwa [dial]|Akwa [dial]|--|Prev: Akwa [org]|Kakwa [org]|Kako [org]|Yamba [org]|Kako (A.90) [L10]
    ##   link comment source
    ## 1 <NA>    <NA>   <NA>
    ## 2 <NA>    <NA>   <NA>
    ## 3 <NA>    <NA>   <NA>

### Add new links to a LEDA object

Having hand-coded the link between the new list of ethnic groups and the
language tree, one can now add the new list of groups to the LEDA
object. The list now enters the object in the same manner as all
‘native’ LEDA lists, as well as any lists added beforehand.

    ## First we need to encode links to the lanugage tree:
    newlink.df$link[newlink.df$group == "Alur"] <- "Alur [L9]"
    newlink.df$link[newlink.df$group == "Iteso"] <- "Teso [L7]"
    newlink.df$link[newlink.df$group == "Kakwa"] <- "Kakwa [org]"
    newlink.df$comment[newlink.df$group == "Kakwa"] <- "Kakwa same language as Bari, differs between language datasets."


    ## Add to LEDA
    leda$add_tree_links(tree.link.df = newlink.df,
                        idvars = c("iso3c", "marker"),
                        type = "My Survey")

    ## [1] "Added 1 lists to list dictionary"
    ## [1] "Added new entries to link dictionary."

    ## Check type list
    print(unique(leda$get_list_dict()$type))

    ##  [1] "AMAR"          "DHS"           "SIDE"          "EPR"          
    ##  [5] "Fearon"        "FRT"           "GREG"          "Murdock_Map"  
    ##  [9] "IPUMS"         "Afrobarometer" "WLMS"          "PREG"         
    ## [13] "My Survey"

For full traceability, the newly coded data is now also available in the
raw data attached to LEDA and can be queried accordingly:

    ## Query raw link data
    raw_ethno_links <- leda$get_raw_ethnolinks(param_list = list(type = "My Survey"))

    ## Have a look
    head(raw_ethno_links[, c("type","group", "link")])

    ##                  type group        link
    ## My Survey.1 My Survey  Alur   Alur [L9]
    ## My Survey.2 My Survey Iteso   Teso [L7]
    ## My Survey.3 My Survey Kakwa Kakwa [org]

### Join own data with other ethnic group lists

The new list can now be linked to any other list of ethnic groups in the
LEDA object, in the same way as discussed above.

    ## Get set link from my survey to FRT
    setlink <- leda$link_set(lists.a = list(type = c("My Survey"), iso3c = "UGA"),
                             lists.b = list(type = c("FRT"), iso3c = "UGA"),
                             link.level = "dialect", by.country = T,
                             drop.a.threshold = 0, drop.b.threshold = 0)

    ## Have a look
    head(setlink[, c("a.group", "b.group", "a.type", "b.type")])

    ##   a.group b.group    a.type b.type
    ## 1    Alur    Alur My Survey    FRT
    ## 2   Iteso    Teso My Survey    FRT
    ## 3   Kakwa   Kakwa My Survey    FRT

### Submit new lists to LEDA project

Given that the value of LEDA increases exponentially with the number of
lists available in the R-package, we would greatly appreciate if you
could share any new lists that you link to the language tree. New lists
can be new rounds of survey data (e.g. Afrobarometer, DHS) or any list
of ethnic groups that is based on publicly available data. You can do so
by sending us an email to carlvs /at/ ethz.ch or opening an issue with
the attached link file via LEDA’s Github page. Shared link files should
have the format returned by the method `LEDA$prepare_newlink_table()`
and have the `link` column filled wherever possible.

## Change Log

5.5.2020: Added DHS Rounds 7 to LEDA

8.7.2021: Corrected language tree error for Lomwe, Malawi.
