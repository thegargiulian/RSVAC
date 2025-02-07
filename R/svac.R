#' Sexual Violence in Armed Conflict (SVAC) data
#'
#' Sexual Violence in Armed Conflict (SVAC) data set version 3.0 (Cohen and Nordås 2014) <http://www.sexualviolencedata.org/>.
#'
#' @docType data
#'
#' @format `svac`
#' A data frame with 9,282 rows and 22 columns.
#' \describe{
#'   \item{year}{Year.}
#'   \item{conflictid_old}{Old UCDP/PRIO Conflict ID (before version 17.1). From UCDP/PRIO.}
#'   \item{conflictid_new}{New UCDP/PRIO Conflict ID (starting with version 17.1 of UCDP data). From UCDP/PRIO.}
#'   \item{actor}{Name of country if the actor is a government, otherwise name of organization if this is a rebel organization or militia. From UCDP/PRIO.}
#'   \item{actorid}{Old UCDP Non-State Actor ID (before version 17.1). From UCDP/PRIO.}
#'   \item{actorid_new}{In version 17.1 of all UCDP data ID the system for conflicts, actors and dyads was changed in order to make them unique across all UCDP core data sets and all UCDP types of violence. From UCDP/PRIO.}
#'   \item{actor_type}{Type of actor: 1 - state or incumbent government ("Side A" in UCDP dyadic), 2 - State A2 ("Side A2nd" in UCDP dyadic), states supporting State 1 involved with the conflict on its territory, 3 - Rebel ("Side B" in UCDP dyadic), 4 - State supporting "Side B" in other country ("Side B2nd" in UCDP dyadic), 5 - Second state in interstate conflict ("Side B" in UCDP dyadic), 6 - Pro-government militias. From SVAC.}
#'   \item{type}{Conflict type: 2 - interstate conflict, 3 - intrastate conflict, 4 - internationalized armed conflict. From UCDP/PRIO.}
#'   \item{incomp}{Conflict issue: 1 - territory, 2 - government, 3 - government and territory. From UCDP/PRIO.}
#'   \item{region}{Numeric coding of geographic region. From UCDP/PRIO.}
#'   \item{location}{The name(s) of the country/countries of fighting and whose government(s) have a primary claim to the territory in dispute. From UCDP/PRIO.}
#'   \item{gwnoloc}{Gleditsch/Ward country ID of location variable 1.}
#'   \item{gwnoloc2}{Gleditsch/Ward country ID of location variable 2.}
#'   \item{gwnoloc3}{Gleditsch/Ward country ID of location variable 3.}
#'   \item{gwnoloc4}{Gleditsch/Ward country ID of location variable 4.}
#'   \item{conflictyear}{Binary indicator of an active conflict-year. From UCDP/PRIO.}
#'   \item{interm}{Binary indicator of an post-conflict-year. From UCDP/PRIO.}
#'   \item{postc}{Binary indicator of an active conflict-year. From UCDP/PRIO.}
#'   \item{state_prev}{Ordinal prevalence measure from 0--3 using the annual U.S. State Department "Country Report on Human Rights Practices". `NA` indicates no report found and no data available.}
#'   \item{ai_prev}{Ordinal prevalence measure from 0--3 using the annual reports and special topical reports of Amnesty International. `NA` indicates no report found and no data available.}
#'   \item{hrw_prev}{Ordinal prevalence measure from 0--3 using the annual reports and special topical reports of Human Rights Watch. `NA` indicates no report found and no data available.}
#'   \item{form}{Forms of conflict-related sexual violence committed by the armed conflict actor: 1 - rape, 2 - sexual slavery, 3 - forced prostitution, 4 - forced pregnancy, 5 - forced sterilization/abortion, 6 - sexual mutilation, 7 - sexual torture.}
#' }
#' @source <http://www.sexualviolencedata.org/>
#'
"svac"

# done.
