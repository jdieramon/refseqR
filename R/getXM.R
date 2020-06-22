#'  Get the XM accession from XP accession
#'
#' Returns the XM accession from a single XP accession.
#'
#' @usage
#' getXM(xp)
#' @param
#' xp a vector containing a single XP id.
#'
#' @examples
#' xp <- "XP_020244413"
#' getXM(xp)
#' @author Jose V. Die
#' @export
#'
#'


getXM <- function(xp) {
    # Get the transcript elink.
    transcript_elink <- rentrez::entrez_link(dbfrom = "protein", id = xp, db = "nuccore")
    # Get the transcript id
    transcript_id <- transcript_elink$links$protein_nuccore_mrna
    # Get the item list for that transcript id
    transcript <- rentrez::entrez_summary(db = "nuccore", id = transcript_id)
    # Get the XM id
    transcript$caption
}
