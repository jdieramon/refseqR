#'  Get the XP accession from XM accession
#'
#' Returns the XP accession from a single XM accession.
#'
#' @usage
#' getXP(xm)
#' @param
#' xm a vector containing a single XM id.
#'
#' @examples
#' xm <- "XM_020388824"
#' getXP(xm)
#' @author Jose V. Die
#' @export
#'
#'
getXP <- function(xm) {
    # Get the protein elink.
    protein_elink <- rentrez::entrez_link(dbfrom = "nuccore", id = xm, db = "protein")
    # Get the protein id
    protein_id <- protein_elink$links$nuccore_protein
    # Get the item list for that protein id
    protein <- rentrez::entrez_summary(db = "protein", id = protein_id)
    # Get the XP id
    protein$caption
}
