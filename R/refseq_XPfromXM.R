#'  Get the XP accession from XM accession
#'
#' `refseq_XPfromXM()` Returns the XP accession from a single XM accession.
#'
#' @usage
#' refseq_XPfromXM(xm)
#'
#' @param xm A character string of the XP id.
#'
#' @returns A character vector containing the XP id encoded by the XM especified as `xm`.
#'
#' @seealso \code{\link{refseq_XMfromXP}} to obtain the XM ids that encode a set of XP ids.
#'
#' @examples
#'  # Get the XP id from a single XM accession
#'  xm <- "XM_004487701"
#'  refseq_XPfromXM(xm)
#'
#'  \donttest{
#'  # Get the XP ids from a set of XM accessions
#'  xm = c("XM_004487701", "XM_004488493")
#'  sapply(xm, function(x) refseq_XPfromXM(x), USE.NAMES = FALSE) }
#'
#' @author Jose V. Die
#'
#' @export


refseq_XPfromXM <- function(xm) {
  tryCatch({
    # Get the protein elink
    protein_elink <- rentrez::entrez_link(dbfrom = "nuccore", id = xm, db = "protein")
    # Get the protein id
    protein_id <- protein_elink$links$nuccore_protein
    # Get the item list for that protein id
    protein <- rentrez::entrez_summary(db = "protein", id = protein_id)
    # Get the XP id
    return(ifelse(is.na(protein$caption), NA, protein$caption))
  },
  error = function(e) {
    protein_elink <- rentrez::entrez_link(dbfrom = "nuccore", id = xm, db = "protein")
    protein_id <- protein_elink$links$nuccore_protein
    protein <- rentrez::entrez_summary(db = "protein", id = protein_id)
    return(ifelse(is.na(protein$caption), NA, protein$caption))
  })
}




