#'  Get the sequence Description
#'
#' `refseq_description` Returns the sequence description from a single XM, XP, or Gene id. accession.
#'
#' @usage
#' refseq_description(id)
#' @param
#' id a character string of the XP, XM, or Gene id.
#'
#' @returns A character vector containing the sequence description corresponding to the specified sequence as `id`.
#'
#' @seealso \code{\link{refseq_XMfromXP}} to obtain the XM ids that encode a set of XP ids.
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#'  # Get the sequence descriptions from a set of XM accessions
#'  xm = c("XM_004487701", "XM_004488493")
#'  sapply(xm, function(x) refseq_description(x), USE.NAMES = FALSE)
#'
#'  # Get the sequence descriptions from a set of XP accessions
#'  xp = c("XP_004487758", "XP_004488550")
#'  sapply(xp, function(x) refseq_description(x), USE.NAMES = FALSE)
#'
#' # Get the sequence descriptions from a set of Gene accessions
#' locs <- c("LOC101512347", "LOC101506901")
#' sapply(locs, function(x) refseq_description(x), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export
#'
#'


refseq_description <- function(id) {
    esearch = rentrez::entrez_search(db = "gene", term = id)
    esumm = rentrez::entrez_summary(db = "gene", id = esearch$ids )
    # Get the description
    esumm$description }
