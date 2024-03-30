#' @title Get the protein length from an XP accession
#'
#' @description \code{refseq_XPlength()} Returns the amino acid length from a single XP accession.
#'
#' @usage
#' refseq_XPlength(xp)
#'
#' @param xp A character string of the XP id.
#'
#' @returns A numeric value representing the aa length of the protein especified as `xp`.
#'
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#'  # Get the XM ids from a set of XP accessions
#'  xp = c("XP_004487758", "XP_004488550")
#'  sapply(xp, function(x) refseq_XPlength(x), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export

refseq_XPlength <- function(xp) {
  # Get the item list for that protein id
  xpinfo <- rentrez::entrez_summary(db = "protein", id = xp)
  # Get the protein length
  xpinfo$slen
}

