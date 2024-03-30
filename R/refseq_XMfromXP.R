#' @title  Get the XM accession from XP accession
#'
#' @description \code{refseq_XMfromXP()} Returns the XM accession from a single XP accession.
#'
#' @usage
#' refseq_XMfromXP(xp)
#'
#' @param xp A character string of the XP id.
#'
#' @returns A character vector containing the XM ids that encode the XP especified as `xp`.
#'
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#'  # Get the XM id from a single XP accession
#'  xp <- "XP_020244413"
#'  refseq_XMfromXP(xp)
#'
#' \donttest{
#'  # Get the XM ids from a set of XP accessions
#'  xp = c("XP_004487758", "XP_004488550")
#'  sapply(xp, function(x) refseq_XMfromXP(x), USE.NAMES = FALSE)}
#'
#' @author Jose V. Die
#'
#' @export

refseq_XMfromXP <- function(xp) {
  tryCatch({
    # Get the transcript elink.
    transcript_elink <- rentrez::entrez_link(dbfrom = "protein", id = xp, db = "nuccore")
    # Get the transcript id
    transcript_id <- transcript_elink$links$protein_nuccore_mrna
    # Get the item list for that transcript id
    transcript <- rentrez::entrez_summary(db = "nuccore", id = transcript_id)
    # Get the XM id
    return(ifelse(is.na(transcript$caption), NA, transcript$caption))
  },
  error = function(e) {
    Sys.sleep(2) #if error, sleep 2 sec, then redo
    transcript_elink <- rentrez::entrez_link(dbfrom = "protein", id = xp, db = "nuccore")
    transcript_id <- transcript_elink$links$protein_nuccore_mrna
    transcript <- rentrez::entrez_summary(db = "nuccore", id = transcript_id)
    return(ifelse(is.na(transcript$caption), NA, transcript$caption))

  })
}

