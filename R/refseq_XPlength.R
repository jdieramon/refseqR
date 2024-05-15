#' @title Get the protein length from an XP accession
#'
#' @description \code{refseq_XPlength()} Returns the amino acid length from a single XP accession.
#'
#' @usage
#' refseq_XPlength(xp, retries)
#'
#' @param xp A character string of the XP id.
#' @param retries A numeric value to control the number of retry attempts to handle internet errors.
#'
#'
#' @returns A numeric value representing the aa length of the protein especified as `xp`.
#'
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#'  # Get the XM ids from a set of XP accessions
#'  xp = c("XP_004487758", "XP_004488550")
#'  sapply(xp, function(x) refseq_XPlength(x, retries = 3), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export

refseq_XPlength <- function(xp, retries = 3) {
  tryCatch({
    # Get the item list for that protein id
    xpinfo <- rentrez::entrez_summary(db = "protein", id = xp)
  }, error = function(e){
    if (inherits(e, "error")) {
      if (grepl("HTTP error: 502", e$message) && retries > 0) {
        message("Retrying...\n")
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        return(refseq_XPlength(xp, retries - 1))
      } else {
        stop(e)
      }
    }
  })
  # Get the protein length
  return(xpinfo$slen)
}

