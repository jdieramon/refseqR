#' @title Get the amino acid length from a protein accession
#'
#' @description \code{refseq_AA_length()} Returns the amino acid length from a single protein accession.
#'
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_AA_length(protein, retries)
#'
#' @param protein A character string of the protein id.
#' @param retries A numeric value to control the number of retry attempts to handle internet errors.
#'
#'
#' @returns A numeric value representing the aa length of the `protein`.
#'
#' @seealso \code{\link{refseq_mRNA2protein}} to obtain the protein ids encoded by a set of transcript ids.
#'
#' @examples
#'  # Get the transcript ids from a set of protein accessions
#'  protein = c("XP_004487758", "XP_004488550")
#'  sapply(protein, function(x) refseq_AA_length(x, retries = 4), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export

refseq_AA_length <- function(protein, retries = 4) {
  tryCatch({
    # Get the item list for that protein id
    xpinfo <- rentrez::entrez_summary(db = "protein", id = protein)
  }, error = function(e){
    if (inherits(e, "error")) {
      if (grepl("HTTP error: 502", e$message) && retries > 0) {
        message("Retrying...\n")
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        return(refseq_AA_length(protein, retries - 1))
      } else {
        stop(e)
      }
    }
  })
  # Get the protein length
  return(xpinfo$slen)
}

