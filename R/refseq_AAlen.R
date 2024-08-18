#' @title Get the amino acid length from a protein accession
#'
#' @description \code{refseq_AAlen()} Returns the amino acid length from a single protein accession.
#'
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_AAlen(protein)
#'
#' @param protein A character string of the protein id.
#'
#' @returns A numeric value representing the aa length of the `protein`.
#'
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein ids encoded by a set of transcript ids.
#'
#' @examples
#'  # Get the amino acid lengths from a set of protein accessions
#'  protein = c("XP_004487758", "XP_004488550")
#'  sapply(protein, function(x) refseq_AAlen(x), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export

 
refseq_AAlen <- function (protein) {
tryCatch(
  expr    = {refseq_AAlen_action(protein, retries = 4)},
  error   = function(e) {message("NCBI servers are busy. Please try again a bit later.")},
  warning = function(w) {message("NCBI servers are busy. Please try again a bit later.")}
)
}
