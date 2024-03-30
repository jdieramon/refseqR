#' @title Extract the amino acid sequence into a Biostrings object
#'
#' @description \code{refseq_AAseq()} Parses a single/multiple XP accessions (GenBank format) and extract
#' the amino acid sequences into a \code{AAStringSet} object.
#'
#' @usage
#' refseq_AAseq(xp)
#'
#' @param xp A character string containing a single/multiple XP ids.
#'
#' @returns An object of \code{AAStringSet} class.
#'
#' @examples
#' \donttest{
#' xp = c("XP_004487758", "XP_004488550", "XP_004501961")
#' my_aa <- refseq_AAseq(xp)}
#' # Now, the `AAStringSet`can be easily used to make a fasta file :
#' # writeXStringSet(x= my_aa, filepath = "aa_result")
#'
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom Biostrings AAStringSet


refseq_AAseq <- function(xp) {
  my_aa <- sapply(xp, function(x) {
    # fetch amino acid sequence in fasta format
    protein_fasta <- rentrez::entrez_fetch(db="protein", id=x, rettype="fasta")
    protein_tidy <-  strsplit(protein_fasta, "\n")
    protein_tidy <- as.character(paste0(protein_tidy[[1]][2:length(protein_tidy[[1]])], collapse = ""))

  }, USE.NAMES = F)

  my_aa <-  AAStringSet(my_aa)
  names(my_aa) <-  xp
  my_aa

}
