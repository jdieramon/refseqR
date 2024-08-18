#' @title Extract the amino acid sequence into a Biostrings object
#'
#' @description \code{refseq_AAseq()} Parses a single/multiple protein accessions (RefSeq format) and extract
#' the amino acid sequence(s) into a \code{AAStringSet} object.  
#'     
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_AAseq(accession)
#'
#' @param accession A character string containing a single/multiple accession ids.
#'
#' @returns An object of \code{AAStringSet} class.
#'
#' @examples
#' \donttest{
#' accession = c("XP_004487758", "XP_004488550", "XP_004501961")
#' my_aa <- refseq_AAseq(accession)}
#' # Now, the `AAStringSet`can be easily used to make a fasta file :
#' # writeXStringSet(x= my_aa, filepath = "aa_result")
#'
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom Biostrings AAStringSet


refseq_AAseq <- function(accession) {
  my_aa <- sapply(accession, function(x) {
    # fetch amino acid sequence in fasta format
    protein_fasta <- rentrez::entrez_fetch(db="protein", id=x, rettype="fasta")
    protein_tidy <-  strsplit(protein_fasta, "\n")
    protein_tidy <- as.character(paste0(protein_tidy[[1]][2:length(protein_tidy[[1]])], collapse = ""))

  }, USE.NAMES = F)

  my_aa <-  AAStringSet(my_aa)
  names(my_aa) <-  accession
  my_aa

}

