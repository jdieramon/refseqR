#' @title Get the sequence Description
#'
#' @description \code{refseq_description()} Returns the sequence description from a single transcript, protein, or GeneID accession.
#'    
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes transcript_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_description(id)
#'
#' @param id A character string of the transcript, protein, or GeneID accession.
#'
#' @returns A character vector containing the sequence description corresponding to the specified sequence as `id`.
#'
#' @seealso \code{\link{refseq_protein2RNA}} to obtain the transcript ids that encode a set of protein ids.
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein ids encoded by a set of transcript ids.
#'
#' @examples
#'  # Get the sequence descriptions from a set of transcript accessions
#'  transcript = c("XM_004487701")
#'  sapply(transcript, function(x) refseq_description(x), USE.NAMES = FALSE)
#'
#'  # Get the sequence descriptions from a set of protein accessions
#'  protein = c("XP_004487758")
#'  sapply(protein, function(x) refseq_description(x), USE.NAMES = FALSE)
#'
#' \donttest{
#' #' # Get the sequence descriptions from a set of Gene accessions
#' locs <- c("LOC101512347", "LOC101506901")
#' sapply(locs, function(x) refseq_description(x), USE.NAMES = FALSE)}
#'
#' @author Jose V. Die
#'
#' @export


refseq_description <- function(id) {
  esearch = rentrez::entrez_search(db = "gene", term = id)
  esumm = rentrez::entrez_summary(db = "gene", id = esearch$ids[1])
  # Get the description
  esumm$description }

