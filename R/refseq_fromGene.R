#' @title Get the mRNA or protein accession
#'
#' @description \code{refseq_fromGene()} Returns the mRNA or protein accession from a single GeneID.
#'
#' @usage
#' refseq_fromGene(GeneID, sequence)
#'
#' @param GeneID A character string of the GeneID.
#' @param sequence A character string of the mRNA or protein accession to fetch data from mRNA or protein databases, respectively.
#'
#' @returns A character vector containing the mRNA or protein accession corresponding to the especified `GeneID`.
#'
#' @seealso \code{\link{refseq_protein2RNA}} to obtain the transcript accessions that encode a set of protein accessions.
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein accessions encoded by a set of transcript accessions.
#'
#' @examples \donttest{
#' # Get the XM accessions from a set of gene ids
#' locs <- c("LOC101512347")
#' sapply(locs, function(x) refseq_fromGene (x, sequence = "transcript"), USE.NAMES = FALSE)
#'
#' # Get the XP accessions from a set of gene ids
#' locs <- c("LOC101512347")
#' sapply(locs, function(x) refseq_fromGene (x, sequence = "protein"), USE.NAMES = FALSE)}
#'
#' @author Jose V. Die
#'
#' @export

refseq_fromGene <- function (GeneID, sequence = "transcript") {
  tryCatch(
    expr    = {refseq_fromGene_action(GeneID, sequence = sequence, retries = 4)},
    error   = function(e) {message("NCBI servers are busy. Please try again a bit later.")},
    warning = function(w) {message("NCBI servers are busy. Please try again a bit later.")}
  )
}
