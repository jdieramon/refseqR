#' @title Get the gene symbol
#'
#' @description \code{refseq_geneSymbol()} Returns the gene symbol from a single Gene id. accession.
#'
#' @usage
#' refseq_geneSymbol (id, db)
#'
#' @param id A character string of the transcript or protein id.
#' @param db A character string of the "nuccore" or "protein" database.
#'
#' @returns A character vector containing the gene symbol corresponding to the especified accession as `id`.
#'
#' @seealso \code{\link{refseq_protein2RNA}} to obtain the transcript ids that encode a set of protein ids.
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein ids encoded by a set of transcript ids.
#'
#' @examples
#' \donttest{
#' # Get the gene symbol from a set of transcript accessions
#' id = c("XM_004487701", "XM_004488493")
#' sapply(id, function(x) refseq_geneSymbol (x, db = "nuccore"), USE.NAMES = FALSE)
#'
#' # Get the gene symbol from a set of XP accessions
#' id = c("XP_004487758")
#' sapply(id, function(x) refseq_geneSymbol (x, db = "protein"), USE.NAMES = FALSE)}
#'
#' @author Jose V. Die
#'
#' @export

refseq_geneSymbol <- function(id, db = "protein") {
  tryCatch(
    expr    = {refseq_geneSymbol_action(id, db, retries = 4)},
    error   = function(e) {message("NCBI servers are busy. Please try again a bit later.")},
    warning = function(w) {message("NCBI servers are busy. Please try again a bit later.")}
  )
}
