#'  Get the gene symbol
#'
#' `refseq_geneSymbol()` Returns the gene symbol from a single Gene id. accession.
#'
#' @usage
#' refseq_geneSymbol (id, db)
#'
#' @param id A character string of the XP or XM id.
#' @param db A character string of the "nuccore" or "protein" database.
#'
#' @returns A character vector containing the gene symbol corresponding to the especified accession as `id`.
#'
#' @seealso \code{\link{refseq_XMfromXP}} to obtain the XM ids that encode a set of XP ids.
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#' # Get the gene symbol from a set of XM accessions
#' xm = c("XM_004487701", "XM_004488493")
#' sapply(xm, function(x) refseq_geneSymbol (x, db = "nuccore"), USE.NAMES = FALSE)
#'
#' # Get the gene symbol from a set of XP accessions
#' xp = c("XP_004487758")
#' sapply(xp, function(x) refseq_geneSymbol (x, db = "protein"), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export

refseq_geneSymbol <- function(id, db = "protein") {
  if (db == "protein") {
    id_elink = rentrez::entrez_link(dbfrom = "protein", id = id, db= "gene")
    gene_id = id_elink$links$protein_gene
  } else {
    id_elink = rentrez::entrez_link(dbfrom = "nuccore", id = id, db= "gene")
    gene_id = id_elink$links$nuccore_gene
  }

  gene_summ = rentrez::entrez_summary(db = "gene", id = gene_id)
  gene_summ$name

}
