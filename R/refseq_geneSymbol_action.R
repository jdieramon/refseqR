#' @title Get the gene symbol
#'
#' @description \code{refseq_geneSymbol_action()} Returns the gene symbol from a single Gene id. accession.
#'
#' @usage
#' refseq_geneSymbol_action (id, db, retries)
#'
#' @param id A character string of the transcript or protein id.
#' @param db A character string of the "nuccore" or "protein" database.
#' @param retries A numeric value to control the number of retry attempts to handle internet errors.
#'
#' @returns A character vector containing the gene symbol corresponding to the especified accession as `id`.
#'
#' @seealso \code{\link{refseq_protein2RNA}} to obtain the XM ids that encode a set of XP ids.
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @author Jose V. Die

refseq_geneSymbol_action <- function(id, db = "protein", retries = 3) {

  tryCatch({

    if (db == "protein") {
      id_elink = rentrez::entrez_link(dbfrom = "protein", id = id, db= "gene")
      gene_id = id_elink$links$protein_gene

    } else {
      id_elink = rentrez::entrez_link(dbfrom = "nuccore", id = id, db= "gene")
      gene_id = id_elink$links$nuccore_gene
    }

  }, error = function(e) {
    if (inherits(e, "error")) {
      if (grepl("HTTP error: 502", e$message) && retries > 0) {
        message("Retrying...\n")
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        return(refseq_geneSymbol_action(id, db, retries - 1))
      } else {
        stop(e)
      }

    }
  })

  gene_summ = rentrez::entrez_summary(db = "gene", id = gene_id)
  return(gene_summ$name)

}
