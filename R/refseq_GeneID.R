#' @title Get the GeneID
#'
#' @description \code{refseq_GeneID()} Returns the GeneID from a single transcript or protein accession.
#'     
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_GeneID (accession, db, retries)
#'
#' @param accession A character string of the transcript or protein accession.
#' @param db A character string of the "nuccore" or "protein" database.
#' @param retries A numeric value to control the number of retry attempts to handle internet errors.
#'
#' @returns A character vector containing the GeneID corresponding to the specified accession as `accession`.
#'
#' @seealso \code{\link{refseq_protein2RNA}} to obtain the transcript accessions that encode a set of protein accessions.
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein accessions encoded by a set of transcript accessions.
#'
#' @examples
#' # Get the gene symbol from a set of transcript accessions
#' transcript = c("XM_004487701", "XM_004488493")
#' sapply(transcript, function(x) refseq_GeneID (x, db = "nuccore", retries = 4), USE.NAMES = FALSE)
#'
#' # Get the gene symbol from a set of protein accessions
#' protein = c("XP_004487758")
#' sapply(protein, function(x) refseq_GeneID (x, db = "protein", retries = 4), USE.NAMES = FALSE)
#'
#' @author Jose V. Die
#'
#' @export

refseq_GeneID <- function(accession, db = "protein", retries = 4) {

  tryCatch({

    if (db == "protein") {
      id_elink = rentrez::entrez_link(dbfrom = "protein", id = accession, db= "gene")
      gene_id = id_elink$links$protein_gene

    } else {
      id_elink = rentrez::entrez_link(dbfrom = "nuccore", id = accession, db= "gene")
      gene_id = id_elink$links$nuccore_gene
    }

  }, error = function(e) {
    if (inherits(e, "error")) {
      if (grepl("HTTP error: 502", e$message) && retries > 0) {
        message("Retrying...\n")
        Sys.sleep(5)  # Wait for 5 seconds before retrying
        return(refseq_GeneID(accession, db, retries - 1))
      } else {
        stop(e)
      }

    }
  })

  gene_summ = rentrez::entrez_summary(db = "gene", id = gene_id)
  return(gene_summ$uid)

}
