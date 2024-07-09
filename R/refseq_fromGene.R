#' @title Get the mRNA or protein accession
#'
#' @description \code{refseq_fromGene()} Returns the mRNA or protein accession from a single GeneID.
#'
#' @usage
#' refseq_fromGene(GeneID,sequence, retries)
#'
#' @param GeneID A character string of the GeneID.
#' @param sequence A character string of the mRNA or protein accession to fetch data from mRNA or protein databases, respectively.
#' @param retries A numeric value to control the number of retry attempts to handle 502 errors.
#'
#' @returns A character vector containing the mRNA or protein accession corresponding to the especified `GeneID`.
#'
#' @seealso \code{\link{refseq_protein2mRNA}} to obtain the transcript accessions that encode a set of protein accessions.
#' @seealso \code{\link{refseq_mRNA2protein}} to obtain the protein accessions encoded by a set of transcript accessions.
#'
#' @examples \donttest{
#' # Get the XM accessions from a set of gene ids
#' locs <- c("LOC101512347")
#' sapply(locs, function(x) refseq_fromGene (x, sequence = "XM", retries = 3), USE.NAMES = FALSE)
#'
#' # Get the XP accessions from a set of gene ids
#' locs <- c("LOC101512347")
#' sapply(locs, function(x) refseq_fromGene (x, sequence = "XP", retries = 3), USE.NAMES = FALSE)}
#'
#' @author Jose V. Die
#'
#' @export

refseq_fromGene <- function (GeneID, sequence = "XM", retries = 3) { 
  
  ncbi = c()
  gene_id <- rentrez::entrez_search(db = "gene", term = GeneID)
  loc_links <- rentrez::entrez_link(dbfrom = "gene", id = gene_id$ids, 
                                    db = "all")
  
  tryCatch({
    if (sequence == "XM") {
      if ("gene_nuccore" %in% names(loc_links$links)) {
        mrna_id <- loc_links$links$gene_nuccore_refseqrna
        if(is.null(mrna_id)) {
          ncbi <- paste(paste0("NOTE: The GeneID=", GeneID), "is not listed with any transcript sequence in RefSeq")
        } else {
          for (i in 1:length(mrna_id)) {
            xm_summary <- rentrez::entrez_summary(db = "nuccore", 
                                                  id = mrna_id[i])
            ncbi <- c(ncbi, xm_summary$caption)
          } 
          
        }
      } else { ncbi <- c(ncbi, "no RefSeq transcript") }
    }
    else if (sequence == "XP") {
      if ("gene_protein_refseq" %in% names(loc_links$links)) {
        p_id <- loc_links$links$gene_protein_refseq
        if(is.null(p_id)) { 
          ncbi <- paste(paste0("NOTE: The GeneID=", GeneID), "is not listed with any protein sequence in RefSeq")}
      } else {
        for (i in 1:length(p_id)) {
          xp_summary <- rentrez::entrez_summary(db = "protein", 
                                                id = p_id[i])
          
          ncbi <- c(ncbi, xp_summary$caption)
        }
        
      }
    } else { ncbi <- c(ncbi, "pseudo/no Refseq protein") }
    
  }, error = function(e){
    if (inherits(e, "error")) {
      if (grepl("HTTP error: 502", e$message) && retries > 
          0) {
        message("Retrying...\n")
        Sys.sleep(5)
        return(refseq_fromGene(GeneID, sequence, 
                               retries - 1))
      }
      else {
        stop(e)
      }
    }
  })
  return(ncbi)
}
