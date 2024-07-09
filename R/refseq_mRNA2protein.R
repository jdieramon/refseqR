#' @title Get the XP accession from XM accession
#'
#' @description \code{refseq_mRNA2protein()} Returns the protein accession from a single transcript accession.
#'
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_mRNA2protein(transcript)
#'
#' @param transcript A character string of the protein accession. 
#'
#' @returns A character vector containing the protein id encoded by the mRNA especified as `transcript`.
#'
#' @seealso \code{\link{refseq_protein2mRNA}} to obtain the transcript ids that encode a set of proteins ids.
#'
#' @examples
#'  # Get the protein id from a single transcript accession
#'  transcript <- "XM_004487701"
#'  refseq_mRNA2protein(transcript)
#'
#'  \donttest{
#'  # Get the protein ids from a set of transcript accessions
#'  transcript = c("XM_004487701", "XM_004488493")
#'  sapply(transcript, function(x) refseq_mRNA2protein(x), USE.NAMES = FALSE) }
#'
#' @author Jose V. Die
#'
#' @export


refseq_mRNA2protein <- function(transcript) {
  tryCatch({
    protein_elink <- rentrez::entrez_link(dbfrom = "nuccore", 
                                          id = transcript, db = "protein")
    
    protein_id <- protein_elink$links$nuccore_protein
    if(is.null(protein_id)) {
      return(paste(paste0("NOTE: The transcript id. ", transcript), "is not listed with any protein sequence in RefSeq"))
    } else{
      protein <- rentrez::entrez_summary(db = "protein", id = protein_id)
      return(ifelse(is.na(protein$caption), NA, protein$caption))
    }
  }, error = function(e)  {
    Sys.sleep(2) #if error, sleep 2 sec, then redo
    protein_elink <- rentrez::entrez_link(dbfrom = "nuccore", id = transcript, db = "protein")
    protein_id <- protein_elink$links$nuccore_protein
    protein <- rentrez::entrez_summary(db = "protein", id = protein_id)
    return(ifelse(is.na(protein$caption), NA, protein$caption))
    
  })
}

