#' @title Get the protein accession from the transcript accession
#'
#' @description \code{refseq_RNA2protein()} Returns the protein accession from a single transcript accession.
#'
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_RNA2protein(transcript)
#'
#' @param transcript A character string of the protein accession. 
#'
#' @returns A character vector containing the protein id encoded by the mRNA especified as `transcript`.
#'
#' @seealso \code{\link{refseq_protein2RNA}} to obtain the transcript ids that encode a set of proteins ids.
#'
#' @examples
#'  \dontrun{
#'  # Get the protein id from a single transcript accession
#'  transcript <- "XM_004487701"
#'  refseq_RNA2protein(transcript)
#'
#'  # Get the protein ids from a set of transcript accessions
#'  transcript = c("XM_004487701", "XM_004488493")
#'  sapply(transcript, function(x) refseq_RNA2protein(x), USE.NAMES = FALSE) }
#'
#' @author Jose V. Die
#'
#' @export


refseq_RNA2protein <- function(transcript) {
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
    Sys.sleep(4) #if error, sleep 2 sec, then redo
    protein_elink <- rentrez::entrez_link(dbfrom = "nuccore", id = transcript, db = "protein")
    protein_id <- protein_elink$links$nuccore_protein
    protein <- rentrez::entrez_summary(db = "protein", id = protein_id)
    return(ifelse(is.na(protein$caption), NA, protein$caption))
    
  })
}

