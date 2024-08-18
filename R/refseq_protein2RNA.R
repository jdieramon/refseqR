#' @title  Get the transcript accession from the protein accession
#'
#' @description \code{refseq_protein2RNA()} Returns the transcript accession from a single protein accession.
#'     
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_protein2RNA(protein)
#'
#' @param protein A character string of the protein id.
#'
#' @returns A character vector containing the transcript ids that encode the `protein`.
#'
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein ids encoded by a set of transcript ids.
#'
#' @examples
#'  # Get the transcript id from a single protein accession
#'  protein <- "XP_020244413"
#'  refseq_protein2RNA(protein)
#'
#' \donttest{
#'  # Get the transcript ids from a set of protein accessions
#'  protein = c("XP_004487758", "XP_004488550")
#'  sapply(protein, function(x) refseq_protein2RNA(x), USE.NAMES = FALSE)}
#'
#' @author Jose V. Die
#'
#' @export

refseq_protein2RNA <- function(protein) {
  tryCatch({
    transcript_elink <- rentrez::entrez_link(dbfrom = "protein", 
                                             id = protein, db = "nuccore")
    
    transcript_id <- transcript_elink$links$protein_nuccore_mrna
    if(is.null(transcript_id)) {
      return(paste(paste0("NOTE: The protein id. ", protein), "is not listed with any transcript sequence in RefSeq"))
    } else{
      transcript <- rentrez::entrez_summary(db = "nuccore", 
                                            id = transcript_id)
      return(ifelse(is.na(transcript$caption), NA, transcript$caption))
    }
  }, error = function(e)  {
    Sys.sleep(2) #if error, sleep 2 sec, then redo
    transcript_elink <- rentrez::entrez_link(dbfrom = "protein", id = protein, db = "nuccore")
    transcript_id <- transcript_elink$links$protein_nuccore_mrna
    transcript <- rentrez::entrez_summary(db = "nuccore", id = transcript_id)
    return(ifelse(is.na(transcript$caption), NA, transcript$caption))
    
  })
}

