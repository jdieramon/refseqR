#' @title Extract the coding sequences (CDS) coordinates from a transcript accession
#'
#' @description \code{refseq_CDScoords()} Parses a transcript accession (RefSeq format) and extract the CDS coordinates.
#' The CDS coordinates refer to the mRNA molecule.    
#'   
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @param transcript A character string of the single/multiple transcript id.
#'
#' @seealso \code{\link{refseq_CDSseq}}
#'
#' @returns An `IRanges` object with the start and end position of the CDS of the
#' putative mRNAs.
#'
#' @examples
#' transcript = c("XM_004487701")
#' refseq_CDScoords(transcript)
#'
#' \donttest{
#' transcript = c("XM_004487701", "XM_004488493")
#' refseq_CDScoords(transcript)}
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom IRanges IRanges


refseq_CDScoords <- function(transcript) {
  # initialize some containers
  start <- c()
  stop <- c()
  version <- c()
  
  # loop over the elements of the input
  for (x in transcript) {
    # get the character vector containing the fetched record
    target <- rentrez::entrez_fetch(db = "nuccore", id = x, rettype = "gp")
    # remove whitespaces from the string
    listName <- gsub(" ", "", target)
    listName <- strsplit(listName, "\n")
    
    cds <- grep("^CDS", listName[[1]], value = TRUE)
    filtered_cds <- cds[!grepl("CDSuses", cds)]
    
    if (length(filtered_cds) != 1) {
      stop("Exactly one CDS per file is expected. Contact the maintainer")
    } 
    
    # remove characters "CDS" from the string
    cds <- gsub("CDS", "", filtered_cds)
    # remove special symbols ".." from the string
    cds <- strsplit(cds, "..", fixed = TRUE)
    
    # elements 1-3 of the list contain the start/stop coordinates
    start <- c(start, as.numeric(cds[[1]][1]))
    stop <- c(stop, as.numeric(cds[[1]][2]))
    vxm <- grep("VERSION", listName[[1]], fixed = TRUE, value = TRUE)
    version <- c(version, gsub("VERSION", "", vxm, fixed = TRUE))
    
  }
  
  # Build IRanges object
  IRanges::IRanges(start = start, end = stop, names = version)
}
