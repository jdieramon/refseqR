#' @title Extract the CDS nucleotide sequence into a Biostrings object
#'
#' @description \code{refseq_CDSseq()} Parses a single/multiple transcript accessions (RefSeq format) and extract
#' the CDS nucleotide sequences into a \code{DNAStringSet} object.  
#'   
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#'
#' @usage
#' refseq_CDSseq(transcript)
#'
#' @param transcript A character string of the single/multiple transcript id.
#'
#' @returns An object of \code{DNAStringSet} class.
#'
#' @seealso \code{\link{refseq_CDScoords}}
#'
#' @examples \donttest{
#' transcript <-  c("XM_004487701", "XM_004488493", "XM_004501904")
#' my_cds <- refseq_CDSseq(transcript)
#' # Now, the `DNAStringSet` can easily used to make a fasta file :
#' # writeXStringSet(x= my_cds, filepath = "cds_result")}
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom Biostrings DNAStringSet
#' @importFrom IRanges start end


refseq_CDSseq <- function(transcript) {
  res     <-  refseq_CDScoords(transcript)
  my_cds  <-  sapply(seq(res), function(i) {
    cds   <-  rentrez::entrez_fetch(db = "nuccore", id = names(res)[i],
                                    rettype = "fasta",
                                    seq_start = start(res)[i],
                                    seq_stop = end(res)[i])
    cds_tidy <-  strsplit(cds, "\n")
    cds_tidy <- as.character(paste0(cds_tidy[[1]][2:length(cds_tidy[[1]])], collapse = ""))

  }, USE.NAMES = F)

  my_cds <-  DNAStringSet(my_cds)
  names(my_cds) <-  names(res)
  my_cds

}


