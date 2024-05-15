#' @title Extract the CDS nucleotide sequence into a Biostrings object
#'
#' @description \code{refseq_CDSseq()} Parses a single/multiple XM acessions (Genbank format) and extract
#' the CDS nucleotide sequences into a \code{DNAStringSet} object.
#'
#' @usage
#' refseq_CDSseq(xm)
#'
#' @param xm A character string of the single/multiple XM id.
#'
#' @returns An object of \code{DNAStringSet} class.
#'
#' @seealso \code{\link{refseq_CDScoords}}
#'
#' @examples \donttest{
#' xm <-  c("XM_004487701", "XM_004488493", "XM_004501904")
#' my_cds <- refseq_CDSseq(xm)
#' # Now, the `DNAStringSet` can easily used to make a fasta file :
#' # writeXStringSet(x= my_cds, filepath = "cds_result")}
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom Biostrings DNAStringSet
#' @importFrom IRanges start end


refseq_CDSseq <- function(xm) {
  res     <-  refseq_CDScoords(xm)
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


