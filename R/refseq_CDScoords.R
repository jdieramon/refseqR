#' Extract the coding sequences (CDS) coordinates from a XM accession
#'
#' `refseq_CDScoords()` Parses an XM accession (Genbank format) and extract the CDS coordinates.
#' The CDS coordinates refer to the mRNA molecule.
#'
#' @param xm A character string of the single/multiple XM id.
#'
#' @return An `IRanges` object with the start and end position of the CDS of the
#' putative mRNAs.
#'
#' @examples
#' xm = c("XM_004487701")
#' refseq_CDScoords(xm)
#'
#' \donttest{
#' xm = c("XM_004487701", "XM_004488493")
#' refseq_CDScoords(xm)}
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom IRanges IRanges


refseq_CDScoords <- function(xm) {
  # initialize some containers
  start <- c()
  stop <- c()
  version <- c()

  # loop over the elements of the input
  for (x in xm) {
    # get the character vector containing the fetched record
    target <- rentrez::entrez_fetch(db = "nuccore", id = x, rettype = "gp")
    # remove whitespaces from the string
    listName <- gsub(" ", "", target)
    listName <- strsplit(listName, "\n")

    cds <- grep("^CDS", listName[[1]], value = TRUE)
    if (length(cds) != 1) {
      stop("Exactly one CDS per file is expected. Contact the maintainer")
    }

    # remove characters "CDS" from the string
    cds <- gsub("CDS", "", cds)
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
