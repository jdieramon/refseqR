#' Extract the coding sequences (CDS) coordinates from a XM accession
#'
#' Parses an XM accession (Genbank format) and extract the CDS coordinates.
#' The CDS coordinates refer to the mRNA molecule.
#'
#' @param listName a downloaded flat file from the nuccore NCBI database
#' @return An IRanges object with the start and end position of the CDS of the
#' putative mRNA.
#' @examples
#' xm <- "XM_020388824"
#' # First, get the character vector containing the fetched record
#' target <- rentrez::entrez_fetch(db = "nuccore", id = xm, rettype = "gp")
#' extract_CDSfrom_xm(target)
#' @author Jose V. Die
#' @export
#' @importFrom IRanges IRangres

extract_CDSfrom_xm <- function(listName) {
    # remove whitespaces from the string
    listName <- gsub(" ", "", listName)
    listName <- strsplit(listName, "\n")

    cds <- grep("^CDS", listName[[1]], value = TRUE)
    if (length(cds) > 1) {
        stop("Multiple CDS per file. Contact the maintainer")
    }
    # remove characters "CDS" from the string
    cds <- gsub("CDS", "", cds)
    # remove special symbols ".." from the string
    cds <- strsplit(cds, "..", fixed = TRUE)
    # elements 1-3 of the list contain the start/stop coordinates
    start <- as.numeric(cds[[1]][1])
    stop <- as.numeric(cds[[1]][2])
    version <- grep("VERSION", listName[[1]], fixed = TRUE, value = TRUE)
    version <- gsub("VERSION", "", version, fixed = TRUE)
    IRanges(start = start, end = stop, names = version)
}
