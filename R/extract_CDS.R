#' Extract the CDS coordinates from a XM accession
#'
#' Parses an XM acession (Genbank format) and extract the CDS coordinates.
#' The CDS coordinates refer to the mRNA molecule.
#'
#' @usage
#' extract_CDSfrom_xm(listName)
#' @param
#' listName a downloaded flat file from the nuccore NCBI database
#'
#' @return the start / end CDS coordinates refer to the mRNA molecule
#' @examples
#' xm <- "XM_020388824"
#' # First, get the character vector containing the fetched record
#' target <- rentrez::entrez_fetch(db = "nuccore", id = xm, rettype = "gp")
#' extract_CDSfrom_xm(target)
#' @author Jose V. Die
#' @export


extract_CDSfrom_xm <- function(listName) {
    listName <- strsplit(listName, "\n")
    for (i in seq(listName[[1]])) {
        val <- listName[[1]][i]
        # remove whitespaces from the string
        val <- gsub(" ", "", val)

        # check for feature
        if (substr(val, 1, 3) == "CDS") {
            # remove characters "CDS" from the string
            val <- gsub("CDS", "", val)
            # remove special symbols "..." from the string
            val <- strsplit(val, "[..]")
            # elements 1-3 of the list contain the start/stop coordinates
            start <- as.numeric(val[[1]][1])
            stop <- as.numeric(val[[1]][3])
            cds <- list(startCDS = start, stopCDS = stop)

            # return for downstream analysis
            return(cds)
        }
    }
}
