#' Extract some features from an XM accession
#'
#' Parses an XM acession (Genbank format) and extract some features provided by
#' the user.
#'
#' @usage
#' extract_from_xm(listName, feat = "tissue")
#' @param
#' listName a downloaded flat file from the nuccore NCBI database
#' @param
#' feat a feature to be extracted. Allowed features include "sex", "tissue" or "genotype"
#'
#' @examples
#' xm <- "XM_020388824"
#' # First, get the character vector containing the fetched record
#' mrna_gb <- rentrez::entrez_fetch(db = "nuccore", id = xm, rettype = "gp")
#' extract_from_xm(mrna_gb, feat = "sex")
#' extract_from_xm(mrna_gb, feat = "genotype")
#' extract_from_xm(mrna_gb, feat = "tissue")
#' @author Jose V. Die

extract_from_xm <- function(listName, feat = "tissue") {

    # Defensive programming : check for allowed features
    toMatch <- c("sex", "tissue", "genotype")
    if (!feat %in% toMatch) {
        message("Error. Allowed features: 'sex','genotype, 'tissue'")
    } else {
        listName <- strsplit(listName, "\n")
        for (i in seq(listName[[1]])) {
            val <- listName[[1]][i]
            # remove whitespaces from the string
            val <- gsub(" ", "", val)
            # remove "/" symbol from the string
            val <- gsub("/", "", val)

            # Check for lengths of allowed features
            if (length(grep(paste(toMatch, collapse = "|"), val, value = TRUE)) > 0) {
                if (substr(val, 1, 6) == feat) {
                    val <- strsplit(val, "\"")
                    print(val[[1]][2])
                }
                if (substr(val, 1, 3) == feat) {
                    val <- strsplit(val, "\"")
                    print(val[[1]][2])
                }
                if (substr(val, 1, 8) == feat) {
                    val <- strsplit(val, "\"")
                    print(val[[1]][2])
                }
            }
        }
    }
}
