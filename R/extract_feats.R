#' Extract some features from an XM accession
#'
#' Parses an XM accession (Genbank format) and extract either sex, tissue or
#' genotype.
#'
#' @param listName a downloaded flat file from the nuccore NCBI database
#' @param feat a feature to be extracted. Allowed features include "tissue",
#' "sex", or "genotype". Default is tissue.
#' @examples
#' xm <- "XM_020388824"
#' # First, get the character vector containing the fetched record
#' mrna_gb <- rentrez::entrez_fetch(db = "nuccore", id = xm, rettype = "gp")
#' extract_from_xm(mrna_gb, feat = "sex")
#' extract_from_xm(mrna_gb, feat = "genotype")
#' extract_from_xm(mrna_gb, feat = "tissue")
#' @author Jose V. Die
#' @export
extract_from_xm <- function(listName, feat = c("tissue", "sex", "genotype")) {

    # Check for allowed features
    feat <- match.arg(feat)
    # remove whitespaces from the string
    listName <- gsub(" ", "", listName, fixed = TRUE)
    # remove "/" symbol from the string
    listName <- gsub("/", "", listName, fixed = TRUE)
    # Remove quotes and protection for them
    listName <- gsub('\\\"', "", listName, fixed = TRUE)
    # Split by line
    listName <- strsplit(listName, "\n", fixed = TRUE)
    # Look up for the features
    v <- vector("character", length(listName[[1]]))
    for (i in seq(listName[[1]])) {
        val <- listName[[1]][i]
        # Check for features
        if (grepl(feat, val, fixed = TRUE)) {
            v[i] <- gsub(".+=(.+)$", "\\1", val)
        }
    }
    # Return values (Assuming there can be several otherwise it can be faster)
    v <- unique(v)
    v[v != ""]
}
