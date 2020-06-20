#'  Get the XM accession from LOC symbol
#'
#' Returns a character vector containing the XM accession from a from a single /
#' multiple LOC symbols.
#'
#' @usage
#' getXMfromLOC(locIDs)
#' @param
#' locIDs a characyer vector containing a single / multiple LOC symbols.
#'
#' @examples
#' locIDs <- c("LOC101515097", "LOC101515098", "LOC101515099")
#' getXMfromLOC(locIDs)
#' @author Jose V. Die
#' @export
#'
#'
getXMfromLOC <- function(locIDs) {
    ncbi <- character()
    for (loc in locIDs) {
        gene_id <- gsub("LOC", "", loc) # remove "LOC" to keep just the id
        loc_links <- rentrez::entrez_link(dbfrom = "gene", id = gene_id, db = "all")

        # if there is any mRNA ( = Gene type: protein coding)
        if ("gene_nuccore_refseqrna" %in% names(loc_links$links)) {
            # the mRNA id is under the follwoing link:
            mrna_id <- loc_links$links$gene_nuccore_refseqrna # extract just the mRNA

            # it may be > 1 mRNA
            for (i in 1:length(mrna_id)) {
                xm_summary <- rentrez::entrez_summary(db = "nuccore", id = mrna_id[i])
                ncbi <- c(ncbi, xm_summary$caption)
            }
        } else { # If there is not any mRNA (Gene type: pseudo)
            ncbi <- c(ncbi, "pseudo")
        }
    }

    ncbi
}
