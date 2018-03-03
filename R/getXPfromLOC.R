#'  Get the XP accession from LOC symbol
#'
#' Returns a character vector containing the XP accession from a from a single /
#' multiple LOC symbols.
#'
#' @usage
#' getXPfromLOC(locIDs)
#' @param
#' locIDs a characyer vector containing a single / multiple LOC symbols.
#'
#' @examples
#' locIDs = c("LOC101515097", "LOC101515098", "LOC101515099")
#' getXPfromLOC(locIDs)
#'
#' @author Jose V. Die
#' @export
#'
#'

getXPfromLOC <- function(locIDs){
    ncbi = character()                   # initializes a vector for XP ids
    for(loc in locIDs) {
        gene_id = gsub("LOC", "", loc)   # remove "LOC" to keep just the id
        loc_links = rentrez::entrez_link(dbfrom = "gene", id = gene_id, db = "all")

        # if the link has a protein, it is under: loc_links$links$gene_protein
        if("gene_protein" %in% names(loc_links$links)) {
            p_id = loc_links$links$gene_protein

            # It may be > 1 protein
            for(i in 1:length(p_id)) {
                xp_summary = rentrez::entrez_summary(db = "protein", id = p_id[i])
                xp = xp_summary$caption
                ncbi = c(ncbi, xp)

            }
        } else {                        # if the link does not have any protein
            ncbi = c(ncbi, "pseudo")
            }
    }

    ncbi
}
