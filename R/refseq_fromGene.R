#'  Get the mRNA or protein accession
#'
#' `refseq_fromGene` Returns the XP or XM accession from a single gene id. accession.
#'
#' @usage
#' refseq_fromGene(gene_symbol,sequence)
#' @param
#' gene_symbol a character string of the gene symbol.
#' @param
#' sequence a character string of the "XM" or "XP" to fetch data from mRNA or protein databases, respectively.
#'
#' @returns A character vector containing the XP or XM id accessions corresponding to the especified `gene symbol`.
#'
#' @seealso \code{\link{refseq_XMfromXP}} to obtain the XM ids that encode a set of XP ids.
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#' # Get the XM accessions from a set of gene ids
#' locs <- c("LOC101512347", "LOC101506901")
#' sapply(locs, function(x) refseq_fromGene (x, sequence = "XM"), USE.NAMES = FALSE)
#'
#' # Get the XP accessions from a set of gene ids
#' locs <- c("LOC101512347", "LOC101506901")
#' sapply(locs, function(x) refseq_fromGene (x, sequence = "XP"), USE.NAMES = FALSE)
#' @author Jose V. Die
#'
#' @export
#'
#'

refseq_fromGene <- function(gene_symbol , sequence = "XM") {

    ncbi = c()
    gene_id <- rentrez::entrez_search(db = "gene", term = gene_symbol)
    loc_links <- rentrez::entrez_link(dbfrom = "gene", id = gene_id$ids, db = "all")

    if (sequence == "XM") {

        if ("gene_nuccore" %in% names(loc_links$links)) {
            # if there is any mRNA ( = Gene type: protein coding)
            # the mRNA id is under the following link:
            mrna_id <- loc_links$links$gene_nuccore_refseqrna # extract just the mRNA

            # it may be > 1 mRNA
            for (i in 1:length(mrna_id)) {
                xm_summary <- rentrez::entrez_summary(db = "nuccore", id = mrna_id[i])
                ncbi <- c(ncbi, xm_summary$caption)
            }
        } else { # If there is not any mRNA (Gene type: pseudo)
            ncbi <- c(ncbi, "pseudo")
        }

    } else if (sequence == "XP") {
        if ("gene_protein_refseq" %in% names(loc_links$links))  {
            # the protein id is under the following link:
            p_id <- loc_links$links$gene_protein_refseq
            # It may be > 1 protein
            for (i in 1:length(p_id)) {
                xp_summary <- rentrez::entrez_summary(db = "protein", id = p_id[i])
                xp <- xp_summary$caption
                ncbi <- c(ncbi, xp)
            }
        } else { # if the link does not have any protein
            ncbi <- c(ncbi, "pseudo/no refseq protein")
        }
    }
    ncbi
}
