#'  Save the amino acid sequence into a file
#'
#' Parses a single/multiple XP acessions (Genbank format) and save
#' the amino acid sequences into a single / multiple fasta file.
#'
#' @usage
#' save_AAfasta_from_xps(xpsIds, nameFile)
#' @param
#' xpsIds a vector containing a single/multiple XP ids.
#' @param
#' nameFile the name of the output file (do not include "fasta").
#'
#'
#' @examples
#' \dontrun{
#' # xps = c("XP_020271897", "XP_020271898", "XP_020271899")
#' # save_AAfasta_from_xps(xps, "my_AA")
#' }
#'
#' @author Jose V. Die
#' @export

save_AAfasta_from_xps <- function(xpsIds, nameFile) {
    for (i in seq(length(xpsIds))) {
        protein <- rentrez::entrez_summary(db = "protein", id = xpsIds[i])
        protein_fasta <- rentrez::entrez_fetch(db = "protein", id = protein$uid, rettype = "fasta")
        # save amino acid sequences into a FASTA file ("nameFile"")
        write(protein_fasta, file = paste(nameFile, ".fasta", sep = ""), append = TRUE)
    }
}
