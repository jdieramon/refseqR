#'  Save the CDS nucleotide sequence into a file
#'
#' Parses a single/multiple XM acessions (Genbank format) and save
#' the CDS nucleotide sequences into a single / multiple fasta file.
#'
#' @usage
#' save_CDSfasta_from_xms(xmsIds, nameFile)
#' @param
#' xmsIds a vector containing a single/multiple XM ids.
#' @param
#' nameFile the name of the output file (do not include "fasta").
#'
#'
#' @examples \dontrun{
# xms <- c("XM_020386193", "XM_020389493", "XM_020394534")
# save_CDSfasta_from_xms(xms, "my_CDS")}
#'
#'
#' @author Jose V. Die
#' @export

save_CDSfasta_from_xms <- function(xmsIds, nameFile) {
    for(i in seq(length(xmsIds))) {
        target = rentrez::entrez_fetch(db="nuccore", id=xmsIds[i], rettype="gp")
        coord = extract_CDSfrom_xm(target)
        cds = rentrez::entrez_fetch(db="nucleotide", id=xmsIds[i], rettype="fasta",
                           seq_start = coord$startCDS, seq_stop = coord$stopCDS)

        # save nucleotide sequences into a FASTA file ("nameFile"")
        write(cds, file= paste(nameFile, ".fasta", sep = ""), append = TRUE)
    }
}
