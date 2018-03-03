#' Extract the molecular weigth from an XP accession
#'
#' Parses an XP acession output (Genbank format) and extract the molecular weight
#' (in Daltons). First it is needed to fecth the protein id info (using the
#' 'rentrez' packg). Then, the function takes that output as an argument.
#'
#' @usage
#' extract_mol.wt_from_xp(listName, feat = "calculated_mol_wt")
#' @param
#' listName a downloaded flat file from the protein NCBI database
#' @param
#' feat a feature to be extracted. Allowed feature is only "calculated_mol_wt"
#'
#' @seealso \code{\link[rentrez]{entrez_fetch}}
#'
#' @details
#' First, get the character vector containing the fetched record. Then, this
#' function parses the fectched record and returns the molecular weight.
#'
#' @examples
#' xp <-  "XP_020244413"
#' # First, get the character vector containing the fetched record
#' protein_gb <- rentrez::entrez_fetch(db = "protein", id = xp, rettype = "gp")
#' extract_mol.wt_from_xp(protein_gb)
#'
#' @author Jose V. Die
#' @export


extract_mol.wt_from_xp <- function(listName, feat ="calculated_mol_wt") {
    mol_wt = 0 # keep track of success
    listName = strsplit(listName, "\n")
    for(i in seq(listName[[1]])) {
        val <- listName[[1]][i]
        #remove whitespaces from the string
        val = gsub(" ", "", val)
        #remove "/" symbol from the string
        val = gsub("/", "", val)
        # split the string from "="
        val = strsplit(val, "=")

        if(feat %in% val[[1]][1]) {
            # 2nd element of the list contains the mol.wt
            return(as.numeric(val[[1]][2]))
            mol_wt = mol_wt+1
        }
    }
    # Defensive Programming
    # if the loop reaches the last entry of the list and couldn´t find the 'feat', return 0
    if(i == length(listName[[1]]) & mol_wt == 0) {return(0)}
}
