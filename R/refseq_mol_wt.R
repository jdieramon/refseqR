#' @title Extract the molecular weigth from an XP accession
#'
#' @description \code{refseq_mol_wt()} Parses an XP accession output (Genbank format) and extract the molecular weight
#' (in Daltons).
#'
#' @usage
#' refseq_mol_wt(xp)
#'
#' @param xp A character string of the XP id.
#'
#' @returns A numeric vector represeting the molecular weight of the protein(s) especified as `xp`.
#'
#' @details
#' First, get the character vector containing the fetched record. Then, this
#' function parses the fectched record and returns the molecular weight.
#'
#' @examples
#'  # Get the molecular weight from a single XP accession
#'  xp <- "XP_020244413"
#'  refseq_mol_wt(xp)
#'
#'  # Get the molecular weight from from a set of XP accessions
#'  xp = c("XP_004487758", "XP_004488550")
#'  sapply(xp, function(x) refseq_mol_wt(x), USE.NAMES = TRUE)
#'
#' @author Jose V. Die
#'
#' @export


refseq_mol_wt <- function(xp) {

  # Define the feature
  feat = "calculated_mol_wt"

  # Fetch the accession
  listName <- rentrez::entrez_fetch(db = "protein", id = xp, rettype = "gp")

  mol_wt <-  0 # keep track of success

    listName <-  strsplit(listName, "\n")

    for(i in seq(listName[[1]])) {
        val <- listName[[1]][i]
        #remove whitespaces from the string
        val <-  gsub(" ", "", val)
        #remove "/" symbol from the string
        val <-  gsub("/", "", val)
        # split the string from "="
        val <-  strsplit(val, "=")

        if(feat %in% val[[1]][1]) {
            # 2nd element of the list contains the mol.wt
            return(as.numeric(val[[1]][2]))
            mol_wt <-  mol_wt+1
        }
    }
    # Defensive Programming
    # if the loop reaches the last entry of the list and couldn´t find the 'feat', return 0
    if(i == length(listName[[1]]) & mol_wt == 0) {
      return(0)
      }
}

