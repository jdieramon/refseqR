#' @title Extract the molecular weight from a protein accession
#'
#' @description \code{refseq_AAmol_wt()} Parses a protein accession output (RefSeq format) and extract the molecular weight
#' (in Daltons).
#'
#'Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#'
#' @usage
#' refseq_AAmol_wt(protein)
#'
#' @param protein A character string of the protein id.
#'
#' @returns A numeric vector representing the molecular weight of the `protein`.
#'
#' @details
#' First, get the character vector containing the fetched record. Then, this
#' function parses the fetched record and returns the molecular weight.
#'
#' @seealso \code{\link{refseq_RNA2protein}} to obtain the protein ids encoded by a set of transcript ids.
#' 
#' @examples
#'  # Get the molecular weight from a single protein accession
#'  protein <- "XP_020244413"
#'  refseq_AAmol_wt(protein)
#'
#'  # Get the molecular weight from from a set of protein accessions
#'  protein = c("XP_004487758", "XP_004488550")
#'  sapply(protein, function(x) refseq_AAmol_wt(x), USE.NAMES = TRUE)
#'
#' @author Jose V. Die
#'
#' @export


refseq_AAmol_wt <- function(protein) {

  # Define the feature
  feat = "calculated_mol_wt"

  # Fetch the accession
  listName <- rentrez::entrez_fetch(db = "protein", id = protein, rettype = "gp")

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

