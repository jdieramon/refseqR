#' @title Get mRNA features
#'
#' @description \code{refseq_fromXM()} Returns a number of features from a single/multiple mRNA accession(s).
#'
#' @usage
#' refseq_fromXM(xm , feat)
#'
#' @param xm A character string of the XM id.
#' @param feat A character string of the selected features. Allowed features: 'caption',
#' 'moltype', 'sourcedb', 'updatedate', 'slen', 'organism', 'title'.
#'
#' @returns A \code{tibble} of summarized results including columns:
#' \itemize{
#' \item caption, mRNA accession
#' \item moltype, type of molecule
#' \item sourcedb, database (GenBank)
#' \item updatedate, date of updated record
#' \item slen, molecule length (in bp)
#' \item organism
#' \item title, sequence description
#'   }
#'
#' @seealso \code{\link{refseq_fromGene}} to obtain the XP or XM accession from a single gene id. accession.
#' @seealso \code{\link{refseq_XPfromXM}} to obtain the XP ids encoded by a set of XM ids.
#'
#' @examples
#' # Get several molecular features from a set of mRNA accessions
#' xm = c("XM_004487701", "XM_004488493", "XM_004501904")
#' feat = c("caption", "moltype", "sourcedb", "slen")
#' refseq_fromXM(xm ,feat)
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom tibble tibble

refseq_fromXM <- function(xm, feat) {

  # Defensive programming : check for allowed features
  toMatch <- c("caption", "moltype", "sourcedb", "updatedate", "slen", "organism", "title")
  if (sum(!feat %in% toMatch) != 0) {
    stop("Error. Allowed features: 'caption', 'moltype', 'sourcedb', 'updatedate',
         'slen', 'organism', 'title'")

  } else {
    mrna <-  rentrez::entrez_summary(db="nuccore", id= xm)
    mrna <-  rentrez::extract_from_esummary(esummaries = mrna, elements = feat)


    # Build dataframe
    df <-  data.frame(matrix(unlist(mrna), nrow = length(mrna)/length(feat),
                           byrow = T),
                    stringsAsFactors = F)

    colnames(df) = feat

  }
  tibble::tibble(df)
}
