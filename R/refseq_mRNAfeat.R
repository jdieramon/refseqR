#' @title Get mRNA features
#'
#' @description \code{refseq_mRNAfeat()} Returns a number of features from a single/multiple mRNA accession(s).
#'
#' Depending on the function, available accessions in \code{refseqR} include RefSeq models with the prefixes XM_ (mRNA), XR_ (non-coding RNA), and XP_ (protein), as well as subsequently curated RefSeq records with NM_, NR_, or NP_ accession prefixes.
#' 
#' @usage
#' refseq_mRNAfeat(transcript , feat)
#'
#' @param transcript A character string of the transcript id.
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
#' @seealso \code{\link{refseq_fromGene}} to obtain the XP or transcript accession from a single gene id. accession.
#' @seealso \code{\link{refseq_mRNA2protein}} to obtain the protein accessions encoded by a set of transcript ids.
#'
#' @examples
#' # Get several molecular features from a set of mRNA accessions
#' transcript = c("XM_004487701", "XM_004488493", "XM_004501904")
#' feat = c("caption", "moltype", "sourcedb", "slen")
#' refseq_mRNAfeat(transcript ,feat)
#'
#' @author Jose V. Die
#'
#' @export
#'
#' @importFrom tibble tibble

refseq_mRNAfeat <- function(transcript, feat) {

  # Defensive programming : check for allowed features
  toMatch <- c("caption", "moltype", "sourcedb", "updatedate", "slen", "organism", "title")
  if (sum(!feat %in% toMatch) != 0) {
    stop("Error. Allowed features: 'caption', 'moltype', 'sourcedb', 'updatedate',
         'slen', 'organism', 'title'")

  } else {
    mrna <-  rentrez::entrez_summary(db="nuccore", id= transcript)
    mrna <-  rentrez::extract_from_esummary(esummaries = mrna, elements = feat)


    # Build dataframe
    df <-  data.frame(matrix(unlist(mrna), nrow = length(mrna)/length(feat),
                             byrow = T),
                      stringsAsFactors = F)

    colnames(df) = feat

  }
  tibble::tibble(df)
}
