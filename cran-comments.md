## R CMD check 
R CMD check results  
0 errors ✔ | 0 warnings ✔ | 0 notes ✔  
R CMD check succeeded  

## Resubmission
This is a resubmission. In this version I have made the following changes in response to Benjamin Altmann's comments:
  
  * Please only write package names, software names and API (application programming interface) names in single quotes in title and description.   **'RefSeq' is changed to RefSeq and 'GenBank' is changed to GenBank from the Title filed in the DESCRIPTION file. 'NCBI' , 'RefSeq’, and 'mRNA' are changed to NCBI, RefSeq and mRNA, respectively from the Description field in the DESCRIPTION file. Function titles and details have been corrected as well.**
 If "Possibly misspelled words in DESCRIPTION: GenBank (4:10) RefSeq (3:53, 15:33) mRNA (16:47)", **These are abbreviations.**
  
  * Please provide a link to the used webservices (RefSeq database) to the description field of your DESCRIPTION file in the form <http:...> or <https:...> with angle brackets for auto-linking and no space after 'http:' and 'https:'. **FIXED.**  
    
  * Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar). Missing Rd-tags: refseq_AAseq.Rd: \value refseq_CDSseq.Rd: \value refseq_mol_wt.Rd: \value **FIXED.**  
    
* Unexecutable code in man/refseq_CDSseq.Rd: I believe you forgot to comment out a line there.**FIXED.**  

* You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of cat() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions) -> R/refseq_fromGene.R **FIXED.**
