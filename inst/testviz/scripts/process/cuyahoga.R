#' My mayfly fetcher function
#' 
#' Intentionally switching up the arg order just to show that it can be done
#' (though it's not necessarily a great idea)
process.cuyahoga <- function(version=1, ..., outfile='cache/process/CuyahogaShort.tsv') {
  Cuyahoga=readData('Cuyahoga')
  cuyahoga <- Cuyahoga[30:50,]
  write.table(cuyahoga, outfile, row.names=FALSE, sep='\t')
}