#' My mayfly fetcher function
#' 
#' Intentionally switching up the arg order just to show that it can be done
#' (though it's not necessarily a great idea)
processData.cuyahoga <- function(Cuyahoga=readData('Cuyahoga'), version=1, ..., outfile='cache/process/CuyahogaShort.tsv') {
  cuyahoga <- Cuyahoga[30:50,]
  write.table(cuyahoga, outfile, row.names=FALSE, sep='\t')
}