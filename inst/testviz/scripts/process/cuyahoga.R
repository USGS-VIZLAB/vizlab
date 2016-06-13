processData.cuyahoga <- function(Cuyahoga=readData('Cuyahoga'), version=1, outfile='cache/process/CuyahogaShort.tsv') {
  cuyahoga <- Cuyahoga[30:50]
  write.table(cuyahoga, outfile, header=TRUE, row.names=FALSE, sep='\t')
}