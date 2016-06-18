processData.cuyahoga <- function(viz.id='CuyahogaShort', Cuyahoga=readData('Cuyahoga'), version=1, outfile='cache/process/CuyahogaShort.tsv') {
  cuyahoga <- Cuyahoga[30:50,]
  write.table(cuyahoga, outfile, row.names=FALSE, sep='\t')
}