# in R get summary for ref.fa.fai
x <- read.table("ref.fa.fai", header = F)
xsum <- summary(x$V2)
capture.output(xsum, file = "scaffold_distribution.txt")

# END