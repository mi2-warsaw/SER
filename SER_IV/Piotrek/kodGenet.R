####uwaga, to nie jest oficjalny kod do zadnej publikacji naukowej, 
####przedstawione obliczenia maja tylko charakter pogladowy i czesto sa bardzo uproszczone

#podstawowe źródło danych: UCSC Genome Browser (http://genome.ucsc.edu/cgi-bin/hgGateway)




cytoBands <- read.csv("data/ChromosomeBand_hg19.csv", sep="\t")
head(cytoBands)



genes <- read.csv("data/RefSeq_hg19.csv", sep="\t")
genes[1,]


table(genes$chrom)

chroms <- paste("chr", c(1:22, "X", "Y"), sep="")

genes.proc <- genes[genes$chrom %in% chroms,]

g.chr1 <- genes.proc[genes.proc$chrom == "chr1",]



library(IRanges) #Bioconductor
ir <- IRanges(g.chr1$txStart, g.chr1$txEnd)
ir

width(ir)

ir2 <- ir[1:4]
width(ir2)
ir2 + 1


####prosty przyklad

ir3 <- IRanges(1:5, 3:7)
reduce(ir3)


ir4 <- IRanges(c(1,5,8), c(2,9,9))
##liczymy teraz sumę pokrytych przedziałów

gaps(ir4)
shift(ir4, 3)
intersect(ir3, ir4)
pintersect(ir3, ir4) #blad - dlaczego?
pintersect(ir4, ir3[1:3], resolve.empty="start.x")
#union(), setdiff()...
#punion(), psetdiff()...

##funkcja na podstawie dokumentacji do pakietu IRanges
plotRanges <- function(x, xlim = x, main = deparse(substitute(x)),
 col = "black", sep = 0.5, ...)
 {
 height <- 1
 if (is(xlim, "Ranges"))
 xlim <- c(min(start(xlim)), max(end(xlim)))
 bins <- disjointBins(IRanges(start(x), end(x) + 1))
 plot.new()
 plot.window(xlim, c(0, max(bins)*(height + sep)))
 ybottom <- bins * (sep + height) - height
 rect(start(x)-0.5, ybottom, end(x)+0.5, ybottom + height, col = col, ...)
 title(main)
 axis(1)
}


plotRanges(ir3)
plotRanges(ir4)
plotRanges(reduce(ir4))

findOverlaps(ir3, ir4)
countOverlaps(ir3, ir4)  
#najdluzszy gen

genMaxLen <- numeric(length(chroms))
for (i in 1:length(chroms)){
	chr <- chroms[i]
	g <- genes[genes$chrom == chr,]
	ir <- IRanges(g$txStart, g$txEnd)
	genMaxLen[i] <- max(width(ir))
}

chroms[which.max(genMaxLen)] #na ktorym chromosomie jest najdluzszy gen

coveredByGene <- numeric(length(chroms))
for (i in 1:length(chroms)){
	chr <- chroms[i]
	g <- genes[genes$chrom == chr,]
	ir <- IRanges(g$txStart, g$txEnd)
	coveredByGene[i] <- sum(width(reduce(ir)))
}

names(coveredByGene) <- c(1:22, "X", "Y")
barplot(coveredByGene) 


chromSizes <- numeric(length(chroms))
for (i in 1:length(chroms)){
	chr <- chroms[i]
	cyto <- cytoBands[cytoBands$X.chrom == chr,]
	ir <- IRanges(cyto$chromStart, cyto$chromEnd)
	chromSizes[i] <- max(end(ir))
}


plot(chromSizes) #dlugosci chromosomow
###oczywiscie mozemy teraz latwo znalezc na kazdym chromosomie cytoBand przecinajacy sie z najwieksza liczba genow


library(GenomicRanges)

gr <- GRanges(seqnames = genes.proc$chrom, ranges = IRanges(genes.proc$txStart, genes.proc$txEnd))

###lepsza wersja 
#zalozmy, ze mamy nastepujace dlugosci chromosomow
#    chr1      chr2      chr3      chr4      chr5      chr6      chr7      chr8 
#249250621 243199373 198022430 191154276 180915260 171115067 159138663 146364022 
#     chr9     chr10     chr11     chr12     chr13     chr14     chr15     chr16 
#141213431 135534747 135006516 133851895 115169878 107349540 102531392  90354753 
#    chr17     chr18     chr19     chr20     chr21     chr22      chrX      chrY 
# 81195210  78077248  59128983  63025520  48129895  51304566 155270560  59373566

#wtedy mozemy nadac wartosc seqlengths
seqlengths(gr) <- c("chr1" = 249250621) ##
values(gr) <- DataFrame(score = rep(1, length(gr)))
##mozemy teraz tez robic findOverlaps i countOverlaps


########
###mozemy wiec latwo namierzyc te CNVs (tu tylko przykladowy kod, ktory wymaga dopracowania)

library(rtracklayer)#Bioconductor
library(Biostrings)#Bioconductor
library(BSgenome.Hsapiens.UCSC.hg19)#Bioconductor


segDups <- read.csv("data/SegmentalDups_hg19.csv", sep="\t")

segDups.1 <- segDups[segDups$chrom == segDups$otherChrom,]
segDups.1$Distance <- segDups.1$otherStart - segDups.1$chromEnd + 1
segDups.1$chromSize <- segDups.1$chromEnd - segDups.1$chromStart + 1
segDups.2 <- segDups.1[(segDups.1$Distance > 5e3) & (segDups.1$Distance < 1e7),]
segDups.3 <- segDups.2[(segDups.2$chromSize > 2e3) & (segDups.2$otherSize > 2e3),]
segDups.4 <- segDups.3[segDups.3$fracMatch > 0.95,]
segDups.5 <- segDups.4[segDups.4$strand == "+",]
###mozemy jeszcze usunac chromosomy przecinajace centromery (smiertelne uszkodzenia)


library(quantsmooth) ##przyklad z podrecznika
plot(c(0,4),c(0,3),type="n",xaxt="n",yaxt="n",xlab="",ylab="")
drawSimpleChrom(2,3,fill=c("p","q3"),col=c("red","blue"),orientation="v")

chromLen <- lengthChromosome(c(1:22, "X", "Y"),"bases")




MapInfo1 <- data.frame(substr(genes.proc$chrom, 4, length(genes.proc$chrom)), genes.proc$txStart)
names(MapInfo1) <- c("CHR",  "MapInfo")

chrompos<-prepareGenomePlot(MapInfo1,paintCytobands = TRUE,    organism="hsa", sexChromosomes=T)
points(chrompos[1:3,2],chrompos[1:3,1]+0.3,pch="x",col="red")
segments(chrompos[3,2],chrompos[3,1],chrompos[4,2],chrompos[4,1],col="blue",lwd=2)





