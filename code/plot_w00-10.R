w52 <- read.csv(file = "tables/LIS_nonmissing_hi52_w00-10.csv", header = FALSE,
                stringsAsFactors = FALSE, 
                col.names = c("country", "year", "hi52", "diff"))
tmp <- w52[w52$diff != 0 & !is.na(w52$diff),]
postscript(file = "figures/LIS_nonmissing_hi52exclhi522hi523_w00-10.eps")
plot(tmp$year, tmp$diff, type = "n",
     main = "Nonmissing inter-household transfers excluding child support, alimony, and\nremittances (waves 00-10)",
     xlab = "Year", 
     ylab = "Proportion")
text(tmp$year, tmp$diff, labels = tmp$country, cex = .5)
dev.off()
pdf(file = "figures/LIS_nonmissing_hi52exclhi522hi523_w00-10.pdf", width = 10, 
    height = 6)
plot(tmp$year, tmp$diff, type = "n",
     main = "Nonmissing inter-household transfers excluding child support, alimony, and\nremittances (waves 00-10)",
     xlab = "Year", 
     ylab = "Proportion")
text(tmp$year, tmp$diff, labels = tmp$country, cex = .5)
dev.off()