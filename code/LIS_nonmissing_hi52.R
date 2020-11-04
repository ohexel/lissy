# # Wave 10
# nonmissing <- read.csv("tables/nonmissing52522.csv", header = FALSE,
#                        col.names = c("country", "year", "hi52", "diff52"))
# nonmissing$ccyy <- paste0(toupper(nonmissing$country),
#                           substr(nonmissing$year, 3, 4))

axis_label_pos_black <- as.factor(nonmissing$ccyy)
axis_label_pos_black[nonmissing$hi52 == 0] <- NA
axis_label_lab_black <- nonmissing$ccyy
axis_label_lab_black[nonmissing$hi52 == 0] <- ""
axis_label_pos_grey <- as.factor(nonmissing$ccyy)
axis_label_pos_grey[nonmissing$hi52 != 0] <- NA
axis_label_lab_grey <- nonmissing$ccyy
axis_label_lab_grey[nonmissing$hi52 != 0] <- ""

postscript(file = "figures/LIS_prop_nonmissing_hi52.eps")
plot(as.factor(nonmissing$ccyy), nonmissing$hi52, las = 3,
     main = "Proportion of nonmissing inter-household transfer observations\n(Data: LIS, wave X)",
     xlab = "Country (ISO) Year",
     ylab = "Proportion",
     axes = FALSE)
abline(a = 0.00, b = 0, lty = 3)
abline(a = 0.05, b = 0, lty = 3)
abline(a = 0.10, b = 0, lty = 3)
abline(a = 0.20, b = 0, lty = 3)
abline(a = 0.40, b = 0, lty = 3)
abline(a = 0.60, b = 0, lty = 3)
abline(a = 0.75, b = 0, lty = 3)
axis(2, c(axTicks(2), c(0.05, 0.1, 0.75)), las = 1)
axis(1, labels = axis_label_lab_black, at = axis_label_pos_black,
     las = 3, col.axis = "black")
axis(1, labels = axis_label_lab_grey, at = axis_label_pos_grey,
     las = 3, col.axis = "grey50")
text(x = 40, y = -0.015, label = "In grey: countries with 0 observations",
     col = "grey20")
dev.off()

pdf(file = "figures/LIS_prop_nonmissing_hi52.pdf", width = 10, height = 6)
plot(as.factor(nonmissing$ccyy), nonmissing$hi52, las = 3,
     main = "Proportion of nonmissing inter-household transfer observations\n(Data: LIS, wave X)",
     xlab = "Country (ISO) Year",
     ylab = "Proportion",
     axes = FALSE)
abline(a = 0.00, b = 0, lty = 3)
abline(a = 0.05, b = 0, lty = 3)
abline(a = 0.10, b = 0, lty = 3)
abline(a = 0.20, b = 0, lty = 3)
abline(a = 0.40, b = 0, lty = 3)
abline(a = 0.60, b = 0, lty = 3)
abline(a = 0.75, b = 0, lty = 3)
axis(2, c(axTicks(2), c(0.05, 0.1, 0.75)), las = 1)
axis(1, labels = axis_label_lab_black, at = axis_label_pos_black,
     las = 3, col.axis = "black")
axis(1, labels = axis_label_lab_grey, at = axis_label_pos_grey,
     las = 3, col.axis = "grey50")
text(x = 40, y = -0.015, label = "In grey: countries with 0 observations",
     col = "grey20")
dev.off()


axis_label_pos_black <- as.factor(nonmissing$ccyy)
axis_label_pos_black[nonmissing$diff52 == 0] <- NA
axis_label_lab_black <- nonmissing$ccyy
axis_label_lab_black[nonmissing$diff52 == 0] <- ""
axis_label_pos_grey <- as.factor(nonmissing$ccyy)
axis_label_pos_grey[nonmissing$diff52 != 0] <- NA
axis_label_lab_grey <- nonmissing$ccyy
axis_label_lab_grey[nonmissing$diff52 != 0] <- ""

postscript(file = "figures/LIS_prop_nonmissing_hi52_wo_hi521_hi522.eps")
plot(as.factor(nonmissing$ccyy), nonmissing$diff52,
     las = 3,
     main = "Proportion of nonmissing inter-household transfer observations, excl. child support, alimony,\nand remittances (Data: LIS, wave X)",
     xlab = "Country (ISO) Year",
     ylab = "Proportion",
     axes = FALSE)
abline(a = 0.00, b = 0, lty = 3)
abline(a = 0.05, b = 0, lty = 3)
abline(a = 0.10, b = 0, lty = 3)
abline(a = 0.20, b = 0, lty = 3)
abline(a = 0.40, b = 0, lty = 3)
abline(a = 0.60, b = 0, lty = 3)
abline(a = 0.75, b = 0, lty = 3)
axis(2, c(axTicks(2), c(0.05, 0.1, 0.75)), las = 1)
axis(1, labels = axis_label_lab_black, at = axis_label_pos_black,
     las = 3, col.axis = "black")
axis(1, labels = axis_label_lab_grey, at = axis_label_pos_grey,
     las = 3, col.axis = "grey50")
text(x = 40, y = -0.015, label = "In grey: countries with 0 observations",
     col = "grey20")
dev.off()

pdf(file = "figures/LIS_prop_nonmissing_hi52_wo_hi521_hi522.pdf", width = 10,
    height = 6)
plot(as.factor(nonmissing$ccyy), nonmissing$diff52,
     las = 3,
     main = "Proportion of nonmissing inter-household transfer observations, excl. child support, alimony,\nand remittances (Data: LIS, wave X)",
     xlab = "Country (ISO) Year",
     ylab = "Proportion",
     axes = FALSE)
abline(a = 0.00, b = 0, lty = 3)
abline(a = 0.05, b = 0, lty = 3)
abline(a = 0.10, b = 0, lty = 3)
abline(a = 0.20, b = 0, lty = 3)
abline(a = 0.40, b = 0, lty = 3)
abline(a = 0.60, b = 0, lty = 3)
abline(a = 0.75, b = 0, lty = 3)
axis(2, c(axTicks(2), c(0.05, 0.1, 0.75)), las = 1)
axis(1, labels = axis_label_lab_black, at = axis_label_pos_black,
     las = 3, col.axis = "black")
axis(1, labels = axis_label_lab_grey, at = axis_label_pos_grey,
     las = 3, col.axis = "grey50")
text(x = 40, y = -0.015, label = "In grey: countries with 0 observations",
     col = "grey20")
dev.off()


# Waves 00 to 09
w0010 <- read.csv("tables/waves00-10.csv", header = FALSE,
                  col.names = c("country", "year", "hi52", "diff52"))
w0010$ccyy <- paste0(toupper(w0010$country), substr(w0010$year, 3, 4))
postscript(file = "figures/LIS_prop_nonmissing_hi52_waves00-10.eps")
plot(w0010$year, w0010$diff52, type = "n")
tmp <- w0010[w0010$diff52 != 0 & !is.na(w0010$diff52),]
text(tmp$year, tmp$diff52, labels = toupper(tmp$country), cex = .5)
dev.off()
pdf(file = "figures/LIS_prop_nonmissing_hi52_waves00-10.pdf",
    width = 10,
    height = 6)
plot(w0010$year, w0010$diff52, type = "n")
tmp <- w0010[w0010$diff52 != 0 & !is.na(w0010$diff52),]
text(tmp$year, tmp$diff52, labels = toupper(tmp$country), cex = .5)
dev.off()
