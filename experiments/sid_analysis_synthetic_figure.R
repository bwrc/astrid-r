library(astrid)
source("utilities_analysis.R")

N    <- 500
tmp  <- make_synthetic_dataset(N = N, seed = 42, mg2 = 0.6)
data <- tmp$data

# ---------------------------
tree_true <- list(c(5, 1, 2), c(5, 3), c(0, 4))
dataset_surr_01 <- sid_randomize_dataset(tree_true, dataset)
data <- dataset_surr_01$data
# ---------------------------

data_c0 <- data[1:N,]
data_c1 <- data[(N+1):(2*N),]

pdf(file = "/tmp/data_synthetic.pdf", width = 10, height = 3)
par(mfrow = c(1,3))
plot(data_c0[,1], data_c0[,2], type = "p", col = "blue", pch = 1, xlim = c(-4,4), ylim = c(-4,4), xlab = expression(paste("value of ", a[2])), ylab = expression(paste("value of ", a[1])), main = expression(paste("Attributes ", a[1], " and ",  a[2])), asp = 1)
points(data_c1[,1], data_c1[,2], type = "p", col = "red", pch = 2)

plot(data_c0[,3], type = "p", col = "blue", pch = 1, xlab = "sample index", ylab = expression(paste("value of ", a[3])), main = expression(paste("Attribute ", a[3])), ylim = c(-4,4))
points(data_c1[,3], type = "p", col = "red", pch = 2)

plot(data_c0[,4], type = "p", col = "blue", xlab = "sample index", ylab = expression(paste("value of ", a[4])), main = expression(paste("Attribute ", a[4])), ylim = c(0,1), pch = 1)
points(data_c1[,4], type = "p", col = "red", pch = 2)
dev.off()


## --------------------------------------------------
## Also plot the marginal distrubutions
pdf(file = "/tmp/data_synthetic_margins.pdf", width = 10, height = 2.8)

nc <- ncol(data) - 1
par(mfcol = c(1, nc))

for (i in seq.int(nc)) {
    c0 <- subset(data, class == "0")[,i]
    c1 <- subset(data, class == "1")[,i]

    c0d <- density(c0)
    c1d <- density(c1)
    
    plot(c0d$x, c0d$y, col = "blue", lwd = 2, type = "l", xlim = c(-4,4), xlab = "value", ylab = "density", main = substitute(paste("Attribute ", a[x]), list(x = i)))
    lines(c1d$x, c1d$y, col = "red", lwd = 2, lty = 2)

}
dev.off()


## Generate the correlation matrix
data_tmp <- data[, -5]
colnames(data_tmp) <- LETTERS[1:4]

cmat <- cor(as.matrix(data_tmp))
cmat <- num_matrix_to_char_matrix(cmat)
cmat[upper.tri(cmat)] <- ""

san.col.func <- function(x) {
    paste0("\\multicolumn{1}{c}{\\textbf{", x, "}}")
}

san.row.func <- function(x) {
    paste0("\\textbf{", x, "}")
}


print.xtable(xtable(cmat), booktabs = TRUE, include.rownames = TRUE, sanitize.colnames.function = san.col.func, sanitize.rownames.function = san.row.func)
