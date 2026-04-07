# install.packages(c("readxl"))  # 若没装
library(readxl)

path <- "Personal_Quality_Items_YN.xlsx"
df <- read_excel(path, sheet = "Personal Qualit
                 y")

# ---- 找出 Day 列（Day1..Day49）----
day_cols <- grep("^Day\\s*\\d+$", names(df), value = TRUE)
X <- as.matrix(df[, day_cols])
storage.mode(X) <- "integer"   # 0/1

item_names <- df$Item
k <- nrow(X)       # items
n <- ncol(X)       # days

# ---- Measure: baseline ----
row_ones <- rowSums(X)          # bad counts per item
col_ones <- colSums(X)          # bad counts per day
D <- sum(X)                     # total bad events
O <- k * n                      # total opportunities
p_overall <- D / O
dpmo <- p_overall * 1e6

cat("Items k =", k, " Days n =", n, "\n")
cat("Total defects D =", D, "\n")
cat("Total opportunities O =", O, "\n")
cat("Overall defect rate p =", round(p_overall,4), "\n")
cat("DPMO =", round(dpmo,1), "\n")

# =========================================================
# 1) Bar chart (by item)
barplot(row_ones, names.arg=item_names, las=2, cex.names=0.7,
        main="Number of 1's by item", ylab="Count of 1's", xlab="Item")

# 2) Pareto chart (by item)
o <- order(row_ones, decreasing = TRUE)
counts_sorted <- row_ones[o]
labels_sorted <- item_names[o]
cum_pct <- cumsum(counts_sorted) / sum(counts_sorted) * 100

bp <- barplot(counts_sorted, names.arg=labels_sorted, las=2, cex.names=0.7,
              main="Pareto chart of 1's by item", ylab="Count of 1's")
par(new=TRUE)
plot(bp, cum_pct, type="b", pch=19, axes=FALSE, xlab="", ylab="")
axis(side=4, at=seq(0,100,20))
mtext("Cumulative %", side=4, line=3)
abline(h=80, lty=2)   # 80% line
par(new=FALSE)

# =========================================================
# 3) p-chart and np-chart (treat each item as subgroup size n)
p_i <- rowMeans(X)
pbar <- mean(p_i)

sigma_p <- sqrt(pbar * (1 - pbar) / n)
UCL_p <- min(1, pbar + 3*sigma_p)
LCL_p <- max(0, pbar - 3*sigma_p)

plot(seq_len(k), p_i, type="b", pch=19, xaxt="n", ylim=c(0,1),
     xlab="Item", ylab="Proportion (p)", main="p-Chart (item means)")
axis(1, at=seq_len(k), labels=item_names, las=2, cex.axis=0.7)
abline(h=pbar, lwd=2)
abline(h=c(LCL_p, UCL_p), lty=2, lwd=2)

np_i <- row_ones
npbar <- n * pbar
sigma_np <- sqrt(n * pbar * (1 - pbar))
UCL_np <- min(n, npbar + 3*sigma_np)
LCL_np <- max(0, npbar - 3*sigma_np)

plot(seq_len(k), np_i, type="b", pch=19, xaxt="n", ylim=c(0,n),
     xlab="Item", ylab="Count (np)", main="np-Chart (item totals)")
axis(1, at=seq_len(k), labels=item_names, las=2, cex.axis=0.7)
abline(h=npbar, lwd=2)
abline(h=c(LCL_np, UCL_np), lty=2, lwd=2)

# =========================================================
# 4) Correlation heatmap among items (co-occurrence)
R <- cor(t(X), use="pairwise.complete.obs", method="pearson")
rownames(R) <- item_names; colnames(R) <- item_names
heatmap(R, symm=TRUE)

# =========================================================
# 5) Autocorrelation for one item (pick index)
row_id <- 1
acf(as.numeric(X[row_id, ]), main=paste0("ACF for item: ", item_names[row_id]),
    xlab="Lag", ylab="ACF")

# =========================================================
# 6) (Six Sigma flavor) Daily total bads: I-chart + EWMA
# I-chart (Individuals) for daily totals
x_day <- as.numeric(col_ones)
xbar <- mean(x_day)
MR <- abs(diff(x_day))
MRbar <- mean(MR)
d2 <- 1.128
sigma_hat <- MRbar / d2
UCL_I <- xbar + 3*sigma_hat
LCL_I <- xbar - 3*sigma_hat

plot(x_day, type="b", pch=19, main="I-Chart of daily total 1's",
     xlab="Day", ylab="Total 1's per day")
abline(h=xbar, lwd=2)
abline(h=c(LCL_I, UCL_I), lty=2, lwd=2)

# EWMA on daily proportion (optional)
p_day <- x_day / k
lambda <- 0.2
z <- numeric(length(p_day))
z[1] <- mean(p_day)
for(t in 2:length(p_day)) z[t] <- lambda*p_day[t] + (1-lambda)*z[t-1]

# steady-state limits (approx)
sigma_pday <- sd(p_day)  # simple estimate
L <- 3
UCL <- mean(p_day) + L*sigma_pday*sqrt(lambda/(2-lambda))
LCL <- mean(p_day) - L*sigma_pday*sqrt(lambda/(2-lambda))

plot(z, type="b", pch=19, main="EWMA of daily bad proportion",
     xlab="Day", ylab="EWMA")
abline(h=mean(p_day), lwd=2)
abline(h=c(LCL, UCL), lty=2, lwd=2)