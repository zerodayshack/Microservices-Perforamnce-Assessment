# ====
# Run the stat_diff multiple times (for each #user value)
# ====

# args
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript multi_statdiff_summary.R <dir> <dir2>", call.=FALSE)
} else {
  dir <- args[1]
  dir2 <- args[2]
}

intensity <- c("2", "50", "100", "150", "200", "250", "300", "350")

for(i in intensity) {
  system(paste("Rscript stat_difference_exp.R", dir, dir2, i, sep = " "))
}
