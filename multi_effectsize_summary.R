# ====
# Run the summary multiple times (for each #user value)
# <dir> is the source system (monolith)
# <dir2> is the target system
# ====

# args
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop("Usage: Rscript multi_effectsize_summary.R <source> <target1> <target2> <out_dir>", call.=FALSE)
} else {
  src_dir <- args[1]
  target_dir1 <- args[2]
  target_dir2 <- args[3]
  out_dir <- args[4]
}

intensity <- c("50", "100", "150", "200", "250", "300", "350")

for(i in intensity) {
  system(paste("Rscript effect_size.R", src_dir, target_dir1, target_dir2, i, out_dir, sep = " "))
}
