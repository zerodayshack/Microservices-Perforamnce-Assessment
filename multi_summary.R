# ====
# Run the summary multiple times (for each #user value)
# ====

# args
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript multi_summary.R <dir> <out>", call.=FALSE)
} else {
  dir <- args[1]
  out_file <- args[2]
}

intensity <- c("2", "50", "100", "150", "200", "250", "300", "350")

for(i in intensity) {
  system(paste("Rscript summary.R", dir, i, out_file, sep = " "))
}
