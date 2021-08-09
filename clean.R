# ====
# Run the summary multiple times (for each #user value)
# ====

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(cgwtools)))

# args
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Usage: Rscript clean.R <dir>", call.=FALSE)
} else {
  dir <- args[1]
}

intensity <- c("2", "50", "100", "150", "200", "250", "300", "350")

pushd(dir)

for(i in intensity) {
  tries <- list.files(path = ".", pattern = paste("_", i, "th_", sep = ""))
  for(d in tries) {
    pushd(d)
    jtl <- list.files(path = ".", pattern = "*.jtl")[1]
    other_files <- list.files(path = ".")
    for (f in other_files) {
      if (f != jtl) {
        unlink(f, recursive = TRUE)
        #cat(paste(f, "\n", sep = " "))
      }
    }
    popd()
  }
}
