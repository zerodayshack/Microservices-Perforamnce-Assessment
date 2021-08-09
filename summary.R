# ====
# Produce the summary (response-time, stdev, frequency) from a set of jtl file
# ====

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(cgwtools)))

# args
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript summary.R <dir> <users> <out>", call.=FALSE)
} else {
  dir <- args[1]
  users <- args[2]
  out_file <- args[3]
}

cat(paste("Running summary.R", dir, users, out_file, "...\n", sep = " "))

### debug
# dir <- "micro3_cqrs_1-5"
# users <- "2"
# out_file <- "out.csv"
###

pushd(dir)

tries <- list.files(path = ".", pattern = paste("_", users, "th_", sep = ""))
totRows <- 0
i <- 1
for(d in tries) {
  pushd(d)
  jtl <- list.files(path = ".", pattern = "*.jtl")[1]
  if (i == 1) {
    tmp <- read.csv(jtl, header = TRUE)
    totRows <- totRows + nrow(tmp)
    data <- tmp %>% filter(responseCode == 200)
    i <- i + 1
  } else {
    tmp <- read.csv(jtl, header = TRUE)
    totRows <- totRows + nrow(tmp)
    tmp <- tmp %>% filter(responseCode == 200)
    data <- rbind(data, tmp)
  }
  popd()
}

actualRows <- nrow(data)
cat(paste("Discarded rows = ", totRows - actualRows, "\n", sep = " "))

labels <- data[,'label']
unique_labels <- vector(length = length(labels))
total_calls <- length(data[,'label'])

i <- 1
for (l in labels) {
  label_pieces <- strsplit(l, " ")[[1]]
  unique_labels[i] <- paste(substr(label_pieces[2], 1, nchar(label_pieces[2])-2), gsub('[0-9]+', '', label_pieces[3]), sep = "_")
  i <- i + 1
}

unique_labels <- sort(unique(unique_labels))
response_time <- vector(length = length(unique_labels))
stdev <- vector(length = length(unique_labels))
frequency <- vector(length = length(unique_labels))

i <- 1
for (l in unique_labels) {
  label_pieces <- strsplit(l, "_")[[1]]
  if (endsWith(label_pieces[2], "getStreet")) {
    rows <- data %>% filter(grepl(label_pieces[1], label, ignore.case = TRUE) & grepl(label_pieces[2], label, ignore.case = TRUE) & !grepl("getStreets", label, ignore.case = TRUE))
  } else {
    rows <- data %>% filter(grepl(label_pieces[1], label, ignore.case = TRUE) & grepl(label_pieces[2], label, ignore.case = TRUE))
  }
  response_time[i] <- mean(rows[,'Latency'] / 1000)
  stdev[i] <- sd(rows[,'Latency'] / 1000)
  frequency[i] <- length(rows[,'timeStamp']) / total_calls
  i <- i + 1
}

m <- matrix(c(t(response_time), t(stdev), t(frequency)), nrow=3, ncol=length(unique_labels), byrow = TRUE)
summary <- as.data.frame(m)
colnames(summary) <- unique_labels
#cat(summary, header = FALSE, row.names = FALSE)

csv <- paste("../", out_file, sep = "")
suppressWarnings(write.table(summary, csv, col.names = !file.exists(csv), sep = ",", append = TRUE, row.names=FALSE))

popd()
