# ====
# Detect statistical difference of response-time values between runs (set of jtl files)
# It uses the Mann-Whitney U test
# ====

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(cgwtools)))

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
  stop("Usage: Rscript effect_size.R <dir> <dir2> <users>", call.=FALSE)
} else {
  dir <- args[1]
  dir2 <- args[2]
  users <- args[3]
}

### debug
#dir <- "Mono_try0-1-2"
#dir2 <- "CQRS_try0-1-2"
#users <- "50"
###

cat(paste("Running stat_difference_exp.R", dir, dir2, users, "...\n", sep = " "))

readData <- function(dir, users) {
  pushd(dir)
  tries <- list.files(path = ".", pattern = paste("_", users, "th_", sep = ""))
  i <- 1
  for(d in tries) {
    pushd(d)
    jtl <- list.files(path = ".", pattern = "*.jtl")[1]
    if (i == 1) {
      data <- read.csv(jtl, header = TRUE)
      i <- i + 1
    } else {
      data <- rbind(data, read.csv(jtl, header = TRUE))
    }
    popd()
  }

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
  response_time <- list()
  #colnames(response_time_df) <- unique_labels

  i <- 1
  for (l in unique_labels) {
    label_pieces <- strsplit(l, "_")[[1]]
    if (endsWith(label_pieces[2], "getStreet")) {
      rows <- data %>% filter(grepl(label_pieces[1], label, ignore.case = TRUE) & grepl(label_pieces[2], label, ignore.case = TRUE) & !grepl("getStreets", label, ignore.case = TRUE))
    } else {
      rows <- data %>% filter(grepl(label_pieces[1], label, ignore.case = TRUE) & grepl(label_pieces[2], label, ignore.case = TRUE))
    }
    response_time[[i]] <- rows[,'Latency']
    i <- i + 1
  }
  popd()
  names(response_time) <- unique_labels
  return(response_time)
}

min_size <- function(vector1, vector2) {
  l1 <- length(vector1)
  l2 <- length(vector2)
  if (l1 < l2) {
    return(l1)
  }
  return(l2)
}

stat_diff <- function(x, y, alpha = 0.05) {
  size <- min_size(x, y)
  x <- x[1:size]
  y <- y[1:size]
  u_test <- wilcox.test(x, y)$p.value
  detected <- u_test < alpha
  return(detected)
}

rt <- readData(dir, users)
rt2 <- readData(dir2, users)

for (i in 1:length(rt)) {
  es <- stat_diff(rt[[i]], rt2[[i]])
  cat(paste(names(rt)[i], es, "\n", sep = " "))
}

#cat(summary, header = FALSE, row.names = FALSE)

#csv <- paste("../", out_file, sep = "")
#suppressWarnings(write.table(summary, csv, col.names = !file.exists(csv), sep = ",", append = TRUE, row.names=FALSE))
