# ====
# Produce the summary of effect-size (Vargha-Delaney A12) and p-value (wilcox U-Test) from a set of jtl files
# <dir> is the source system (monolith)
# <dir2> is the target system
# ====

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(cgwtools)))
suppressWarnings(suppressMessages(library(ggplot2)))

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 5) {
  stop("Usage: Rscript effect_size.R <dir1> <dir2> <dir3> <users> <out_dir>", call.=FALSE)
} else {
  dir <- args[1]
  dir2 <- args[2]
  dir3 <- args[3]
  users <- args[4]
  out_dir <- args[5]
}

### debug
#dir <- "Mono_iter1_bal_day"
#dir2 <- "Role_iter1_bal_day"
#dir3 <- "Cqrs_iter1_bal_day"
#users <- "350"
#out_dir <- "plots"
###

cat(paste("Running effect_size.R", dir, dir2, users, "...\n", sep = " "))

readData <- function(dir, users) {
  pushd(dir)
  tries <- list.files(path = ".", pattern = paste("_", users, "th_", sep = ""))
  i <- 1
  for(d in tries) {
    pushd(d)
    jtl <- list.files(path = ".", pattern = "*.jtl")[1]
    if (i == 1) {
      tmp <- read.csv(jtl, header = TRUE)
      data <- tmp %>% filter(responseCode == 200)
      i <- i + 1
    } else {
      tmp <- read.csv(jtl, header = TRUE)
      tmp <- tmp %>% filter(responseCode == 200)
      data <- rbind(data, tmp)
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
    response_time[[i]] <- rows[,'Latency'] / 1000
    i <- i + 1
  }
  popd()
  names(response_time) <- unique_labels
  return(response_time)
}

minSize <- function(vector1, vector2) {
  l1 <- length(vector1)
  l2 <- length(vector2)
  if (l1 < l2) {
    return(l1)
  }
  return(l2)
}

statDiff <- function(x, y) {
  size <- minSize(x, y)
  x_tmp <- x[1:size]
  y_tmp <- y[1:size]
  u_test <- wilcox.test(x, y)$p.value
}

effectSize <- function(x, y) {
  m <- length(x)
  n <- length(y)
  r <- sum(rank(c(x, y))[seq_along(x)])
  (r/m - (m + 1)/2)/n
}

rt_source <- readData(dir, users)
rt_target1 <- readData(dir2, users)
rt_target2 <- readData(dir3, users)

# build boxplot
df <- data.frame(operation = c(), rt = c(), architecture = c())
for (i in 1:length(rt_source)) {
  tmp <- data.frame(operation = names(rt_source)[i], rt = rt_source[[i]], architecture = dir)
  df <- rbind(df, tmp)
}
for (i in 1:length(rt_target1)) {
  tmp <- data.frame(operation = names(rt_target1)[i], rt = rt_target1[[i]], architecture = dir2)
  df <- rbind(df, tmp)
}
for (i in 1:length(rt_target2)) {
  tmp <- data.frame(operation = names(rt_target2)[i], rt = rt_target2[[i]], architecture = dir3)
  df <- rbind(df, tmp)
}

# ggplot(df, aes(x=operation, y=rt, fill=architecture)) +
#  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

name_pieces <- strsplit(dir2, "_")[[1]]
out_file <- paste(out_dir, "/boxplot_", name_pieces[2], "_", name_pieces[3], "_", users, ".csv", sep = "")
write.csv(df, out_file, row.names = FALSE)

# compute effect size
es <- c()
sd <- c()
for (i in 1:length(rt_source)) {
  es[i] <- effectSize(rt_source[[i]], rt_target1[[i]])
  sd[i] <- statDiff(rt_source[[i]], rt_target1[[i]])
}
df <- data.frame(operation = names(rt_source), effectsize = es, pvalue = sd, architecture = dir2, workload = users)
name_pieces <- strsplit(dir2, "_")[[1]]
out_file <- paste(out_dir, "/heatmap_", name_pieces[1], "_", name_pieces[2], "_", name_pieces[3], "_", users, ".csv", sep = "")
write.csv(df, out_file, row.names = FALSE)

es <- c()
sd <- c()
for (i in 1:length(rt_source)) {
  es[i] <- effectSize(rt_source[[i]], rt_target2[[i]])
  sd[i] <- statDiff(rt_source[[i]], rt_target2[[i]])
}
df <- data.frame(operation = names(rt_source), effectsize = es, pvalue = sd, architecture = dir3, workload = users)
name_pieces <- strsplit(dir3, "_")[[1]]
out_file <- paste(out_dir, "/heatmap_", name_pieces[1], "_", name_pieces[2], "_", name_pieces[3], "_", users, ".csv", sep = "")
write.csv(df, out_file, row.names = FALSE)

#cat(paste(names(rt_source)[i], es, sd, "\n", sep = " "))

#cat(summary, header = FALSE, row.names = FALSE)

#csv <- paste("../", out_file, sep = "")
#suppressWarnings(write.table(summary, csv, col.names = !file.exists(csv), sep = ",", append = TRUE, row.names=FALSE))
