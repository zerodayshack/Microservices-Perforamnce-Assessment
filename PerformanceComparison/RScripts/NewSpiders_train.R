## Create plots for spiders and ridges for per operation performance analysis.
#Version:  August 8th 2021
#Requires start.R to identify the operations and calculate the failing operations

# install.packages("ggridges", repos = "http://cran.us.r-project.org")

library(fmsb)
library(stringr)

#Define radial coordinates
coord_radar <- function(theta = "x",
                        start = 0,
                        direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x")
    "y"
  else
    "x"
  ggproto(
    "CordRadar",
    CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    is_linear = function(coord)
      TRUE
  )
}

#Define loads as defined in aggregatedValues in start.R
loads <- as.character(unique(aggregatedValues[, 1]))
par <- 5

#DATASETS and PATHS
setwd("../")
wd <- getwd()
list.files(paste(wd, "/Results/", sep = ""))
workingDir <- paste(wd, "/Results/", sep = "")


pathData <- './Results/5_TrainTicket_results_failingMicroservice.csv'

cat("path to datasets have been created")

operations_labels <- c(
  expression(italic(o[1])),
  expression(italic(o[2])),
  expression(italic(o[3])),
  expression(italic(o[4])),
  expression(italic(p[1])),
  expression(italic(p[2])),
  expression(italic(t[1])),
  expression(italic(t[2])),
  expression(italic(t[3])),
  expression(italic(t[4]))
)

#failing loads, operations' values and response times
compute_df <- function(dataset, operations) {
  operations <- as.vector(unlist(operations))
  df <- NULL
  dfm <- NULL
  drt <- NULL
  for (i in 1:length(operations)) {
    if (length(grep(paste("^", operations[i], "$", sep = ""), dataset$microservice)) >
      0) {
      value <-
        dataset$loadIntensity[min(grep(
          paste("^", operations[i], "$", sep = ""),
          dataset$microservice
        ))]
      measure <-
        dataset$mix[min(grep(
          paste("^", operations[i], "$", sep = ""),
          dataset$microservice
        ))]
      responseTime <-
        dataset$avg[min(grep(
          paste("^", operations[i], "$", sep = ""),
          dataset$microservice
        ))]
    } else {
      #define fake values for services that do not fail
      value <- 400
      measure <- 1.1
      responseTime <- max(dataset$avg) + 0.2
    }
    df <- cbind(df, value)
    dfm <- cbind(dfm, measure)
    drt <- cbind(drt, responseTime)
  }
  colnames(df) <- operations
  colnames(dfm) <- operations
  colnames(drt) <- operations
  return(list(df, dfm, drt))
}

temp <<- 0
breaks <- NULL
i <<- 0
breaks0 <- aggregatedValues[, 2]
breaks <- as.numeric(unlist(lapply(breaks0, function(x) {
  i <<- i + 1
  if (!i == length(breaks0)) {
    temp <<- x + temp
  } else {
    x <<- 1
  }
})))
aggregatedValuesCum <- aggregatedValues
aggregatedValuesCum[, 2] <- breaks

#Make the plot with ggplot and list
cat("Make the plot with ggplot and list")
plot_list = list()
plotLine_list = list()
plotLineUSD_list = list()
plotLineRT_list = list()
plotLineRTO_list = list()

#Define the colors for the operations
myColors <- c("purple", #o1
              "black", #o2
              "red", #o3
              "red", #o4
              "purple", #p1
              "black", #p2
              "purple", #t1
              "black", #t2
              "red", #t3-
              "red" #t4-
)

#PLOTS
cat("")
type <- NULL

failedOperations <- as.data.frame(read.csv(pathData))
ddd_failingS <-
  failedOperations[grep("ddd", failedOperations[, 1]),]
add_failingS <-
  failedOperations[grep("add", failedOperations[, 1]),]

df_ddd_temp <- NULL
df_add_temp <- NULL

#values and response times
df_ddd_temp <- compute_df(ddd_failingS, operations)
df_add_temp <- compute_df(add_failingS, operations)

#Define the titles or files' names
indexN <-
  grep(pathData,
       c(
         pathData
       ))
print(indexN)
if (indexN %in% c(1, 2)) {
  index <- 1
} else {
  index <- 2
}
print(index)
typeT <- grep("unbal", pathData)
print(typeT)

if (length(typeT) == 0) {
  type <- "bal"
} else {
  type <- "unbal"
}

#RADARs for scalability footprint
#Compute the cumulative performance of the last load in which operations do not fail
df_ddd <- df_ddd_temp
df_add <- df_add_temp
for (i in 1:length(operations)) {
  if (!length(grep(
    paste("^", df_ddd_temp[[1]][i], "$", sep = ""),
    aggregatedValuesCum[, 1]
  )) == 0) {
    df_ddd[[2]][i] <-
      aggregatedValuesCum[grep(paste("^", df_ddd_temp[[1]][i], "$", sep = ""),
                               aggregatedValuesCum[, 1]) - 1, 2]
  } else {
    df_ddd[[2]][i] <- 1.1
  }
  if (!length(grep(
    paste("^", df_add_temp[[1]][i], "$", sep = ""),
    aggregatedValuesCum[, 1]
  )) == 0) {
    df_add[[2]][i] <-
      aggregatedValuesCum[grep(paste("^", df_add_temp[[1]][i], "$", sep = ""),
                               aggregatedValuesCum[, 1]) - 1, 2]
  } else {
    df_add[[2]][i] <- 1.1
  }
}

#Create a melted dataset
dataset <-
  as.data.frame(rbind(df_ddd[[1]], df_add[[1]]))
dataset1 <-
  as.data.frame(rbind(df_ddd[[2]], df_add[[2]]))

breaksAug <- c(breaks, 1.1)
dataset$model <- c("ddd", "add")
dataset1$model <- c("ddd", "add")
ds_melted <- NULL
ds_melted0 <- reshape2::melt(dataset)
ds_melted1 <- reshape2::melt(dataset1)
ds_melted2 <- ds_melted1
ds_melted <- cbind(ds_melted0, ds_melted1$value)[-3]

#Give names that can be used in the radar chart model and scale up the grid
colnames(ds_melted) <- colnames(ds_melted1)
ds_melted$value <- as.numeric(ds_melted$value)
#y labels
breakLabels <-
  paste(c(round(breaksAug[-c(13:14)], 2), 1, 1.1), "(", c(loads, ""), ")", sep =
    "")

#Create the radarplots
cat("Create the radarplots ")
p <- ggplot(ds_melted, aes(x = variable, y = value)) +
  geom_polygon(
    aes(
      group = model,
      color = model,
      fill = model
    ),
    alpha = 0.1,
    size = 0.5,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[13],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[12],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[11],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[10],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[9],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[8],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[7],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[6],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[5],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[4],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[3],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_hline(
    yintercept = breaksAug[2],
    linetype = 1,
    size = 0.1,
    show.legend = F
  ) +
  geom_line(aes(color = model), size = 0.5, show.legend = F) +
  geom_point(aes(color = model), size = 0.5, show.legend = F) +
  scale_y_continuous(
    name = "",
    breaks = c(breaksAug),
    limits = c(0, breaksAug[14])
  ) +
  scale_x_discrete(
    name = "",
    # labels = operations
    labels = operations_labels
  ) +
  theme_bw() +
  scale_colour_manual(
    values = c("blue", "darkgreen"),
    limits = c('ddd', 'add'),
    aesthetics = c("colour", "fill")
  ) +
  coord_polar(start = pi / 11 - 2 * pi / 11,
              direction = -1,
              clip = "off") +
  annotate(
    "text",
    x = 1,
    y = c(breaksAug[-c(2:6, 13, 14)], 1.05),
    label = c(loads[-c(2:6)]),
    size = 5,
    vjust = 0.7,
    hjust = 1.05
  ) +
  theme(
    axis.text.x = element_text(
      colour = myColors,
      size = 16,
      angle = 0,
      vjust = 0.8,
      hjust = 0.9
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(
      size = 0.1,
      linetype = 'solid',
      colour = "white"
    ),
    panel.grid.major.x = element_line(
      size = 0.1,
      linetype = 'solid',
      colour = "black"
    ),
    axis.line.x = element_blank(),
    plot.margin = margin(0, 0, 10, 0, "pt")
  )
# if (FALSE) {
#RIDGES
cat("Building ridgeplots ")
#Compute the contribution to the performance gap for each operation at the last load at which does not fail
df_ddd <- df_ddd_temp
df_add <- df_add_temp

#Create the gap of frequencies. Only frequency no target f()
for (i in 1:length(operations)) {
  if (!length(grep(
    paste("^", df_ddd_temp[[1]][i], "$", sep = ""),
    aggregatedValuesCum[, 1]
  )) == 0) {
    df_ddd[[2]][i] <- df_ddd_temp[[2]][i]
  } else {
    df_ddd[[2]][i] <- 0
  }
  if (!length(grep(
    paste("^", df_add_temp[[1]][i], "$", sep = ""),
    aggregatedValuesCum[, 1]
  )) == 0) {
    df_add[[2]][i] <- df_add_temp[[2]][i]
  } else {
    df_add[[2]][i] <- 0
  }
}
dataset <-
  as.data.frame(rbind(df_ddd[[1]], df_add[[1]]))
dataset1 <-
  as.data.frame(rbind(df_ddd[[2]], df_add[[2]]))

breaksAug <- c(breaks, 1.1)
dataset$model <- c("add", "ddd")
dataset1$model <- c("add", "ddd")
ds_meltedR <- NULL
ds_meltedR0 <- reshape2::melt(dataset)
ds_meltedR1 <- reshape2::melt(dataset1)
ds_meltedR2 <- ds_melted1
ds_meltedR <- cbind(ds_meltedR0, ds_meltedR1$value)

colnames(ds_meltedR) <- c("model", "variable", "load", "value")
ds_meltedR$value <- as.numeric(ds_meltedR$value)
ds_meltedR <- ds_meltedR[!ds_meltedR$load == 400,]
t <- 2.2 * as.numeric(ds_meltedR$load) / 100
loadsNum <- 2.2 * as.numeric(loads) / 100

library(ggridges)
#Create ridges without title. Bar represents the call frequency for the corresponding operation times the probability of the load. The bar sits on the load at which the operation fails
pRidge <- ggplot(ds_meltedR, aes(x = variable, fill = model)) +
  geom_crossbar(
    aes(
      y = value + t,
      ymin = t,
      ymax = value + t,
      fatten = 0.3,
      group = model,
      color = model,
      fill = model
    ),
    size = 0.4,
    wisth = 0.2,
    show.legend = F
  ) +
  geom_hline(yintercept = loadsNum[13],
             linetype = 1,
             size = 0.1) +
  geom_hline(yintercept = loadsNum[12],
             linetype = 1,
             size = 0.1) +
  geom_hline(yintercept = loadsNum[11],
             linetype = 1,
             size = 0.1) +
  geom_hline(yintercept = loadsNum[10],
             linetype = 1,
             size = 0.1) +
  geom_hline(yintercept = loadsNum[9],
             linetype = 1,
             size = 0.1) +
  scale_x_discrete(
    name = "",
    labels = c(
      expression(italic(o[1])),
      expression(italic(o[2])),
      expression(italic(o[3])),
      # expression(italic(o[4])),
      expression(italic(p[1])),
      expression(italic(p[2])),
      expression(italic(t[1])),
      expression(italic(t[2]))
      # expression(italic(t[3])),
      # expression(italic(t[4]))
    )
  ) +
  theme_bw() +
  ylab("") +
  guides(color = guide_legend(ncol = 1)) +
  scale_colour_manual(
    values = c("blue", "darkgreen"),
    limits = c('add', 'ddd'),
    aesthetics = c("colour")
  ) +
  scale_fill_manual(
    values = alpha(c("blue", "darkgreen"), .1),
    limits = c('add', 'ddd'),
    aesthetics = c("fill")
  ) +
  annotate(
    "text",
    x = 0,
    y = loadsNum[-c(1:8)],
    label = loads[-c(1:8)],
    size = 5,
    vjust = -0.6,
    hjust = 0
  ) +
  theme(
    axis.text.x = element_text(
      colour = c(
        "purple", #o1
        "black", #o2
        "maroon", #o3
        # "maroon",#o4
        "purple", #p1
        "black", #p2
        "purple", #t1
        "black" #t2
        # "red",#t3-
        # "red"#t4-
      ),
      size = 20,
      vjust = 0.8,
      hjust = 0.3
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 24),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24),
    plot.margin = margin(0, 0.7, 0, 0, "cm")
  )


#RIDGES with response time
cat("Create ridges with response time")
#Computing the contribution in response time per idividual operation of the total average response time
df_ddd <- df_ddd_temp
df_add <- df_add_temp

for (i in 1:length(operations)) {
  if (!length(grep(
    paste("^", df_ddd_temp[[1]][i], "$", sep = ""),
    aggregatedValuesCum[, 1]
  )) == 0) {
    df_ddd[[2]][i] <- df_ddd_temp[[3]][i]
  } else {
    df_ddd[[2]][i] <- 0
  }
  if (!length(grep(
    paste("^", df_add_temp[[1]][i], "$", sep = ""),
    aggregatedValuesCum[, 1]
  )) == 0) {
    df_add[[2]][i] <- df_add_temp[[3]][i]
  } else {
    df_add[[2]][i] <- 0
  }
}
breaksAug <- c(breaks, 1.1)
datasetRT <-
  as.data.frame(rbind(df_ddd[[1]], df_add[[1]]))
datasetRT1 <-
  as.data.frame(rbind(df_ddd[[2]], df_add[[2]]))
datasetRT$model <- c("ddd", "add")
datasetRT1$model <- c("ddd", "add")

ds_meltedRT0 <- NULL
ds_meltedRT1 <- NULL
ds_meltedRT0 <- reshape2::melt(datasetRT)
ds_meltedRT1 <- reshape2::melt(datasetRT1)

ds_meltedRT <- cbind(ds_meltedRT0, ds_meltedRT1$value)

colnames(ds_meltedRT) <- c("model", "variable", "load", "value")
ds_meltedRT$value <- as.numeric(ds_meltedRT$value)

#add a column expressing the threshold for each failing microservice
ds_meltedRT$threshold <- rep(0, nrow(ds_meltedRT))
for (i in 1:length(operations)) {
  if (!length(grep(paste("^", names(threshold[i + 2]), "$", sep = ""), ds_meltedRT[, 2])) ==
    0) {
    ds_meltedRT$threshold[grep(paste("^", names(threshold[i + 2]), "$", sep =
      ""), ds_meltedRT[, 2])] <-
      unlist(rep(threshold[i + 2], length(grep(
        paste("^", names(threshold[i + 2]), "$", sep = ""), ds_meltedRT[, 2]
      ))))
  }
  else {
    ds_meltedRT$threshold[grep(paste("^", names(threshold[i + 2]), "$", sep =
      ""), ds_meltedRT[, 2])] <- 0
  }
}
ds_meltedRT <- ds_meltedRT[!ds_meltedRT$load == 400,]

ds_meltedRT_offset <- ds_meltedRT
ds_meltedRT$offset <- rep(0, nrow(ds_meltedRT))
ds_meltedRT_offset$offset <-
  (ds_meltedRT$value - ds_meltedRT$threshold) / ds_meltedRT$threshold
g <- 6 * as.numeric(ds_meltedRT$load) / 100
loadsNumOff <- 6 * as.numeric(loads) / 100
#to avoid overlapping labels
library(ggrepel)
pRidgeRT_offset <-
  ggplot(ds_meltedRT_offset, aes(x = variable, fill = model)) +
    geom_crossbar(
      aes(
        y = offset + g,
        ymin = g,
        ymax = offset + g,
        fatten = 0.3,
        group = model,
        color = model,
        fill = model
      ),
      size = 0.4,
      wisth = 0.2,
      show.legend = F
    ) +
    geom_text_repel(
      aes(y = offset + g, label = percent(offset, accuracy = 1)),
      cex = 5,
      hjust = 0.45,
      vjust = 0.9,
      # segment.colour = alpha("white", 0.1)
    ) +
    geom_hline(yintercept = loadsNumOff[13],
               linetype = 1,
               size = 0.1) +
    geom_hline(yintercept = loadsNumOff[12],
               linetype = 1,
               size = 0.1) +
    geom_hline(yintercept = loadsNumOff[11],
               linetype = 1,
               size = 0.1) +
    geom_hline(yintercept = loadsNumOff[10],
               linetype = 1,
               size = 0.1) +
    geom_hline(yintercept = loadsNumOff[9],
               linetype = 1,
               size = 0.1) +
    scale_x_discrete(
      name = "",
      labels = c(
        expression(italic(o[1])),
        expression(italic(o[2])),
        expression(italic(o[3])),
        # expression(italic(o[4])),
        expression(italic(p[1])),
        expression(italic(p[2])),
        expression(italic(t[1])),
        expression(italic(t[2]))
        # expression(italic(t[3])),
        # expression(italic(t[4]))
      )
    ) +
    theme_bw() +
    ylab("") +
    guides(color = guide_legend(ncol = 1)) +
    scale_colour_manual(
      values = c("blue", "darkgreen"),
      limits = c('ddd', 'add'),
      aesthetics = c("colour")
    ) +
    scale_fill_manual(
      values = alpha(c("blue", "darkgreen"), .1),
      limits = c('ddd', 'add'),
      aesthetics = c("fill")
    ) +
    annotate(
      "text",
      x = -0.5,
      y = c(loadsNumOff[-c(1:8)]),
      label = loads[-c(1:8)],
      size = 5,
      vjust = -0.6,
      hjust = 0
    ) +
    theme(
      axis.text.x = element_text(
        colour = c(
          "purple", #o1
          "black", #o2
          "red", #o3
          # "red",#o4
          "purple", #p1
          "black", #p2
          "purple", #t1
          "black" #t2
          # "red",#t3-
          # "red"#t4-
        ),
        size = 20,
        vjust = 0.8,
        hjust = 0.3
      ),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      axis.title.y = element_text(size = 24),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_blank(),
      legend.text = element_text(size = 24),
      legend.title = element_text(size = 24),
      plot.margin = margin(0.5, 0.7, 0, 0, "cm")
    )

#SAVE PLOTS
cat("Save plots ")
#Save each plot individually

print(getwd())
#Radar
myFile <-
  paste("Results/Plots/TT-spider.png", sep = "")
# paste("/home/colarusso/tmp/Microservices-Performance-Assessment-master/PerformanceComparison/Results/Plots/spider.pdf", sep = "")
ggsave(file = myFile, p)

#Ridge
myFiler <-
  paste("Results/Plots/TT-ridge_sg.png", sep = "")
ggsave(file = myFiler, pRidge)

# #Response time offset ridge
myFilerRT_off <-
  paste("Results/Plots/TT-ridge_po.png", sep = "")
ggsave(file = myFilerRT_off, pRidgeRT_offset)



