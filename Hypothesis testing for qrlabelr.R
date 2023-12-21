
# Fig. 4. Microbenchmarking the QR code generation functions in qrlabelr and baRcodeR packages

# Generating data to test the hypothesis that the QR code generation functionality in
# the qrlabelr pckage is faster than its counterpart in the baRocdeR package

# We microbenchmarked the two functions on a PC and a MacBook with following specs
# Windows HP PC (8 cores)
# Processor:	Intel(R) Core(TM) i7-10510U CPU @ 1.80GHz   2.30 GHz
# Installed RAM:	16.0 GB (15.7 GB usable)
# System type:	64-bit operating system, x64-based processor

# MacBook Pro (12 cores)
# Chip: Apple M2 Max
# Memory: 32 GB
# macOS Sonoma 14.2
# Platform: aarch64-apple-darwin20 (64-bit)

# We generated microbenchmarking data independently on both computers and saved 
# the data for results summary and plotting on the MacBook Pro.

# We saved the data as follows:
# MacBook Pro: time_mac.csv
# Windows PC: time_win.csv

################################################################################
# QR code generation function in baRcodeR package
qrcode_make <- function(Labels, ErrCorr){
  # Create text label
  Xtxt<-gsub("_", "-", Labels)
  if(nchar(Xtxt) <= 1){
    Xtxt <- paste0("\\s\\s", Xtxt)
    warning("Label is single character or blank. Padding with empty spaces.")
  }
  # Create qrcode
  Xpng <- grid::rasterGrob(
    abs(qrcode::qr_code(paste0(Xtxt), 
                        ecl = ErrCorr) - 1), 
    interpolate = FALSE)
  return(Xpng)
} # end of qrcode_make function
################################################################################

# QR code generation function in qrlabelr
make_qrcode <- function(my_id, ec_level = 3){
  
  assertthat::assert_that(!missing(my_id),
                          msg = "Please provide a unique ID.")
  assertthat::assert_that(ec_level >= 0 && ec_level <= 3, msg = "Please provide an error
                          correction level value in the range 0 - 3.")
  # Create qrcode and store as a rasterGrob image
  aa <- raster::raster(qrencoder::qrencode_raw(as.character(my_id), ec_level))
  qr <- grid::rasterGrob(raster::as.raster(aa, col = c('white','black')),
                         interpolate = FALSE)
  return(qr)
} # end of make_qrcode function
################################################################################

################################################################################
# Test data
# Generate 200 plot unique IDs to micro-benchmark the two functions
unique_id <- paste("KUMASI2023_PYT", c(1001:1200), 
                  rep(1:5, 40), rep(1:8, 25), sep = "_")

################################################################################

################################################################################
# Test how long it takes the functions to run on test data-- run 100 times
# Takes a long time to run
if(!require('microbenchmark')) install.packages('microbenchmark')
library(microbenchmark)

perf_results <- microbenchmark::microbenchmark(
  qrlabelr = unique_id |> purrr::map(\(x) make_qrcode(ec_level = 3, x)),
  baRcodeR = sapply(as.character(unique_id), qrcode_make, ErrCorr = 'H', 
                        USE.NAMES = TRUE, simplify = FALSE),
  times = 100L
)
################################################################################

# Summaries -- time units in milliseconds
# The microbenchmark package does automatic unit conversion
perf_results


# Save original performance results as a microbenchmark object
saveRDS(perf_results, "time_mac.rds")

# Convert microbenchmark object to a data frame and rename column 1 as 'package'
res <- as.data.frame(perf_results)
colnames(res)[1] <- 'package'

# Convert time to milliseconds in converted data frame
res$milli.time <- res$time/1e6

# Save converted data frame as a csv file in working directory
write.csv(res, file = 'time_mac.csv', row.names = FALSE)
#file.show("time_mac.csv")

################################################################################
# Import already run data for summary and plotting

res_mac <- read.csv("time_mac.csv", header = TRUE, stringsAsFactors = TRUE)
res_win <- read.csv("time_win.csv", header = TRUE, stringsAsFactors = TRUE)

# Re-order levels of the package factor (qrlabelr as the first level)
res_mac$package <- factor(res_mac$package, levels = c('qrlabelr', 'baRcodeR'))
res_win$package <- factor(res_win$package, levels = c('qrlabelr', 'baRcodeR'))

str(res_mac)
str(res_win)
################################################################################

################################################################################
# Summarize results using the dplyr package
summ_mac <- res_mac |> dplyr::group_by(package) |> dplyr::summarise(min = signif(min(milli.time), 3),
                                                            mean = signif(mean(milli.time), 3),
                                                            max = signif(max(milli.time), 3),
                                                           st.dev = signif(sd(milli.time), 3))
summ_win <- res_win |> dplyr::group_by(package) |> dplyr::summarise(min = signif(min(milli.time), 3),
                                                                    mean = signif(mean(milli.time), 3),
                                                                    max = signif(max(milli.time), 3),
                                                                    st.dev = signif(sd(milli.time), 3))
# View summarized results

# View summarized results
as.data.frame(summ_mac)
as.data.frame(summ_win)


# Calculate how fast qrlabelr is over baRcodeR
summ_mac[,-c(1,5)][2,]/summ_mac[,-c(1, 5)][1,] # On MacBook
summ_win[,-c(1,5)][2,]/summ_win[,-c(1, 5)][1,] # On PC
################################################################################


################################################################################
# Test for significance difference between the average times for qrlabelr and BaRcodeR
ttest_mac <- t.test(milli.time ~ package, data = res_mac, alternative = "less", 
                var.equal = FALSE, paired = FALSE)

gtext_mac <- paste(paste0('t-statistic = ', round(ttest_mac$statistic, 2), ';'), 
               'p value =', signif(ttest_mac$p.value, 2))
gtext_mac


ttest_win <- t.test(milli.time ~ package, data = res_win, alternative = "less", 
                    var.equal = FALSE, paired = FALSE)

gtext_win <- paste(paste0('t-statistic = ', round(ttest_win$statistic, 2), ';'), 
                   'p value =', signif(ttest_win$p.value, 2))
gtext_win
################################################################################

hypothesis <- paste('Hypothesis: time(qrlabelr) < time(baRcodeR)') 

################################################################################


################################################################################
# Make violin plots to visualize microbenchmarking results
# Install the ggplot2 and ggpmisc packages if not already installed
if(!require('ggplot2')) install.packages('ggplot2')
if(!require('ggpmisc')) install.packages('ggpmisc')

library(ggplot2)
library(ggpmisc)


# Set y axis (Time axis) limits
y_min <- 0

################################################################################
# For MacBook data
# Set upper limit of the y axis to 5 percent more than the maximum value
y_max_m <- 1.05*max(res_mac$milli.time)

# Make the plot for MacBook data
pp <- ggplot(res_mac, aes(x = package, y = milli.time, color = package)) 
      

pp <- pp + coord_cartesian(ylim = c(y_min, y_max_m))

pp <- pp + stat_ydensity(linewidth = .8) + 
      geom_boxplot(width = 0.1, fill = "white", linewidth = .5)


pp <- pp + scale_x_discrete(name = "Package")

# Log transform Time axis 
pp <- pp + scale_y_log10(name = "Time [milliseconds]")

# Flip axis
pp <- pp + ggplot2::coord_flip()

# Change to classical theme
pp <- pp + theme_classic()

pp <- pp + geom_text(aes(label = hypothesis, y = 2500, x = 1.4), color = "blue") 
pp <- pp + geom_text(aes(label = gtext_mac, y = 3000, x = 1.3), color = "blue") 

pp <- pp +  annotate("table_npc", npcx = "center", npcy = "bottom",
               label = list(summ_mac))
pp <- pp + theme(axis.title.y = element_blank())
pp
################################################################################

################################################################################
# Plot for Windows PC data
# Set upper limit of the y axis to 5 percent more than the maximum value
y_max_w <- 1.05*max(res_win$milli.time)

# Make the plot for MacBook data
ppp <- ggplot(res_win, aes(x = package, y = milli.time, color = package)) 


ppp <- ppp + coord_cartesian(ylim = c(y_min, y_max_w))

ppp <- ppp + stat_ydensity(linewidth = .8) + 
  geom_boxplot(width = 0.1, fill = "white", linewidth = .5)


ppp <- ppp + scale_x_discrete(name = "Package")

# Log transform Time axis 
ppp <- ppp + scale_y_log10(name = "Time [milliseconds]")

# Flip axis
ppp <- ppp + ggplot2::coord_flip()

# Change to classical theme
ppp <- ppp + theme_classic()

ppp <- ppp + geom_text(aes(label = hypothesis, y = 10000, x = 1.4), color = "blue") 
ppp <- ppp + geom_text(aes(label = gtext_win, y = 10000, x = 1.3), color = "blue") 

ppp <- ppp +  annotate("table_npc", npcx = "center", npcy = "bottom",
               label = list(summ_win))
ppp <- ppp + theme(legend.position = 'none')
ppp
################################################################################

# Arrange the two plots
# Save all plots into a list object
library(gridExtra)
library(ggpubr)

plts <- list(ppp, pp)



# Plot for all runs in one pdf file
ggsave(filename = paste0("violin_plots",  ".pdf"), 
       plot = marrangeGrob(plts, nrow = 1, ncol = 2), 
       device = "pdf", path = getwd(), units = "in",
       width = 10.5, height = 5, onefile = TRUE)



library(parallel)
detectCores()
sessionInfo()
