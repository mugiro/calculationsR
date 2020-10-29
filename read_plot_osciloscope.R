#
# Load the data of 2016 race

rm(list = ls()) # clean variables
graphics.off() # close graphics windows

require(stringr)
require(readr)
require(dplyr)
require(tibble)
library(tidyr)
require(ggplot2)
require(gridExtra)
# require(purrr)
# require(mosaic)

Sys.setenv("DISPLAY" = ":0") # to make View works on vscode
# theme_set(theme_minimal())   # Establish the theme set for the plots


work_dir_gen <- "/Users/andres/Google Drive/0.Script_Code/rProjects/calculationsR"
pattern <- "T0001.CSV" # First test
vars  <- c("CH1", "CH2", "CH3", "CH4")
colors <- c("#0022bb", "#E7B800", "#00e713", "#e7003a")

dir_data <- paste(work_dir_gen, "data/Sonda_1_en_frecuencia", sep = "/")
dir_results <- paste(work_dir_gen, "results/Sonda_1_en_frecuencia", sep = "/")

archivos  <- dir(dir_data)

data_raw  <- read_csv(paste0(dir_data, "/", archivos[which(str_detect(archivos, pattern))]), skip = 15) #, n_max = 10)

summary(data_raw)
# names(data_raw)
# [1] "TIME"            "CH1"             "CH1 Peak Detect" "CH2"             "CH2 Peak Detect" "CH3"            
# [7] "CH3 Peak Detect" "CH4"             "CH4 Peak Detect"


################################################
# 1. Transform the data into long format. We only represent CH1 - CH4
df <- data_raw %>%
  select(TIME, CH1, CH2, CH3, CH4) %>%
  gather(key = "variable", value = "value", -TIME)
# head(df, 3)

# Multiple line plot in the same plot with all the data
g1 <- ggplot(df, aes(x = TIME, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#0022bb", "#E7B800", "#00e713", "#e7003a")) +
  theme_minimal()

ggsave(g1, device = "png",
        filename = paste0(dir_results,"/", pattern, "_plot.png"),
        height = 15, width = 120, units = "cm")


################################################
# 2. Multiple line plots in one file each one with all the data
for (i in seq_len(length(vars))) {
    data_aux <- data_raw %>% select(TIME, vars[i]) %>% rename(value = vars[i])
    p1 <- ggplot(data_aux, aes(x = TIME, y = value)) +
        geom_line(color = colors[i], size = 1) +
        theme_minimal() +
        ggtitle(label = vars[i])

    ggsave(p1, device = "png",
            filename = paste0(dir_results,"/", pattern, "_plot_", vars[i], "_",".png"),
            height = 15, width = 120, units = "cm")
}


################################################
# 3. Select data with line numbers
n_i <- 1
n_f <- 1200
data_sel <- data_raw %>% slice(., (n_i + 1) : n_f)
df <- data_sel %>%
  select(TIME, CH1, CH2, CH3, CH4) %>%
  gather(key = "variable", value = "value", -TIME)

# Multiple line plot in the same plot with all the selected data
g1 <- ggplot(df, aes(x = TIME, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#0022bb", "#E7B800", "#00e713", "#e7003a")) +
  theme_minimal()

ggsave(g1, device = "png",
        filename = paste0(dir_results, "/", pattern, "_plot_",
                            "_", n_i, "-", n_f, ".png"),
        height = 15, width = 120, units = "cm")


################################################
# 4. Multiple line plots in one file each one with selected data
for (i in seq_len(length(vars))) {
    data_aux <- data_sel %>% select(TIME, vars[i]) %>% rename(value = vars[i])
    p1 <- ggplot(data_aux, aes(x = TIME, y = value)) +
        geom_line(color = colors[i], size = 1) +
        theme_minimal() +
        ggtitle(label = vars[i])

    ggsave(p1, device = "png",
            filename = paste0(dir_results,"/", pattern, "_plot_", 
                            vars[i], "_", n_i, "-", n_f, ".png"),
            height = 15, width = 120, units = "cm")
}