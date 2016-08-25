library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# folder address
dataFolder <- "K:\\CPDiary\\Data\\FRNL\\CS 2.12\\YEAR 2\\SoilMoisture\\"

# files names
file616 <- "Logger A (RS Oats)_Oats616.dat"
file650 <- "Logger A (RS Oats)_Oats650.dat"

# read layout
fileLayout <- "Layout.txt"
df_layout <- read.table(paste0(dataFolder,fileLayout), header = TRUE) # reads and skips the unit line
head(df_layout)
summary(df_layout)

# read files and tidy up the labels

# get 616
df_temp <- read.table(paste0(dataFolder,file616), skip = 1, header = TRUE, blank.lines.skip = TRUE, sep =",") # reads and skips the unit line
colnames616 <- colnames(df_temp)
colnames616 <- gsub("\\.", "_", colnames616)
colnames616

# get 650
df_temp <- read.table(paste0(dataFolder,file650), skip = 1, header = TRUE, blank.lines.skip = TRUE, sep =",") # reads and skips the unit line
colnames650 <- colnames(df_temp)
colnames650 <- gsub("\\.", "_", colnames650)
as.data.frame(colnames650)

# read data
data616 <- read.table(paste0(dataFolder,file616), skip = 4, header = FALSE, blank.lines.skip = TRUE, sep =",") # reads and skips the unit line
data650 <- read.table(paste0(dataFolder,file650), skip = 4, header = FALSE, blank.lines.skip = TRUE, sep =",") # reads and skips the unit line

# check data
head(data616)
head(data650)

# format data fields
data616$V1 <- ymd_hms(data616$V1)
data650$V1 <- ymd_hms(data650$V1)

# add column names back
colnames(data616) <- colnames616
colnames(data650) <- colnames650

# check
summary(data616)
summary(data650)

# Transpose using tidyr 
df <- data616

df <- df %>% 
  gather("Variable","Value",3:ncol(data616)) %>%
  mutate(Value = as.numeric(Value))

# check
summary(df)

# merge with labels
merge_df <- merge(df,df_layout, by.y = "R_Name", by.x = "Variable")

# sort out formats
merge_df$Value <- ifelse(Value == "NAN", NA, as.numeric(Value))
merge_df$Plot <- as.factor(merge_df$Plot)
merge_df$Block <- as.factor(merge_df$Block)
merge_df$Treat <- as.factor(merge_df$Treat)
merge_df$ThisDate <- as.Date(merge_df$TIMESTAMP)
merge_df

# check
head(merge_df)
str(merge_df)
summary(merge_df)

# plot
merge_df %>%
  #  filter(Block == 1) %>%
  filter(Measurement == "VWC") %>%
  ggplot(aes(x = TIMESTAMP, y = Value)) + # here we use ggplot
  geom_line(aes(linetype  = factor(Treat))) +
  facet_grid(Block+Depth~Crop, scales="free") +
  ylab("Volumetric soil mositure (fractional)") +
  xlab("Date")
