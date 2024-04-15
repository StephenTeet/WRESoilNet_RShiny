### WRE Soil Network Data Processing ###
# Author: Stephen Teet

# *****Get daily averages using lubridate, tidyverse, and dplyr*****
# Import libraries
library(tidyverse)
library(lubridate)
library(dplyr)

df1 <- df %>% mutate_all(~ifelse(is.nan(.), NA, .))

# Fix Date/Time
df1$TIMESTAMP = mdy_hm(df1$TIMESTAMP, tz = "America/Chicago")

df2 <- data.frame(DateTime = df1$TIMESTAMP,
                  # batt_volt = df1$batt_volt,
                  VWC_1 = df1$VWC,
                  T_1 = df1$T,
                  VWC_2 = df1$VWC_2,
                  T_2 = df1$T_2,
                  VWC_3 = df1$VWC_3,
                  T_3 = df1$T_3,
                  VWC_4 = df1$VWC_4,
                  T_4 = df1$T_4,
                  VWC_5 = df1$VWC_5,
                  T_5 = df1$T_5,
                  VWC_6 = df1$VWC_6,
                  T_6 = df1$T_6)

# Get rid of any extreme values manually for now
# df2 = df2 %>% mutate(VWC_1 = replace(VWC_1, VWC_1>1, NA)) 
# Get daily values
df3 <- df2 %>% group_by(Date = date(DateTime)) %>%
  summarise(VWC_1 = mean(VWC_1),
            T_1 = mean(T_1),
            VWC_2 = mean(VWC_2),
            T_2 = mean(T_2),
            VWC_3 = mean(VWC_3),
            T_3 = mean(T_3),
            VWC_4 = mean(VWC_4),
            T_4 = mean(T_4),
            VWC_5 = mean(VWC_5),
            T_5 = mean(T_5),
            VWC_6 = mean(VWC_6),
            T_6 = mean(T_6))

# prepare data for plotting in strip chart
vwc10a <- df3$VWC_1
vwc10b <- df3$VWC_2
vwc10c <- df3$VWC_3
vwc22a <- df3$VWC_4
vwc22b <- df3$VWC_5
vwc22c <- df3$VWC_6
x <- list("85 cm"=vwc85,"55 cm"=vwc55, "35 cm"=vwc35, "15 cm"=vwc15, "5 cm"=vwc05)

ec05 <- df3$EC1_Avg
ec15 <- df3$EC2_Avg
ec35 <- df3$EC3_Avg
ec55 <- df3$EC4_Avg
ec85 <- df3$EC5_Avg
y <- list("85 cm"=ec85,"55 cm"=ec55, "35 cm"=ec35, "15 cm"=ec15, "5 cm"=ec05)

# build strip charts
VWC <- stripchart(x,
                  main="Stripchart of VWC by Depth",
                  xlab="Volumetric Water Content (VWC)",
                  ylab="Sensor Depth",
                  method="jitter",
                  col=c("brown3", "darkorange1", "darkolivegreen1", "darkolivegreen", "deepskyblue3", "darkorchid3"),
                  pch=16)

EC <- stripchart(y,
                 main="Stripchart of EC by Depth",
                 xlab="Soil Electroconductivity (EC)",
                 ylab="Sensor Depth",
                 method="jitter",
                 col=c("brown3", "darkorange1", "darkolivegreen1", "darkolivegreen", "deepskyblue3"),
                 pch=16)

# Write cleaned data table to .csv
# write.csv(df3, "WRE_SoilNetOuput.csv", row.names = FALSE)

# # Print last days minimum battery voltage
# tail(df3$batt_volt, 1)
# 
# # Plots to check for issues with daily data
# library(ggplot2)
# 
# # Plot of VWC
# VWC_plot <- ggplot(df3, aes(DateTime)) +   
#   geom_line(aes(y = VWC1_Avg), color = "black") + 
#   geom_line(aes(y = VWC2_Avg), color = "red") + 
#   geom_line(aes(y = VWC3_Avg), color = "green") + 
#   geom_line(aes(y = VWC4_Avg), color = "blue") + 
#   geom_line(aes(y = VWC5_Avg), color = "purple") 
# VWC_plot
# 
# # Plot of Electroconductivity
# EC_plot <- ggplot(df3, aes(DateTime)) +   
#   geom_line(aes(y = EC1_Avg), color = "black") + 
#   geom_line(aes(y = EC2_Avg), color = "red") + 
#   geom_line(aes(y = EC3_Avg), color = "green") + 
#   geom_line(aes(y = EC4_Avg), color = "blue") + 
#   geom_line(aes(y = EC5_Avg), color = "purple") 
# EC_plot
# 
# # Plot of Soil Temp
# TSoil_plot <- ggplot(df3, aes(DateTime)) +   
#   geom_line(aes(y = TSoil1_Avg), color = "black") + 
#   geom_line(aes(y = TSoil2_Avg), color = "red") + 
#   geom_line(aes(y = TSoil3_Avg), color = "green") + 
#   geom_line(aes(y = TSoil4_Avg), color = "blue") + 
#   geom_line(aes(y = TSoil5_Avg), color = "purple") 
# TSoil_plot
# 
# # Plot of Permittivity
# Perm_plot <- ggplot(df3, aes(DateTime)) +   
#   geom_line(aes(y = Perm1), color = "black") + 
#   geom_line(aes(y = Perm2), color = "red") + 
#   geom_line(aes(y = Perm3), color = "green") + 
#   geom_line(aes(y = Perm4), color = "blue") + 
#   geom_line(aes(y = Perm5), color = "purple") 
# Perm_plot
# 
# #Plot of PerAvg
# PerAvg_plot <- ggplot(df3, aes(DateTime)) +   
#   geom_line(aes(y = PerAvg1), color = "black") + 
#   geom_line(aes(y = PerAvg2), color = "red") + 
#   geom_line(aes(y = PerAvg3), color = "green") + 
#   geom_line(aes(y = PerAvg4), color = "blue") + 
#   geom_line(aes(y = PerAvg5), color = "purple") 
# PerAvg_plot
# 
# # Plot of Volt
# VoltR_plot <- ggplot(df3, aes(DateTime)) +   
#   geom_line(aes(y = VoltR1), color = "black") + 
#   geom_line(aes(y = VoltR2), color = "red") + 
#   geom_line(aes(y = VoltR3), color = "green") + 
#   geom_line(aes(y = VoltR4), color = "blue") + 
#   geom_line(aes(y = VoltR5), color = "purple") 
# VoltR_plot