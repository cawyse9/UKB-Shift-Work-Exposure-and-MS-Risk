
## Script name:         SW_plots221223.R

## Purpose of script:   Make plots of lifetime shiftwork data

## Author:              Cathy Wyse

## Date Created:        2023-22-12

## Contact:             cathy.wyse@mu.ie

library(viridis)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library (cowplot)

#######################################################################################################
#
# Barplots of changes in shlftwork over the lifetime
#
#######################################################################################################

#   Need to know the frequency of the shiftwork categories at each age bracket 
#   There may have been two kinds of jobs, and a mix of SW/NonSW.  
#   Use table to get frequencies of shiftwork occupations by age bracket


#  Barplot 1 Occupation of shiftworkers at each age bracket
###############################################################################################

SW_type_by_agebracket <- (prop.table(table(shiftwork$bracket_SW_type, shiftwork$agebracket), margin=2))

SW_type_by_agebracket <- replace(SW_type_by_agebracket, is.na(SW_type_by_agebracket), 0)

col <- viridis(length(labels), option = "D", begin = 0, end = 1, direction = 1 )

# tab:blue : #1f77b4
#   tab:orange : #ff7f0e
#   tab:green : #2ca02c
#   tab:red : #d62728
#   tab:purple : #9467bd
#   tab:brown : #8c564b
#   tab:pink : #e377c2
#   tab:gray : #7f7f7f
#   tab:olive : #bcbd22
#   tab:cyan : #17becf

#col<-pal_jco("default")(9)

col <-c('#1f77b4', #tableau matlab
        '#ff7f0e',
        '#2ca02c',
        '#d62728',
        '#9467bd',
        '#8c564b',
        '#e377c2',
        '#7f7f7f',
        '#bcbd22',
'#17becf')

shiftwork$bracket_SW_type <- factor(shiftwork$bracket_SW_type, levels = c("Assoc_prof","Prof","Machine","Managers","Trades","Service","Element","Admin","Sales"))

labels <- c("Assoc Professional","Professional","Machine","Managers","Trades","Service","Element","Admin","Sales")
par(mar = c(5, 4, 4, 2)) 
jpeg("occupation_barplot1.jpg", width = 800, height = 600, pointsize = 14)
par(mar = c(5, 4, 4, 12))
barplot((SW_type_by_agebracket), col = col,border = NA, space = .1, yaxt = "n")
title(main = "Occuption of Shiftworkers by Age Bracket", col.main = "black", font.main = 2, cex=.6)
title(xlab = "Age Bracket", col.lab = "black", font.lab = 2)
legend("right", x= 11, y = 1, bty = "n", xpd = TRUE,  border = NA, legend = labels, fill = col)
axis(2, at = c(0,0.2,0.4,0.6,0.8,1),las = 1, cex.axis = 1, labels = c("0","20%","40%","60%", "80%", "100%"))  
dev.off()
par(mar = par("mar"))


##  Barplot 3  Shiftwork by type night day and mix across the ages
###############################################################################################

# Create a new categorical variable based on the three SW variables
shiftwork <- shiftwork %>%
  mutate(
    SW_summary = case_when(
      bracket_total_hr_daySW >1 ~ "Night",
      bracket_total_hr_nightSW >1 ~ "Day",
      bracket_total_hr_mixSW > 1 ~ "Mixed",
      TRUE ~ NA  
    ))

shiftwork$SW_summary <- factor(shiftwork$SW_summary, levels = c("Mixed", "Night", "Day"))
labels2 <- c("Mixed  Shiftwork", "Night  Shiftwork", "Day Shiftwork")

#col<-pal_jco("default")(9)
#"#0073C2FF" "#EFC000FF" "#868686FF" "#CD534CFF" "#7AA6DCFF" "#003C67FF" "#8F7700FF" "#3B3B3BFF" "#A73030FF"
viridis_pal(option = "D")(5)
#col<-c( "#21908CFF", "#3B528BFF")
#col<-c( "#21908CFF","#440154FF","#3B528BFF")
#col <- c("#CD534CFF", "#003C67FF", "#5DC863FF")
col <- c("#0073C2FF","#3B3B3BFF","#EFC000FF")
all_SW_agebracket <- prop.table(table(factor(shiftwork$SW_summary),factor(shiftwork$agebracket)), margin=2)

  dev.new()

#Type of Shift Work by Age Bracket
par(mar = c(5, 4, 1, 6))  
svg("SW_barplot.svg", width = 20/2.54, height = 14/2.54)
#par(mar = c(5, 4, 2, 6))
barplot((all_SW_agebracket), col = col, border = NA, space = .1, yaxt = "n")
title(main = "", col.main = "black", font.main = 2, cex=.6)
title(xlab = "Age Bracket", col.lab = "black", font.lab = 2)
legend(
  "top", 
 inset = c(0, -0.2), # Adjusts the legend position
  bty = "n", 
  xpd = TRUE, 
  border = NA, 
  horiz = TRUE, 
  legend = labels2, 
  fill = col, 
  cex = 1 
)

axis(2, at = c(0,0.2,0.4,0.6,0.8,1),las = 1, cex.axis = 1, labels = c("0","20%","40%","60%", "80%", "100%"))  
dev.off()

#par(mar = par("mar"))
par(mar = c(5, 4, 1, 6))

##  Barplot 3  Shiftwork or not across the ages
###############################################################################################

#use table to get the frequencies of all shiftwork by age
shiftwork$allSW <- shiftwork$bracket_total_hr_daySW + shiftwork$bracket_total_hr_night + shiftwork$bracket_total_hr_mixSW

# Creating the new categorical variable
shiftwork <- shiftwork %>%
  mutate(
    SW_YN = case_when(
      allSW == 0 & bracket_total_hr > 0 ~ "Not a Shiftworker",
      allSW > 0 & bracket_total_hr > 0 ~ "Shiftworker",
      bracket_total_hr == 0 ~ "Not working",
      TRUE ~ NA_character_
    )
  )

SW_agebracket <- prop.table(table(shiftwork$SW_YN,shiftwork$agebracket), margin=2)
labels3 <- c("Non ShiftWorker" , "Not Working", "ShiftWorker" )
labels4 <- c('15-20',      '21-25', '26-30',   '31-35', '36-40',  '41-45', '46-50','51-55', '56-60','61-65')


viridis_pal(option = "D")(5)
col<-c( "#21908CFF", "#3B528BFF", "#440154FF")

#col <- c("#CD534CFF","#EFC000FF", "#003C67FF", "#5DC863FF")
#col <- viridis(length(labels), option = "D", begin = 1, end = 0, direction = 1 )

par(mar = c(5, 4, 1, 6))  
svg("SW_type_barplot.svg", width = 20 / 2.54, height = 14 / 2.54) # Convert cm to inches for svg
barplot(SW_agebracket, col = col, space = .1, border = NA, yaxt = "n")
title(main = "", col.main = "black", font.main = 2, cex = 1)
title(xlab = "Age Bracket", col.lab = "black", font.lab = 2)
legend(
  "top", 
  inset = c(0, -0.2), 
  bty = "n", 
  xpd = TRUE, 
  border = NA, 
  horiz = TRUE, 
  legend = labels3, 
  fill = col, 
  cex = 1
)

axis(2, at = c(0,0.2,0.4,0.6,0.8,1),las = 1, cex.axis = 1, labels = c("0","20%","40%","60%", "80%", "100%"))  

#par(mar = par("mar"))
dev.off()

# Read the JPEG images as raster objects
jpeg1 <- ggdraw() + draw_image("SW_YN_barplot.jpg")
jpeg2 <- ggdraw() + draw_image("SW_type_barplot.jpg")


grid.arrange(jpeg1, jpeg2, ncol=2)


# Labels for each plot
combined_plot <- jpeg1 + jpeg2 + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(size = 6))
# Save the combined plot to a new file
ggsave("combined_barplots.jpg", combined_plot, width = 8, height = 2)
      

      