#------------------------ library ---------------------------

# Load the modeest library for mode calculation
install.packages("modeest")
library(modeest)  


#to make a latex script from a table
install.packages("xtable")
library(xtable)

#for plot
install.packages("ggplot2")
library("ggplot2")

#color of bar plot
install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("dplyr")
install.packages("dplyr")
library(dplyr)

#scatter plot
install.packages("scales")                                         
library("scales") 


install.packages("forcats")
library(forcats)

#correlation
install.packages("Hmisc")
library("Hmisc")



#------------------------ labels ---------------------------

attach(ESS)

cntry
print_labels(cntry)

netustm
print_labels(netustm)


nwspol
print_labels(nwspol)

clmchng
print_labels(clmchng)

#=========================================================================================

#----------------------------------- mean, median, mode ------------------------------------

########################################################


# Custom function to calculate the mode
get_mode <- function(x) {
  uniq <- unique(x)             # Get unique values
  tab <- tabulate(match(x, uniq))  # Count frequencies of each unique value
  uniq[tab == max(tab)]         # Return value(s) with maximum frequency
}

# Filter out the rows for NA values of netustm, nwspol ad clmchng variables
filtered_data <- ESS %>%
  filter(!is.na(netustm)) %>%
  filter(!is.na(nwspol)) %>%
  filter(!is.na(clmchng))

# Calculate mean for filtered data
mean_nwspol_filtered <- mean(filtered_data$nwspol)
mean_netustm_filtered <- mean(filtered_data$netustm)
mean_clmchng_filtered <- mean(filtered_data$clmchng)

# Calculate median for filtered data
median_nwspol_filtered <- median(filtered_data$nwspol)
median_netustm_filtered <- median(filtered_data$netustm)
median_clmchng_filtered <- median(filtered_data$clmchng)

# Calculate mode for filtered data
mode_nwspol_filtered <- get_mode(filtered_data$nwspol)
mode_netustm_filtered <- get_mode(filtered_data$netustm)
mode_clmchng_filtered <- get_mode(filtered_data$clmchng)

# Calculate mean for complete dataset
mean_nwspol_complete <- mean(ESS$nwspol, na.rm = TRUE)
mean_netustm_complete <- mean(ESS$netustm, na.rm = TRUE)
mean_clmchng_complete <- mean(ESS$clmchng, na.rm = TRUE)

# Calculate median for complete dataset
median_nwspol_complete <- median(ESS$nwspol, na.rm = TRUE)
median_netustm_complete <- median(ESS$netustm, na.rm = TRUE)
median_clmchng_complete <- median(ESS$clmchng, na.rm = TRUE)

# Calculate mode for complete dataset
mode_nwspol_complete <- get_mode(ESS$nwspol)
mode_netustm_complete <- get_mode(ESS$netustm)
mode_clmchng_complete <- get_mode(ESS$clmchng)

# Create a table
output_table <- data.frame(
  Variable = c("nwspol", "netustm", "clmchng"),
  Mean_Filtered = c(mean_nwspol_filtered, mean_netustm_filtered, mean_clmchng_filtered),
  Median_Filtered = c(median_nwspol_filtered, median_netustm_filtered, median_clmchng_filtered),
  Mode_Filtered = c(mode_nwspol_filtered, mode_netustm_filtered, mode_clmchng_filtered),
  Mean_Complete = c(mean_nwspol_complete, mean_netustm_complete, mean_clmchng_complete),
  Median_Complete = c(median_nwspol_complete, median_netustm_complete, median_clmchng_complete),
  Mode_Complete = c(mode_nwspol_complete, mode_netustm_complete, mode_clmchng_complete)
)

# Display the output table
output_table

# Convert the output_table to an xtable object
xtable_output <- xtable(output_table)

# Print the LaTeX code for the table
print(xtable_output, include.rownames = FALSE)




#--------------------- recheck the values from from previous output_tables -----------------

# Calculate summary statistics for variables in the original dataset
summary_original <- summary(ESS[c("nwspol", "netustm", "clmchng")])
summary_original

# Calculate summary statistics for variables in the filtered dataset
summary_filtered <- summary(filtered_data[c("nwspol", "netustm", "clmchng")])
summary_filtered



#----------------------------------- nominal variable ------------------------------------

#--------------- mode : country ---------------
#since it's not convenient to calculate the mode of non numeric value with the previous function,
#the mode of the "cntry" variable is calculated as below


#---------------------- mode value of "cntry" with filter the NA values ------------------
#calculate the mode value of cntry variable with applying the netustm, nwspol and clmchng filter without NA values
mode_result_of_cntry_filtered <- ESS %>%
  filter(!is.na(netustm)) %>%
  filter(!is.na(nwspol)) %>%
  filter(!is.na(clmchng)) %>%
  count(cntry) %>%
  summarise(mode_value = cntry[n == max(n)],
            max_count = max(n))

cat("Mode value(s) of 'cntry' variable (filtered): ", mode_result_of_cntry_filtered$mode_value, "\n")
cat("Maximum count (filtered): ", mode_result_of_cntry_filtered$max_count, "\n")

#------------------------ mode value of "cntry" without tapplying the filter ---------------------
#calculate the mode value of cntry variable with applying the netustm, nwspol and clmchng filter without NA values
mode_result_of_cntry <- ESS %>%
  count(cntry) %>%
  summarise(mode_value = cntry[n == max(n)],
            max_count = max(n))

cat("Mode value(s) of 'cntry' variable (without filtered): ", mode_result_of_cntry$mode_value, "\n")
cat("Maximum count (without filted): ", mode_result_of_cntry$max_count, "\n")





#--------------- bar plot : country ---------------

#bar plot of country with nespol, netustm and clmchng variable's respondant
ESS %>%
  filter(!is.na(netustm)) %>%
  filter(!is.na(nwspol)) %>%
  filter(!is.na(clmchng)) %>%
  mutate(cntry = as_factor(cntry)) %>%
  group_by(cntry) %>% 
  summarise(n = n()) %>% 
  mutate(freq = 100 * prop.table(n)) %>%
  ggplot(aes(x = cntry, y = freq, fill = cntry)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Frequency (%)") +
  ggtitle("Frequency of Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "none", # Adjust the size of axis label
  )



#----------------------------------- ordinal variable ------------------------------------


#--------------- bar plot : clmchng ---------------


#margin of the plot
par(mar=c(4.2,4.2,1,1))
#color of the bars
coul <- brewer.pal(7, "Set2")
#bar plot of clmchng with NA values
barplot(100*prop.table(table(clmchng, useNA = "ifany")),
        names.arg=c("1","2","3","4","NA"),
        cex.names=1.4  ,
        col=coul,
        ylim=c(0,60),
        xlab = "Opinion about climate change", mgp = c(4, 4, 4),
        ylab = "Percentage (%)", mgp = c(3, .5, 0),
        cex.axis = 1.5,
        cex.lab = 1.4,
)
par(mar=c(3,0,0,0))
#legend of the barplot
legend("topright",
       legend = c("1 - Definitely changing", "2 - Probably changing",
                  "3 - Probably not changing","4 - Definitely not changing","NA"), 
       fill = c(coul))



### -------------- recheck the barplot values from a table values

#package janitor
install.packages("janitor")
library(janitor)

#match the barplot with values by creating a table
ESS%>%
  tabyl(clmchng)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()%>%
  knitr::kable() 

#this is another way to crossmatch the values
100*prop.table(table(clmchng, useNA = "ifany"))


#----------------------------------- metric variable ------------------------------------

#--------------- histagram: netustm ---------------

netustm


#margin of the plot
par(mar=c(4.5,4.5,1,1))

# Create a histogram of 'netustm' variable using the subsetted data
hist(filtered_data$netustm, 
     breaks = 40,                      # Set the number of histogram bins
     main = "Histogram: Internet use", # Set the main title of the plot
     xlab = "Internet use on typical day (in minutes)", # Set the x-axis label
     mgp = c(3, 1, 0),                 # Set the margin line locations
     ylim = c(0, 7000),                # Set the y-axis limits
     cex.axis = 1.2,                   # Set the size of axis labels
     cex.lab = 1.3                     # Set the size of the x-axis label
)


###################################################################################

#----------------------------- bivariant analysis ------------------------------

######################------- correlation between two nominal variable ------------######################


### bivariate analysis between two nominal variable
#Iportant: since the clmchng is an ordinal variable, we grouped the data into two sebset to make the data nominal.

# nominal variable: 1. cntry
#                   2. climate_change
#                       >> "Changing" - "Definitely changing" and "Probably changing" are grouped together ,
#                       >> "Not changing" - "Probably not changing" and "Definitely not changing" are grouped together


climate_change <- c()
climate_change[clmchng == 1 | clmchng == 2] <- "Changing"
climate_change[clmchng == 3 | clmchng == 4] <- "Not changing"

ESS$climate_change <- climate_change

#cross table with gndr with climate_change
sjPlot::tab_xtab(var.row = gndr, 
                 var.col = climate_change, 
                 var.labels =  c("Gender", "Opinioon about climate change"),
                 title = "Gender & Opinioon about climate change", 
                 show.row.prc = TRUE,
                 show.col.prc = TRUE,
                 show.summary = TRUE, statistics = "cramer")

install.packages("lsr")
library(lsr)
#cramersV value
cramersV(gndr, climate_change)


#create a prpportional table with climate_change and gndr variable
climate_change_and_gndr <- 100*prop.table(table(gndr, climate_change),2) 

#barplot of climate_change and gndr
par(mar=c(2,4.5,2,1))
barplot(climate_change_and_gndr,
        cex.names=1.4,
        main = "Ratio of Gender on climate change opinion",
        #col=coul,
        #ylim=c(0, 120),
        ylab = "Proportion of Gender (in %)", mgp = c(3, .5, 0),
        cex.axis = 1.4,
        cex.lab = 1.6,
        legend = c("Male", "Female"), 
        )


################### correlation between netustm and nwspol with variable regarding to climate change ###################


#correlation between all the variable related to the chosen topic of this report.
#The "agea" variable is included for interest.
rcorr(as.matrix(ESS[,c("nwspol","netustm" ,"clmchng", "agea","gndr","eneffap","rdcenr","wrclmch")]))
correlations_all <- rcorr(as.matrix(ESS[,c("nwspol","netustm" ,"clmchng", "agea","gndr","eneffap","rdcenr","wrclmch")]))
correlations_all
#Important Conclusion: from the output of correlations_all object, we can say that there is no significant relation between nwspol to clmchng and netustm to clmchng



################# correlation between freely selectable variable (agea with netustm) ####################


#From the correlation_all output, we have seen a moderate negative linear relationship between the variables "agea" and "netustm"
#now in this section we drive more into the relation between netustm with agea

#rechack the correlation
cor.test( agea, netustm, method = "pearson")

#graphical representation of correlation between netustm and agea variable
par(mar=c(4.5,4.5,2,1))
plot(agea, netustm,
     xlab = "Age (in year)", mgp = c(4, 4, 4),
     ylab = "Net use in a typical day (in min)", mgp = c(3, .5, 0),
     cex.axis = 1.4,
     cex.lab = 1.4,
)


#divide the netustm into some categories
netustm_rec <- c()
netustm_rec[netustm<=10] <- 1
netustm_rec[netustm>=11 & netustm <= 30] <- 2
netustm_rec[netustm>=31 & netustm <= 60] <- 3
netustm_rec[netustm>=61 & netustm <= 120] <- 4
netustm_rec[netustm>=121 & netustm <= 180] <- 5
netustm_rec[netustm>=181 & netustm <= 240] <- 6
netustm_rec[ netustm > 240] <- 7

ESS$netustm_rec <- netustm_rec

#create a proportional table with netustm_rec and agea variable
climate_change_to_nwspol <- 100*prop.table(table(  netustm_rec, agea),2) 

#barplot of different categories as a function of age
par(mar=c(4.5,4.5,1,1))
barplot(climate_change_to_nwspol,
        col=coul,
        xlab = "",
        ylab = "",
        cex.names=1.4,
        xlim=c(0, 120),
        cex.axis = 1.4,
        cex.lab = 1.4,)

title(xlab = "Age (in year)", mgp = c(3, 1, 0),cex.lab = 1.4)    # Add x-axis text
title(ylab = "Net use category (in %)", mgp = c(3, 1, 0),cex.lab = 1.4)    # Add y-axis text
legend("topright",
       legend = c("<= 10 min","11 to 30 min","31 min to 1h","1 to 2 h","2 to 3 h","3 to 4 h","> 4 h"), 
       fill = c(coul))

#confirm the correlation between agea and netustm
cor.test( netustm_rec, agea, method = "pearson")


detach(ESS)
############################### End ###########################

