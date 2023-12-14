# Load necessary package
library(ggplot2)
library(readxl)
library(vcd)
library(ggpubr)
#ChatGPT_JCO_Supplemental_RG_Working_1_ <- read_excel("C:/Users/longw/OneDrive/Desktop/Chatgpt/ChatGPT JCO Supplemental RG Working (1).xlsx")
ChatGPT_JCO_Supplemental_RG_Working_1_ <-read_excel("C:/Users/longw/Downloads/chatgpt_medonc_exam_supplementary_12oct2023 IH reviewed.xlsx")

chartdf <- data.frame(
  extent = ChatGPT_JCO_Supplemental_RG_Working_1_$`Extent of possible harm`,
  likelihood = ChatGPT_JCO_Supplemental_RG_Working_1_$`Likelihood of possible harm`
)
chartdf<- na.omit(chartdf)
sorteddata<- chartdf[order(chartdf$extent),]

sorteddata$extent <- ifelse(sorteddata$extent==1, 'No Harm', sorteddata$extent)
sorteddata$extent <- ifelse(sorteddata$extent==2, 'Moderate', sorteddata$extent)
sorteddata$extent <- ifelse(sorteddata$extent==3, 'Death or Severe', sorteddata$extent)
sorteddata$likelihood <- ifelse(sorteddata$likelihood=='Moderate', 'Medium', sorteddata$likelihood)
sorteddata$likelihood <- ifelse(sorteddata$likelihood=='Mediiun', 'Medium', sorteddata$likelihood)

sorteddata$extent <- factor(sorteddata$extent, levels = c("No Harm", "Moderate", "Death or Severe"))
sorteddata$likelihood <- factor(sorteddata$likelihood, levels = c("Low", "Medium", "High"))

#balloon plot
my_cols=c("red","#0073C2FF", "#EFC000FF")
df <- as.data.frame(table(sorteddata))
colnames(df)[3] <- "Frequency"
#df$Freq <- ifelse(df$Freq==0, NA, df$Freq)
ggballoonplot(df,
              fill = "value",
              color = "grey",
              size="Frequency",
              size.range = c(0, 14),
              shape = 21,
              sorted=FALSE,
              show.label = TRUE,
              rotate.x.text = FALSE,
              ggtheme = theme_classic())+
  scale_fill_gradientn(colors = "#0073C2FF")+
  scale_size(range = c(0,11), breaks=c(0,3,6,9,12,14))

ggsave(
  "testplot.png",
  plot = last_plot(),
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = "retina",
  limitsize = TRUE,
  bg = NULL
)
#mosaic
mosaic(table(sorteddata), shade = TRUE, legend = TRUE) 

table(sorteddata)
