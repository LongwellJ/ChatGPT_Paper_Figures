# Load necessary package
library(ggplot2)
library(readxl)
#ChatGPT_JCO_Supplemental_RG_Working_1_ <- read_excel("C:/Users/longw/OneDrive/Desktop/Chatgpt/ChatGPT JCO Supplemental RG Working (1).xlsx")
ChatGPT_JCO_Supplemental_RG_Working_1_ <-read_excel("C:/Users/longw/Downloads/chatgpt_medonc_exam_supplementary_12oct2023 IH reviewed.xlsx")
chartdf <- data.frame(
  test_name = ChatGPT_JCO_Supplemental_RG_Working_1_$Test_name,
  concordance = ChatGPT_JCO_Supplemental_RG_Working_1_$Answer_correctness_ChatGPT4,
  value = matrix(1, ncol = 1, nrow = 147)
  )
chartdf$concordance <- factor(chartdf$concordance)
#make the total cat
secondsorted <- chartdf
secondsorted$test_name <- "Total"

finaldf <- rbind(chartdf,secondsorted)

# Stacked + percent
ggplot(finaldf, aes(fill=concordance, y=value, x=test_name)) + 
  geom_bar(position="fill", stat="identity") +
 ylab("Classification of Explanations Provided (%)") +
 xlab("Question Set")+ 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  
  theme_classic() +
  labs(fill = "") +
  scale_fill_manual(values = c("1" = "red", "2" = "grey", "3" = "#EFC000FF", "4" = "#0073C2FF"), labels=c("Wrong with major error","Wrong with minor error","Right with minor error","Right with no error"))


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

  
  
  
  
  