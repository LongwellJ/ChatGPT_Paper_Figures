library(readxl)
library(ggplot2)
library(dplyr)
#ChatGPT_JCO_Supplemental_RG_Working_1_ <- read_excel("C:/Users/longw/OneDrive/Desktop/Chatgpt/ChatGPT JCO Supplemental RG Working.xlsx")
ChatGPT_JCO_Supplemental_RG_Working_1_ <-read_excel("C:/Users/longw/Downloads/chatgpt_medonc_exam_supplementary_12oct2023 IH reviewed.xlsx")



 df <- ChatGPT_JCO_Supplemental_RG_Working_1_
 df$Year <- ifelse(is.na(df$Year), 2000, df$Year)
 df$Year <- ifelse(df$Year == 2017, 2000, df$Year)
 df$Year <- ifelse(df$Year == 202, 2020, df$Year)
 table(df$Year, df$Correct_First_ChatGPT4_Answer)
 
 
prop.table(table(df$Year, df$Correct_First_ChatGPT4_Answer), margin = 1)
 
 
 barplot(t(prop.table(table(df$Year, df$Correct_First_ChatGPT4_Answer), margin = 1))[1, ], las = 1, names.arg = c('<2018', 2018:2022))
 
 
 
 fisher.test(table(df$Year >= 2018, df$Correct_First_ChatGPT4_Answer))
 
 
 fisher.test(table(df$Year >= 2019, df$Correct_First_ChatGPT4_Answer))
 
 
 fisher.test(table(df$Year >= 2020, df$Correct_First_ChatGPT4_Answer))
 
 fisher.test(table(df$Year >= 2021, df$Correct_First_ChatGPT4_Answer))
 
 
 wilcox.test(subset(df, Correct_First_ChatGPT4_Answer == 1)$Year, subset(df, Correct_First_ChatGPT4_Answer == 0)$Year)
 
 fisher.test(table(subset(df, Year <= 2021)$Year >= 2018, subset(subset(df, Year <= 2021), Year <= 2021)$Correct_First_ChatGPT4_Answer))
 fisher.test(table(subset(df, Year <= 2021)$Year >= 2019, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer))
 
 
 fisher.test(table(subset(df, Year <= 2021)$Year >= 2020, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer))
 
 
 
 fisher.test(table(subset(df, Year <= 2021)$Year >= 2021, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer))
 
 wilcox.test(subset(df, Correct_First_ChatGPT4_Answer == 1 & Year <= 2021)$Year, subset(df, Correct_First_ChatGPT4_Answer == 0 & Year <= 2021)$Year)
 
 
 
 table(df$Year >= 2020, df$Correct_First_ChatGPT4_Answer)
 
 prop.table(table(df$Year >= 2020, df$Correct_First_ChatGPT4_Answer), margin = 1)
 
 fisher.test(table(df$Year >= 2020, df$Correct_First_ChatGPT4_Answer))
 
 barplot(t(prop.table(table(df$Year >= 2020, df$Correct_First_ChatGPT4_Answer), margin = 1))[1, ], las = 1, names.arg = c('Before 2020', '2020-2022'))
 
 
 table(subset(df, Year <= 2021)$Year >= 2020, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer)
 
 prop.table(table(subset(df, Year <= 2021)$Year >= 2020, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer), margin = 1)
 
 
 fisher.test(table(subset(df, Year <= 2021)$Year >= 2020, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer))
 
 barplot(t(100 * prop.table(table(subset(df, Year <= 2021)$Year >= 2020, subset(df, Year <= 2021)$Correct_First_ChatGPT4_Answer), margin = 1))[1, ], las = 1, names.arg = c('Before 2020', '2020-2021'), ylab = 'Incorrect Answers (%)', col = 'blue')
 
 barplot(c(4/21*100,7/21*100,10/21*100), names.arg = c('Low', 'Medium', 'High'), ylab = 'Answers (%)', xlab = 'Likelihood of Potential Harm', col = 'blue')
 
 barplot(c(1/21*100,10/21*100,10/21*100), names.arg = c('No Harm', 'Moderate', 'Death or Severe'), ylab = 'Answers (%)', xlab = 'Extent of Potential Harm', col = 'blue')
 
 dat <- data.frame(
   "GPT-3.5" = c(23, 35),
   "GPT-4" = c(13, 9),
   row.names = c("Minor", "Major"),
   stringsAsFactors = FALSE
 )
 colnames(dat) <- c("3.5", "4")
 
 dat
 colnames(dat) <- c("3.5", "4")
 fisher.test(dat)
 
 table_df <-as.data.frame(table(df$Year, df$Correct_First_ChatGPT4_Answer))
 percentage <- table_df$Freq[7:12]/(table_df$Freq[1:6]+table_df$Freq[7:12])*100
 data1 <- mutate(table_df[7:12,],percentage)
 
 upperlow <- data.frame(
   lower = c(
     binom.test(data1$percentage[1]/100*106, 106, 0.25)[["conf.int"]][1]*100,
     binom.test(data1$percentage[2]/100*11, 11, 0.25)[["conf.int"]][1]*100,
     binom.test(data1$percentage[3]/100*9, 9, 0.25)[["conf.int"]][1]*100,
     binom.test(data1$percentage[4]/100*15, 15, 0.25)[["conf.int"]][1]*100,
     binom.test(data1$percentage[5]/100*4, 4, 0.25)[["conf.int"]][1]*100,
     binom.test(data1$percentage[6]/100*2, 2, 0.25)[["conf.int"]][1]*100
   ),
   upper = c(
     binom.test(data1$percentage[1]/100*106, 106, 0.25)[["conf.int"]][2]*100,
     binom.test(data1$percentage[2]/100*11, 11, 0.25)[["conf.int"]][2]*100,
     binom.test(data1$percentage[3]/100*9, 9, 0.25)[["conf.int"]][2]*100,
     binom.test(data1$percentage[4]/100*15, 15, 0.25)[["conf.int"]][2]*100,
     binom.test(data1$percentage[5]/100*4, 4, 0.25)[["conf.int"]][2]*100,
     binom.test(data1$percentage[6]/100*2, 2, 0.25)[["conf.int"]][2]*100
   )
   
 )
 
 data2 <- mutate(data1,upperlow)
 

 
 p1 = ggplot(data2, aes(x = Var1, y = percentage)) + 
   geom_bar(stat = "identity", fill="#0073C2FF")+
   
   geom_errorbar(
     stat='identity',
     aes(ymin = lower, ymax = upper),
     position = position_dodge(width = 0.6), 
     width = 0.5,
     color = "grey",
     linewidth = 0.5
   )+
   
   theme_classic()+
   scale_y_continuous(limits=c(0, 100))+
   ylab("Correct Answers (%)") +
   xlab("Most Recent Publication Year")
p1 
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


 