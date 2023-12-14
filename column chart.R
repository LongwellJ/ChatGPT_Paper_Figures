# Load necessary package
library(ggplot2)
library(dplyr)
library(stringr)
set.seed(123)
#ChatGPT_JCO_Supplemental_RG_Working_1_ <- read_excel("C:/Users/longw/OneDrive/Desktop/Chatgpt/ChatGPT JCO Supplemental RG Working.xlsx")
ChatGPT_JCO_Supplemental_RG_Working_1_ <-read_excel("C:/Users/longw/Downloads/chatgpt_medonc_exam_supplementary_12oct2023 IH reviewed.xlsx")


chartdf <- data.frame(
  test_name = ChatGPT_JCO_Supplemental_RG_Working_1_$Test_name,
  correct = ChatGPT_JCO_Supplemental_RG_Working_1_$Correct_First_ChatGPT4_Answer
)

sorteddata<- chartdf[order(chartdf$test_name),]
secondsorted <- sorteddata
secondsorted$test_name <- "total"
finaldf <- rbind(sorteddata,secondsorted)

asco <- finaldf %>% filter(str_detect(test_name, fixed("ASCO")))
esmo <- finaldf %>% filter(str_detect(test_name, fixed("ESMO")))
original <- finaldf %>% filter(str_detect(test_name, fixed("Original")))
total <- finaldf %>% filter(str_detect(test_name, fixed("total")))

gpt4percent <- c(sum(asco$correct)/nrow(asco),sum(esmo$correct)/nrow(esmo),sum(original$correct)/nrow(original),sum(total$correct)/nrow(total))

chartdf <- data.frame(
  test_name = ChatGPT_JCO_Supplemental_RG_Working_1_$Test_name,
  correct = ChatGPT_JCO_Supplemental_RG_Working_1_$Correct_First_ChatGPT35_Answer
)
sorteddata<- chartdf[order(chartdf$test_name),]
secondsorted <- sorteddata
secondsorted$test_name <- "total"
finaldf <- rbind(sorteddata,secondsorted)

asco <- finaldf %>% filter(str_detect(test_name, fixed("ASCO")))
esmo <- finaldf %>% filter(str_detect(test_name, fixed("ESMO")))
original <- finaldf %>% filter(str_detect(test_name, fixed("Original")))
total <- finaldf %>% filter(str_detect(test_name, fixed("total")))

gpt35percent <- c(sum(asco$correct)/nrow(asco),sum(esmo$correct)/nrow(esmo),sum(original$correct)/nrow(original),sum(total$correct)/nrow(total))



# data for upper and lower CI 

binomquantilefuntion <- function(x,n,p){

  props <- replicate(x, {
    sample_data <- rbinom(n, 1, p)
    sample_prop <- sum(sample_data) / n
    sample_prop
  })
  
  interval <- quantile(props, c(0.025, 0.975))
  interval
  
}

binomquantilefuntion(10000,52,0.247)


rc <- data.frame(
  model = c("a","a","a","a"),
  test_name = c("ASCO", "ESMO", "Original", "Total"),
  percent = c(24.7, 20.1, 24, 22.9),
  lower = c(
            binomquantilefuntion(10000,52,0.247)[1]*100,
            binomquantilefuntion(10000,75,0.201)[1]*100,
            binomquantilefuntion(10000,20,0.24)[1]*100,
            binomquantilefuntion(10000,147,0.229)[1]*100
            ),
  
  
  
  upper = c(
            binomquantilefuntion(10000,52,0.247)[2]*100,
            binomquantilefuntion(10000,75,0.201)[2]*100,
            binomquantilefuntion(10000,20,0.24)[2]*100,
            binomquantilefuntion(10000,147,0.229)[2]*100
            )
)


gpt35 <- data.frame(
  model = c("GPT-3.5","GPT-3.5","GPT-3.5","GPT-3.5"),
  test_name = c("ASCO", "ESMO", "Original", "Total"),
  percent = gpt35percent*100,
  lower = c(
            binom.test(gpt35percent[1]*52, 52, 0.247)[["conf.int"]][1]*100,
            binom.test(gpt35percent[2]*75, 75, 0.201)[["conf.int"]][1]*100,
            binom.test(gpt35percent[3]*20, 20, 0.24)[["conf.int"]][1]*100,
            binom.test(gpt35percent[4]*147, 147, 0.229)[["conf.int"]][1]*100
            ),
  
  upper = c(
            binom.test(gpt35percent[1]*52, 52, 0.247)[["conf.int"]][2]*100,
            binom.test(gpt35percent[2]*75, 75, 0.201)[["conf.int"]][2]*100,
            binom.test(gpt35percent[3]*20, 20, 0.24)[["conf.int"]][2]*100,
            binom.test(gpt35percent[4]*147, 147, 0.229)[["conf.int"]][2]*100
            )
)

gpt4 <- data.frame(
  model = c("GPT-4","GPT-4","GPT-4","GPT-4"),
  test_name = c("ASCO", "ESMO", "Original", "Total"),
  percent = gpt4percent*100,
  lower = c(
            binom.test(gpt4percent[1]*52, 52, 0.247)[["conf.int"]][1]*100,
            binom.test(gpt4percent[2]*75, 75, 0.201)[["conf.int"]][1]*100,
            binom.test(gpt4percent[3]*20, 20, 0.24)[["conf.int"]][1]*100,
            binom.test(gpt4percent[4]*147, 147, 0.229)[["conf.int"]][1]*100
            ),
  upper = c(
            binom.test(gpt4percent[1]*52, 52, 0.247)[["conf.int"]][2]*100,
            binom.test(gpt4percent[2]*75, 75, 0.201)[["conf.int"]][2]*100,
            binom.test(gpt4percent[3]*20, 20, 0.24)[["conf.int"]][2]*100,
            binom.test(gpt4percent[4]*147, 147, 0.229)[["conf.int"]][2]*100
            )
  
)



fulldf<-rbind(rc, gpt35, gpt4)

p <- ggplot(fulldf, aes(x = test_name, y = percent, group = model)) +
  geom_bar(
          stat = "identity",
          aes(color = model, fill = model),
          position = position_dodge(width = 0.6),
          width = 0.6
          ) +
  geom_errorbar(
          stat='identity',
          aes(ymin = lower, ymax = upper),
          position = position_dodge(width = 0.6), 
          width = 0.5,
          color = "grey",
          linewidth = 0.5
  )+
  ylab("Correct Answers (%)") +
  xlab("")+ 
  scale_color_manual(values = c("red","#0073C2FF", "#EFC000FF"))+
  theme_classic()+
  labs(fill = "") +
  scale_fill_manual(values = c("red","#0073C2FF", "#EFC000FF"), labels=c("Random chance","GPT-3.5","GPT-4"))

p

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

