##MDA Assignment#1 Association Rule Mining
######Step1 Data transformation######
library(readr)
library(arules)
library(arulesViz)
library(wordcloud)


#1 csv파일에서Item Name에 해당하는 네 개의 변수를 불러와Institute, Course, Region, Degree에 저장합니다.  
mooc_dataset <- read.csv("big_student_clear_third_version.csv")
Institute <- mooc_dataset$institute
Course <- mooc_dataset$course_id
Region <- mooc_dataset$final_cc_cname_DI
Degree <- mooc_dataset$LoE_DI

#2 Region에 해당하는 변수에서 한 칸 공백(“ “) 및 특수문자를 제거합니다. 
Region <- gsub(" ", "", Region)
Region <- gsub("\\/", "", Region)
Region <- gsub("\\&", "", Region)
Region <- gsub("\\.", "", Region)
Region <- gsub("\\,", "", Region)

#3 네 변수를 밑줄(_)로 연결하여 RawTransactions에 저장합니다.
RawTransactions <- paste(Institute, Course, Region, Degree, sep = "_")

#4 
MOOC_transactions <- paste(mooc_dataset$userid_DI, RawTransactions)

#5 MOOC_transactions 변수를 MOOC_User_Course.csv라는 파일명으로 저장합니다. 
write.csv(MOOC_transactions, file="MOOC_User_Course.csv", row.names = FALSE)



######STEP2 데이터 불러오기 및 기초 통계량 확인######
#1 Read MOOC_User_Course.csv using read.transactions()
MOOC <- read.transactions("MOOC_User_Course.csv", format = "single", cols = c(1,2), rm.duplicates=TRUE, skip =1)
summary(MOOC)

#2 Draw Wordcloud 
itemName <- itemLabels(MOOC)
itemCount <- itemFrequency(MOOC)*nrow(MOOC)
summary(itemName)

col <- brewer.pal(6, "Set2") #Color를 Set2의 6로 지정합니다. 
wordcloud(words = itemName, freq = itemCount, min.freq = 3000, scale = c(2, 1), col = col , random.order = FALSE)

#3 Draw itemFrequencyPlot
itemFrequencyPlot(MOOC, support = 0.01, cex.names=0.8)
support1 <- itemFrequency(MOOC)
top5 <- support1[order(support1, decreasing=TRUE)[1:5],drop=FALSE]
top5  #상위 5개의 서포트값을 살펴보았습니다. 


#####STEP3 규칙 생성 및 결과 해석 ######
# Rule generation by Apriori 
rules <- apriori(MOOC, parameter=list(support=0.001, confidence=0.05))

# Check the generated rules
inspect(rules)

# List the first three rules with the highest lift values
inspect(sort(rules, by="support"))
inspect(sort(rules, by="confidence"))
inspect(sort(rules, by="lift"))

#Support ×  Confidence ×  Lift로 정의한 효용성 지표가 높은 1~3순위를 찾았습니다. 
rules@quality$support
lhs1 <- rules@lhs
rhs1 <- rules@rhs
measure <- (rules@quality$support)*(rules@quality$confidence)*(rules@quality$lift)
rules@quality <- cbind(rules@quality, measure)
inspect(rules@quality)
rules@quality
inspect(rules)
inspect(sort(rules, by="measure"))


# Plot the rules
plot(rules, method="graph",engine = "htmlwidget")
plot(rules, method="matrix",engine = "htmlwidget")

