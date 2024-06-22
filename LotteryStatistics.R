#install.packages("rvest")
#install.packages("dplyr")
#install.packages("ggplot2")

library(rvest)
library(dplyr)
library(ggplot2)

get_lotto_numbers <- function(n) {
  # URL 구성
  url <- paste0("https://dhlottery.co.kr/gameResult.do?method=byWin&drwNo=", n)
  
  webpage <- read_html(url)
  
  # 당첨 번호 추출
  lotto_numbers <- webpage %>%
    html_nodes(css = '.ball_645') %>%
    html_text()
  
  # 데이터 정리
  lotto_numbers <- as.numeric(lotto_numbers)
  main_numbers <- lotto_numbers[1:6]
  
  # 결과 반환
  main_numbers
}

lotto_data <- data.frame(
  Number = integer(),
  Draw = factor()
)

# 1회차부터 5회차까지
for (i in 1:5) {
  Sys.sleep(2)  # 2초 지연
  numbers <- get_lotto_numbers(i)
  
  temp_df <- data.frame(
    Number = numbers,
    Draw = as.factor(rep(i, length(numbers)))
  )
  
  lotto_data <- rbind(lotto_data, temp_df)
}

# 시각화
number_freq <- lotto_data %>% 
  group_by(Number) %>% 
  summarise(Frequency = n())

all_numbers <- data.frame(Number = 1:45)

numbers <- merge(all_numbers, number_freq, by = "Number", all.x = TRUE)
numbers[is.na(numbers$Frequency), "Frequency"] <- 0

# 결과 확인
ggplot(numbers, aes(x = as.factor(Number), y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Numbers 1 to 45",
       x = "Number",
       y = "Frequency") +
  theme_minimal()

