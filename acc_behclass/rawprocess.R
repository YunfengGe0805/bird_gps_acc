# 1. 安装并加载必要的包
# 如果您还没有安装 dplyr，请先运行 install.packages("dplyr")
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
# 2. 读取 CSV 文件
#需要调整
M140_202210 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202210.xlsx")
M140_202211 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202211.xlsx")
M140_202212 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202212.xlsx")
M140_202301 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202301.xlsx")
M140_202302 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202302.xlsx")
M140_202303 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202303.xlsx")
M140_202304 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202304.xlsx")
M140_202305 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202305.xlsx")
M140_202306 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202306.xlsx")
M140_202307 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202307.xlsx")
M140_202308 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202308.xlsx")
M140_202309 <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202309.xlsx")
data<-as.data.frame(rbind(M140_202210,M140_202211,M140_202212,M140_202301,M140_202302,M140_202303,M140_202304,M140_202305,M140_202306,
                          M140_202307,M140_202308,M140_202309))
#需要调整
file_path <- read_excel("C:/ALAN_StreakedShearwater/acc/M140/M140_202301.xlsx")
df<-as.data.frame(file_path)
colnames(df)<-c("ID","IMEID","Time","Y","X","Z")
df <- df[, c(1,2,3,5,4,6)]
df$ID<-"SYSUL140"
df <- df %>% mutate( X = na.approx(X),
                         Y = na.approx(Y),
                         Z = na.approx(Z))
# 3. 提取时间戳的小数部分
get_decimal_part <- function(timestamp) {
  as.numeric(sub(".*\\.", "0.", timestamp)) # 提取小数点后的部分并转换为数值
}

df$decimal_part <- sapply(df$Time, get_decimal_part)
df[is.na(df)] <- 0

# 4. 创建分组 ID。 关键在于如何正确识别每个组。
# 这里假设数据是按照时间顺序排列的，并且 .9 到 .0 是一个连续的组。
# 如果数据不是严格排序的，您可能需要调整分组逻辑。

group_id <- 0
group_ids <- numeric(nrow(df)) # 创建一个和数据框行数一样长的数值向量
current_group <- c()

for (i in 1:nrow(df)) {
  current_group <- c(current_group, i)
  decimal <- df$decimal_part[i]
  if (decimal == 0) {
    group_ids[current_group] <- group_id
    group_id <- group_id + 1
    current_group <- c()
  } else if (i == nrow(df)) { # 处理最后一个不完整组
    group_ids[current_group] <- group_id # 即使不完整，也分配一个ID
    group_id <- group_id + 1
    current_group <- c()
  }
}

df$group_id <- group_ids

# 5. 统计每个组的行数
group_counts <- df %>%
  group_by(group_id) %>%
  summarize(count = n())

# 6. 找出需要删除的组 (行数小于 10 的组)
groups_to_remove <- group_counts %>%
  filter(count < 10) %>%
  pull(group_id)

# 7. 从 DataFrame 中删除这些组
df_filtered <- df %>%
  filter(!group_id %in% groups_to_remove)

# 8. 删除辅助列 'decimal_part' 和 'group_id'
df_filtered <- df_filtered %>%
  select(-decimal_part, -group_id)

# 9. 保存结果到新的 CSV 文件
output_file_path <- "C:/ALAN_StreakedShearwater/acc/M140/M140_202301filter.csv" # 替换为您想要保存结果的文件路径
write.csv(df_filtered, file = output_file_path, row.names = FALSE)

