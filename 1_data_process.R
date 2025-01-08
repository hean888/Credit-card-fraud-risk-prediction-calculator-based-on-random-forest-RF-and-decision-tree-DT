# 下载并安装包
install.packages("parallelly")
install.packages("parallelly", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")    # 导入Excel文件
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")    # 数据处理和可视化
if (!requireNamespace("gmodels", quietly = TRUE)) install.packages("gmodels")    # 统计建模
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")    # 绘制图形
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")    # 组合图形
if (!requireNamespace("tidymodels", quietly = TRUE)) install.packages("tidymodels")    # 整洁建模
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")    # 统计分析
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")    # 绘制ROC曲线


# 载入库
library(readxl)    # 导入Excel文件
library(tidyverse)    # 数据处理和可视化
library(gmodels)    # 统计建模
library(ggplot2)    # 绘制图形
library(cowplot)    # 组合图形
library(tidymodels)    # 整洁建模
library(caret)    # 统计分析
library(pROC)    # 绘制ROC曲线

##使用setwd()函数，将工作目录设置为C:/Users/69509/Desktop
#setwd("F:/RecentFiles/SBDcourses/Third Term/huatai/ShinyApp")
##使用getwd()函数确保工作目录设置正确
getwd()

# 读取数据表
## 读取第一个工作表：客户信用记录
Customer_credit_history <- read_xlsx("data/data.xlsx", sheet = 1)
## 读取第二个工作表：申请客户信息
Request_customer_information <- read_xlsx("data/data.xlsx", sheet = 2)
## 读取第三个工作表：拖欠历史记录
Delinquency_history <- read_xlsx("data/data.xlsx", sheet = 3)
## 读取第四个工作表：消费历史记录
Consumption_history <- read_xlsx("data/data.xlsx", sheet = 4)

# 查看每张表的结构和摘要信息
## 查看客户信用记录表的结构和摘要
str(Customer_credit_history)
## 查看申请客户信息表的结构和摘要
str(Request_customer_information)
## 查看拖欠历史记录表的结构和摘要
str(Delinquency_history)
## 查看消费历史记录表的结构和摘要
str(Consumption_history)

# 检查 Customer_credit_history 表的客户号唯一性
unique_customers <- unique(Customer_credit_history[["客户号"]])
is_unique <- length(unique_customers) == nrow(Customer_credit_history)
print(paste("Customer_credit_history 的客户号列是否为主键:", is_unique))

# 检查 Request_customer_information 表的主键
unique_requests <- unique(Request_customer_information[["客户号"]])
is_unique <- length(unique_requests) == nrow(Request_customer_information)
print(paste("Request_customer_information 的客户号列是否为主键:", is_unique))

# 检查 Delinquency_history 表的主键
unique_delinquency <- unique(Delinquency_history[["客户号"]])
is_unique <- length(unique_delinquency) == nrow(Delinquency_history)
print(paste("Delinquency_history 的客户号列是否为主键:", is_unique))

# 检查 Consumption_history 表的主键
unique_consumption <- unique(Consumption_history[["客户号"]])
is_unique <- length(unique_consumption) == nrow(Consumption_history)
print(paste("Consumption_history 的客户号列是否为主键:", is_unique))

# 多表关联
# 左连接 Consumption_history 和 Customer_credit_history，并选择特定列
temp_1 <- merge(Consumption_history,
                subset(Customer_credit_history, select = c('客户号', '性别', '年龄_连续',
                                                           '婚姻状态', '户籍', '教育程度',
                                                           '居住类型', '职业类别',
                                                           '工作年限', '保险缴纳', '车辆情况',
                                                           '信用总评分', '信用等级')),
                by.x = "客户号",
                by.y = "客户号",
                all.x = TRUE,
                all.y = FALSE)
# 将 temp_1 与 Request_customer_information 左连接，并选择特定列
fraud_data <- merge(temp_1,
                    subset(Request_customer_information, select = c('客户号', '信贷情况')),
                    by.x = "客户号",
                    by.y = "客户号",
                    all.x = TRUE,
                    all.y = FALSE)
# 打印关联后的数据
print(head(fraud_data))

## 查看关联表的结构和摘要
str(fraud_data)

# 加载必要的包
if (!require("caTools")) install.packages("caTools")
library(caTools)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

#数据预处理
# 删除卡号和币种代码这两个变量
fraud_data[["卡号"]] <- NULL
fraud_data[["币种代码"]] <- NULL
# 查看数据框，确保变量已被删除
print(fraud_data)

# 检查缺失值
missing_values <- sapply(fraud_data, function(x) sum(is.na(x)))
print(missing_values)

# 检查重复行
sum(duplicated(fraud_data))
# 删除重复行
fraud_data <- distinct(fraud_data)

# 转换分类变量为因子
cat_vars <- c("卡类别", "性别", "婚姻状态", "户籍", "教育程度", "居住类型", "职业类别", "保险缴纳", "车辆情况", "信用等级", "信贷情况")
print(cat_vars)
fraud_data[cat_vars] <- lapply(fraud_data[cat_vars], factor)

# 计算欺诈客户数量
fraud_count <- table(fraud_data$是否存在欺诈)["1"]
print(fraud_count)
# 计算总客户数量
total_customers <- nrow(fraud_data)
# 计算欺诈客户占比
fraud_percentage <- fraud_count / total_customers
# 输出结果
fraud_percentage

install.packages("openxlsx")
# 加载 openxlsx 包
library(openxlsx)
# 保存数据到 fraud_data.xlsx 文件
write.xlsx(fraud_data, file = "fraud_data.xlsx")

# 提示保存成功
cat("数据已成功保存到 fraud_data.xlsx 文件。\n")
#可视化
# 第一个脚本：不同性别的样本数量
card_type1 <- fraud_data %>%
  count(性别) %>%                       # 计算每种性别的样本数量
  rename(样本数量 = n)                   # 重命名计数列
card_type1_plot <- ggplot(card_type1, aes(x = 性别, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同卡类别的欺诈比例
card_type2 <- fraud_data %>%
  group_by(卡类别) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种卡类别的欺诈比例
card_type2_plot <- ggplot(card_type2, aes(x = 卡类别, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(card_type1_plot, card_type2_plot, ncol = 2)

# 第一个脚本：不同性别的样本数量
gender_type1 <- fraud_data %>%
  count(性别) %>%                       # 计算每种性别的样本数量
  rename(样本数量 = n)                   # 重命名计数列
gender_type1_plot <- ggplot(gender_type1, aes(x = 性别, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同性别欺诈比例
gender_type2 <- fraud_data %>%
  group_by(性别) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种性别的欺诈比例
gender_type2_plot <- ggplot(gender_type2, aes(x = 性别, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(gender_type1_plot, gender_type2_plot, ncol = 2)

# 第一个脚本：不同婚姻状态的样本数量
marital_status_type1 <- fraud_data %>%
  count(婚姻状态) %>%                       # 计算每种婚姻状态的样本数量
  rename(样本数量 = n)                   # 重命名计数列
marital_status_type1_plot <- ggplot(marital_status_type1, aes(x = 婚姻状态, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同婚姻状态的欺诈比例
marital_status_type2 <- fraud_data %>%
  group_by(婚姻状态) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种婚姻状态的欺诈比例
marital_status_type2_plot <- ggplot(marital_status_type2, aes(x = 婚姻状态, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(marital_status_type1_plot, marital_status_type2_plot, ncol = 2)

# 第一个脚本：不同教育程度的样本数量
education_level_type1 <- fraud_data %>%
  count(教育程度) %>%                       # 计算每种教育程度的样本数量
  rename(样本数量 = n)                   # 重命名计数列
education_level_type1_plot <- ggplot(education_level_type1, aes(x = 教育程度, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同教育程度的欺诈比例
education_level_type2 <- fraud_data %>%
  group_by(教育程度) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种教育程度的欺诈比例
education_level_type2_plot <- ggplot(education_level_type2, aes(x = 教育程度, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(education_level_type1_plot, education_level_type2_plot, ncol = 2)

### 第一个脚本：不同居住类型的样本数量
credit_rating_type1 <- fraud_data %>%
  count(居住类型) %>%                       # 计算每种居住类型的样本数量
  rename(样本数量 = n)                   # 重命名计数列
credit_rating_type1_plot <- ggplot(credit_rating_type1, aes(x = 居住类型, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
### 第二个脚本：不同居住类型的欺诈比例
credit_rating_type2 <- fraud_data %>%
  group_by(居住类型) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种居住类型的欺诈比例
credit_rating_type2_plot <- ggplot(credit_rating_type2, aes(x = 居住类型, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
### 设置绘图大小并绘制网格图
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(credit_rating_type1_plot, credit_rating_type2_plot, ncol = 2)

# 第一个脚本：不同职业类别的样本数量
occupation_type1 <- fraud_data %>%
  count(职业类别) %>%                       # 计算每种职业类别的样本数量
  rename(样本数量 = n)                   # 重命名计数列
occupation_type1_plot <- ggplot(occupation_type1, aes(x = 职业类别, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同职业类别的欺诈比例
occupation_type2 <- fraud_data %>%
  group_by(职业类别) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种职业类别的欺诈比例
occupation_type2_plot <- ggplot(occupation_type2, aes(x = 职业类别, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(occupation_type1_plot, occupation_type2_plot, ncol = 2)

# 第一个脚本：不同保险缴纳情况的样本数量
insurance_payment_type1 <- fraud_data %>%
  count(保险缴纳) %>%                       # 计算每种保险缴纳情况的样本数量
  rename(样本数量 = n)                   # 重命名计数列
insurance_payment_type1_plot <- ggplot(insurance_payment_type1, aes(x = 保险缴纳, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同保险缴纳情况的欺诈比例
insurance_payment_type2 <- fraud_data %>%
  group_by(保险缴纳) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种保险缴纳情况的欺诈比例
insurance_payment_type2_plot <- ggplot(insurance_payment_type2, aes(x = 保险缴纳, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(insurance_payment_type1_plot, insurance_payment_type2_plot, ncol = 2)

# 第一个脚本：不同车辆情况的样本数量
vehicle_condition_type1 <- fraud_data %>%
  count(车辆情况) %>%                       # 计算每种车辆情况的样本数量
  rename(样本数量 = n)                   # 重命名计数列
vehicle_condition_type1_plot <- ggplot(vehicle_condition_type1, aes(x = 车辆情况, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同车辆情况的欺诈比例
vehicle_condition_type2 <- fraud_data %>%
  group_by(车辆情况) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种车辆情况的欺诈比例
vehicle_condition_type2_plot <- ggplot(vehicle_condition_type2, aes(x = 车辆情况, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(vehicle_condition_type1_plot, vehicle_condition_type2_plot, ncol = 2)

# 第一个脚本：不同信用等级的样本数量
credit_rating_type1 <- fraud_data %>%
  count(信用等级) %>%                       # 计算每种信用等级的样本数量
  rename(样本数量 = n)                   # 重命名计数列
credit_rating_type1_plot <- ggplot(credit_rating_type1, aes(x = 信用等级, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 第二个脚本：不同信用等级的欺诈比例
credit_rating_type2 <- fraud_data %>%
  group_by(信用等级) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种信用等级的欺诈比例
credit_rating_type2_plot <- ggplot(credit_rating_type2, aes(x = 信用等级, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
# 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
# 绘制网格图
plot_grid(credit_rating_type1_plot, credit_rating_type2_plot, ncol = 2)

#第一个脚本：不同贷款情况的样本数量
credit_rating_type1 <- fraud_data %>%
  count(信贷情况) %>%                       # 计算每种贷款情况的样本数量
  rename(样本数量 = n)                   # 重命名计数列
credit_rating_type1_plot <- ggplot(credit_rating_type1, aes(x = 信贷情况, y = 样本数量, label = 样本数量)) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("左：样本数量") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
#第二个脚本：不同贷款情况的欺诈比例
credit_rating_type2 <- fraud_data %>%
  group_by(信贷情况) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种贷款情况的欺诈比例
credit_rating_type2_plot <- ggplot(credit_rating_type2, aes(x = 信贷情况, y = 欺诈比例, label = scales::percent(欺诈比例))) +
  geom_col() +
  geom_text(size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
  ggtitle("右：欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 20), plot.title = element_text(face = "bold"))
### 设置绘图大小
options(repr.plot.width = 16, repr.plot.height = 10)
### 绘制网格图
plot_grid(credit_rating_type1_plot, credit_rating_type2_plot, ncol = 2)

### 不同户籍类型的欺诈比例

credit_rating_type2 <- fraud_data %>%
  group_by(户籍) %>%
  summarize(欺诈比例 = mean(是否存在欺诈))  # 计算每种户籍类型的欺诈比例

credit_rating_type2_plot <- ggplot(credit_rating_type2, aes(x =欺诈比例 , y = 户籍, label = scales::percent(欺诈比例))) +
  geom_col(position = "identity", orientation = "y") +  # 设置竖直显示
  geom_text(size = 3, vjust = -0.25) +
  ggtitle("户籍类型的欺诈比例") +
  theme_bw() +
  theme(text = element_text(size = 10), plot.title = element_text(face = "bold"))
### 设置绘图大小并绘制图形
# 设置绘图大小
options(repr.plot.width = 8, repr.plot.height = 6)
# 绘制图形
credit_rating_type2_plot

# 将'是否存在欺诈'列转换为因子类型
fraud_data$是否存在欺诈 <- as.factor(fraud_data$是否存在欺诈)
library(ggplot2)

#创建柱状图：额度
ggplot(fraud_data, aes(y=额度)) +
  geom_bar(aes(fill=是否存在欺诈), position = "dodge") + # 分开不同类别的柱子
  theme(legend.position = "right") + # 将图例放在右侧
  labs(title = "额度分布", y = "额度", fill = "是否存在欺诈") + # 添加标题和标签
  theme_minimal() # 使用简洁的主题

## 创建散点图：消费金额和消费次数
ggplot(fraud_data, aes(x=日均消费金额, y=日均次数, colour=是否存在欺诈)) +
  geom_point(alpha = 0.7, size = 3) + # 设置点的透明度和大小
  scale_colour_manual(values = c("red", "blue")) + # 自定义颜色
  theme_minimal() + # 使用简洁的主题
  labs(title = "日均消费金额 vs 日均次数", x = "日均消费金额", y = "日均次数", colour = "是否存在欺诈") + # 添加标题和标签
  theme(legend.position = "right") + # 将图例放在右侧
  theme(plot.title = element_text(hjust = 0.5)) # 将标题居中

# 创建密度图：单笔消费最小、最大金额
# 创建第一个图
plot1 <- ggplot(fraud_data, aes(x = 单笔消费最小金额)) +
  geom_density(aes(fill = 是否存在欺诈), alpha = 0.25) +
  theme(legend.position = "bottom") # 将图例移到底部
# 创建第二个图
plot2 <- ggplot(fraud_data, aes(x = 单笔消费最大金额)) +
  geom_density(aes(fill = 是否存在欺诈), alpha = 0.25) +
  theme(legend.position = "bottom") # 同样将图例移到底部
# 使用 grid.arrange() 来排列两个图
grid.arrange(plot1, plot2, ncol = 2)

# 创建箱线图：个人收入分布
# 移除个人收入中非有限值的行
cleaned_data <- fraud_data %>%
  filter(is.finite(个人收入_连续))
# 然后使用 cleaned_data 进行绘图
ggplot(cleaned_data, aes(x=是否存在欺诈, y=个人收入_连续)) +
  geom_boxplot(aes(colour=是否存在欺诈)) +
  scale_colour_manual(values = c("red", "blue")) +
  ylim(0, 3000000) +
  theme_minimal() +
  labs(title = "个人收入分布", x = "是否存在欺诈", y = "个人收入", colour = "是否存在欺诈") +
  theme(plot.title = element_text(hjust = 0.5))


# 工作年限分布直方图
work_years_hist <- ggplot(fraud_data, aes(x = 工作年限)) +
  geom_histogram(binwidth = 5, color = "black", fill = "pink") +
  labs(title = "工作年限分布直方图",
       x = "工作年限",
       y = "频数")

# 欺诈与非欺诈客户工作年限分布箱型图
work_years_box <- ggplot(fraud_data, aes(x = 是否存在欺诈, y = 工作年限)) +
  geom_boxplot(color = c("red", "blue")) +
  labs(title = "欺诈与非欺诈客户工作年限分布箱型图",
       x = "是否存在欺诈",
       y = "工作年限")

# 信用总评分分布直方图
credit_score_hist <- ggplot(fraud_data, aes(x = 信用总评分)) +
  geom_histogram(binwidth = 10, color = "black", fill = "pink") +
  labs(title = "信用总评分分布直方图",
       x = "信用总评分",
       y = "频数")

# 欺诈与非欺诈客户信用总评分分布箱型图
credit_score_box <- ggplot(fraud_data, aes(x = 是否存在欺诈, y = 信用总评分)) +
  geom_boxplot(color = c("red", "blue")) +
  labs(title = "欺诈与非欺诈客户信用总评分分布箱型图",
       x = "是否存在欺诈",
       y = "信用总评分")

# 使用 gridExtra 包来组合四个图形
library(gridExtra)
grid.arrange(work_years_hist, credit_score_hist, work_years_box, credit_score_box, ncol = 2)
