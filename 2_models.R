# 随机森林算法
# 下载并导入必要的包
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("ranger", quietly = TRUE)) install.packages("ranger")
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")

##使用setwd()函数，设置工作目录（按照自己的位置修改）
setwd(setwd("C:/Users/25360/Desktop/Credit_card_fraud_prediction_calculator")
)
##使用getwd()函数确保工作目录设置正确
getwd()
library(caret)
library(randomForest)
library(ranger)
library(pROC)
library(rsample)
library(dplyr)
library(readxl)

# 导入数据
# 这里的fraud_data为源文件连接后的sheet
fraud_data <- read_excel("data/fraud_data.xlsx")
fraud_data

# 设置随机种子
set.seed(2024)

# 数据预处理
# 将字符型变量转换为因子型变量
char_vars <- sapply(fraud_data, is.character)
fraud_data[char_vars] <- lapply(fraud_data[char_vars], as.factor)

# 确保目标变量也是因子型，并转换因子水平为有效的R变量名
fraud_data$是否存在欺诈 <- factor(fraud_data$是否存在欺诈, levels = unique(fraud_data$是否存在欺诈))
levels(fraud_data$是否存在欺诈) <- make.names(levels(fraud_data$是否存在欺诈))

# 检查数据处理后的结构
str(fraud_data)


# 拆分数据集
# 分层抽样拆分数据集
data_rf_split <- initial_split(fraud_data, prop = 0.7, strata = "是否存在欺诈")
data_rf_train <- subset(training(data_rf_split), select = -客户号)
data_rf_test <- testing(data_rf_split)
data_rf_train
data_rf_test

# 确保训练集和测试集中包含所有因子水平
data_rf_train$是否存在欺诈 <- factor(data_rf_train$是否存在欺诈, levels = levels(fraud_data$是否存在欺诈))
data_rf_test$是否存在欺诈 <- factor(data_rf_test$是否存在欺诈, levels = levels(fraud_data$是否存在欺诈))

# 检查拆分后的数据集结构
str(data_rf_train)
str(data_rf_test)


# 预设调优参数
# 设置参数调优网格
tunegrid <- expand.grid(
  mtry = 10:20,
  splitrule = c("gini", "extratrees", "hellinger"),
  min.node.size = c(25, 45, 65)
)

# 设置训练控制参数
control <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE
)


# 训练随机森林模型
rf_model <- train(
  是否存在欺诈 ~ .,
  data = data_rf_train,
  method = "ranger",
  metric = "ROC",
  tuneGrid = tunegrid,
  trControl = control,
  importance = "impurity_corrected"
)
rf_model

# 参数调优情况
# 绘制模型调优情况图
options(repr.plot.width = 20, repr.plot.height = 8)
plot(rf_model)


# 最终模型参数
# 查看最终模型参数
final_model <- rf_model$finalModel
final_model

# 提取性能最佳的参数组合
best_params <- rf_model$bestTune
best_params



# 测试集评估
# 预测概率
rf_test <- cbind(data_rf_test, 预测概率 = predict(rf_model, data_rf_test, type = "prob")[, 2])

# 绘制ROC曲线
rf_test_roc <- roc(rf_test$是否存在欺诈, rf_test$预测概率, smooth = FALSE, auc = TRUE, main = "Confidence intervals", ci = TRUE, print.auc = TRUE)
options(repr.plot.width = 16, repr.plot.height = 10)
plot(rf_test_roc, print.auc = TRUE, print.auc.x = 0.5, print.auc.y = 0.5, auc.polygon = TRUE, max.auc.polygon = TRUE, print.thres = TRUE, print.thres.cex = 1, legacy.axes = TRUE)



# 变量重要性
# 计算变量重要性
random_forest_varimp <- caret::varImp(rf_model)
random_forest_varimp

# 绘制变量重要性
plot(random_forest_varimp, top = 16)

# 将随机森林rf_model模型保存为.rds文件
saveRDS(rf_model, file = "models/rf_model.rds")


# 决策树算法
# 数据预处理
# 下载并导入必要的包
install.packages("bitops")
install.packages("rattle")
library(readxl)
library(rpart)
library(tibble)
library(bitops)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# 导入数据
# 读取Excel文件中的第四张表
file_path <- "data/fraud_data.xlsx"
fraud_data <- read_excel(file_path, sheet = 4)
fraud_data

# 删除不必要的标识符列，如“客户号、卡号、额度、币种代码”
# 这里的fraud_data为源文件删减过后的sheet4
fraud_data$客户号 <- NULL
fraud_data$卡号 <-NULL
fraud_data$额度 <- NULL
fraud_data$币种代码 <- NULL
fraud_data

# 设置随机种子以确保结果可复现
set.seed(2024)
# 确保目标变量是否存在欺诈是因子类型
fraud_data$是否存在欺诈 <- as.factor(fraud_data$是否存在欺诈)


# 拆分数据集
# 计算拆分点
split_point <- floor(0.7 * nrow(fraud_data))
# 创建随机抽样的索引
sub <- sample(1:nrow(fraud_data), split_point)
# 创建训练集和测试集
train <- fraud_data[sub, ]
test <- fraud_data[-sub, ]
train
test

# 确保测试数据集的列名和顺序与训练数据集一致
test <- test[, colnames(train)]

# 确认是否存在欺诈列在测试数据集中
print("Test dataset columns:")
print(colnames(test))
print("Train dataset columns:")
print(colnames(train))


# 构建决策树模型
model <- rpart(是否存在欺诈~ ., data = train, control = rpart.control(cp = 0.001))

# 输出复杂度参数表
printcp(model)


# 进行剪枝
pruned_tree <- prune(model,cp=0.005)

# 画出剪枝后的模型树
prp(pruned_tree,
    faclen = 0,  # 使用完整标签名称
    extra = 1,  # 显示每个终端节点数量
    roundint = TRUE,  # 输出数值近似为整数
    digits = 5  # 输出显示小数位数5位
)



# 进行评估和预测
pred <- predict(pruned_tree, test, type = "class")
table(test$是否存在欺诈,pred)

# 将决策树模型pruned_tree保存为.rds文件
saveRDS(pruned_tree, file = "models/pruned_tree.rds")


