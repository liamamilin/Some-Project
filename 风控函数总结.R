
# 函数总结 --------------------------------------------------------------------



# 1.平衡数据 ------------------------------------------------------------------


# the balancing technique to use (ubOver, ubUnder, ubSMOTE, ubOSS,
#ubCNN, ubENN, ubNCL, ubTomek).

# ubTomek: only numeric features are allowed to compute nearest neighbors
# 输入 ：原始特征 标签 平衡的方法
# 输出 ： 平衡后的样本
BalanceData <- function(Raw_data, Label, Type = 'ubUnder') {
  require(unbalanced)
  input <- Raw_data
  
  output <- Label
  output <- as.factor(output)
  
  print(paste("样本标签的比例为:", table(output)[1] / table(output)[2]))
  data <- ubBalance(X = input, Y = output, type = Type)
  # data<-ubCNN(X= input,Y=output)
  balancedData <- cbind(data$X, data$Y)
  names(balancedData) <- c(names(input), 'label')
  print(paste('平衡之后的标签比例为:', table(data$Y)[1] / table(data$Y)[2]))
  return(balancedData)
}



# 2.IV --------------------------------------------------------------------

# 计算IV，进行woe变换
# 输入 ： 原始数据 标签 是否进行woe变换 是否筛选变量（输入一个IV值0-1）
# 输出 ： 1.woe变换的变量 2. 筛选后的特征名字
# 8月.6日 碰到一个BUG ，结果是Error #13: Assertion failure at kmp_runtime.cpp(6480).
#  所以需要重新进行woe变换
IV_WOE <- function(Raw_data,
                   Label,
                   Woe_t = F,
                   Filter = 0) {
  Data <- cbind(Raw_data, Label)
  label <- names(Data)[ncol(Data)]
  require(Information)
  require(woe)
  require(scorecard)
  require(tidyverse)
  IV <- scorecard::iv(B_New_cod_sample[, -1], y = 'label')
  
  print(IV)
  if (Woe_t == T) {
    bins <-  scorecard::woebin(Data, y = label)
    print(bins)
    dt_woe <- scorecard::woebin_ply(Data, bins)
    return(dt_woe)
  }
  if (Filter != 0) {
    N <- IV %>% filter(info_value > Filter) %>% select(variable)
    return(N)
  }
  
  
  
}

# 3.划分数据集 -------------------------------------------------------------------

# 生成训练样本，测试样本 -------------------------------------------------------------
# 输入 ： 原始数据集 标签 划分的比例
SplitSample <- function(Raw_data, Label, rate) {
  require(caret)
  div_part_2 <-
    createDataPartition(y = Label, p = rate, list = F)
  
  # Training Sample for Neural Network
  train_num <- Raw_data[div_part_2, ] # 70% here
  train_num <- cbind(train_num, Label[div_part_2])
  
  # Test Sample for Neural Network
  test_num <-
    Raw_data[-div_part_2, ] # rest of the 30% data goes here
  test_num <- cbind(test_num, Label[-div_part_2])
  names(train_num) <- c(names(Raw_data), 'label')
  names(test_num) <- c(names(Raw_data), 'label')
  return(list(train_num, test_num))
  
}

Train_Test <-
  SplitSample(Raw_data = B_New_cod_sample_woe[,-1],
              Label = B_New_cod_sample_woe$Label,
              rate = 0.04)



# 4.训练模型 ------------------------------------------------------------------

# care 他训练模型 --------------------------------------------------------------------
  
 Caret_Model <- function(train, test, model, search = F) {
    require(caret)
    
    if (search == F) {
      print('build model---------------------------------')
      Model <- train(label ~ ., data = train, method = model)
    }
    if (search == T) {
      print('build model---------------------------------')
      Model <- train(
        label ~ .,
        data = train,
        method = model,
        trControl = trainControl(search = 'random')
      )
    }
    print('predict---------------------------------')
    dt_pred1 = predict(Model, type = 'prob', test)
    require(scorecard)
    perf_eva(test$label, dt_pred1$`1`, type = c("ks", "lift", "roc", "pr"))
    
    
    
    
  }

# 5.训练模型mlr ---------------------------------------------------------------

l <- mlr::listLearners()

Mlr_Modle <- function(Train,Test,model){
  require(mlr)
  tasktrain <- makeClassifTask(data = Train,target = "label")
  tasktest <- makeClassifTask(data = Test,target = "label")
  
  lnr <- makeLearner(cl = model,predict.type = 'prob')
  
  
  mdl <- mlr::train(lnr,tasktrain)
  prd <- predict(mdl,tasktest)
  require(scorecard)
  perf_eva(test$label, prd$data[,3], type = c("ks", "lift", "roc", "pr"))
}


