# 机器学习
library(mlr3)

task = tsk("iris")
learner = lrn("classif.rpart")

# 为任务的一个子集（前120行）训练这个学习器的模型
learner$train(task, row_ids = 1:120)
# 决策树模型
learner$model
# 进行预测
predictions = learner$predict(task, row_ids = 121:150)
predictions


# 创建任务
# https://shixiangwang.github.io/blog/mlr3-basics/
data("mtcars", package = "datasets")
df = mtcars[, 1:3]
task_mtcars = as_task_regr(df, target = "mpg", id = "cars")
print(task_mtcars)

library(mlr3viz)
library(GGally)
autoplot(task_mtcars, type = "pairs")

mlr_tasks
as.data.table(mlr_tasks)


task_penguins = tsk("penguins")
print(task_penguins)
as.data.table(mlr_tasks)[, 1:4]


tsks()
lrns()

dat = tsk("german_credit")$data()
task = as_task_classif(dat, target = "credit_risk")
task

task$select(cols = setdiff(task$feature_names, "telephone"))
task

set.seed(1)
split = partition(task, ratio = 0.7)
split$train
split$test

library(ranger)
library(mlr3extralearners)
learner = lrn("classif.rpart", num.trees = 100,
              predict_type = "prob")
learner
learner$train(task, row_ids = split$train)
learner$model
prediction = learner$predict(task, row_ids = split$test)
prediction


#==============================================
library(ggplot2)

head(iris)
pairs(iris, col = iris$Species)


############################


# 逻辑回归
dat<-iris
dat<-dat[51:150,]
dat$Species<-ifelse(dat$Species=="virginica",1,0)
log_model<-glm(Species~.,family=binomial(link='logit'),data=dat)
summary(log_model)

# 逐步回归
log_model2<-step(log_model)
summary(log_model2)

# 混淆矩阵
pred<-predict(log_model2)
prob<-exp(pred)/(1+exp(pred))
yhat<-1*(prob>0.5)
table(dat$Species,yhat)


#==============================================

# 决策树

library(rpart)
library(visNetwork)
library(sparkline)
library(ggpol)

dtree_model <- rpart(Species~., data = iris, control = rpart.control(cp = 0.019))
visTree(dtree_model)

# 预测
dtree_pred <- predict(dtree_model,iris,type = "class")

## 混淆矩阵
ggplot() + geom_confmat(aes(x = iris$Species, y = dtree_pred),
                        normalize = TRUE, text.perc = TRUE)+
  labs(x = "Reference",y = "Prediction")+
  scale_fill_gradient2(low="darkblue", high="lightgreen")


#====================================================
# bayes
library(klaR)

nb_model <- NaiveBayes(Species ~ ., iris ,na.action = na.pass)
nb_pred <- predict(nb_model,iris)

par(mfrow=c(1,1))
plot(nb_model)

ggplot() + geom_confmat(aes(x = iris$Species, y = nb_pred$class),
                        normalize = TRUE, text.perc = TRUE)+
  labs(x = "Reference",y = "Prediction")+
  scale_fill_gradient2(low="darkblue", high="lightgreen")


#=====================================================
# xgboost

library("xgboost")
library("Matrix")

y <- as.numeric(iris$Species) - 1
x <- as.matrix(iris[,-5])
xgb_model<-xgboost(data=x,label=y,max_depth=4,eta=0.5,
             nthread=2,nrounds=10,subsample=0.7,objective="multi:softprob",
             num_class=3)
# 变量重要性
xgb_imp<-xgb.importance(model=xgb_model)
xgb.plot.importance(xgb_imp,rel_to_first = TRUE)
# 预测
xgb_pred<-matrix(predict(xgb_model,x),ncol=3,byrow = TRUE)
xgb_pred<-data.frame(xgb_pred)
xgb_pred$result<-apply(xgb_pred,1,which.max)
xgb_pred$label<-levels(iris$Species)[xgb_pred$result]
# 评估
ggplot() + geom_confmat(aes(x = iris$Species, y = xgb_pred$label),
                        normalize = TRUE, text.perc = TRUE)+
  labs(x = "Reference",y = "Prediction")+
  scale_fill_gradient2(low="darkblue", high="lightgreen")


#=========================================================
# 线性回归
# Ozone臭氧 Solar.R太阳辐射 Wind风 Temp温度

dat<-na.omit(airquality)
lm_model<-lm(Temp~Ozone+Solar.R+Wind,data=dat)
summary(lm_model)
par(mfrow=c(2,2))
plot(lm_model)

#lm_model2<-step(lm_model)

lm_pred<-predict(lm_model,interval="prediction")
mdf<-cbind(dat[,1:4],lm_pred)	 

ggplot() + 
  geom_line(data=mdf[,c("Ozone","Temp")], aes(x=Ozone, y=Temp), color='steelblue') + 
  geom_line(data=mdf[,c("Ozone","fit")], aes(x=Ozone, y=fit), color='coral2')+
  geom_line(data=mdf[,c("Ozone","lwr")], aes(x=Ozone, y=lwr), color='green') +
  geom_line(data=mdf[,c("Ozone","upr")], aes(x=Ozone, y=upr), color='green')

# 线性回归
ggplot(mdf[,c("Ozone","Temp")], aes(Ozone, Temp)) +
  geom_point() +
  geom_smooth(method = lm)

# 三次样条回归
ggplot(mdf[,c("Ozone","Temp")], aes(Ozone, Temp)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE)

#===============================
# kmeans

library(GGally)
res <- kmeans(iris[,1:4], centers=3)
ggpairs(iris,columns = 1:5,
        mapping=aes(colour=as.character(res$cluster)))

library(flexclust)
clk2 <- cclust(iris[,-5], k=3);clk2
barchart(clk2,legend=TRUE)


#===================================
# KNN

library(dbscan)
nn <- kNN(iris[,-5], k=5)
nn

# 打印与33号最近的5个点
idx<-33
cols = ifelse(1:nrow(iris[,-5]) %in% nn$id[idx,],"red", "black")
cols[idx]<-'blue'
plot(iris[,-5],pch = 19, col = cols)

plot(nn, iris[,-5])


# dbscan

res <- dbscan(iris[,-5], eps = 0.5, minPts = 5)
res
pairs(iris, col = res$cluster + 1L)

data("moons")
head(moons)
cl <- hdbscan(moons, minPts = 5)
cl
plot(moons, col=cl$cluster+1, pch=20)



#=====================================
# PCA


# SVD计算特征值
pca1 <- prcomp (iris[,-5],center = TRUE,scale. = TRUE) 
biplot(pca1)
summary(pca1)

xlab1 <- paste0("PC1(",round(summ1$importance[2,1]*100,2),"%)")
ylab1 <- paste0("PC2(",round(summ1$importance[2,2]*100,2),"%)")

df1 <- data.frame(pca1$x)

ggplot(data = df1,aes(x = PC1,y = PC2,color = iris$Species))+
stat_ellipse(aes(fill = iris$Species),
             type = "norm",geom = "polygon",alpha = 0.25,color = NA)+ # 添加置信椭圆
geom_point(size = 3.5)+
labs(x = xlab1,y = ylab1,color = "Condition",title = "PCA Scores Plot")+
guides(fill = "none")+
theme_bw()+
scale_fill_manual(values = c("purple","orange","pink"))+
scale_colour_manual(values = c("purple","orange","pink"))+
theme(plot.title = element_text(hjust = 0.5,size = 15),
      axis.text = element_text(size = 11),axis.title = element_text(size = 13),
      legend.text = element_text(size = 11),legend.title = element_text(size = 13),
      plot.margin = unit(c(0.4,0.4,0.4,0.4),'cm'))


# 协方差矩阵计算特征值
dat<-na.omit(airquality)
pca2 <- princomp (dat) 
pca2$loadings
summary(pca2)

biplot(pca2)
screeplot(pca2)


#==========================================
#mlr3
library(mlr3)
library(ggplot2)
library(mlr3measures)
library(mlr3extralearners)
library(mlr3verse)

# 设计模型任务
task_penguins = as_task_classif(species ~ ., data = palmerpenguins::penguins)
task_penguins
# 选择学习器
learner = lrn("classif.rpart", cp = .01)
# 划分测试集/训练集
split = partition(task_penguins, ratio = 0.67)
# 训练
learner$train(task_penguins, split$train_set)
# 预测
prediction = learner$predict(task_penguins, split$test_set)
# 混淆矩阵
prediction$confusion
# 模型评价
measure = msr("classif.acc")
prediction$score(measure)
# 5折交叉验证
resampling = rsmp("cv", folds = 5L)
rr = resample(task_penguins, learner, resampling)
rr$score(measure)[, .(task_id, learner_id, iteration, classif.acc)]
rr$aggregate(measure)


# 可以是多个任务
tasks = tsk("sonar") 
learners = lrns(c("classif.rpart", "classif.kknn",
                  "classif.ranger", "classif.svm"),
                predict_type = "prob")
design = benchmark_grid(tasks, learners,rsmps("cv", folds = 5))
bmr = benchmark(design) # 执行基准测试
bmr$aggregate(list(msr("classif.acc"), msr("classif.auc")))
autoplot(bmr, type = "roc")
autoplot(bmr, measure = msr("classif.auc"))


#===============================================
# 混淆矩阵
library(caret)
dtree_model <- rpart(Species~., data = iris, 
                     control = rpart.control(cp = 0.019))
dtree_pred <- predict(dtree_model,iris,type = "class")
table(dat$Species,yhat)
cf<-confusionMatrix(as.factor(dat$Species),as.factor(yhat));cf
fourfoldplot(as.table(cf_4),color=c("green","red"))


# table(iris$Species,nb_pred$class)
# cf<-confusionMatrix(iris$Species,nb_pred$class);cf
# cf_4<-cf$table[1:2,1:2]
# fourfoldplot(as.table(cf_4),color=c("green","red"))


#=========================================

library(pROC)

data(aSAH)

rocobj <- roc(aSAH$outcome, aSAH$s100b)
plot(rocobj,
     legacy.axes = TRUE,
     main="ROC曲线最佳阈值点",
     thresholds="best", # 基于youden指数选择roc曲线最佳阈值点
     print.thres="best") # 在roc曲线上显示最佳阈值点

ggroc(rocobj, 
      alpha = 0.5, colour = "red", 
      linetype = 2, size = 2) +
  theme_minimal() + 
  ggtitle("My ROC curve") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
               color="grey", linetype="dashed")

# ROC
library(ROCR)
pred <- prediction(dat$Species,yhat)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

data(ROCR.hiv)
manypred = prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
many.roc.perf = performance(manypred, measure = "tpr", x.measure = "fpr")
plot(many.roc.perf, col=1:10)
abline(a=0, b= 1)

#===============================================
# ROC














