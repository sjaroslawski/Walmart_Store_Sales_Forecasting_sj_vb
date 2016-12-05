s_dt_m <- matrix(nrow = length(unique(train_sd$Store)), ncol = length(unique(train_sd$dt)))
s_dt <- data.frame(s_dt_m)
row.names(s_dt) <- unique(train_sd$Store)
colnames(s_dt) <- unique(train_sd$dt)

for (i in 1:length(train_sd$Store)) {
  s_dt[train_sd$Store[i], toString(train_sd$dt[i])] <- train_sd$Weekly_Sales[i]
}

write.csv(s_dt, "s_dt.csv")

d_st_m <- matrix(nrow = 99, ncol = length(unique(train_sd$st)))
d_st <- data.frame(d_st_m)
row.names(d_st) <- 1:99
colnames(d_st) <- unique(train_sd$st)

for (i in 1:length(train_sd$Store)) {
  d_st[train_sd$Dept[i], toString(train_sd$st[i])] <- train_sd$Weekly_Sales[i]
}

write.csv(d_st, "d_st.csv")

t_sd_m <- matrix(nrow = length(unique(train_sd$weeknum)), ncol = length(unique(train_sd$sd)))
t_sd <- data.frame(t_sd_m)
row.names(t_sd) <- 2:53
colnames(t_sd) <- unique(train_sd$sd)

counter_m <- matrix(rep(0,172276), nrow = length(unique(train_sd$weeknum)), ncol = length(unique(train_sd$sd)))
counter <- data.frame(counter_m)
row.names(counter) <- 2:53
colnames(counter) <- unique(train_sd$sd)

for (i in 1:length(train_sd$Store)) {
  if (!is.na(t_sd[train_sd$weeknum[i]-1, toString(train_sd$sd[i])])) {
    counter[train_sd$weeknum[i]-1, toString(train_sd$sd[i])] <- counter[train_sd$weeknum[i]-1, toString(train_sd$sd[i])] + 1
    t_sd[train_sd$weeknum[i]-1, toString(train_sd$sd[i])] <- train_sd$Weekly_Sales[i]
  }
  else {
    counter[train_sd$weeknum[i]-1, toString(train_sd$sd[i])] <- counter[train_sd$weeknum[i]-1, toString(train_sd$sd[i])] + 1
    t_sd[train_sd$weeknum[i]-1, toString(train_sd$sd[i])] <- (train_sd$Weekly_Sales[i]*(counter[train_sd$weeknum[i]-1, toString(train_sd$sd[i])]-1) + train_sd$Weekly_Sales[i])/counter[train_sd$weeknum[i]-1, toString(train_sd$sd[i])]
  }
}

write.csv(t_sd, "t_sd.csv")

training <- read.csv("training_data.csv")
val <- read.csv("validation_data.csv")
test <- read.csv("test_data.csv")

Xs <-cbind(Xs,store2 = 1:45)
Xd <-cbind(Xd,dept2 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
                51,52,54,55,56,58,59,60,65,67,71,72,74,77,78,79,80,81,82,83,85,87,90,91,92,93,94,95,96,97,98,99))
Xt <-cbind(Xt,week2 = 2:53)
Xs <- data.frame(Xs)
Xd <- data.frame(Xd)
Xt <- data.frame(Xt)
train_x <- sqldf('select * from training t, Xs, Xd, Xt where t.Store = Xs.store2 and 
                 t.Dept = Xd.dept2 and t.week = Xt.week2')
train_x$store2 <- NULL
train_x$dept2 <- NULL
train_x$week2 <- NULL

val_x <- sqldf('select * from val v, Xs, Xd, Xt where v.Store = Xs.store2 and 
                v.Dept = Xd.dept2 and v.week = Xt.week2')
val_x$store2 <- NULL
val_x$dept2 <- NULL
val_x$week2 <- NULL
write.csv(train_x, "train_pca_ls.csv")
write.csv(val_x, "val_pca_ls.csv")

test_x <- sqldf('select * from test t, Xs, Xd, Xt where t.Store = Xs.store2 and 
                 t.Dept = Xd.dept2 and t.week = Xt.week2')
test_x$store2 <- NULL
test_x$dept2 <- NULL
test_x$week2 <- NULL

write.csv(test_x, "test_pca_ls.csv")
