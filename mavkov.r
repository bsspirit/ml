#####################
# 用R语言手搓马尔可夫决策过程markov
# http://blog.fens.me/r-markov/
######################

setwd("C:/work/R/stat")

# 6种状态
s <- c("s1", "s2", "s3", "s4", "s5", "s6")
s

# 转移概率矩阵
p <- matrix(c(
  0.9,0.1,  0,  0,  0,  0,
  0.5,  0,0.5,  0,  0,  0,
    0,  0,  0,0.6,  0,0.4,
    0,  0,  0,  0,0.3,0.4,
    0,0.2,0.3,0.5,  0,  0,
    0,  0,  0,  0,  0,  1
), nrow = 6, byrow = TRUE, dimnames = list(s, s))
p

# 画图
library(diagram)
plotmat(p,lwd = 1, box.lwd = 1, cex.txt = 0.8, 
        box.size = 0.02, box.type = "circle", shadow.size = 0.001,
        box.col = "light blue",  arr.length=0.1, arr.width=0.1,
        self.cex = 1.5,self.shifty = -0.01,self.shiftx = 0.04, 
        main = "Markov Chain")


# 模拟马尔科夫链的函数
simulate_markov_chain <- function(transition_matrix, initial_state, n_steps, stop=NULL) {
  states <- rownames(transition_matrix)
  chain <- character(n_steps + 1)
  chain[1] <- initial_state
  
  for (i in 1:n_steps) {
    current_state <- chain[i]
    next_state <- sample(states, size = 1, prob = transition_matrix[current_state, ])
    chain[i + 1] <- next_state
  }
  
  if(!is.null(stop)){
    chain<-chain[1:which(chain==stop)[1]]  
  }
  
  return(chain)
}

# 设置初始状态和步数
initial_state <- "s1"
n_steps <- 100
stop_state<-'s6'

# 模拟马尔科夫链
markov_chain <- simulate_markov_chain(p, initial_state, n_steps,stop_state)
markov_chain

library(ggplot2)
library(dplyr)

# 准备数据用于绘图
chain_df <- data.frame(
  step = 1:length(markov_chain),
  state = factor(markov_chain, levels = s)
)

# 绘制马尔科夫链路径
ggplot(chain_df, aes(x = step, y = state, group = 1)) +
  geom_line(color = "gray") +
  geom_point(aes(color = state), size = 3) +
  scale_x_continuous()+
  theme_minimal()


# 模拟多条马尔科夫链观察收敛性
n_simulations <- 5
n_steps <- 50
simulations <- list()

for (i in 1:n_simulations) {
  simulations[[i]] <- simulate_markov_chain(p, initial_state, n_steps)
}

# 准备数据用于绘图
sim_df <- data.frame(
  step = rep(0:n_steps, n_simulations),
  state = unlist(simulations),
  simulation = rep(1:n_simulations, each = n_steps + 1)
)

# 绘制多条马尔科夫链
ggplot(sim_df, aes(x = step, y = state, group = simulation, color = factor(simulation))) +
  geom_line(alpha = 0.6) +
  geom_point(size = 1.5) +
  labs(title = "多条马尔科夫链模拟",
       x = "时间步", y = "状态",
       color = "模拟编号") +
  theme_minimal()


# 计算马尔科夫链的稳态分布
calculate_steady_state <- function(transition_matrix, tolerance = 1e-6) {
  n_states <- nrow(transition_matrix)
  # 初始猜测
  pi <- rep(1/n_states, n_states)
  
  while (TRUE) {
    pi_new <- pi %*% transition_matrix
    if (max(abs(pi_new - pi)) < tolerance) break
    pi <- pi_new
  }
  
  return(as.vector(pi_new))
}

steady_state <- calculate_steady_state(p)
names(steady_state) <- s

#稳态分布:
steady_state

# 可视化稳态分布
barplot(steady_state, col = c("gold", "gray", "blue"),
        main = "马尔科夫链稳态分布",
        ylab = "概率", xlab = "状态",
        ylim = c(0, 0.6))



# 模拟带奖励的马尔科夫链
simulate_markov_reward <- function(transition_matrix, rewards, initial_state, n_steps, gamma = 0.9, stop=NULL) {
  states <- rownames(transition_matrix)
  chain <- character(n_steps + 1)
  reward_sequence <- numeric(n_steps + 1)
  discounted_rewards <- numeric(n_steps + 1)
  
  chain[1] <- initial_state
  reward_sequence[1] <- rewards[initial_state]
  discounted_rewards[1] <- (gamma^0) * rewards[initial_state]
  
  for (i in 1:n_steps) {
    current_state <- chain[i]
    next_state <- sample(states, size = 1, prob = transition_matrix[current_state, ])
    chain[i + 1] <- next_state
    reward_sequence[i + 1] <- rewards[next_state]
    discounted_rewards[i + 1] <- (gamma^i) * rewards[next_state]
  }
  
  return(list(
    chain = chain,
    reward = unlist(reward_sequence),
    discounted_reward = unlist(discounted_rewards),
    total_reward = sum(unlist(reward_sequence)),
    total_discounted_reward = sum(unlist(discounted_rewards))
  ))
}

# 模拟参数
initial_state <- "s1"
n_steps <- 100
gamma <- 0.9  # 折扣因子
rewards<-data.frame("s1"=-1,"s2"=-2,"s3"=-3,"s4"=10,"s5"=1,"s6"=0)

# 运行模拟
result <- simulate_markov_reward(p, rewards, initial_state, n_steps, gamma)

# 打印结果
print(paste("总奖励:", result$total_reward))
print(paste("总折现奖励:", result$total_discounted_reward))

# 可视化马尔科夫链状态序列

library(ggplot2)
library(gridExtra)

# 准备数据
sim_data <- data.frame(
  step = 0:n_steps,
  state = factor(result$chain, levels = s),
  reward = result$reward,
  discounted_reward = result$discounted_reward
)


# 绘制状态和奖励
p1 <- ggplot(sim_data, aes(x = step, y = state, color = state)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), color = "gray") +
  labs(title = "马尔科夫链状态序列", x = "时间步", y = "状态")

p2 <- ggplot(sim_data, aes(x = step, y = reward)) +
  geom_col(aes(fill = state)) +
  scale_fill_manual(values = c("Sunny" = "gold", "Cloudy" = "gray", "Rainy" = "blue")) +
  labs(title = "即时奖励", x = "时间步", y = "奖励")

p3 <- ggplot(sim_data, aes(x = step, y = discounted_reward)) +
  geom_col(aes(fill = state)) +
  scale_fill_manual(values = c("Sunny" = "gold", "Cloudy" = "gray", "Rainy" = "blue")) +
  labs(title = paste0("折现奖励 (γ=", gamma, ")"), x = "时间步", y = "折现奖励")

# 组合图形
grid.arrange(p1, p2, p3, ncol = 1)

# 价值函数计算求解Bellman方程

# 计算状态价值函数
calculate_state_values <- function(transition_matrix, rewards, gamma = 0.9, epsilon = 1e-6) {
  n_states <- nrow(transition_matrix)
  I <- diag(n_states)
  
  # 构建线性方程组: V = R + γPV → (I - γP)V = R
  A <- I - gamma * transition_matrix
  b <- rewards
  
  # 解线性方程组
  V <- solve(A, b)
  
  return(V)
}

# 参数
p
rewards
gamma

# 计算状态价值
state_values <- calculate_state_values(p, rewards, gamma)

print("状态价值函数:")
print(state_values)
