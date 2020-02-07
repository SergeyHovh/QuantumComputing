KH(4, generate_conf(4, T)) %>% draw

state("001") %M% 
  H(3) %M% 
  oracle_function(generate_conf(2, F)) %M% 
  CX_Cascade(3) %>% 
  M(times = 10)

state("0000001") %M%
  H(7) %M%
  oracle_function(c(1,0,0,0,0,1,0,0,1,1,1,
                       1,1,1,1,1,1,0,1,0,0,1,
                       1,1,1,0,0,1,0,0,0,0,1,
                       1,1,1,0,1,1,0,0,0,0,1,
                       1,0,1,0,0,0,0,0,0,0,0,
                       0,1,1,0,1,1,1,1,0)) %M%
  CX_Cascade(7) %>%
  M()

draw(KH(6, generate_conf(6)))

state("00", "10") %>% M(1, 10)

KH(4, random_conf(4)) %>% M(1, 10)

#### DO NOT RUN THIS
# df <- c()
# for (i in 2:10) {
#   acc <- c()
#   for (j in 1:15) {
#     acc <- c(accuracy(monte_carlo(i)), acc)
#   }
#   df <- cbind(df, acc)
# }
# colnames(df) <- paste("qubits", 2:8, sep = "_")
# df <- as.data.frame(df)
# write.csv(df, file = "accuracy.csv", row.names = FALSE)
#### DO NOT RUN THIS

df %>%
  # select(2:10 - 1) %>%
  melt %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot()

y <- c()
for(n in 2:9) {
  x <- c()
  for(i in 1:2^10) {
    conf <- random_conf(n)
    while(is_useful(conf)) {
      conf <- random_conf(n)
    }
    k = 1
    KH(n, conf) -> kh
    while(T) {
      kh %>% M(times = k + n) -> tmp
      if(length(unique(tmp)) == 2) {
        x <- c(x, k)
        break
      } else {
        k = k + 1
      }
    }
  }
  y <- c(y, mean(x + n))
}


KH(4, random_conf(4)) -> i
i %>% probs %>% t %>% cumsum
i %>% M(times = 10)