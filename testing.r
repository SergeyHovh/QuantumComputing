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

KH(4, random_conf(4)) %>% M(1, 16)

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

KH(4, random_conf(4)) %>% M(2, 100)

random_conf(3) %>% as.numeric()

KH(3, c(1, 0, 0, 0, 0, 1, 0, 1)) %>%
  # nth_qubit_prob(3)
  M(3, 1000) %>% as.numeric %>% sum

KH(6, c(1,0,0,0,0,1,0,0,1,1,1,
        1,1,1,1,1,1,0,1,0,0,1,
        1,1,1,0,0,1,0,0,0,0,1,
        1,1,1,0,1,1,0,0,0,0,1,
        1,0,1,0,0,0,0,0,0,0,0,
        0,1,1,0,1,1,1,1,0)) %>%
  # nth_qubit_prob(2)
  M(2, 1000) %>% as.numeric() %>% sum

CX_Cascade()

complex(modulus = 1, argument = pi/4) # A * e^(i*theta)

phase_shift <- function(theta = pi/2) {
  matrix(c(1, 0, 0, complex(modulus = 1, argument = theta)), nrow = 2, ncol = 2)
}

# shit
{
  set.seed(12)
  theta <- pi/20
  state("0001") %M%
    H(4) %M%
    oracle_function(c(0, 1, 1, 0, 1, 0, 0, 1)) %M%
    # (phase_shift(theta) %x% phase_shift(theta) %x% I()) %M%
    CX_Cascade(4) %>% 
    nth_qubit_prob()
    # M(times = 1000) %>% as.numeric() %>% sum
}

phase_shift(2*pi - 1) %>% Mod

draw(KH(5, random_conf(5)))

# state("0001") %M%
#   H(4) %M%
#   oracle_function(generate_conf(3, F)) %M%
#   H(4) %>% non_zero()



c(matrix(1, ncol = 255), 0) -> conf

state("000000001") %M%
  H(9) %M%
  oracle_function(conf) %M%
  CX_Cascade(9) %>%
  M(times = 1000) %>% as.numeric() %>% sum
  
  # nth_qubit_prob()

monte_carlo(8)

state("001") %M%
  H(3) %M%
  oracle_function(generate_conf(2, F)) %M%
  CX_Cascade(3) %>%
  M(times = 100, bit_num = 1)



state("0001") %M%
  H(4) %M%
  oracle_function(generate_conf(size = 3, even = F)) %M%
  (H(4)) %>%
  non_zero() %>% draw

oracle_function(generate_conf(3))

state("0001") %M%
  H(4) %M%
  oracle_function(c(0, 0, 1, 1, 1, 1, 0, 0)) %M%
  H(4) %>%
  non_zero()



conf3 <- c(0, 0, 1, 0, 0, 0, 0, 0)
conf4 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
conf5 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

new_algorithm(conf5) %M%
  (I(4) %x% X() %x% I()) %M%
  (H() %x% I(5)) %M%
  oracle_function(conf5) %M%
  CX_Cascade(6) %M%
  oracle_function(conf5) %M%
  (I(5) %x% H()) %>%
  non_zero() %>%
  nth_qubit_prob()



new_algorithm(conf3) %>% nth_qubit_prob()



state("0001") %M%
  (I() %x% H(3)) %M%
  (H() %x% I(3)) %M%
  oracle_function(conf3) %M%
  (CX(1, 2) %x% I(2)) %M%
  (CX(1, 3) %x% I(1)) %M%
  (H() %x% I(3)) %M%
  (CX(1, 2) %x% I(2)) %M%
  (CX(1, 3) %x% I(1)) %M%
  oracle_function(conf3) %M%
  (X() %x% I(3)) %M%
  (CX(1, 2) %x% I(2)) %M%
  (X() %x% I(3)) %M%
  (H() %x% I(3)) %M%
  oracle_function(conf3) %M%
  (CX(1, 2) %x% I(2)) %M%
  (CX(1, 3) %x% I(1)) %M%
  (H() %x% I(3)) %M%
  (CX(1, 2) %x% I(2)) %M%
  (CX(1, 3) %x% I(1)) %M%
  oracle_function(conf3) %>%
  non_zero() %>%
  nth_qubit_prob()





new_algorithm(conf3) %M%
  (X() %x% I(3)) %M%
  (H() %x% I(3)) %M%
  oracle_function(conf3) %M%
  (CX(1, 2) %x% I(2)) %M%
  (CX(1, 3) %x% I(1)) %M%
  (H() %x% I(3)) %M%
  (CX(1, 2) %x% I(2)) %M%
  (CX(1, 3) %x% I(1)) %M%
  oracle_function(conf3) %>%
  non_zero() %>%
  nth_qubit_prob()
  