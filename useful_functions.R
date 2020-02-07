library(dplyr)
library(ggplot2)
library(R.utils)
library(reshape2)

###
##### Helper Functions
###

# Matrix Multiplication Operator
# syntax - A %M% B
`%M%` <- function(A, B) {
  require(R.utils)
  res <- A %*% B
  colnames(res) <- intToBin(0:(ncol(B) - 1))
  rownames(res) <- colnames(0:(nrow(B) - 1))
  return(res)
}

# Returns only those states, that have non-zero probability
non_zero <- function(df) {
  df <- df %>% as.data.frame()
  return(df[which(abs(df) > 0.001)])
}

# Makes the measurement on given register on given qubit
# by default measures the first qubit
M <- function(df, bit_num = 1, times = 1) {
  df %>%
    non_zero() %>% probs() %>% sort(T) -> s
  
  ss <- s %>% t %>% cumsum
  if(times == 1) {
    min(which(ss > runif(times))) -> x
    print(s[x])
  } else {
    r <- runif(times)
    sapply(ss, function(z){z > r}) -> d
    x <- c()
    for (i in 1:nrow(d)) {
      x <- c(min(which(d[i,])), x)
    }
  }
  s[x] %>%
    colnames() %>%
    substring(bit_num, bit_num)
}

# Returns the square of the coefficients of the states - probabilities to
# be measured
probs <- function(df) {
  df^2
}

# Generates an even/odd function configuration for the oracle_function
# given the number of *input* qubits
generate_conf <- function(size, even = T) {
  configuration <- c()
  n <- 2^size
  first <- sample(0:1, n/2, replace = T)
  configuration <- c(first, rev((first + even + 1) %% 2))
  return(configuration)
}

# Generates a random configuration for the oracle_function
# given the number of *input* qubits
random_conf <- function(size) {
  configuration <- c()
  n <- 2^size
  first <- sample(0:1, n, replace = T)
  configuration <- first
  return(configuration)
}

# Checks if the given configuration is useful or not
is_useful <- function(conf) {
  return(
    all(conf == rev(conf)) || 
      all(conf == rev((conf+1) %% 2))
  )
}

###

###
##### Gates and Functions
###

# Identity Gate
# n - number of bits to act on
I <- function(n = 1) {
  if(n == 0) {return(1)}
  res <- diag(2^n)
  colnames(res) <- intToBin(0:(ncol(res) - 1))
  rownames(res) <- intToBin(0:(nrow(res) - 1))
  return(res)
}

# Example
I(3) # returns an identity matrix, that can act on 4 qubits in the register

# Hadamard's Gate
# n - number of bits to act on
H <- function(n = 1) {
  H_1 <- matrix(c(1, 1, 1, -1)/sqrt(2), 2, 2)
  if(n == 0) return(1)
  if(n == 1) return(H_1)
  return(kronecker(H_1, H(n-1)))
}

# Example
H(2) # returns the Hadamard's gate for 2 qubits

# Takes a set of strings and converts it to a vector of states
state <- function(...) {
  require(R.utils)
  len <- 2^nchar(gsub("[-]", "", list(...)[1]))
  io <- matrix(0, 1, len)
  for (i in list(...)) {
    temp <- matrix(0, 1, len)
    temp[abs(strtoi(i, 2)) + 1] = 1
    if(startsWith(i, "-")) {
      io = io - temp
    } else {
      io = io + temp
    }
  }
  io <- io/sqrt(length(list(...)))
  colnames(io) <- intToBin(0:(len - 1))
  return(io)
}

# Example 
state("001", "011", "-100")

# Imitates the oracle function given the vector form of the function itself
oracle_function <- function(conf) {
  require(R.utils)
  d <- 2*length(conf)
  y <- diag(d/2)
  for(i in 1:length(conf)) {
    if(conf[i] == 0) {
      y[i, i] = 0
    }
  }
  res <- (diag(d) + kronecker(y, matrix(1, 2, 2))) %% 2
  colnames(res) <- intToBin(0:(ncol(res) - 1))
  rownames(res) <- intToBin(0:(nrow(res) - 1))
  return(res)
}

# Example
oracle_function(c(1, 0, 0, 1))
oracle_function(c(1, 0, 0, 0, 1, 1))


# CNOT gate with variable control and target bits
CX <- function(control = 1, target = 2) {
  require(R.utils)
  CNOT <- function(to = 2) {
    C <- matrix(0, 1, 2^(to-2))
    C <- c(C, (C+1)%%2)
    return(oracle_function(C))
  }
  
  rev_CNOT <- function(to = 2) {
    H(to) %M%
      CNOT(to) %M%
      H(to)
  }
  
  if(target > control) {
    m <- I(control - 1) %x% CNOT(target - control + 1)
  } else {
    m <- I(target - 1) %x% rev_CNOT(control - target + 1)
  }
  colnames(m) <- intToBin(0:(ncol(m) - 1))
  rownames(m) <- colnames(m)
  return(m)
}

# Example

# 1
# "adds" the value of the 1st bit to the 3rd one
state("101") %M%
  CX(1, 3) %>%
  non_zero()

# 2
# "adds" the value of the 3rd bit to the 4th one
# note that we need to act on all bits, thus use I() on the last one
state("10110") %M%
  (CX(3, 4) %x% I()) %>%
  non_zero()

# Not gate, or Pauli's X gate
# negates the state of the qubit
X <- function(n = 1) {
  if(n == 1) return(oracle_function(1))
  else {
    return(X(1) %x% X(n-1))
  }
}
###

###
##### Algorithms and Testing
###

# a function, that takes a function f, and calls it for 16 
# different configurations
# used to test 2 bit input algorithm
test <- function(f) {
  for(i in 0:1) {
    for(j in 0:1) {
      for(k in 0:1) {
        for(l in 0:1) {
          conf <- paste0(i, j, k, l)
          print(
            paste(strtoi(conf, 2), ":",
                  i, j, k, l, 
                  "is:", 
                  paste(
                    f(c(i, j, k, l)),
                    collapse = "")),sep = " ")
        }
      }
    }
  }
}

# checks if the function is even/odd or any other
# if returns 0 - even/odd
# if returns 1 - other function
check <- function(config) {
  require(dplyr)
  # setup
  func <- oracle_function(config)
  # circuit
  res <- state("001", "011") %M%
    (I(2) %x% H()) %M%    # hadamard the 3rd qubit
    func           %M%    # evaluate function
    (X() %x% I(2)) %M%    # revert the first qubit
    func           %M%    # evaluate function
    (I() %x% H() %x% I()) # hadamard the 2nd qubit
  
  # measuring second qubit
  res %>% 
    non_zero() %>%
    M(2) -> second_q
  
  return(second_q)
}

# main algorithm to solve the evennes problem with all possible 
# function configurations
evennes_check <- function(configuration) {
  if(check(configuration) == 1) {
    return("neither even, nor odd")
  }
  func <- oracle_function(configuration)
  
  res <- state("111") %M%
    H(3) %M%                # hadamard all 3 qubits
    func %M%                # evaluate function
    (CX(1,2) %x% I()) %M%    # C_Not the input qubits
    (H(2) %x% I())          # hadamard input qubits
  
  # measuring the first qubit
  res %>%
    non_zero() %>%
    M() -> first_q
  
  return (ifelse(first_q == 0, "even", "odd"))
}

test(check)
test(evennes_check)

# the CNOT cascade part of the algorithm for general case
CX_Cascade <- function(n) {
  if(n == 3) {
    return((CX() %x% I()) %M% H(n))
  }
  result <- CX(1, 2) %x% I(n - 2)
  for (i in (n-3):1) {
    result <- result %M%
      (CX(control = 1, target = n-i) %x% I(i))
  }
  result <- result %M% H(n)
  return(result)
}

# The generalized algorithm for n input bits
KH <- function(n, f) {
  require(dplyr)
  c(matrix(0, nrow = 1, ncol = n), 1) %>%
    paste(collapse = "") %>%
    state() %M%
    H(n+1) %M%
    oracle_function(f) %M%
    CX_Cascade(n+1) %>%
    non_zero()
}

# A function, that draws all possible states with their 
# respective state coefficients/probabilities
draw <- function(df) {
  require(dplyr)
  require(reshape2)
  require(ggplot2)
  df %>%
    melt %>%
    ggplot(aes(variable, value, fill = value)) +
    geom_bar(stat = "identity") +
    ylim(c(-1, 1)) +
    xlab("states")
}

# Monte Carlo simulations for given number of input qubits
monte_carlo <- function(n) {
  df <- c()
  for (i in 1:2^10) {
    conf <- random_conf(n)
    real <- is_useful(conf)
    if(real) {
      df <- rbind(df, c(real, TRUE))
    } else {
      KH(n, conf) %>% M(times = n+2) -> tmp
      df <- rbind(df, c(real, length(unique(tmp)) == 1))
    }
  }
  return(table(df[,1], df[,2]))
}

# Measuring the accuracy of the Monte Carlo simulation
accuracy <- function(t) {
  sum(diag(t))/sum(t)
}

# The probability of given qubit in the register to be
# measured 0
nth_qubit_prob <- function(df, n = 1) {
  which(df %>%
          non_zero() %>%
          colnames() %>%
          substring(n,n) == "0") -> indices
  if(length(indices) != 0) {
    df %>%
      non_zero() %>%
      as.data.frame() %>%
      select(indices) -> s
    return(s %>% probs %>% sum)
  } else {
    return(0)
  }
}
