library(rootSolve)

get_alpha_beta_from_multiroot_result <- function(root, p, q, num_trt)
{
  if (p == 1) {
    beta_root <- root$root[(q+1):(q+num_trt)]
    beta_root <- t(as.matrix(beta_root))
  } else {
    beta_root <- matrix(NA, nrow = p, ncol = num_trt)
    for (k in 1:num_trt) {
      beta_root[,k] <- root$root[(q + 1 + (k-1)*p) : (q + k*p)]
    }
  }
  if (q == 1) {
    alpha_root <- root$root[1]
  } else {
    alpha_root <- as.matrix(root$root[1:q])
    #alpha_root <- as.vector(root$root[1:q])
  }
  return(list(alpha = alpha_root, beta = beta_root))
}

# indicator vector of 1(A_t = 1), ..., 1(A_t = num_trt)
ind <- function(A_t, num_trt){
  A_vec = c()
  cand_vec = 1:num_trt
  for (k in 1:length(cand_vec)) {
    if (A_t == cand_vec[k]){
      A_vec = c(A_vec, 1)
    }else{
      A_vec  = c(A_vec, 0)
    }
  }
  return(A_vec)
}

find_change_location <- function(v){
  n <- length(v)
  if (n <= 1) {
    stop("The vector need to have length > 1.")
  }
  return(c(1, 1 + which(v[1:(n-1)] != v[2:n])))
}
