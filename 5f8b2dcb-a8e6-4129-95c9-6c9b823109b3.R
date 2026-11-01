# Maximización de utilidad en R
# Definir parámetros del problema
p1 <- NULL     # Precio del bien 1
p2 <- NULL     # Precio del bien 2
m <- NULL      # Presupuesto del consumidor

# Sustitutivos perfectos: U(x1, x2) = a*x1 + b*x2
a <- NULL      # Peso del bien 1
b <- NULL      # Peso del bien 2

sustitutivos_perfectos <- function(p1, p2, m, a, b) {
  if (a / b > p1 / p2) {
    x1 <- m / p1
    x2 <- 0
  } else if (a / b < p1 / p2) {
    x1 <- 0
    x2 <- m / p2
  } else {
    x1 <- m / (p1 + p2)
    x2 <- x1
  }
  return(c(x1 = x1, x2 = x2))
}

# Complementarios perfectos: U(x1, x2) = min(a*x1, b*x2)
complementarios_perfectos <- function(p1, p2, m, a, b) {
  x1 <- m / (p1 + p2 * (a / b))
  x2 <- (a / b) * x1
  return(c(x1 = x1, x2 = x2))
}

# Saciedad: U(x1, x2) = -( (x1 - x1_star)^2 + (x2 - x2_star)^2 )
x1_star <- NULL   # Punto de saciedad para x1
x2_star <- NULL   # Punto de saciedad para x2

saciedad <- function(p1, p2, m, x1_star, x2_star) {
  lambda <- (x1_star * p2 + x2_star * p1) / (p1 + p2)
  x1 <- max(0, min(m / p1, x1_star))
  x2 <- max(0, min(m / p2, x2_star))
  return(c(x1 = x1, x2 = x2))
}

# Neutrales: U(x1, x2) = x1
neutrales <- function(p1, p2, m) {
  x1 <- m / p1
  x2 <- 0
  return(c(x1 = x1, x2 = x2))
}

# Bienes discretos: U(x1, x2) = a*x1 + b*x2 (enteros)
bienes_discretos <- function(p1, p2, m, a, b) {
  max_utilidad <- 0
  best_x1 <- 0
  best_x2 <- 0
  for (x1 in 0:(m / p1)) {
    for (x2 in 0:(m / p2)) {
      if (p1 * x1 + p2 * x2 <= m) {
        utilidad <- a * x1 + b * x2
        if (utilidad > max_utilidad) {
          max_utilidad <- utilidad
          best_x1 <- x1
          best_x2 <- x2
        }
      }
    }
  }
  return(c(x1 = best_x1, x2 = best_x2))
}

# Cobb-Douglass
Cobb <- function(a, b, p1, p2, m){
  x1 <- (a*m)/(p1*(a+b))
  x2 <- (b*p1*x1)/(a*p2)
  return(c("x1"= x1,"x2" = x2))
}

