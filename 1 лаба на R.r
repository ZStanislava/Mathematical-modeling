
# Практикум по математическим моделям и статистическим методам
# 1 лабораторная работа
# 4 вариант

get_u_alpha = function(alpha) {
  value_alpha = 1 - (alpha / 2)
  if (value_alpha == 0.9)
    return (1.282)
  if (value_alpha == 0.95)
    return (1.645)
  if (value_alpha == 0.975)
    return (1.960)
  if (value_alpha == 0.99)
    return (2.326)
  if (value_alpha == 0.995)
    return (2.576)
  if (value_alpha == 0.999)
    return (3.090)
  return (0)
}

laba1 = function() {
  # ИСХОДНЫЕ ДАННЫЕ
  alpha = 0.2
  x = c(100, 120, 140)
  y = c(26, 33, 40, 47)
  matrix_data = matrix(c(3, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 3),
                       nrow = 4, ncol = 3, byrow = TRUE)
  print(matrix_data)
  
  # РЕШЕНИЕ
  
  # Считается nx, ny, n
  nx = colSums(matrix_data)
  ny = rowSums(matrix_data)
  n = sum(nx)
  #print(nx)
  #print(ny)
  #print(n)
  
  # Считается x_, y_
  sum_x = 0
  for (i in 1:3) {
    sum_x = sum_x + nx[i] * x[i]
  }
  x_ = (1 / n) * sum_x
  sum_y = 0
  for (j in 1:4) {
    sum_y = sum_y + ny[j] * y[j]
  }
  y_ = (1 / n) * sum_y
  #print(x_)
  #print(y_)
  
  # Считается x2_ и y2_
  sum_x2 = 0
  for (i in 1:3) {
    sum_x2 = sum_x2 + nx[i] * x[i] * x[i]
  }
  x2_ = (1 / n) * sum_x2
  
  sum_y2 = 0
  for (j in 1:4) {
    sum_y2 = sum_y2 + ny[j] * y[j] * y[j]
  }
  y2_ = (1 / n) * sum_y2
  #print(x2_)
  #print(y2_)
  
  # Считается s2x и s2y
  s2x = x2_ - (x_) * (x_)
  s2y = y2_ - (y_) * (y_)
  #print(s2x)
  #print(s2y)
  
  #Считается xy__
  sum_xy = 0
  for (i in 1:3) {
    for (j in 1:4) {
      sum_xy = sum_xy + x[i] * y[j] * matrix_data[j, i]
    }
  }
  xy__ = (1 / n) * sum_xy
  #print(xy__)
  
  # Считается sxy
  sxy = xy__ - x_ * y_
  #print(sxy)
  
  # Считается pxy
  pxy = sxy / (sqrt(s2x) * sqrt(s2y))
  #print(pxy)
  print("pxy:")
  print(pxy)
  
  # Поиск доверительного интервала
  fraction1 = (1 / 2) * log((1 + pxy) / (1 - pxy))
  #u_alpha = get_u_alpha(alpha)
  u_alpha = qnorm(1 - (alpha / 2))
  fraction2 = u_alpha / sqrt(n - 3)
  interval_log1 = fraction1 - fraction2
  interval_log2 = fraction1 + fraction2
  interval_pxy1 = (exp(2 * interval_log1) - 1) / (exp(2 * interval_log1) + 1)
  interval_pxy2 = (exp(2 * interval_log2) - 1) / (exp(2 * interval_log2) + 1)
  #print(fraction1)
  #print(u_alpha)
  #print(fraction2)
  #print(interval_log1)
  #print(interval_log2)
  print("Интервал:")
  print(interval_pxy1)
  print(interval_pxy2)
}

laba1()
