dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

lin_reg = lm(formula = Salary ~ .,
             data = dataset)

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
poly_reg = lm(formula = Salary ~.,
              data = dataset)

#Linear Regression Model
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'Red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            color = 'Blue') +
  ggtitle('Salary vs Level') +
  xlab('Levels') +
  ylab('Salary')

#Polynomial Regression Model
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             color = 'Red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            color = 'Blue') +
  ggtitle('Salary vs Levels') +
  xlab('Levels') +
  ylab('Salary')

#Predicting Salary at Level = 6.5
y_pred = predict(lin_reg, data.frame(Level = 6.5))

y_poly_pred = predict(poly_reg, data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3))