UsedHondas = read.delim("http://www.rossmanchance.com/iscam3/data/UsedHondas.txt")

plot(UsedHondas$price~UsedHondas$year)
cor(UsedHondas$price,UsedHondas$year)

plot(UsedHondas$price~UsedHondas$mileage)
cor(UsedHondas$price,UsedHondas$mileage)

plot(UsedHondas$price~UsedHondas$age)
cor(UsedHondas$price,UsedHondas$age)
