install.packages("hydroTSM")
library(hydroTSM)


### data
data("SanMartinoPPts")

six <- window(SanMartinoPPts, start = as.Date("1985-01-01"))

six  # selecting only 6-years

month <- daily2monthly(six, FUN = sum)
month

dates <- time(six)

nyears <- yip(from = start(six), to = end(six), out.type = "nmbr")  # Amount of years in 'six'
nyears


### EDA
smry(six)
hydroplot(six, var.type = "Precipitation", main = "at San Martino", pfreq = "dm", from = "1987-01-01")

dwi(six)  # Amount of days with information per year

dwi(six, out.unit = "mpy")  # mpy = month per year

# Creating a matrix 
month_mt <- matrix(month, ncol = 12, byrow = T)
colnames(month_mt) <- month.abb
rownames(month_mt) <- unique(format(time(month), "%Y"))
month_mt

require(lattice)
print(matrixplot(month_mt, ColorRamp = "Precipitation", main = "Monthly precipitation at San Martino st., [mm/month]"))

# Annual analysis
year <- daily2annual(six, FUN = sum, na.rm = T)  # Annual values of precipitation
mean(year)

# Monthly analysis
monthlyfunction(month, FUN = median)

cmonth <- format(time(month), "%b")  # vector with the three_letter abbreviations for the month names
cmonth

month_mf <- factor(cmonth, levels = unique(cmonth), ordered = T)
month_mf

boxplot(coredata(month) ~ month_mf, col = "lightblue", main = "Monthly Precipitation", ylab = "Precipitation, [mm]", xlab = "Month")

# Seasonal analysis
seasonalfunction(month, FUN = sum) / nyears
DJF <- dm2seasonal(month, season = "DJF", FUN = sum); DJF
MAM <- dm2seasonal(month, season = "MAM", FUN = sum); MAM
JJA <- dm2seasonal(month, season = "JJA", FUN = sum); JJA
SON <- dm2seasonal(month, season = "SON", FUN = sum); SON

hydroplot(six, pfreq = "seasonal", FUN = sum, stype = "default")
dev.off()

# Some extreme indices
ppts <- window(SanMartinoPPts, start= as.Date("1988-01-01"))

hydroplot(ppts, ptype = "ts", pfreq = "o", var.unit = "mm")

r10 <- length(ppts[ppts > 10])  # Heavy precipitation ( > 10mm)
r10

wet_index <- which(ppts >= 1)  # daily precipitation >= 1mm
prwn95 <- quantile(ppts[wet_index], probs = .95, na.rm = T)
prwn95

very_wet_index <- which(ppts >= prwn95)
very_wet_index

r95p <- sum(ppts[very_wet_index])
r95p

# 5-day total precipitation
x.5max <- rollapply(data = ppts, width = 5, FUN = sum, fill = NA, partial = T, align = "center")
head(x.5max)

hydroplot(x.5max, ptype = "ts", pfreq = "o", var.unit = "mm")

x.5max.annaul <- daily2annual(x.5max, FUN = max)
x.5max.annaul

# Climograph
data("MaquehueTemuco")  # Southern Hemisphere
head(MaquehueTemuco)
pcp <- MaquehueTemuco[, 1]  # precipitation
tmx <- MaquehueTemuco[, 2]  # maximum temp
tmn <- MaquehueTemuco[, 3]  # minimum temp
m <- climograph(pcp = pcp, tmx = tmx, tmn = tmn)
