abc <- ts(5 + matrix(sort(rnorm(1600)), ncol = 16, nrow = 100))
sex <- rep(c("female", "male"), each = 8)
state <- rep(c("NSW", "VIC", "QLD", "SA", "WA", "NT", "ACT", "TAS"), 2)
gc <- rbind(sex, state)  # a matrix consists of strings.
gn <- rbind(rep(1:2, each = 8), rep(1:8, 2))  # a numerical matrix
rownames(gc) <- rownames(gn) <- c("Sex", "State")
x <- gts(abc, groups = gc)
y <- gts(abc, groups = gn)

# Example 2 with two simple hierarchies (geography and product) to show the argument "characters"
bnames1 <- c("VICMelbAA", "VICMelbAB", "VICGeelAA", "VICGeelAB",  
             "VICMelbBA", "VICMelbBB", "VICGeelBA", "VICGeelBB",
             "NSWSyndAA", "NSWSyndAB", "NSWWollAA", "NSWWollAB", 
             "NSWSyndBA", "NSWSyndBB", "NSWWollBA", "NSWWollBB")
bts1 <- matrix(ts(rnorm(160)), ncol = 16)
colnames(bts1) <- bnames1
x1 <- gts(bts1, characters = list(c(3, 4), c(1, 1)))

# Example 3 with a non-hierarchical grouped time series of 3 grouping variables (state, age and sex)
bnames2 <- c("VIC1F", "VIC1M", "VIC2F", "VIC2M", "VIC3F", "VIC3M",
             "NSW1F", "NSW1M", "NSW2F", "NSW2M", "NSW3F", "NSW3M")
bts2 <- matrix(ts(rnorm(120)), ncol = 12)
colnames(bts2) <- bnames2
x2 <- gts(bts2, characters = c(3, 1, 1))
#================================================================================================================================#


library(hts)

# Generate some artificial data    
matrix_tseries <- ts(matrix(rnorm(14*75),ncol=14))
nodes <- list(2, c(7,7))

# Split data into training and test sets
hierarchical <- hts(window(matrix_tseries, end=40), nodes)
test <- hts(window(matrix_tseries, start=41), nodes)

# Produce forecasts
fcast <- forecast(hierarchical, h = 35 ,level = c(80,95),
                  fmethod = "ets", method = "comb")

# Compute accuracy measures      
accuracy.gts(fcast, test)

#================================================================================================================================#


y <- ts(matrix(rnorm(900),ncol=45,nrow=20))
blnames <- paste(c(rep("A",20),rep("B",25)), # State
                 rep(1:9,each=5),                # County
                 rep(c("X","X","X","Y","Y"),9),  # Industry
                 rep(c("a","b","c","a","b"),9),  # Sub-industry
                 sep="")
colnames(y) <- blnames

#================================================================================================================================#



abc <- ts(5 + matrix(sort(rnorm(1600)), ncol = 16, nrow = 100))
sex <- rep(c("female", "male"), each = 8)
state <- rep(c("NSW", "VIC", "QLD", "SA", "WA", "NT", "ACT", "TAS"), 2)
gc <- rbind(sex, state)  # a matrix consists of strings.
gn <- rbind(rep(1:2, each = 8), rep(1:8, 2))  # a numerical matrix
rownames(gc) <- rownames(gn) <- c("Sex", "State")
x <- gts(abc, groups = gc)
y <- gts(abc, groups = gn)




y <- ts(matrix(rnorm(900),ncol=45,nrow=20))
blnames <- paste(c(rep("A",20),rep("B",25)), # State
                 rep(1:9,each=5),                # County
                 rep(c("X","X","X","Y","Y"),9),  # Industry
                 rep(c("a","b","c","a","b"),9),  # Sub-industry
                 sep="")
colnames(y) <- blnames
gy <- gts(y, characters=list(c(1,1),c(1,1)))
gps <- rbind(
  c(rep(1,20),rep(2,25)), # State
  rep(1:9,each=5),        # County
  rep(c(1,1,1,2,2),9),    # Industry
  rep(1:5, 9),            # Sub-industry
  c(rep(c(1,1,1,2,2),4),rep(c(3,3,3,4,4),5)), # State x industry
  c(rep(1:5, 4),rep(6:10, 5)),                # State x Sub-industry
  rep(1:18, rep(c(3,2),9))                    # County x industry
)
gy <- gts(y, groups=gps)

fc <- forecast(gy)




