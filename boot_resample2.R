dat <- read.csv("exploration_app/data/all_data.csv")


# Resample data according to X replicates of Y groups
# Calculate mean and sd R^2 values for each group of a given size based on two columns
# The function outputs a list containing four objects:
# 1. Data minus outliers (data frame)
# 2. Outliers (data frame)
# 3. Mean R^2 for each group size across X iterations (data.frame)
# 4. Plot of mean R^2 & SD from resampled data
#
# df = data frame
# sp = species. string (e.g. "Blackbird")
# v1 = column number of variable 1
# v2 = column number of variable 2
# mi = minimum group size (default = 1)
# ma = maximum group size (default = 100)
# n = number of replicates (default = 5)
#
# E.g. pTResample(pTarsi.dat, sp = "Blackbird", mi = 1, ma = 100, n = 5)
#
pTResample <- function(df, sp, v1, v2, mi = 1, ma = 100, n = 5){
  
  require(dplyr)
  require(ggplot2)
  
  # Filter by species
  a <- df %>% 
    filter(species == sp) %>%
    #select(c(10:11)) %>%
    mutate(., t = .[,v1]*.[,v2]) # Create interaction variable for the identification of outliers
  
  # Function to remove outliers based on Tukey's 1.5*IQR threshold
  #
  # dt = data frame
  # var = variable to examine for outliers
  # Adapted from https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
  outlierKD <- function(dt, var) {
    var_name <- eval(substitute(var),eval(dt))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    na2 <- sum(is.na(var_name))
    m2 <- mean(var_name, na.rm = T)
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    dt2 <- dt[!complete.cases(dt),] # data frame for outliers
    dt <- dt[complete.cases(dt),] # data frame minus outliers
    cat("Outliers successfully removed", na2 - na1)
    l <- list(dt, dt2)
    names(l) <- c("b", "o")
    return(invisible(l))
  }
  
  k <- outlierKD(a, t)
  b <- k$b
  b$t <- NULL
  o <- k$o
  o$t <- NULL
  
  # Function to resample data with replacement, with between 1-100 birds per sample and calculate R^2
  #
  # f = dataframe (here, a, above)
  grpResample <- function(f, v1, v2){
    d <- data.frame(groupsize = c(mi:ma), s = NA)
    for(i in nrow(d)){
      for(i in mi:ma){
        a <- f[sample(x = 1:nrow(f), size = i, replace = T), ]
        d$s[i] <- cor(a[,v1],a[,v2])^2
      }
      return(d)
    }
  }
  
  # Replicate resampling function x times
  r <- replicate(n, grpResample(b, v1, v2))
  
  # Collapse list to dataframe, transposing to preserve row-column structure
  d <- data.frame(t((matrix(unlist(r), nrow=length(r), byrow=T))))
  # Remove odd-numbered rows (group size)
  d <- d[,seq(2,ncol(d),2)]
  
  # Calculte mean and standard deviation across rows
  w <- d %>% mutate(avg = apply(.,1,mean),
                    sd = apply(.,1,sd),
                    n.b = n()) %>%
    mutate(se.b = sd/sqrt(n.b),
           lci = avg - qt(1 - (0.05/2), n.b - 1) * se.b,
           uci = avg + qt(1 - (0.05/2), n.b - 1) * se.b)
  w$groupsize <- c(mi:ma)
  w[is.na(w)] <- 0
  
  # Plot data
  p <- ggplot(w, aes(x = groupsize, y = avg)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd)) +
    geom_smooth()
  
  l <- list(b,p,w,o)
  
  names(l) <- c("data", "plot", "resampled", "outliers")
  
  return(l)
}

  
 
n <- pTResample(dat, "Great Tit", 10, 11, 1, 100, 5)
