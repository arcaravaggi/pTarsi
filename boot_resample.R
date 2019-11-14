dat <- read.csv("exploration_app/data/all_data.csv")


# Resample data according to X replicates of Y groups. Calculate mean and sd R^2 values for each group of a given size
#
# df = data frame
# sp = species. string (e.g. "Blackbird")
# mi = minimum group size (default = 1)
# ma = maximum group size (default = 100)
# n = number of replicates (default = 5)
#
# E.g. pTResample(pTarsi.dat, sp = "Blackbird", mi = 1, ma = 100, n = 5)
#
pTResample <- function(df, sp, mi = 1, ma = 100, n = 5){
  
  require(dplyr)
  require(ggplot2)
  
  # Filter by species
  a <- df %>% 
    filter(species == sp) %>%
    select(c(10:11))
  
  # Function to resample data with replacement, with between 1-100 birds per sample and calculate R^2
  #
  # f = dataframe (here, a, above)
  grpResample <- function(f){
    d <- data.frame(groupsize = c(mi:ma), s = NA)
    for(i in nrow(d)){
      for(i in mi:ma){
        a <- f[sample(x = 1:nrow(f), size = i, replace = T), ]
        d$s[i] <- cor(a[,1],a[,2])^2
      }
      return(d)
    }
  }
  
  # Replicate resampling function x times
  r <- replicate(n, grpResample(a))
  
  # Collapse list to dataframe, transposing to preserve row-column structure
  d <- data.frame(t((matrix(unlist(r), nrow=length(r), byrow=T))))
  # Remove odd-numbered rows (group size)
  d <- d[,seq(2,ncol(d),2)]
  
  # Calculte mean and standard deviation across rows
  o <- d %>% mutate(avg = apply(.,1,mean),
                    sd = apply(.,1,sd))
  o$groupsize <- c(mi:ma)
  o[is.na(o)] <- 0
  
  # Plot data
  p <- ggplot(o, aes(x = groupsize, y = avg)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd)) +
    geom_smooth()
  
  l <- list(o,p)
  
  names(l) <- c("data", "plot")
  
  return(l)
}

  
 
n <- pTResample(dat, "Chaffinch", 1, 100, 5)
  

  
n$plot
  
  

bb <- dat %>% 
  filter(species == "Blackbird") %>%
  select(c(10:11))



test <- function(df){
    d <- data.frame(groupsize = c(1:100), s = NA)
    for(i in nrow(d)){
      for(i in 1:100){
    a <- df[sample(x = 1:nrow(df), size = i, replace = T), ]
    d$s[i] <- cor(a[,1],a[,2])^2
    }
    return(d)
}
}

result <- replicate(50, test(bb)) # Replicate resampling x times

df <- data.frame(t((matrix(unlist(result), nrow=length(result), byrow=T)))) # Collapse list to dataframe

df <- df[,seq(2,ncol(df),2)] # Keep data columns

a <- df %>% mutate(avg = apply(.,1,mean),
              sd = apply(.,1,sd)) %>%
  select(c(51:52))
a$groupsize <- c(1:100)


library(ggplot2)
ggplot(a, aes(x = groupsize, y = avg)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg-sd, ymax = avg+sd)) +
  geom_smooth()


