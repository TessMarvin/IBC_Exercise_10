#Exercise 10 -- Tess Marvin
#Simulate the growth of tumor population 
tumorpopulation <- function(N0=2, r0=0.1,knum=1000000)
{
  #The tumor begins with a population of nonmutant cells
  nmpop <- c(N0)
  #The mutation begins at zero 
  mpop <- c(1)
  #carryingcapacity
  K <- knum 
  #growthrate 
  rinit <- r0
  while(nmpop[length(nmpop)] < 9.905e+05)
  {
    if(nmpop[length(nmpop)] < 100)
    {
    nmpopnum <- nmpop[length(nmpop)] + (rinit)*(nmpop[length(nmpop)])*(1-((nmpop[length(nmpop)]+mpop[length(mpop)])/knum))
    nmpop <- c(nmpop, nmpopnum)
    mpop <- c(mpop, 1)
    }
    else if(nmpop[length(nmpop)] > 100)
    {
      mpopnum <- mpop[length(mpop)] + (rinit)*(mpop[length(mpop)])*(1-((nmpop[length(nmpop)]+mpop[length(mpop)])/knum))
      nmpopnum <- nmpop[length(nmpop)] + (rinit)*(nmpop[length(nmpop)])*(1-((nmpop[length(nmpop)]+mpop[length(mpop)])/knum))
      nmpop <- c(nmpop, nmpopnum)
      mpop <- c(mpop, mpopnum)
    }
  }
  #After equillibrium, drug treatment begins 
  mutr <- rinit/2
  nonmutr <- -0.1
  while(nmpop[length(nmpop)] > 50)
  {
    mpopnum <- mpop[length(mpop)] + (mutr)*(mpop[length(mpop)])*(1-((nmpop[length(nmpop)]+mpop[length(mpop)])/knum))
    nmpopnum <- nmpop[length(nmpop)] + (nonmutr)*(nmpop[length(nmpop)])*(1-((nmpop[length(nmpop)]+mpop[length(mpop)])/knum))
    print(nmpopnum)
    nmpop <- c(nmpop, nmpopnum)
    mpop <- c(mpop, mpopnum)
  }
  results=data.frame(days=1:length(nmpop))
  results$nonmutantpop=nmpop
  results$mutantpop=mpop
  return(results)
}
populationdata <- tumorpopulation()
#plot the data 
library(ggplot2)
a = ggplot() +
  geom_line(data = populationdata, aes(x=days, y=nonmutantpop), color = "blue") +
  geom_line(data = populationdata, aes(x=days, y=mutantpop), color = "red") + 
  xlab("Days Elapsed") +
  ylab("Tumor Population") +
  theme_classic()
a
