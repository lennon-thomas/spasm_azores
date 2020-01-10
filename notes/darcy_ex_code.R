a = seq(0,1,length.out=10)
b = seq(0.1,0.3,length.out=3)
c = c(0,1)

d = expand.grid(a,b,c)
names(d) <- c('a','b','c')

N <- dim(d)[1]

d %>%
  for(i in N) {
  function(){
  output1 = catch
  output2 = something_else
  }
  output[[i]] <- data.frame("list",N)
  }


ouput <- bind_rows(output)
