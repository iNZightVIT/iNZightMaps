seq(7,1,by = 2)

n = 3
if(n == 1) 
  {out = c(-1,1)
}else{
  a = c(n,rep(seq(n-2,1)[c(TRUE,FALSE)],each = 2))
  out = c(-a,a[length(a):1])
}
out
rep(x,length(out)) + rep(s.length.x,length(out)) * rep(out,each = length(x))

n = 4
if(n == 2)
{
  out = c(0,2)
}else
{
  a = c(n,rep(((n-2):2)[c(T,F)],each = 2),0)
  out = c(-a,a[length(a):1])
}
rep(x,length(out)) + rep(s.length.x,length(out)) * rep(out,each = length(x))


rep(x,length(out)) + rep(s.length.x,length(out)) * rep(out,each = length(x))


bar.obj = bar.coor(obj = obj,var = var, data = dataIn, xmax = 1, ymax = 5)

