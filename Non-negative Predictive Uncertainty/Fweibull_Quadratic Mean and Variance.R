Fweibull<-function(dq,pdvar){

a=0
pdvar

c = 1.1
c1 = c
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dq*dq/g2))**0.5)
fOld = b*b * (g2 - g1**2) - pdvar

c = c1 + 1
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dq*dq/g2))**0.5)
fNew = b*b * (g2 - g1**2) - pdvar

while (fOld*fNew > 0)
{c1 = c;
c = c1 + 1;
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dq*dq/g2))**0.5)
fNew = b*b * (g2 - g1**2) - pdvar}

inc = c - c1;
while (abs(fNew) > 1e-8)
  {
inc = -fNew * inc / (fNew - fOld);
c = c + inc;
fOld = fNew;
g1 = gamma(1 + 1/c);
g2 = gamma(1 + 2/c);
b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dq*dq/g2))**0.5);
fNew =b*b * (g2 - g1**2) - pdvar;
}
abc<-cbind(a,b,c)
abc
}
