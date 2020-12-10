Fweibull<-function(dm,pdvar){

a=0
pdvar

c = 0.1
c1 = c
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
# b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dm*dm/g2))**0.5)
b = (dm-a)/g1
fOld = b*b * (g2 - g1**2) - pdvar

c = c1 + 1
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
# b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dm*dm/g2))**0.5)
b = (dm-a)/g1
fNew = b*b * (g2 - g1**2) - pdvar
fOld2 = fNew

while (fOld*fNew > 0)
{
c1 = c
c = c1 + 1
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
# b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dm*dm/g2))**0.5)
b = (dm-a)/g1
fNew = b*b * (g2 - g1**2) - pdvar
# print(fNew)
}

inc = c - c1;
while (abs(fNew) > 1e-8)
  {
inc = -fNew * inc / (fNew - fOld)
if(inc>1) inc=0.1
if(inc < (-1)) inc=-0.1
if((c + inc) >0) {c = c + inc} else {c=c/2}
fOld = fNew
g1 = gamma(1 + 1/c)
g2 = gamma(1 + 2/c)
# b = ((-a*g1)/g2) + ((((a/g2)**2)*(g2*g2-g1) + (dm*dm/g2))**0.5);
b = (dm-a)/g1
fNew =b*b * (g2 - g1**2) - pdvar
# print(fOld)
# print(fNew)
# print(c)
}
abc<-cbind(a,b,c)
abc
}
