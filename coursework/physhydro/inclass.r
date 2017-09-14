# air entry pressure
psi.a=35.6# cm
# porosity
n=0.420
# height above water table  
z1=0 # cm (water table)
z2=150 # cm (soil surface)
z=c(psi.a,seq(z1,z2,5)) # cm
z=sort(z,decreasing=T)
b=4.9

# soil moisture content
theta=n*(z/psi.a)^(-1/b)
# upper limit of theta = n
theta[which(theta>n)]=n
plot(theta,z, xlim=range(theta),type="o", ylab="height above water table, cm", xlab="Soil moisture content, theta", main="Moisture Content %\nfrom soil surface (150 cm)\nto water table (0 cm)",xaxt="n", yaxt="n")
abline(h=seq(z1,z2,25), v=seq(round(min(theta),2),round(n,2),0.01),lty="dashed", col="light gray")
axis(1,at=seq(round(min(theta),2),round(n,2),0.01), labels=seq(round(min(theta),2),round(n,2),0.01))
axis(2,at=seq(0,150,25), labels=seq(0,150,25))

psia=35.6 #cm
b=4.9
n=0.420
thetar=0.15
psi=150 # cm
theta=thetar+(n-thetar)*(-psi/psia)^(-1/b)
z=c(0.1,1:150) # cm

thetaz=function(z,n,thetar,b,psia){
  theta=ifelse(z<psia,n,thetar+(n-thetar)*(z/psia)^(-1/b))
}

theta=thetaz(z,n,thetar,b,psia)

plot(theta, z, type="l")
grid()

D=(n-thetar)*(z2-z1)-(n-thetar)/(psia^(-1/b))*(z2^(1-1/b)-z1^(1-1/b))/(1-1/b)
