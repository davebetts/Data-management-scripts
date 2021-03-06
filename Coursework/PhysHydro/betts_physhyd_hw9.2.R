# HW 9
# HW9.1
# Assume the following for a snowpack
# Snowpack depth:
hs=1 # m
# Snowpack density:
rho.s=250 # kg/m^3
# Average snow temperature:
Ts=-3 # \degrees C
# Surface albedo:
a=0.6 # proportion
# Air temperature:
Ta=4 # \degrees C
# Air relative humidity:
Wa=0.6 # proportion
# Air emissivity
eps.a=0.9
# Snow emissivity
eps.s=1

# Additional variables
# Water density:
rho.w=1000 # kg/m^3
# Stefan Boltzmann constant
sigma=4.9*10^(-9) # MJ/m^2 day
# Ice heat capacity:
c.i=2102 # J/kg K
# Latent heat of fusion:
lambda.f=0.334 # MJ/kg

# HW9.1.a) Compute the energy required to raise this snowpack to 0o C [MJ m^-2]??????????? ???????????2]
# Area of the snow:
A=1 # m^2
# Mass of the snow:
M=rho.s*hs*A # 250 kg/m^2
# Ending temperature:
Tf=0 # \degree C
# change in temperature:
DeltaT=Tf-Ts # \degree C
# energy to 0
E.1=M*c.i*DeltaT/(10^6) # MJ/m^2
E.1

# HW9.1.b) Compute the energy required for this snowpack to melt completely starting from 0oC [MJ m^-2]
# ?what is the energy in melting?
# latent heat of fusion:
E.2=M*lambda.f # MJ/kg
E.2
# we should see that the energy to melt the snow is MUCH more than the energy to change the temperature of the snow.
# ?(djb) at what degrees does the change in snow temp = the energy to melt?
## part of what this means is that we can be okay with not knowing the exact starting temperature of the snow, b/c the melting process is a much bigger percentage of the energy that needs to go into the system

# HW9.1.c) Use the SolarRad.xls spreadsheet that you worked with in the previous homework to calculate the clear sky radiation on a flat surface at latitude 41 degrees North on April 1.
#Clear sky radiation at the given location: 
Kcs=23.474 # MJ/day

# HW9.1.d) Compute the daily net radiation assuming a snow surface temperature of 0 oC.
# Incoming radiation:
Kin=Kcs # MJ/day
# Net radiation = In - Out
#K=Kin-Kout
K=Kin*(1-a)
K
# HW9.1.e) Considering only radiation (i.e. neglecting any other energy fluxes) calculate how long it would take for this snow to be warmed to 0 oC.
Deltat=E.1/K
daylen=12.538
Deltat=Deltat*daylen

# HW9.1.f) Considering only radiation calculate how long it would take for this snow to melt completely starting from 0 oC. (The answer may be multiple days so the assumption is that days with similar conditions persist.)
Deltat.2=E.2/K
0.89281758541365*daylen
# 11.19 hrs
0.19415*60
# 12min

# HW9.1.g) Repeat (d) to (f) with clear sky radiation for June 1. 
# Use the SolarRad.xls spreadsheet that you worked with in the previous homework to calculate the clear sky radiation on a flat surface at latitude 41 degrees North on June 1.
#Clear sky radiation at the given location: 
Kcs=30.77348095 # MJ/day

# HW9.1.g.d) Compute the daily net radiation assuming a snow surface temperature of 0 oC.
# Incoming radiation:
Kin=Kcs # MJ/day
# Net radiation = In - Out
#K=Kin-Kout
K=Kin*(1-a)
K
# [1] 12.30939
# HW9.1.g.e) Considering only radiation (i.e. neglecting any other energy fluxes) calculate how long it would take for this snow to be warmed to 0 oC.
Deltat=E.1/K
daylen=14.754
Deltat=Deltat*daylen
(Deltat-1)*60
# [1] 53.37528 # min

# HW9.1.f) Considering only radiation calculate how long it would take for this snow to melt completely starting from 0 oC. (The answer may be multiple days so the assumption is that days with similar conditions persist.)
Deltat.2=E.2/K
(Deltat.2-6)*daylen
# [1] 11.55884 # hrs
((Deltat.2-6)*daylen-11)*60
# [1] 33.53065 # min

# HW9.1.h) Comment on the effect of time of year on melt rates, and on the energy required to raise the temperature versus melt the snow.




# now using the SolRadexample.xlsx sheet to calculate K.cs
K.cs=29.38 MJ/m^2/day
# ?2.5 days to equal the energy needed to melt the snow?
# BUT WAIT... not all of the energy goes into melting the snow
# K.cs can be reflected: K.cs(1-A); A=albedo
diagram of reflecting k.cs, L.win, L.wout
A=.6
ksed=(1-0.5)29.38
Lw=\Sigma\sigmaT^4
Lwout=1*4.9*10^-9 * (273.2)^4
=27.297 MJ/m^2 day
Lwin=\epsilon_a\sigma T_a^4
=0.9
1(273.2+4)^4
...
Q.net=Kswnet+Lwin-Lwout

what you'll find is that the raising from -3 to 0 (the ripening) = .15 days
the time to melt = 8.??? days

in the graphs of the swe, we can see the melting just drops off so fast.  the days are getting long,er and the climate conditions are so much more radiation/energy, but there is a preciptous drop off


