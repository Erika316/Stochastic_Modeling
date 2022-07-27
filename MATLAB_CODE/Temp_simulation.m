# Simulation process temperature
% The 420 monthly temperature values will be simulated
% That is, from January 1, 1979 to December 12, 2013
% Function is defined
function [F]=Simulacion_Temp(meses,trayectorias)

% Initial values are defined
meses=12
trayectorias=10
% Days for each month.
dias_en_meses=[30 28 31 30 31 30 31 31 30 31 30 31];
% Initial condition.
% Gamma value from the data.
gamma_1=0,0998128652883;
% Sigma value from data.
sigma_gamma=0.07974044143;
% Gamma trend calculated from the data.
tend_gamma=0.152565426;
Temp_inicial=26.3776; % Temperature in January 2009.
% Estimated values in the regression.
phi=1.256038;
a_gamma=0.581502741;
omega=(2*pi)/12;
a=0.161490323;
A=27.385347;
B=0.0008088;
C=-0.688706;
% Contador
count=1;
start=0;
Temp_inicial=26.3776
gamma_2=zeros(trayectorias,1);
F=zeros(trayectorias,10);
F(:,1)=Temp_inicial;
for monthss=1:meses
% Simulating volatility, generating random numbers
% of a normal distribution
    gamma_2=gamma_1+a_gamma*
    (tend_gamma-gamma_1)+sigma_gamma*randn
    (trayectorias,1);
    for j=1:dias_en_meses
    (rem(meses,12)+1*(rem(meses,12)==0))

           count=count+1;

% Approximation by means of the Euler-Maruyama method

            F(:,count)=B+C*omega*cos (omega*(count-1+420)+phi)+F(:,count-1)+
            a*((A+B*(count-1+420)+C*
            sin(omega*(count-1+420)+phi))-
            F(:,count-1))+gamma_2.*randn(trayectorias,1);
    end

gamma_1=gamma_2;

end
plot(F(:,count))
