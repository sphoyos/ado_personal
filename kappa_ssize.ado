
cap program drop kappa_ssize

*! version 0.1 - sph- 04/11/13
*! Program to calculate sample size  for kappa concordance index (based on N.cohen.kappa.R 
*! syntax [if] , 	
*! rate1(real) Prevalence detected by measure 1 
*! rate2(real) Prevalence detected by meaure 2
*!kappa(real) Kappa observed
*! hokappa(real)  Kappa under Ho (0.70)
*! [power(real .80)]  Power 
*!  [alpha(real .05)] Type error I
*!  [NOTwosided]  Not Two sided test


program define kappa_ssize, rclass
 version 11.2

 syntax [if] , rate1(real) rate2(real) kappa(real) [hokappa(real 0.70)] [power(real .80)]   [alpha(real .05)]  [TWOsided]
  
   if "`TWOsided'" =="" {
	local d=1
	}
	else {
	local d=2
	}

 local pi2_=1-`rate1'
 local pi_2=1-`rate2'
 local pie=`rate1'*`rate2'+`pi2_'*`pi_2'
 local pi0= `kappa'*(1-`pie')+`pie'
 local pi22=(`pi0'-`rate1'+`pi_2')/2
 local pi11=`pi0'-`pi22'
 local pi12=`rate1'-`pi11'
 local pi21=`rate2'-`pi11'
 local pi0_h= `hokappa'*(1-`pie')+`pie'
 local pi22_h=(`pi0_h'-`rate1'+`pi_2')/2
 local pi11_h=`pi0_h'-`pi22_h'
 local pi12_h=`rate1'-`pi11_h'
 local pi21_h=`rate2'-`pi11_h'
 local Q=(1-`pie')^(-4)*(`pi11'*(1-`pie'-(`rate2'+`rate1')*(1-`pi0'))^2+ ///
  `pi22'*(1-`pie'-(`pi_2'+`pi2_')*(1-`pi0'))^2+(1-`pi0')^2*(`pi12' *(`rate2'+`pi2_')^2+ ///
  `pi12'*(`pi_2'+`rate1')^2)-(`pi0'*`pie'-2*`pie'+`pi0')^2) 
 local Q_h=(1-`pie')^(-4)*(`pi11_h'*(1-`pie'-(`rate2'+`rate1')*(1-`pi0_h'))^2+ /// 
  `pi22_h'*(1-`pie'-(`pi_2'+`pi2_')*(1-`pi0_h'))^2+(1-`pi0_h')^2*(`pi12_h'*(`rate2'+`pi2_')^2+ ///
  `pi12_h'*(`pi_2'+`rate1')^2)-(`pi0_h'*`pie'-2*`pie'+`pi0_h')^2) 
 local Nsize=((invnorm(1-`alpha'/`d')*sqrt(`Q_h')+invnorm(`power')*sqrt(`Q'))/(`kappa'-`hokappa'))^2

 disp in yellow "Tamaño muestral para estudio de Kappa"
 disp in yellow "______________________________________"
 disp in yellow "Prevalencias"
 disp in yellow "Poblacion 1=" `rate1'
 disp in yellow "Poblacion 2=" `rate2'
 disp in yellow "Kappa en H0= " `hokappa'
 disp in yellow "Kappa esperado=" `kappa'  
 disp in yellow "N= " %6.1f `Nsize'
  ret scalar Nsize= `Nsize'
 end
  
  
  

  
  