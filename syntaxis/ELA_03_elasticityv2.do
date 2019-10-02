********************************************************************************
* @Desc: Initial regressions for estimating the intensity
* @Author: Paul Rodriguez
********************************************************************************

if "`c(username)'"=="paul.rodriguez" {
	glo dropbox="F:\Paul.Rodriguez\Dropbox\tabaco"
}
else if "`c(username)'"=="andro" {
	glo dropbox="C:\Users\andro\Dropbox\tabaco"
}
else {
	glo dropbox="C:\Users\\`c(username)'\Dropbox"
	glo bases ="$dropbox\Tabaco y Enfermedades Respiratorias (1)\Bases de Datos"
	glo output="$dropbox\Tabaco y Enfermedades Respiratorias (1)\output"	
}

glo bases ="$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos"
glo output="$dropbox\Tabaco y Enfermedades Respiratorias\output"


* *******************************************************************************
* A program for doing linear test but adding the results to the estimate store
* test: The code to be tested
* reg: The name of the est store
* name: The name of e(xx) scalar macro in the est store

cap program drop get_lincomest
program define 	get_lincomest 
	syntax , test(string) reg(string) name(string)
	est store `reg'
	lincomest `test'
	mat A=r(table)
	est restore `reg'
	estadd scalar `name' = A[1,1]
	estadd scalar `name'_p = A[4,1] 	
	est store `reg'
end

* *******************************************************************************
* Let's do a regression! That is, get the elasticity

*cd "$output\log"
*cap log close
*log using "ELA_02_elasticity.smcl", replace
*set linesize 255
*log off


use "$bases\Main\ENCSP_PanelTabaco.dta" , clear
drop _merge
merge n:1 year depart using "$bases\IPC\Annual_Tobacco_prices1.dta"
drop if _merge==2
drop _merge



gen ipc_cigr=ipc_tabaco/ipc
gen ipc_alir=ipc_alcohol/ipc
gen ipc_foir=ipc_alimentos/ipc

gen ipc_cigr15=ipc_tabaco15/ipc15
gen ipc_alir15=ipc_alcohol15/ipc15
gen ipc_foir15=ipc_alimentos15/ipc15

foreach varDep in cig ipc ipc_tabaco ipc_alimentos ipc_alcohol ipc_cigr ipc_alir ipc_foir ipc15 ipc_tabaco15 ipc_alimentos15 ipc_alcohol15 ipc_cigr15 ipc_alir15 ipc_foir15 p_cig {
	gen l`varDep'=ln(`varDep')
}
   
gen year13= (year==2013)

label define ldept 5 "ANTIOQUIA"  8 "ATLANTICO"  13 "BOLIVAR"  15 "BOYACA"  17 "CALDAS"  18 "CAQUETA"  19 "CAUCA"  20 "CESAR"  23 "CORDOBA"  25 "CUNDINAMARCA"  27 "CHOCO"  41 "HUILA"  44 "LA GUAJIRA"  47 "MAGDALENA"  50 "META"  52 "NARINO"  54 "NORTE DE SANTANDER"  63 "QUINDIO"  66 "RISARALDA"  68 "SANTANDER"  70 "SUCRE"  73 "TOLIMA"  76 "VALLE DEL CAUCA"  81 "ARAUCA"  85 "CASANARE"  86 "PUTUMAYO"  91 "AMAZONAS"  94 "GUAINIA"  95 "GUAVIARE"  97 "VAUPES"  99 "VICHADA"  88 "SAN ANDRES Y PROVIDENCIA"  11 "BOGOTA D.C." ,replace
label val depart ldept


gen cigZ=cig
	replace cigZ=0 if smokenP==0
	label var cigZ "Number of cigs. per-week including 0 for non-smokers"

gen cons= (cig>14) if cig!=.

gen ipc2008= ipc if year==2008
bys depart: egen ipc2008x=max(ipc2008) 
gen deflactor=ipc/ipc2008x

replace p_cig = p_cig/(deflactor*10)
label var p_cig "Real price in 100s of pesos"

********************************************************************************
* Macros' definition

glo fex="[pw=exp]"
glo fex=""

recode sexo (1=1 "Male") (2=0 "Female"), g(sexo1)
drop sexo 
rename sexo1 sexo

recode educ (0=0 "Less than primary") (1=1  "Primary") (2=2 "Secondary")(3/4=3 "Tertiary"), g(educ1)
recode civil (0=0 "Single")(1=1 "Married") (2=0), g(civil1)
drop civil educ 
rename educ1 educ 
rename civil1 civil

gen monthyear=year*100+month
		
glo controls="i.monthyear i.estratoSES i.sexo i.edad2 i.educ i.civil i.ocupa"	
	
foreach var in alcohol marijuana cocaine calming basuco inhalable ecstasy {
tab `var'Ever `var'P
}

// Años que lleva fumando
gen init_smoke=edad-smokStartAge if edad!=. & smokStartAge!=.

gen init_3=(init_smoke<4 & year13==1) if init_smoke!=.

gen 	cess_3=0 if init_smoke!=.
replace cess_3=1 if smokeEver==1 & init_smoke>3 & init_smoke!=. & smoken12==0 & year13==1


	
	recode edad (0/25=1 "10-25") (26/44=2 "26-44") (45/65=3 "45-65"), g(grupo_edad1)
	recode estratoSES (1/2=1 "1-2")(3=2 "3")(4/6=3 "4-6"), g(estrato_)
	
	
	gen joven = (edad<25) if edad!=.
	gen viejo = 1-joven if edad!=.
	
	gen male = sexo
	gen female1 = 1-sexo		
	
	
	tab grupo_edad1, g(juv_)
	
	gen pcigXjoven = p_cig*juv_1
	gen pcigXadulto = p_cig*juv_2
	gen pcigXviejo = p_cig*juv_3
	

	gen pcigXmale = p_cig*male
	gen pcigXfemale = p_cig*female
	
	tab estrato_ , g(est_)
	
	gen pcigXest1 = p_cig*est_1
	gen pcigXest2 = p_cig*est_2
	gen pcigXest3 = p_cig*est_3	

	gen mesano=year*100+month	
	
********************************************************************************
* Margen extensivo con efectos fijos de depto
********************************************************************************
if 1==0 {
	*glo fex="[pw=exp]"
	glo fex=""
				
	glo controls="i.municipi i.mesano i.estrato_ i.sexo i.grupo_edad i.educ jefeH i.ocupa civil alcoholP marijuanaEver   "
		
	* Prevalence estimates for the elasticities ..........
	sum p_cig if year==2008  [iw=exp]
	glo precio = r(mean)
	
	sum smokenP if year==2008  [iw=exp]
	glo prev_g= r(mean)
	sum smokenP if year==2008 & male==1 [iw=exp]
	glo prev_m= r(mean)
	sum smokenP if year==2008 & male==0 [iw=exp]
	glo prev_f= r(mean)
	sum smokenP if year==2008 & juv_1==1 [iw=exp]
	glo prev_age1= r(mean)
	sum smokenP if year==2008 & juv_2==1 [iw=exp]
	glo prev_age2= r(mean)
	sum smokenP if year==2008 & juv_3==1 [iw=exp]
	glo prev_age3= r(mean)
	sum smokenP if year==2008 & estrato_==1 [iw=exp]
	glo prev_est1= r(mean)	
	sum smokenP if year==2008 & estrato_==2 [iw=exp]
	glo prev_est2= r(mean)
	sum smokenP if year==2008 & estrato_==3	[iw=exp]
	glo prev_est3= r(mean)
	
	sum smokenP if year==2008 & juv_1==1 [iw=exp]
	glo prev_in_g= r(mean)
	sum smokenP if year==2008 & male==1 & juv_1==1 [iw=exp]
	glo prev_in_m= r(mean)
	sum smokenP if year==2008 & male==0 & juv_1==1 [iw=exp]
	glo prev_in_f= r(mean)
	sum smokenP if year==2008 & estrato_==1 & juv_1==1 [iw=exp]
	glo prev_in_est1= r(mean)	
	sum smokenP if year==2008 & estrato_==2 & juv_1==1 [iw=exp]
	glo prev_in_est2= r(mean)
	sum smokenP if year==2008 & estrato_==3	& juv_1==1 [iw=exp]
	glo prev_in_est3= r(mean)
	
	sum smokenP if year==2008 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_g= r(mean)
	sum smokenP if year==2008 & male==1 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_m= r(mean)
	sum smokenP if year==2008 & male==0 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_f= r(mean)
	sum smokenP if year==2008 & juv_2==1 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_age2= r(mean)
	sum smokenP if year==2008 & juv_3==1 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_age3= r(mean)
	sum smokenP if year==2008 & estrato_==1 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_est1= r(mean)	
	sum smokenP if year==2008 & estrato_==2 & init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_est2= r(mean)
	sum smokenP if year==2008 & estrato_==3	& init_smoke>=5 & edad>25 & smokeEver==1 [iw=exp]
	glo prev_cess_est3= r(mean)
	
	
	* ....................................................	
	
		logit smokenP p_cig  i.depart i.year $fex, r
		margins, dydx(p_cig)  post
		get_lincomest , reg(r2) test( _b[p_cig] *($precio/$prev_g)) name(pe_base)
			
		logit smokenP p_cig  $controls $fex , r
		margins, dydx(p_cig) post
		get_lincomest , reg(r3) test( _b[p_cig] *($precio/$prev_g)) name(pe_g)

	
		probit smokenP p_cig $controls  $fex, r
		margins, dydx(p_cig) post		
		get_lincomest , reg(r4) test( _b[p_cig] *($precio/$prev_g)) name(pe_prob)		

		
		reg smokenP p_cig  $controls $fex, r
		margins, dydx(p_cig) post
		get_lincomest , reg(r5) test( _b[p_cig] *($precio/$prev_g)) name(pe_lpm)
		
		logit smokenP pcigXjoven pcigXadulto pcigXviejo $controls  $fex, r
		margins, dydx(pcigXjoven pcigXadulto pcigXviejo ) post
		get_lincomest , reg(r6) test("_b[pcigXjoven]*($precio/$prev_age1)-_b[pcigXadulto]*($precio/$prev_age2)") name(test1)
		get_lincomest , reg(r6) test("_b[pcigXadulto]*($precio/$prev_age2)-_b[pcigXviejo]*($precio/$prev_age3)") name(test2)	
		get_lincomest , reg(r6) test(" _b[pcigXjoven] *($precio/$prev_age1)") name(pe_age1)
		get_lincomest , reg(r6) test(" _b[pcigXadulto]*($precio/$prev_age2)") name(pe_age2)
		get_lincomest , reg(r6) test(" _b[pcigXviejo] *($precio/$prev_age3)") name(pe_age3)
		
		logit smokenP pcigXmale pcigXfemale $controls $fex, r
		margins, dydx(pcigXmale pcigXfemale  ) post
		get_lincomest , reg(r7) test("_b[pcigXmale]*($precio/$prev_m)-_b[pcigXfemale]*($precio/$prev_f)")  name(test5)
		get_lincomest , reg(r7) test(" _b[pcigXmale]*($precio/$prev_m)")    name(pe_m)
		get_lincomest , reg(r7) test(" _b[pcigXfemale] *($precio/$prev_f)") name(pe_f)		

		logit smokenP pcigXest1 pcigXest2 pcigXest3 $controls  $fex , r
		margins, dydx(pcigXest1 pcigXest2 pcigXest3 ) post
		get_lincomest , reg(r8) test("_b[pcigXest1]*($precio/$prev_est1)-_b[pcigXest2]*($precio/$prev_est2)") name(test3)
		get_lincomest , reg(r8) test("_b[pcigXest2]*($precio/$prev_est2)-_b[pcigXest3]*($precio/$prev_est3)") name(test4)	
		get_lincomest , reg(r8) test(" _b[pcigXest1]*($precio/$prev_est1)") name(pe_est1)
		get_lincomest , reg(r8) test(" _b[pcigXest2]*($precio/$prev_est2)") name(pe_est2)
		get_lincomest , reg(r8) test(" _b[pcigXest3]*($precio/$prev_est3)") name(pe_est3)
		
		
		*log on
		*saveresults extensiveMargin.pdf, replace: 
		esttab r2 r3 r6 r7 r8 using "$output/tables/tableME.csv", star(* 0.1 ** 0.05 *** 0.001) ///
			stats(	N test1 test1_p test2 test2_p test5 test5_p test3 test3_p test4 test4_p   ///
					pe_g pe_g_p pe_age1 pe_age1_p pe_age2 pe_age2_p pe_age3 pe_age3_p pe_m pe_m_p pe_f pe_f_p pe_est1 pe_est1_p pe_est2 pe_est2_p pe_est3 pe_est3_p ///
			) se keep(p_cig pcigXjoven pcigXadulto pcigXviejo pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3 )  csv replace
		*log off
		
		** INITIATION
		glo conda = "if (init_smoke==. | init_smoke<=5) & edad<=25"
		
		logit smokenP p_cig  $controls $fex  $conda , r
		margins, dydx(p_cig) post
		get_lincomest , reg(r3_in) test( _b[p_cig] *($precio/$prev_g)) name(pe_g)
		
		logit smokenP pcigXmale pcigXfemale  $controls  $fex $conda , r
		margins, dydx(pcigXmale pcigXfemale) post
		get_lincomest , reg(r7_in) test("_b[pcigXmale]  *($precio/$prev_in_m)-_b[pcigXfemale]*($precio/$prev_in_f)")  name(test5)
		get_lincomest , reg(r7_in) test("_b[pcigXmale]  *($precio/$prev_in_m)")    name(pe_m)
		get_lincomest , reg(r7_in) test("_b[pcigXfemale]*($precio/$prev_in_f)") name(pe_f)			
				
		logit smokenP pcigXest1 pcigXest2 pcigXest3 $controls  $fex $conda , r
		margins, dydx(pcigXest1 pcigXest2 pcigXest3  ) post
		get_lincomest , reg(r8_in) test("_b[pcigXest1] *($precio/$prev_in_est1)-_b[pcigXest2]*($precio/$prev_in_est2)") name(test3)
		get_lincomest , reg(r8_in) test("_b[pcigXest2] *($precio/$prev_in_est2)-_b[pcigXest3]*($precio/$prev_in_est3)") name(test4)	
		get_lincomest , reg(r8_in) test(" _b[pcigXest1]*($precio/$prev_in_est1)") name(pe_est1)
		get_lincomest , reg(r8_in) test(" _b[pcigXest2]*($precio/$prev_in_est2)") name(pe_est2)
		get_lincomest , reg(r8_in) test(" _b[pcigXest3]*($precio/$prev_in_est3)") name(pe_est3)
				
		esttab r3_in r7_in r8_in using "$output/tables/tableME_in.csv", star(* 0.1 ** 0.05 *** 0.001) ///
			stats(	N test5 test5_p test3 test3_p test4 test4_p   ///
					pe_g pe_g_p pe_m pe_m_p pe_f pe_f_p pe_est1 pe_est1_p pe_est2 pe_est2_p pe_est3 pe_est3_p ///
			) se keep(p_cig pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3 )  csv replace		
		

		** CESSATION
		glo controlsC ="$controls  smokStartAge " 
		glo conda = "if init_smoke>=5 & edad>25 & smokeEver==1"
		
		logit smokenP p_cig  $controls $fex  $conda , r
		margins, dydx(p_cig) post
		get_lincomest , reg(r3_cess) test( _b[p_cig] *($precio/$prev_cess_g)) name(pe_g)		
		
		logit smokenP pcigXadulto pcigXviejo $controlsC  $fex $conda, r
		margins, dydx(pcigXadulto pcigXviejo  ) post
		get_lincomest , reg(r6_cess) test("_b[pcigXadulto] *($precio/$prev_cess_age2)-_b[pcigXviejo]*($precio/$prev_cess_age3)") name(test2)	
		get_lincomest , reg(r6_cess) test(" _b[pcigXadulto]*($precio/$prev_cess_age2)") name(pe_age2)
		get_lincomest , reg(r6_cess) test(" _b[pcigXviejo] *($precio/$prev_cess_age3)") name(pe_age3)				
		
		logit smokenP pcigXmale pcigXfemale  $controlsC  $fex $conda , r
		margins, dydx(pcigXmale pcigXfemale) post
		get_lincomest , reg(r7_cess) test("_b[pcigXmale]*($precio/$prev_cess_m)-_b[pcigXfemale]*($precio/$prev_cess_f)")  name(test5)
		get_lincomest , reg(r7_cess) test(" _b[pcigXmale]*($precio/$prev_cess_m)")    name(pe_m)
		get_lincomest , reg(r7_cess) test(" _b[pcigXfemale] *($precio/$prev_cess_f)") name(pe_f)			
						
		
		logit smokenP pcigXest1 pcigXest2 pcigXest3  $controlsC  $fex $conda , r
		margins, dydx(pcigXest1 pcigXest2 pcigXest3  ) post
		get_lincomest , reg(r8_cess) test("_b[pcigXest1]*($precio/$prev_cess_est1)-_b[pcigXest2]*($precio/$prev_cess_est2)") name(test3)
		get_lincomest , reg(r8_cess) test("_b[pcigXest2]*($precio/$prev_cess_est2)-_b[pcigXest3]*($precio/$prev_cess_est3)") name(test4)	
		get_lincomest , reg(r8_cess) test(" _b[pcigXest1]*($precio/$prev_cess_est1)") name(pe_est1)
		get_lincomest , reg(r8_cess) test(" _b[pcigXest2]*($precio/$prev_cess_est2)") name(pe_est2)
		get_lincomest , reg(r8_cess) test(" _b[pcigXest3]*($precio/$prev_cess_est3)") name(pe_est3)
						
				
		esttab r3_cess r6_cess r7_cess r8_cess using "$output/tables/tableME_cess.csv", star(* 0.1 ** 0.05 *** 0.001) ///
			stats(	N test2 test2_p test5 test5_p test3 test3_p test4 test4_p   ///
					pe_age2 pe_age2_p pe_age3 pe_age3_p pe_g pe_g_p pe_m pe_m_p pe_f pe_f_p pe_est1 pe_est1_p pe_est2 pe_est2_p pe_est3 pe_est3_p ///
			) se keep(p_cig pcigXadulto pcigXviejo pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3 )  csv replace		
		
}




