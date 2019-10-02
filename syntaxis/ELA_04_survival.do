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
}


glo bases ="$dropbox\Tabaco y Enfermedades Respiratorias\Bases de Datos"
glo output="$dropbox\Tabaco y Enfermedades Respiratorias\output"

* *****************************
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

replace p_cig = p_cig/deflactor
drop lp_cig
gen lp_cig=ln(p_cig)


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

********************************************************************************
* Margen extensivo con efectos fijos de depto
********************************************************************************
if 1==0 {
	glo fex="[pw=exp]"
	glo fex=""
	
	gen mesano=year*100+month
	
	glo controls="i.mesano i.estratoSES i.sexo i.edad2 i.educ jefeH i.ocupa civil alcoholP ipc ipc_alimentos " // lipc_alir  

	correl ipc ipc_tabaco ipc_alimentos ipc_alcohol p_cig
	
	recode edad (0/25=4 "10-25") (26/40=3 "26-40") (41/55=2 "41-55") (56/100=1 "56+"), g(grupo_edad)
	recode edad (0/25=3 "10-25") (26/44=2 "26-44") (44/65=1 "44+65"), g(grupo_edad1)
	recode edad (12/25=1 "<25")(26/45=2 "26-45")(46/65=3 "46+"), g(juventud)
	recode estratoSES (1/2=1 "1-2")(3=2 "3")(4/6=3 "4-6"), g(estrato_)
	
	gen joven = (edad<25) if edad!=.
	gen viejo = 1-joven if edad!=.
	
	tab juventud, g(juv_)
	
	gen pcigXjoven = lp_cig*juv_1
	gen pcigXadulto = lp_cig*juv_2
	gen pcigXviejo = lp_cig*juv_3
	
	gen male = sexo
	gen female1 = 1-sexo
	gen pcigXmale = lp_cig*male
	gen pcigXfemale = lp_cig*female1
	
	tab estrato_ , g(est_)
	
	gen pcigXest1 = lp_cig*est_1
	gen pcigXest2 = lp_cig*est_2
	gen pcigXest3 = lp_cig*est_3
	
	
	* I guess this "intensive" margin is completely useless... we should do only the extensive one!
	*  Nope, very bad, incredibly elastic!
		
		logit smokenP lp_cig  i.depart i.mesano $fex, r
		margins, dydx(lp_cig )  post
		est store r2
			
		logit smokenP lp_cig  i.depart $controls $fex , r
		margins, dydx(lp_cig  ) post
		est store r3
	
		probit smokenP lp_cig   i.depart $controls  $fex, r
		margins, dydx(lp_cig  ) post
		est store r4
		
		reg smokenP lp_cig     i.depart $controls $fex, r
		margins, dydx(lp_cig   ) post
		est store r5
		

		
		logit smokenP pcigXjoven pcigXadulto pcigXviejo i.depart $controls  $fex, r
		margins, dydx(pcigXjoven pcigXadulto pcigXviejo ) post
		lincom _b[pcigXjoven]-_b[pcigXadulto]
		estadd scalar test1 = r(estimate)
		estadd scalar test1_p = r(p) 
		lincom _b[pcigXadulto]-_b[pcigXviejo]
		estadd scalar test2 = r(estimate)
		estadd scalar test2_p = r(p) 
		est store r6
		
		logit smokenP pcigXmale pcigXfemale i.depart $controls  $fex, r
		margins, dydx(pcigXmale pcigXfemale  ) post
		lincom _b[pcigXmale]-_b[pcigXfemale]
		estadd scalar test5 = r(estimate)
		estadd scalar test5_p = r(p) 
		est store r7
		
		logit smokenP pcigXest1 pcigXest2 pcigXest3 i.depart $controls  $fex, r
		margins, dydx(pcigXest1 pcigXest2 pcigXest3 ) post
		lincom _b[pcigXest1]-_b[pcigXest2]
		estadd scalar test3 = r(estimate)
		estadd scalar test3_p = r(p) 
		lincom _b[pcigXest2]-_b[pcigXest3]
		estadd scalar test4 = r(estimate)
		estadd scalar test4_p = r(p) 		 
		est store r8

		*log on
		*saveresults extensiveMargin.pdf, replace: 
		esttab r2 r3 r4 r5 r6 r7 r8, star(* 0.1 ** 0.05 *** 0.001) stats(N test1 test1_p test2 test2_p test5 test5_p test3 test3_p test4 test4_p) se keep(lp_cig pcigXjoven pcigXadulto pcigXviejo pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3 ) tex
		*log off
		
		** INITIATION
		glo conda = "if (init_smoke==. | init_smoke<=5) & edad<=25"
		
		logit smokenP lp_cig i.depart $controls $fex $conda , r
		margins, dydx(lp_cig   ) post
		est store r3_in
		
		logit smokenP pcigXmale pcigXfemale  i.depart $controls  $fex $conda , r
		margins, dydx(pcigXmale pcigXfemale   ) post
		lincom _b[pcigXmale]-_b[pcigXfemale]
		estadd scalar test5 = r(estimate)
		estadd scalar test5_p = r(p) 
		est store r7_in
		
		logit smokenP pcigXest1 pcigXest2 pcigXest3  i.depart $controls  $fex $conda , r
		margins, dydx(pcigXest1 pcigXest2 pcigXest3  ) post
		lincom _b[pcigXest1]-_b[pcigXest2]
		estadd scalar test3 = r(estimate)
		estadd scalar test3_p = r(p) 
		lincom _b[pcigXest2]-_b[pcigXest3]
		estadd scalar test4 = r(estimate)
		estadd scalar test4_p = r(p) 
		est store r8_in
		
		esttab r3_in r7_in r8_in, star(* 0.1 ** 0.05 *** 0.001) stats(N test5 test5_p test3 test3_p test4 test4_p) se keep(lp_cig pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3  ) tex

		** CESSATION
		glo controlsC ="$controls  smokStartAge " 
		glo conda = "if init_smoke>=5 & edad>25 & smokeEver==1"
		
		logit smokenP lp_cig  i.depart $controlsC $fex $conda , r
		margins, dydx(lp_cig   ) post
		est store r3_cess
		
		logit smokenP pcigXadulto pcigXviejo i.depart $controlsC  $fex $conda, r
		margins, dydx(pcigXadulto pcigXviejo  ) post
		lincom _b[pcigXadulto]-_b[pcigXviejo]
		estadd scalar test2 = r(estimate)
		estadd scalar test2_p = r(p) 
		est store r6_cess
		
		logit smokenP pcigXmale pcigXfemale i.depart $controlsC  $fex $conda, r
		margins, dydx(pcigXmale pcigXfemale   ) post
		lincom _b[pcigXmale]-_b[pcigXfemale]
		estadd scalar test5 = r(estimate)
		estadd scalar test5_p = r(p) 
		est store r7_cess
		
		logit smokenP pcigXest1 pcigXest2 pcigXest3 i.depart $controlsC  $fex $conda, r
		margins, dydx(pcigXest1 pcigXest2 pcigXest3  ) post
		lincom _b[pcigXest1]-_b[pcigXest2]
		estadd scalar test3 = r(estimate)
		estadd scalar test3_p = r(p) 
		lincom _b[pcigXest2]-_b[pcigXest3]
		estadd scalar test4 = r(estimate)
		estadd scalar test4_p = r(p) 
		est store r8_cess
		
		esttab r3_cess r6_cess r7_cess r8_cess, star(* 0.1 ** 0.05 *** 0.001) stats(N test2 test2_p test5 test5_p test3 test3_p test4 test4_p) se keep(lp_cig pcigXadulto pcigXviejo pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3  )tex

}


********************************************************************************
* Survival analysis de la edad de inicio (COX)
********************************************************************************

	qui {
	
		preserve // These are smokers...
			collapse (count) fail=smokenP ///
					 (mean) lipc_cigr ipc ipc_cigr lipc_foir lipc_alir lipc_cigr15 ipc15 lipc_foir15 lipc_alir15 p_cig lp_cig ///
					 (max) cons alcoholP marijuanaP cocaineP calmingP basucoP inhalableP ecstasyP ///
					 if smokeEver==1 & edad >10 & edad<25, by(smokStartAge depart year13  month estratoSES sexo educ jefeH ocupa civil)	
			rename smokStartAge edad
			gen censored=0
			tempfile smok
			save `smok'
		restore
		// These are non-smokers...
		collapse (count) censored=smokenP (mean) ipc_cigr lipc_cigr ipc lipc_foir lipc_alir lipc_cigr15 ipc15 lipc_foir15 lipc_alir15 p_cig lp_cig ///
				 (max) cons alcoholP marijuanaP cocaineP calmingP basucoP inhalableP ecstasyP ///
				 if smokeEver==0 & edad >10  & edad<25, by(edad depart year13  month estratoSES sexo educ jefeH ocupa civil)	
		gen fail=0
		
		append using `smok'	
		}
		
		
		recode edad (12/25=1 "<25")(26/50=2 "26-50")(51/65=3 "51+"), g(juventud)
		recode estratoSES (1/2=1 "1-2")(3=2 "3")(4/6=3 "4-6"), g(estrato_)
		
		gen male = sexo
		gen female1 = 1-sexo
		gen pcigXmale = lp_cig*male
		gen pcigXfemale = lp_cig*female1
		
		tab estrato_ , g(est_)
		
		gen pcigXest1 = lp_cig*est_1
		gen pcigXest2 = lp_cig*est_2
		gen pcigXest3 = lp_cig*est_3
	
		sort depart year13 edad 
		ctset edad fail censored, by( depart year13)
		cttost , clear
		stdes // Censored obs: 37%

		gen date= year*100+month
		
		gen ipc_cigG=(lipc_cigr>0.0820922)
		label var ipc_cigG "High price variation"
		
		gen ipc_cigGG=(lipc_cigr>0.2)
		label var ipc_cigGG "High price variation"

		
		gen 	cig_ipc_H=1 if depart==17 | depart==23 | depart==41 |depart==50 |depart==52 |depart==54 |depart==66 |depart==76 
		replace cig_ipc_H=0 if cig_ipc_H==. & depart!=. 
		
		rename cig_ipc_H High_Prices
	
		glo controls="i.date i.estratoSES i.sexo i.educ jefeH i.ocupa civil" // lipc_alir  

	
// Regresiones principales
if 1==1{
		set seed 42
						
		stcox lp_cig  year13 i.depart i.month $controls, hr
		est store rNPar1
		
		stcox lp_cig year13 lipc_foir15 i.depart $controls, hr        // <<<<<<<<<<<<<<<<< Principal
		est store rNPar2
				
		stcurve , survival at1(year13=0)at2(year13=1) legend(label(1 "2008") label(2 "2013"))
		graph export "$output\images\survival_year.pdf", as(pdf) replace
		
		streg lp_cig  year13 lipc_foir15 i.depart $controls, dist(w) hr
		est store rPar
		
		stcox pcigXmale pcigXfemale year13 lipc_foir i.depart $controls, hr        // <<<<<<<<<<<<<<<<< Principal
		est store rNPar3
		
		stcox pcigXest1 pcigXest2 pcigXest3 year13 lipc_foir i.depart $controls, hr        // <<<<<<<<<<<<<<<<< Principal
		est store rNPar4
				
		/*stcox High_Prices year13  lipc_alir15 lipc_foir15 i.depart $controls_time $controls_demo $controls_subs
		est store rNPar5*/
		
		* check proportionality assumptions, etc. In any case, it seems that the effect is 0

		*log on
		disp in red "Cox survival model (age<25)"
		*esttab r0 r1 r2 r3 r4 , star(* 0.1 ** 0.05 *** 0.001) p keep(lipc_cigr ipc lipc_foir lipc_alir year13) ///
		*						mtitle(Baseline DpFE IndCon IndCon IndCon) stats(N N_clust r2_a vif1 )
								
		*esttab r000 r11 r22 r33 r44 , star(* 0.1 ** 0.05 *** 0.001) p keep(cig_ipc_H ipc15 lipc_foir15 lipc_alir15 year13) ///
		*						mtitle(Baseline DpFE IndCon IndConF IndConFA) stats(N N_clust r2_a vif1 )
		esttab  rNPar1 rNPar2 rPar rNPar3 rNPar4, star(* 0.1 ** 0.05 *** 0.001) p keep(lp_cig  lipc_foir15  year13 pcigXmale pcigXfemale pcigXest1 pcigXest2 pcigXest3) stats(N) tex
		
		
		
		*log off	
}
	
/*
stcox lp_cig  lipc_foir15 i.date i.depart i.estratoSES i.sexo, basec(hc0) basehc(h0) basesurv(surv0)		// Me lanza la estimación de las funciones básicas
line hc0 _t, c(J) sort
line surv0 _t, c(J) sort  // Este puede caer a cero ya que se puede reescalar la funcion respecto a las características

gen surv1=surv0^(exp(-2.019226))		// Se puede hacer esto por proporcionalidad

line surv0 surv1 _t, c(J J) sort


line h0 _t, c(J) sort

*Se suaviza la funcion hazard 
stcurve, hazard kernel(gaussian) width(3)
*/

********************************************************************************	

		**********************************************************
					*** HETEROGENEOUS EFFECTS ***
		**********************************************************
		recode educ (0/1=0 "Less than secondary")(2/3=1 "Secondary or more"), g(educ_bi)
		recode ocupa (0=0 "Not working")(1=1 "Working")(2/3=0), g(ocupa_bi)
	
		sts graph, hazard cihazard  by(educ_bi) legend(pos(6))
		sts graph, hazard cihazard  by(civil)	legend(pos(6))	

		sts graph, hazard cihazard  by(sexo) legend(pos(6))
		sts graph, hazard cihazard  by(ocupa_bi) legend(pos(6))

	eststo clear
	// Base 		
	eststo: stcox lp_cig  lipc_foir15 i.depart $controls_time $controls_demo          // <<<<<<<<<<<<<<<<< Principal
	
	// Education 
	** Categorical (base: Less than primary)
	stcox c.lp_cig##i.educ  lipc_foir15 i.depart $controls_time $controls_demo 
	
	** Binary (base: Less than secondary)
	eststo: stcox c.lp_cig##educ_bi  lipc_foir15 i.depart $controls_time i.estratoSES sexo i.ocupa civil

	// Marital Status	
	eststo: stcox c.lp_cig##civil  lipc_foir15 i.depart $controls_time $controls_demo

	// Gender 	
	eststo: stcox c.lp_cig##sexo  lipc_foir15 i.depart $controls_time $controls_demo
	
	// Occupation
	** Categorical (base: Not working)
	stcox c.lp_cig##i.ocupa  lipc_foir15 i.depart $controls_time $controls_demo 
	
	** Binary (base: Less than secondary)
	eststo: stcox c.lp_cig##ocupa_bi  lipc_foir15 i.depart $controls_time i.estratoSES sexo i.educ civil
	
	esttab, se star(* 0.1 ** 0.05 *** 0.001) keep(lp_cig  lipc_foir15 1.educ_bi#c.lp_cig 1.civil#c.lp_cig 1.sexo#c.lp_cig 1.ocupa_bi#c.lp_cig) ///
			stats(N) tex
			
			
			** Reinforcement, peer effects 
			** Usar variables familiares y amigos
			
			** Creencias sobre consumo de tabaco (p24)
	
log close	

translator set smcl2pdf pagesize custom
translator set smcl2pdf pagewidth 9
translate ELA_02_elasticity.smcl ELA_02_elasticity.pdf
