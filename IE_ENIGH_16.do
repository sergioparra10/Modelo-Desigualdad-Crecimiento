*********************************************
*Tesis: Desigualdad de  oportunidades y crecimiento economico en Mexico
*AUTOR: Sergio Eduardo Parra Pliego
*CIDE
*********************************************

*BASE DE DATOS: ENIGH 2016


*Índice Educación**
*************



use "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/poblacion.dta",clear 


drop if parentesco>"400" & parentesco<"500"
drop if parentesco>"700" & parentesco<="715"

**Asistencia Escolar**

destring asis_esc, replace
gen AsisEsc=1 if asis_esc==1 
replace AsisEsc=0 if asis_esc==2


**Escolaridad esperada**
*********************************

destring nivelaprob gradoaprob antec_esc, replace

gen Esc=. if nivelaprob<1
replace Esc=0 if nivelaprob==0
replace Esc=0 if nivelaprob==1
**Primaria Secundaria y Prepa**
replace Esc=1 if nivelaprob==2 & gradoaprob==1
replace Esc=2 if nivelaprob==2 & gradoaprob==2
replace Esc=3 if nivelaprob==2 & gradoaprob==3
replace Esc=4 if nivelaprob==2 & gradoaprob==4
replace Esc=5 if nivelaprob==2 & gradoaprob==5
replace Esc=6 if nivelaprob==2 & gradoaprob>=6
replace Esc=6 if nivelaprob==3 & gradoaprob==0
replace Esc=7 if nivelaprob==3 & gradoaprob==1
replace Esc=8 if nivelaprob==3 & gradoaprob==2
replace Esc=9 if nivelaprob==3 & gradoaprob>=3
replace Esc=6 if nivelaprob==6 & gradoaprob>=1 & antec_esc==.
replace Esc=10 if nivelaprob==4 & gradoaprob==0
replace Esc=10 if nivelaprob==4 & gradoaprob==1
replace Esc=11 if nivelaprob==4 & gradoaprob==2
replace Esc=12 if nivelaprob==4 & gradoaprob>=3


**5 Normal. Antecedentes: 2 sec, 3 prepa. Grados Aprobados:1,2,3,4
***M·ximo 4
replace Esc=6 if nivelaprob==5 & antec_esc==1 & gradoaprob==0
replace Esc=7 if nivelaprob==5 & antec_esc==1 & gradoaprob==1
replace Esc=8 if nivelaprob==5 & antec_esc==1 & gradoaprob==2
replace Esc=9 if nivelaprob==5 & antec_esc==1 & gradoaprob==3
replace Esc=10 if nivelaprob==5 & antec_esc==1 & gradoaprob>=4
replace Esc=9 if nivelaprob==5 & antec_esc==2 & gradoaprob==0
replace Esc=10 if nivelaprob==5 & antec_esc==2 & gradoaprob==1
replace Esc=11 if nivelaprob==5 & antec_esc==2 & gradoaprob==2
replace Esc=12 if nivelaprob==5 & antec_esc==2 & gradoaprob==3
replace Esc=13 if nivelaprob==5 & antec_esc==2 & gradoaprob>=4
replace Esc=12 if nivelaprob==5 & antec_esc==3 & gradoaprob==0
replace Esc=13 if nivelaprob==5 & antec_esc==3 & gradoaprob==1
replace Esc=14 if nivelaprob==5 & antec_esc==3 & gradoaprob==2
replace Esc=15 if nivelaprob==5 & antec_esc==3 & gradoaprob==3
replace Esc=16 if nivelaprob==5 & antec_esc==3 & gradoaprob>=4


***Hay dos casos que tienen licenciatura + normal se topo en 18 años
replace Esc=18 if nivelaprob==5 & antec_esc==4

**6 Carrera Tecnica. Antecedentes: 1 prim, 2 sec, 3 prepa. Grados Aprobados:1,2,3,4,5,6****
***M·ximo 3

replace Esc=6+gradoaprob if nivelaprob==6 & antec_esc==1
replace Esc=9 if nivelaprob==6 & antec_esc==1 & gradoaprob>3
replace Esc=9+gradoaprob if nivelaprob==6 & antec_esc==2
replace Esc=12 if nivelaprob==6 & antec_esc==2 & gradoaprob>=3
replace Esc=12 if nivelaprob==6 & antec_esc==3 & gradoaprob==0
replace Esc=13 if nivelaprob==6 & antec_esc==3 & gradoaprob==1
replace Esc=14 if nivelaprob==6 & antec_esc==3 & gradoaprob==2
replace Esc=15 if nivelaprob==6 & antec_esc==3 & gradoaprob>=3
replace Esc=17 if nivelaprob==6 & antec_esc>=4

**7 Profesional. Antecedentes: 3 prepa. Grados Aprobados:1,2,3,4,5,6**
replace Esc=12 if nivelaprob==7 & gradoaprob==0
replace Esc=13 if nivelaprob==7 & gradoaprob==1
replace Esc=14 if nivelaprob==7 & gradoaprob==2
replace Esc=15 if nivelaprob==7 & gradoaprob==3
replace Esc=16 if nivelaprob==7 & gradoaprob==4
replace Esc=17 if nivelaprob==7 & gradoaprob>=5
replace Esc=17 if nivelaprob==7 & antec_esc==3 & gradoaprob>5

**8 Maestria. Antecedentes: 4 Licenciatura. Grados Aprobados:1,2,3,4,5,6**

replace Esc=17 if nivelaprob==8 & antec_esc==4 & gradoaprob==0
replace Esc=18 if nivelaprob==8 & gradoaprob==1
replace Esc=18 if nivelaprob==9 &  gradoaprob==1
replace Esc=19 if nivelaprob==9 &  gradoaprob==2
replace Esc=19 if nivelaprob==8  & gradoaprob>=2

**9 Doctorado. Antecedentes: 4 Licenciatura, 5 maestria. Grados Aprobados:1,2,3,4,5,6**
replace Esc=19 if nivelaprob==9 & gradoaprob==0
replace Esc=20 if nivelaprob==9 & gradoaprob==1
replace Esc=21 if nivelaprob==9 & gradoaprob==2
replace Esc=22 if nivelaprob==9 & gradoaprob==3
replace Esc=22 if nivelaprob==9 & gradoaprob==4
replace Esc=22 if nivelaprob==9 & gradoaprob>=5




gen norm=1 if edad==6
replace norm=2 if edad==7
replace norm=3 if edad==8
replace norm=4 if edad==9
replace norm=5 if edad==10
replace norm=6 if edad==11
replace norm=7 if edad==12
replace norm=8 if edad==13
replace norm=9 if edad==14
replace norm=10 if edad==15
replace norm=11 if edad==16
replace norm=12 if edad==17
replace norm=13 if edad==18
replace norm=14 if edad==19
replace norm=15 if edad==20
replace norm=16 if edad==21
replace norm=17 if edad==22
replace norm=18 if edad==23
replace norm=18 if edad==24
replace norm=15 if edad>=25

replace Esc=. if edad<=5

*Generamos escolaridad esperada

gen esc_esp=.
replace esc_esp=Esc+AsisEsc 
replace esc_esp=22 if nivelaprob==9 & gradoaprob>=3

***Casos especiales
*replace esc_esp=AsisEsc if  & nivelaprob==.


*replace esc_esp=Esc if edad>24


**Generamos la tasa de esc dividiendo la esccolaridad entre la norma**
gen IE=esc_esp/norm if edad>=6
replace IE=1 if IE>1 & IE!=.

**Asignamos a los menores de 6 el indice promedio del hogar**
sort  folioviv foliohog
gen folio=folioviv+foliohog
sort folio
by folio: egen MediaIE=mean(IE)

replace IE=MediaIE if edad<6

label var AsisEsc "Asistencia escolar"
label var Esc "Escolaridad mayores 25 aÒos"
label var norm "Norma"
label var esc_esp "Escolaridad esperada"
label var MediaIE "media IE"
label var IE "Õndice de educaciÛn"

gen sex=0 if sexo=="1"
replace sex=1 if sexo=="2"

drop sexo
rename sex sexo
sort folioviv foliohog numren

save "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/IE_ENIGH_16.dta", replace

*base de hijos
 

use "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/IE_ENIGH_16.dta", clear 

keep if madre_hog=="1" | padre_hog=="1" 

gen IDp= folioviv+foliohog+padre_id 

gen IDm= folioviv+foliohog+madre_id 



rename * *hij

rename foliovivhij folioviv

renam foliohoghij foliohog

rename IDphij IDp

rename IDmhij IDm




save "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_hijos_mf.dta", replace 
 

use "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/IE_ENIGH_16.dta", clear
*base de padres /*nos quedamos con los hombres*/

keep if sexo==0 

tostring numren, replace


gen leng=strlen(numren)

replace numren="0" + numren if leng==1

* generamos identificadores unicos de hogar*/
gen IDp = folioviv +  foliohog + numren 

rename * *pad
gen padre=1
rename foliovivpad folioviv

renam foliohogpad foliohog

rename IDppad IDp


 
save "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_padre_mf.dta", replace 
 *base de madres
 

use "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/IE_ENIGH_16.dta", clear
 keep if sexo==1


tostring numren, replace


gen leng=strlen(numren)

replace numren="0" + numren if leng==1


gen IDm = folioviv +  foliohog + numren /* generamos identificadores unicos de hogar*/

rename * *mad
gen madre=1

rename foliovivmad folioviv

renam foliohogmad foliohog

rename IDmmad IDm


save "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_madre_mf.dta", replace 


use  "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_madre_mf.dta", clear 






*merges

 
 use "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_hijos_mf.dta", clear
 
merge m:1 folioviv foliohog IDp using "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_padre_mf.dta", gen(mergep)
 
 
 

    *Result                           # of obs.
*    *-----------------------------------------
 *   not matched                       114,495
 *      from master                    33,294  (mergep==1)
  *      from using                     81,201  (mergep==2)

*    matched                            91,409  (mergep==3)


merge m:1 folioviv foliohog IDm using "/Users/sergioparra/Desktop/Tesis/Bases de datos/MCS 2016/base_madre_mf.dta", gen(mergem)
*    -----------------------------------------

* merge m:1 folioviv foliohog IDm using `madres', gen(mergem) 

*    Result                           # of obs.
 *   -----------------------------------------
  *  not matched                       157,617
   *     from master                    86,712  (mergem==1)
    *    from using                     70,905  (mergem==2)

   * matched                           119,192  (mergem==3)
    *-----------------------------------------

drop if mergep==2  
drop if mergem==2 

*********************************************************************************


