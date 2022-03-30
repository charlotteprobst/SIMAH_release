
/////////////////////////////////////////////////////////////////
//First step: code cause of disease (COD) for each mortality data
/////////////////////////////////////////////////////////////////

forvalues year=2000/2020 { 

    use "c:\temp\mort`year'.dta", clear

    keep icd10 year race sex age age_gp edclass R_condition*

  // Unintentional injuries: different from SIMAH codes commented out
  gen uij_temp = 1 if inrange(icd10, "V01", "X599") | inrange(icd10, "Y85", "Y869")
  *gen uij_temp = 1 if inrange(icd10, "V01", "X40") | inrange(icd10, "X43", "X43") ///
   *| inrange(icd10, "X46", "X599")	| inrange(icd10, "Y40", "Y869") ///
   *| inrange(icd10, "Y88", "Y899")
   
   // Motor Vehicle accident: same as SIMAH codes
  gen mvacc = 1 if inrange(icd10, "V02", "V049") | inlist(icd10, "V090", "V092")  ///
	| inrange(icd10, "V12", "V149") | inrange(icd10, "V190", "V192") ///
	| inrange(icd10, "V194", "V196") ///
	| inrange(icd10, "V20", "V799") | inrange(icd10, "V803", "V805") ///
	| inrange(icd10, "V810", "V811") | inrange(icd10, "V820", "V821") ///
	| inrange(icd10, "V83", "V869") | inrange(icd10, "V870", "V878") ///
	| inrange(icd10, "V880", "V888") | inlist(icd10, "V890",  "V892")
   
   // opioid poisoning: SIMAH APC analysis (with and without alcohol)
  gen drug_underly = 1 if inrange(icd10,"X40","X449") |  inrange(icd10,"X60","X649") | ///
     icd10=="X85" | inrange(icd10,"Y10","Y149")
  gen opioid_poi = .
   forvalues i=1/20 {
     replace opioid_poi = 1 if inlist(R_condition_`i',"T400","T401","T402","T403","T404","T406") ///
	   & drug_underly == 1	
   } 
   
 *  /// drug poisoing (excl opioid poisoning)
 *  gen drug_poi = 1 if inrange(icd10,"X40","X459") | inrange(icd10,"Y10","Y149") | ///
 *    inrange(icd10,"Y45","Y459") | inrange(icd10,"Y47","Y479") | inrange(icd10,"Y49","Y499")
 *  replace drug_poi = . if opioid_poi == 1
   
   /// alcohol poisoing (excl opioid poisoning): SIMAH APC analysis 
   /// code alcohol from both underlying and contributing causes here
   /// create the final alc_poi later on accounting for other causes
   gen alc_poi_underly = 1 if inrange(icd10, "X45", "X459") | icd10=="F100"
   gen alc_poi_contrb = .
   forvalues i=1/20 {
 	   replace alc_poi_contrb = 1 if inrange(R_condition_`i', "X45", "X459") | R_condition_`i'=="F100"
   }  
   forvalues i=1/20 {
       replace alc_poi_contrb = 1 if inlist(R_condition_`i',"T510","T519") 	
   }  

   
   /// final unintentional injury, excl MVA and all poisoning 
   gen uij = 1 if uij_temp == 1 & mvacc == . & alc_poi_underly == . & opioid_poi == . 
   drop uij_temp
  
     // Suicide (Intentional self harm) [X60-X84, Y87.0]
   gen sij = 1 if inrange(icd10, "X60", "X849") | inrange(icd10, "Y870", "Y870")
  
     //other injury: following Word Document, only keep one digit after decimal point
   gen othj = 1 if inrange(icd10,"V00","V869") | inrange(icd10,"V872","V873") | ///
	    inrange(icd10,"V882","V883") | inrange(icd10,"V90","V988") /*Transport injuries*/		
   replace othj = 1 if inrange(icd10,"D695","D695") | inrange(icd10,"D701","D702") ///
        | inrange(icd10,"D78","D788") | inrange(icd10,"D898","D898") | icd10=="E032" ///
		| icd10=="E064" | inrange(icd10,"E09","E099") | icd10=="E160" | icd10=="E231" ///
		| icd10=="E242" | icd10=="E273" | inrange(icd10,"E36","E368") | icd10=="E661" ///
		| inrange(icd10,"E860","E879") | inrange(icd10,"E89","E899") | inrange(icd10,"G210","G211") ///
		| inrange(icd10,"G240","G240") | icd10=="G251" | icd10=="G254" | inrange(icd10,"G256","G257") ///
		| icd10=="G620" | icd10=="G720" | icd10=="G937" | icd10=="G960" | icd10=="G961" ///
		| inrange(icd10,"G97","G979") | inrange(icd10,"H028","H028") | inrange(icd10,"H053","H053") ///
		| inrange(icd10,"H054","H055") | inrange(icd10,"H446","H447") | inrange(icd10,"H59","H598") ///
		| inrange(icd10,"H910","H910") | inrange(icd10,"H95","H959") | inrange(icd10,"I952","I958") ///
		| inrange(icd10,"I97","I979") | inrange(icd10,"J70","J705") | inrange(icd10,"J95","J959") ///
		| inrange(icd10,"K085","K085") | inrange(icd10,"K43","K439") | icd10=="K520" | icd10=="K627" ///
		| icd10=="K681" | inrange(icd10,"K91","K919") | inrange(icd10,"K94","K958") | icd10=="L233" ///
		| inrange(icd10,"L270","L271") | inrange(icd10,"L55","L559") | inrange(icd10,"L560","L561") ///
		| inrange(icd10,"L58","L589") | icd10=="L640" | inrange(icd10,"L76","L768") | inrange(icd10,"M102","M102") ///
		| inrange(icd10,"M602","M602") | inrange(icd10,"M871","M871") | inrange(icd10,"M96","M969") ///
		| inrange(icd10,"N14","N144") | inrange(icd10,"N304","N304") | inrange(icd10,"N460","N461") ///
		| inrange(icd10,"N522","N523") | inrange(icd10,"N65","N651") | inrange(icd10,"N99","N999") ///
		| inrange(icd10,"P93","P938") | icd10=="P962" | icd10=="P965" | inrange(icd10,"R502","R508") ///
		| inrange(icd10,"W00","W462") | inrange(icd10,"W49","W629") | inrange(icd10,"W64","W709") ///
		| inrange(icd10,"W73","W819") | inrange(icd10,"W83","W949") | icd10=="W979" | inrange(icd10,"W99","X069") ///
		| inrange(icd10,"X08","X449") | inrange(icd10,"X46","X589") | inrange(icd10,"Y10","Y149") ///
		| inrange(icd10,"Y16","Y199") | inrange(icd10,"Y40","Y849") | inrange(icd10,"Y88","Y883") ///
		| icd10=="Z210" | inrange(icd10,"Z42","Z430") | inrange(icd10,"Z438","Z439") | inrange(icd10,"Z48","Z489") ///
		| inrange(icd10,"Z51","Z519") | inrange(icd10,"Z88","Z889") | inrange(icd10,"Z92","Z940") ///
		| icd10=="Z946" | inrange(icd10,"Z948","Z949") | inrange(icd10,"Z96","Z964") | inrange(icd10,"Z966","Z972") ///
		| inrange(icd10,"Z978","Z991") | inrange(icd10,"Z993","Z999") /*Unintentional injuries*/
   replace othj = 1 if inrange(icd10,"T742","U03") | inrange(icd10,"X60","X649") | inrange(icd10,"X66","Y089") ///
	    | inrange(icd10,"Y35","Y389") | inrange(icd10,"Y870","Y872") | inrange(icd10,"Y890","Y891") ///
		/*Self-harm and interpersonal violence*/
   replace othj = . if inlist(1,uij,mvacc,opioid_poi,alc_poi_underly,sij)

   //COVID-19: U07.1
   gen covid19 = 1 if icd10=="U071"

   //Influenza and pneumonia: J09–J18
   gen flu = 1 if inrange(icd10,"J09","J189")
	 
   //Other infectious diseases (Excluding the infectious diseases listed above)
   gen oth_inf = 1 if inrange(icd10,"A10","A14") | inrange(icd10,"A15","A188") | inrange(icd10,"A19","A199") ///
   | icd10=="A48.1" | icd10=="A70" | inrange(icd10,"B90","B909") | inrange(icd10,"B960","B961") | icd10=="B972" ///
   | inrange(icd10,"B974","B976") | inrange(icd10,"H65","H709") | inrange(icd10,"J00","J069") ///
   | inrange(icd10,"J09","J182") | inrange(icd10,"J188","J189") | inrange(icd10,"J196","J229") ///
   | inrange(icd10,"J36","J360") | icd10=="J851" | icd10=="J910" | icd10=="K673" | icd10=="K930" | icd10=="M490" ///
   | inrange(icd10,"N740","N741") | inrange(icd10,"P23","P239") | icd10=="P370" | inrange(icd10,"U04","U049") ///
   | icd10=="U843" | icd10=="Z030" | icd10=="Z111" | icd10=="Z201" | icd10=="Z232" | icd10=="Z251" ///
   /*Respiratory infections and tuberculosis*/
   replace oth_inf = 1 if inrange(icd10,"A00","A088") | icd10=="A09" | inrange(icd10,"A80","A809") ///
   | icd10=="B91" | icd10=="K521" | icd10=="Z110" | inrange(icd10,"Z200","Z200") | icd10=="Z221" | icd10=="Z230" ///
   /*Enteric infections*/
   replace oth_inf = 1 if inrange(icd10,"A30","A309") | inrange(icd10,"A68","A689") | inrange(icd10,"A692","A692") ///
   | inrange(icd10,"A698","A699") | inrange(icd10,"A71","A719") | icd10=="A740" | inrange(icd10,"A75","A759") ///
   | inrange(icd10,"A77","A799") | inrange(icd10,"A82","A829") | inrange(icd10,"A90","A910") ///
   | inrange(icd10,"A92","A969") | inrange(icd10,"A98","A990") | inrange(icd10,"B330","B331") ///
   | inrange(icd10,"B50","B500") | inrange(icd10,"B508","B520") | inrange(icd10,"B528","B531") ///
   | inrange(icd10,"B538","B575") | inrange(icd10,"B60","B608") | inrange(icd10,"B64","B839") ///
   | icd10=="B89" | icd10=="B92" | icd10=="B940" | icd10=="K931" | icd10=="P371" | inrange(icd10,"P373","P374") ///
   | inrange(icd10,"U06","U069") | icd10=="Z116" | icd10=="Z203" | inrange(icd10,"Z242","Z243") | icd10=="Z260" ///
   /*Neglected tropical diseases and malaria*/
   replace oth_inf = 1 if inrange(icd10,"A20","A289") | inrange(icd10,"A31","A399") | inrange(icd10,"A42","A449") ///
   | inrange(icd10,"A48","A480") | inrange(icd10,"A482","A499") | inrange(icd10,"A65","A650") ///
   | inrange(icd10,"A69","A691") | icd10=="A74" | inrange(icd10,"A748","A749") | inrange(icd10,"A81","A819") ///
   | inrange(icd10,"A83","A852") | inrange(icd10,"A858","A860") | inrange(icd10,"A87","A890") ///
   | inrange(icd10,"B00","B069") | inrange(icd10,"B10","B108") | inrange(icd10,"B15","B199") ///
   | inrange(icd10,"B25","B279") | icd10=="B33" | inrange(icd10,"B333","B349") | inrange(icd10,"B37","B372") ///
   | inrange(icd10,"B375","B471") | inrange(icd10,"B479","B49") | inrange(icd10,"B58","B59") | icd10=="B94" ///
   | inrange(icd10,"B941","B950") | inrange(icd10,"B952","B96") | inrange(icd10,"B962","B972") ///
   | inrange(icd10,"B972","B973") | inrange(icd10,"B977","B978") | icd10=="B978" | inrange(icd10,"B99","B999") ///
   | icd10=="D703" | icd10=="D868" | icd10=="D893" | icd10=="F021" | icd10=="F071" | inrange(icd10,"G00","G099") ///
   | inrange(icd10,"G14","G146") | icd10=="I00" | icd10=="I02" | icd10=="I029" | inrange(icd10,"I96","I969") ///
   | icd10=="I981" | inrange(icd10,"J85","J850") | inrange(icd10,"J852","J853") | inrange(icd10,"J86","J869") ///
   | icd10=="K750" | icd10=="K753" | icd10=="K763" | icd10=="M491" | inrange(icd10,"M896","M896") ///
   | inrange(icd10,"P35","P359") | icd10=="P37" | icd10=="P372" | inrange(icd10,"P375","P379") ///
   | inrange(icd10,"R02","R029") | inrange(icd10,"U82","U84") | inrange(icd10,"U85","U89") | icd10=="Z11" ///
   | icd10=="Z112" | inrange(icd10,"Z115","Z115") | inrange(icd10,"Z118","Z119") | inrange(icd10,"Z16","Z163") ///
   | icd10=="Z20" | inrange(icd10,"Z204","Z205") | inrange(icd10,"Z208","Z209") | inrange(icd10,"Z22","Z220") ///
   | inrange(icd10,"Z222","Z223") | inrange(icd10,"Z225","Z229") | inrange(icd10,"Z233","Z237") ///
   | inrange(icd10,"Z240","Z241") | inrange(icd10,"Z244","Z250") | icd10=="Z831" /*Other infectious diseases*/
   replace oth_inf = . if covid19==1 | flu==1

   ///Diseases of heart: I00–I09, I11, I13, I20–I51
   gen heart = 1 if inrange(icd10,"I00","I099") | inrange(icd10,"I11","I119") | inrange(icd10,"I13","I139") ///  
     | inrange(icd10,"I20","I519")
   
   //Malignant neoplasms (cancer): C00–C97
   gen cancer = 1 if inrange(icd10,"C00","C979") 

   //Cerebrovascular diseases (stroke): I60–I69
   gen stroke = 1 if inrange(icd10,"I60","I699") 
   
   //Chronic lower respiratory diseases: J40–J47
   gen respirat = 1 if inrange(icd10,"J40","J479")
   
   //Alzheimer disease: G30
   gen alzheimer = 1 if inrange(icd10,"G30","G309")
   
   //Diabetes mellitus: E10–E14
   gen diabetes = 1 if inrange(icd10,"E10","E149")
   
   //Nephritis, nephrotic syndrome and nephrosis (kidney disease): N00–N07, N17–N19, N25–N27
   gen kidney = 1 if inrange(icd10,"N00","N079") | inrange(icd10,"N17","N199") ///
     | inrange(icd10,"N25","N279")
 
   //Liver disease and cirrhosis: K70, K71.3-K71.5, K71.7, K72-K74
   gen liver = 1 if inrange(icd10,"K70","K709") | inrange(icd10,"K713","K715") ///
     | icd10=="K717" | inrange(icd10,"K72","K749")
	 
    //Alcohol use disorder: F10-F10.9, G31.2, G72.1, Q86.0, R78.0 (excl alcohol/opioid poisoning)
	gen aud = 1 if inrange(icd10,"F10","F109") | icd10=="G312" | icd10=="G721" ///
	  | icd10=="Q860" | icd10=="R780"
	//since F10.0 appears in both AUD and alcohol poisoning, code to alc_poi
	replace aud = . if alc_poi_underly == 1 
	  
	//Other NCD (Excluding the other NCDs listed above)
	//Unrecognized codes: C7A00-C7B8, use C78.0-C79.8
	//Unrecognized codes: C47-C4A, use C47-C48.9 	
	//Unrecognized codes: D3A00-D3A8, use D38.0-D38.8	
	gen oth_ncd = 1 if inrange(icd10,"C00","C07") | inrange(icd10,"C08","C190") | icd10=="C20" ///
	  | inrange(icd10,"C21","C218") | inrange(icd10,"C22","C224") | inrange(icd10,"C227","C23") ///
	  | inrange(icd10,"C24","C261") | inrange(icd10,"C268","C269") | inrange(icd10,"C30","C301") ///
	  | inrange(icd10,"C31","C33") | inrange(icd10,"C34","C349") | inrange(icd10,"C37","C370") ///
	  | inrange(icd10,"C38","C399") | inrange(icd10,"C40","C414") | inrange(icd10,"C418","C419") ///
	  | inrange(icd10,"C43","C452") | icd10=="C457" | icd10=="C459" | inrange(icd10,"C47","C489") ///
	  | inrange(icd10,"C50","C506") | inrange(icd10,"C508","C52") | inrange(icd10,"C53","C543") ///
	  | inrange(icd10,"C548","C562") | inrange(icd10,"C569","C580") | inrange(icd10,"C60","C642") ///
	  | inrange(icd10,"C649","C699") | inrange(icd10,"C70","C701") | inrange(icd10,"C709","C73") ///
	  | inrange(icd10,"C74","C755") | inrange(icd10,"C758","C799") | inrange(icd10,"C80","C814") ///
	  | inrange(icd10,"C817","C817") | inrange(icd10,"C819","C852") | inrange(icd10,"C857","C866") ///
	  | inrange(icd10,"C88","C903") | inrange(icd10,"C91","C937") | inrange(icd10,"C939","C952") ///
	  | inrange(icd10,"C957","C979") | inrange(icd10,"D00","D249") | inrange(icd10,"D260","D399") ///
	  | inrange(icd10,"D4","D499") | icd10=="E340" | inrange(icd10,"K514","K514") | inrange(icd10,"K620","K623") ///
	  | icd10=="K635" | inrange(icd10,"N60","N609") | inrange(icd10,"N840","N841") | inrange(icd10,"N87","N879") ///
	  | icd10=="Z031" | inrange(icd10,"Z08","Z099") | inrange(icd10,"Z12","Z129") | inrange(icd10,"Z80","Z809") ///
	  | inrange(icd10,"Z85","Z859") | inrange(icd10,"Z860","Z860")  /*Neoplasms*/
    replace oth_ncd = 1 if inrange(icd10,"B332","B332") | icd10=="D868" | inrange(icd10,"G45","G468") ///
      | inrange(icd10,"I01","I019") | icd10=="I020" | inrange(icd10,"I05","I099") | inrange(icd10,"I11","I112") ///
	  | icd10=="I119" | inrange(icd10,"I20","I216") | inrange(icd10,"I219","I270") | inrange(icd10,"I272","I289") ///
	  | inrange(icd10,"I30","I380") | inrange(icd10,"I39","I418") | inrange(icd10,"I42","I438") ///
	  | inrange(icd10,"I44","I448") | inrange(icd10,"I45","I528") | inrange(icd10,"I60","I64") | icd10=="I641" /// 
	  | inrange(icd10,"I65","I839") | inrange(icd10,"I86","I890") | icd10=="I899" | inrange(icd10,"I950","I951") ///
	  | icd10=="I98" | inrange(icd10,"I988","I999") | icd10=="K751" | inrange(icd10,"R00","R012") ///
	  | inrange(icd10,"Z013","Z013") | inrange(icd10,"Z034","Z035") | icd10=="Z136" | icd10=="Z527" ///
	  | inrange(icd10,"Z823","Z824") | inrange(icd10,"Z867","Z867") | inrange(icd10,"Z941","Z943") ///
	  | inrange(icd10,"Z95","Z959") /*Cardiovascular diseases*/
    replace oth_ncd = 1 if inrange(icd10,"D86","D862") | icd10=="D869" | inrange(icd10,"G473","G473") ///
	  | inrange(icd10,"J30","J359") | inrange(icd10,"J37","J399") | inrange(icd10,"J41","J424") ///
	  | inrange(icd10,"J43","J460") | inrange(icd10,"J47","J479") | inrange(icd10,"J60","J689") ///
	  | inrange(icd10,"J708","J709") | inrange(icd10,"J80","J809") | icd10=="J82" | inrange(icd10,"J84","J849") ///
	  | inrange(icd10,"J90","J900") | icd10=="J91" | inrange(icd10,"J918","J931") | inrange(icd10,"J938","J949") ///
	  | inrange(icd10,"J96","J969") | inrange(icd10,"J98","J998") | inrange(icd10,"R050","R069") ///
	  | inrange(icd10,"R09","R098") | inrange(icd10,"R84","R849") | inrange(icd10,"R91","R918") | icd10=="Z825" ///
	  /*Chronic respiratory diseases*/ 
    replace oth_ncd = 1 if inrange(icd10,"I84","I859") | icd10=="I982" | inrange(icd10,"K20","K238") ///
	  | inrange(icd10,"K25","K319") | inrange(icd10,"K35","K389") | inrange(icd10,"K40","K429") ///
	  | inrange(icd10,"K44","K469") | inrange(icd10,"K50","K513") | inrange(icd10,"K515","K52") ///
	  | inrange(icd10,"K522","K529") | inrange(icd10,"K55","K62") | inrange(icd10,"K624","K626") ///
	  | inrange(icd10,"K628","K634") | inrange(icd10,"K638","K67") | inrange(icd10,"K678","K681") ///
	  | inrange(icd10,"K681","K689") | inrange(icd10,"K70","K75") | icd10=="K752" | inrange(icd10,"K754","K762") ///
	  | inrange(icd10,"K764","K778") | inrange(icd10,"K80","K808") | inrange(icd10,"K81","K839") ///
	  | inrange(icd10,"K85","K871") | inrange(icd10,"K90","K909") | inrange(icd10,"K92","K929") ///
	  | icd10=="K938" | inrange(icd10,"R11","R198") | inrange(icd10,"R85","R859") | inrange(icd10,"Z138","Z138") ///
	  | inrange(icd10,"Z431","Z434") | icd10=="Z526" | inrange(icd10,"Z837","Z837") | inrange(icd10,"Z871","Z871") ///
	  | icd10=="Z944" /*Digestive diseases*/
    replace oth_ncd = 1 if inrange(icd10,"F00","F020") | inrange(icd10,"F022","F023") | inrange(icd10,"F028","F039") ///
	  | icd10=="F062" | inrange(icd10,"G10","G100") | inrange(icd10,"G11","G138") | inrange(icd10,"G20","G21") ///
	  | inrange(icd10,"G212","G24") | inrange(icd10,"G241","G250") | inrange(icd10,"G252","G253") ///
	  | icd10=="G255" | inrange(icd10,"G258","G260") | inrange(icd10,"G30","G311") | inrange(icd10,"G318","G328") ///
	  | inrange(icd10,"G35","G350") | inrange(icd10,"G36","G379") | inrange(icd10,"G40","G419") ///
	  | inrange(icd10,"G43","G448") | inrange(icd10,"G50","G541") | inrange(icd10,"G545","G62") ///
	  | inrange(icd10,"G622","G652") | inrange(icd10,"G70","G711") | inrange(icd10,"G713","G72") ///
	  | inrange(icd10,"G721","G737") | inrange(icd10,"G80","G839") | inrange(icd10,"G89","G936") ///
	  | inrange(icd10,"G938","G952") | inrange(icd10,"G958","G96") | icd10=="G961" | inrange(icd10,"G961","G969") ///
	  | inrange(icd10,"G98","G998") | inrange(icd10,"M33","M339") | inrange(icd10,"M60","M601") ///
	  | inrange(icd10,"M608","M609") | icd10=="M797" | inrange(icd10,"R25","R279") | inrange(icd10,"R29","R299") ///
	  | inrange(icd10,"R41","R420") | inrange(icd10,"R56","R569") | inrange(icd10,"R90","R908") ///
	  | icd10=="Z033" | icd10=="Z138" | icd10=="Z138" | icd10=="Z820"  | inrange(icd10,"Z866","Z866") ///
	  /*Neurological disorders*/
    replace oth_ncd = 1 if inrange(icd10,"F04","F061") | inrange(icd10,"F063","F070") | inrange(icd10,"F08","F099") ///
	  | inrange(icd10,"F20","F349") | inrange(icd10,"F38","F529") | inrange(icd10,"F55","F558") ///
	  | inrange(icd10,"F56","F990") | inrange(icd10,"G47","G472") | inrange(icd10,"G474","G479") ///
	  | inrange(icd10,"R40","R404") | inrange(icd10,"R45","R468") | inrange(icd10,"R55","R550") ///
	  | icd10=="Z032" | inrange(icd10,"Z046","Z047") | icd10=="Z134" | icd10=="Z64" | inrange(icd10,"Z81","Z810") ///
	  | icd10=="Z818" | inrange(icd10,"Z865","Z865") /*Mental disorders*/
    replace oth_ncd = 1 if icd10=="E244" | inrange(icd10,"F10","F199") | icd10=="G312" | icd10=="G621" ///
	  | icd10=="P961" | inrange(icd10,"R780","R789") | inrange(icd10,"X45","X459") | inrange(icd10,"X65","X659") ///
	  | inrange(icd10,"Y15","Y159") | inrange(icd10,"Z811","Z814") /*Substance use disorders*/ 
    replace oth_ncd = 1 if icd10=="D631" | inrange(icd10,"E08","E089") | inrange(icd10,"E10","E149") ///
	  | inrange(icd10,"I12","I139") | inrange(icd10,"N00","N088") | icd10=="N150" | inrange(icd10,"N17","N19") ///
	  | inrange(icd10,"Q60","Q632") | inrange(icd10,"Q638","Q639") | inrange(icd10,"Q642","Q649") ///
	  | inrange(icd10,"R73","R739") | icd10=="Z131" | inrange(icd10,"Z49","Z493") | icd10=="Z524" ///
	  | icd10=="Z833" | icd10=="Z992" /*Diabetes and kidney diseases*/
    replace oth_ncd = 1 if inrange(icd10,"A46","A460") | inrange(icd10,"A66","A673") | icd10=="A679" ///
	  | inrange(icd10,"B07","B09") | inrange(icd10,"B35","B369") | inrange(icd10,"B85","B889") ///
	  | icd10=="D863" | inrange(icd10,"E801","E802") | inrange(icd10,"I891","I898") | inrange(icd10,"L00","L059") ///
	  | inrange(icd10,"L08","L089") | inrange(icd10,"L10","L140") | inrange(icd10,"L20","L232") ///
	  | inrange(icd10,"L234","L27") | inrange(icd10,"L272","L309") | inrange(icd10,"L40","L45") ///
	  | inrange(icd10,"L49","L540") | icd10=="L56" | inrange(icd10,"L562","L579") | inrange(icd10,"L59","L609") ///
	  | inrange(icd10,"L62","L64") | inrange(icd10,"L648","L689") | inrange(icd10,"L70","L759") ///
	  | inrange(icd10,"L80","L929") | inrange(icd10,"L94","L959") | inrange(icd10,"L97","L998") ///
	  | inrange(icd10,"M725","M726") | inrange(icd10,"N492","N493") | inrange(icd10,"R20","R240") ///
	  | icd10=="Z207" | icd10=="Z411" | icd10=="Z840" | icd10=="Z872" | icd10=="Z945" /*Skin and subcutaneous diseases*/ 
    replace oth_ncd = 1 if inrange(icd10,"B30","B309") | inrange(icd10,"H00","H028") | inrange(icd10,"H028","H029") ///
	  | inrange(icd10,"H030","H053") | inrange(icd10,"H053","H054") | inrange(icd10,"H058","H063") ///
	  | inrange(icd10,"H10","H119") | inrange(icd10,"H13","H138") | inrange(icd10,"H15","H228") ///
	  | inrange(icd10,"H25","H288") | inrange(icd10,"H30","H368") | inrange(icd10,"H40","H409") ///
	  | inrange(icd10,"H42","H445") | inrange(icd10,"H448","H558") | inrange(icd10,"H57","H589") ///
	  | inrange(icd10,"H60","H628") | inrange(icd10,"H71","H758") | inrange(icd10,"H80","H839") ///
	  | inrange(icd10,"H90","H91") | inrange(icd10,"H911","H948") | inrange(icd10,"Q16","Q169") ///
	  | inrange(icd10,"R43","R449") | inrange(icd10,"Z010","Z011") | icd10=="Z135" | icd10=="Z413" ///
	  | icd10=="Z525" | inrange(icd10,"Z821","Z822") | inrange(icd10,"Z835","Z836") | icd10=="Z947" ///
	  | inrange(icd10,"Z973","Z974") /*Sense organ diseases*/
    replace oth_ncd = 1 if inrange(icd10,"G542","G544") | icd10=="I271" | inrange(icd10,"L93","L932") ///
	  | inrange(icd10,"M00","M036") | inrange(icd10,"M05","M101") | inrange(icd10,"M103","M259") ///
	  | inrange(icd10,"M30","M329") | inrange(icd10,"M34","M368") | inrange(icd10,"M40","M439") ///
	  | inrange(icd10,"M45","M49") | inrange(icd10,"M492","M519") | inrange(icd10,"M53","M549") ///
	  | inrange(icd10,"M61","M638") | inrange(icd10,"M65","M688") | inrange(icd10,"M70","M724") ///
	  | inrange(icd10,"M728","M73") | inrange(icd10,"M75","M779") | inrange(icd10,"M79","M796") ///
	  | inrange(icd10,"M798","M870") | inrange(icd10,"M872","M895") | inrange(icd10,"M897","M959") ///
	  | inrange(icd10,"M99","M999") | inrange(icd10,"Z138","Z138") | inrange(icd10,"Z826","Z826") ///
	  | inrange(icd10,"Z873","Z873") /*Musculoskeletal disorders*/
    replace oth_ncd = 1 if inrange(icd10,"B373","B374") | inrange(icd10,"C780","C798") | inrange(icd10,"D25","D26") ///
	  | inrange(icd10,"D380","D388") | inrange(icd10,"D55","D619") | inrange(icd10,"D64","D694") ///
	  | inrange(icd10,"D696","D700") | inrange(icd10,"D704","D77") | inrange(icd10,"D80","D849") ///
	  | icd10=="D868" | inrange(icd10,"D868","D868") | inrange(icd10,"D868","D868") | inrange(icd10,"D89","D892") ///
	  | icd10=="D898" | inrange(icd10,"D898","D899") | inrange(icd10,"E03","E031") | inrange(icd10,"E033","E063") ///
	  | inrange(icd10,"E065","E079") | inrange(icd10,"E15","E16") | inrange(icd10,"E161","E169") ///
	  | inrange(icd10,"E20","E230") | inrange(icd10,"E232","E241") | icd10=="E243" | inrange(icd10,"E248","E272") ///
	  | inrange(icd10,"E274","E329") | icd10=="E34" | inrange(icd10,"E341","E358") | inrange(icd10,"E65","E660") ///
	  | inrange(icd10,"E662","E68") | inrange(icd10,"E70","E800") | inrange(icd10,"E803","E859") ///
	  | inrange(icd10,"E88","E889") | inrange(icd10,"E90","E998") | icd10=="G712" | inrange(icd10,"K00","K084") ///
	  | inrange(icd10,"K088","K149") | inrange(icd10,"M26","M279") | inrange(icd10,"N10","N139") ///
	  | icd10=="N15" | inrange(icd10,"N151","N168") | inrange(icd10,"N20","N230") | inrange(icd10,"N25","N29") ///
	  | inrange(icd10,"N291","N303") | inrange(icd10,"N308","N378") | inrange(icd10,"N39","N460") ///
	  | inrange(icd10,"N460","N461") | inrange(icd10,"N461","N491") | inrange(icd10,"N498","N521") ///
	  | inrange(icd10,"N528","N539") | inrange(icd10,"N61","N649") | inrange(icd10,"N72","N720") ///
	  | inrange(icd10,"N75","N778") | inrange(icd10,"N80","N819") | inrange(icd10,"N83","N84") ///
	  | inrange(icd10,"N842","N86") | inrange(icd10,"N88","N959") | inrange(icd10,"N97","N989") ///
	  | icd10=="P960" | inrange(icd10,"Q00","Q079") | inrange(icd10,"Q10","Q159") | inrange(icd10,"Q17","Q189") ///
	  | inrange(icd10,"Q20","Q289") | inrange(icd10,"Q30","Q459") | inrange(icd10,"Q50","Q564") ///
	  | icd10=="Q633" | inrange(icd10,"Q64","Q641") | inrange(icd10,"Q65","Q878") | inrange(icd10,"Q89","Q898") ///
	  | inrange(icd10,"Q90","Q939") | inrange(icd10,"Q95","Q999") | inrange(icd10,"R30","R37") ///
	  | inrange(icd10,"R39","R399") | inrange(icd10,"R86","R879") | inrange(icd10,"Z012","Z012") ///
	  | inrange(icd10,"Z014","Z017") | inrange(icd10,"Z137","Z137") | icd10=="Z138" | inrange(icd10,"Z14","Z158") ///
	  | inrange(icd10,"Z31","Z319") | inrange(icd10,"Z435","Z437") | inrange(icd10,"Z827","Z827") ///
	  | inrange(icd10,"Z834","Z834") | inrange(icd10,"Z841","Z842") | inrange(icd10,"Z861","Z861") ///
	  | inrange(icd10,"Z874","Z874") | inrange(icd10,"Z877","Z877") | icd10=="Z96.5" /*Other non-communicable diseases*/
   replace oth_ncd = . if inlist(1,heart,cancer,stroke,respirat,alzheimer,diabetes,kidney,liver,aud)

    //if both suicide and opioid poisoning (X60-X64 appear in both), keep sucide 
	replace opioid_poi = . if sij == 1 & opioid_poi == 1 
	
    // create final alcohol poisoning combining underlying and contributing causes
	// contributing causes exclude other underlying diesease 
	replace alc_poi_contrb = . if inlist(1,mvacc,opioid_poi,uij,sij,othj,covid19,flu,heart,cancer,stroke, ///    
	  respirat, alzheimer, diabetes, kidney, liver,aud)
	gen alc_poi = 1 if alc_poi_underly == 1 | alc_poi_contrb == 1

   //all remaining
   gen rest = 1
   foreach v of varlist mvacc opioid_poi alc_poi uij sij othj covid19 flu oth_inf ///
      heart cancer stroke respirat alzheimer diabetes kidney liver aud oth_ncd {
	  	replace rest = . if `v' == 1
	  } 	  
	  
    //clean up residual multiple coding for "others"
	foreach v of varlist mvacc opioid_poi alc_poi uij sij othj covid19 flu oth_inf {
	   replace oth_ncd = . if oth_ncd == 1 & `v' == 1
	}  
	foreach v of varlist mvacc opioid_poi alc_poi uij sij othj covid19 flu heart cancer stroke respirat alzheimer diabetes            kidney liver aud {
	   replace oth_inf = . if oth_inf == 1 & `v' == 1
	}      
	  	
	 //test each category is coded once 
	 egen test = rowtotal(mvacc opioid_poi alc_poi uij sij othj covid19 flu oth_inf heart cancer stroke respirat alzheimer        diabetes kidney liver aud oth_ncd rest)
    tab test, m
    drop test 
 
    gen total = 1
 
 
   keep year race sex age age_gp edclass mvacc opioid_poi alc_poi uij sij othj covid19 ///
     flu oth_inf heart cancer stroke respirat alzheimer diabetes kidney liver aud oth_ncd rest total
 
   save "c:\temp\mort`year'_newdata.dta", replace

}

use c:\temp\mort2000_newdata.dta, clear 
forvalues i=2001/2020 {
	append using c:\temp\mort`i'_newdata
}

tabstat mvacc-total, by(year) stat(sum)
tabstat mvacc-total if inrange(age,18,300), by(year) stat(sum)

/////////////////////////////////////////////////////////////////
//2nd step: Aggreated by year, age, gender, education and race
/////////////////////////////////////////////////////////////////

forvalues year=2000/2020 {

  use "c:\temp\mort`year'_newdata.dta", clear 

  ///collapse 80-84 and 85+ to 80+
  replace age_gp = 80 if age_gp==85
  lab define age_gplab 80 "80+", modify

  **Restricting age to 18+**
  **drop age < 18 and age missing
  keep if inrange(age_gp,18,80)
  
  // Sum by COD (TOTAL + Catgories + Rest)
  
   gen one=1
   bysort age_gp sex edclass race year: egen Tmort = total(one)

   bysort age_gp sex edclass race year: egen MVACCmort = total(mvacc)
   bysort age_gp sex edclass race year: egen OPDPOImort = total(opioid_poi)
   bysort age_gp sex edclass race year: egen ALCPOImort = total(alc_poi) 
   bysort age_gp sex edclass race year: egen UIJmort = total(uij) 
   bysort age_gp sex edclass race year: egen SIJmort = total(sij)  
   bysort age_gp sex edclass race year: egen OTHJmort = total(othj)  
   bysort age_gp sex edclass race year: egen COVIDmort = total(covid19)    
   bysort age_gp sex edclass race year: egen FLUmort = total(flu)
   bysort age_gp sex edclass race year: egen OTHINFmort = total(oth_inf)    
   bysort age_gp sex edclass race year: egen HEARTmort = total(heart)
   bysort age_gp sex edclass race year: egen CANCERmort = total(cancer)    
   bysort age_gp sex edclass race year: egen STROKEmort = total(stroke)    
   bysort age_gp sex edclass race year: egen RESPmort = total(respirat)
   bysort age_gp sex edclass race year: egen ALZHmort = total(alzheimer)
   bysort age_gp sex edclass race year: egen DMmort = total(diabetes)    
   bysort age_gp sex edclass race year: egen KIDNEYmort = total(kidney) 
   bysort age_gp sex edclass race year: egen LIVERmort = total(liver)    
   bysort age_gp sex edclass race year: egen AUDmort = total(aud)
   bysort age_gp sex edclass race year: egen OTHNCDmort = total(oth_ncd)
   bysort age_gp sex edclass race year: egen RESTmort = total(rest)

   by age_gp sex edclass race year, sort: keep if _n==1

   keep age_gp sex edclass race year *mort 
   
  save "c:\temp\temp`year'.dta", replace 

}

/////////////////////////////////////////////////////////////////
//3rd step: Re-assign missing eduation
/////////////////////////////////////////////////////////////////

use "c:\temp\temp2000.dta", clear 
forvalues year=2001/2020 {
 append using "c:\temp\temp`year'.dta"
} 

**Restricting age 18-74**
*keep if age_gp>=18 & age_gp<=74
  
tabstat *mort , stat(sum) by(year)
egen checkTmort = rowtotal(MVACCmort-RESTmort)
gen checkdif = Tmort - checkTmort
sum Tmort checkTmort checkdif
drop check*
		
keep year sex age_gp edclass race *mort
reshape wide Tmort MVACCmort-RESTmort, ///
  i(year age_gp sex race) j(edclass)
sum *mort99

foreach i in T MVACC OPDPOI ALCPOI UIJ SIJ OTHJ COVID FLU OTHINF HEART CANCER ///
   STROKE RESP ALZH DM KIDNEY LIVER AUD OTHNCD REST {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}
**check if there is entry with all deaths in education missing category**
foreach i in T T MVACC OPDPOI ALCPOI UIJ SIJ OTHJ COVID FLU OTHINF HEART CANCER ///
   STROKE RESP ALZH DM KIDNEY LIVER AUD OTHNCD REST {
list year sex race age_gp MORT`i' `i'mort1 `i'mort2 `i'mort3 `i'mort99 ///
   if (MORT`i' == 0 | MORT`i' == .) & (`i'mort99 > 0 & `i'mort99 != . )
 }  
 
**assign those deaths from missing education into the three education group evenly (1/3 each)
foreach i in T MVACC OPDPOI ALCPOI UIJ SIJ OTHJ COVID FLU OTHINF HEART CANCER ///
   STROKE RESP ALZH DM KIDNEY LIVER AUD OTHNCD REST {
replace `i'mort1 = `i'mort99/3 if (MORT`i' == 0 | MORT`i' == .) & (`i'mort99 > 0 & `i'mort99 != . )
replace `i'mort2 = `i'mort99/3 if (MORT`i' == 0 | MORT`i' == .) & (`i'mort99 > 0 & `i'mort99 != . )
replace `i'mort3 = `i'mort99/3 if (MORT`i' == 0 | MORT`i' == .) & (`i'mort99 > 0 & `i'mort99 != . )
 }  
 
**re-check again**
foreach i in T MVACC OPDPOI ALCPOI UIJ SIJ OTHJ COVID FLU OTHINF HEART CANCER ///
   STROKE RESP ALZH DM KIDNEY LIVER AUD OTHNCD REST {
list year sex race age_gp MORT`i' `i'mort1 `i'mort2 `i'mort3 `i'mort99 ///
   if (MORT`i' == 0 | MORT`i' == .) & (`i'mort99 > 0 & `i'mort99 != . )
 }   
			   
keep year sex race age_gp *mort1 *mort2 *mort3   

reshape long Tmort MVACCmort OPDPOImort ALCPOImort UIJmort SIJmort OTHJmort COVIDmort FLUmort ///
   OTHINFmort HEARTmort CANCERmort STROKEmort RESPmort ALZHmort DMmort KIDNEYmort ///
   LIVERmort AUDmort OTHNCDmort RESTmort, ///
      i(year age_gp sex race) j(edclass)

egen checkTmort = rowtotal(MVACCmort-RESTmort)
gen checkdif = Tmort - checkTmort
sum Tmort checkTmort checkdif
drop check*
	  
tabstat Tmort MVACCmort-RESTmort, stat(sum) by(year)
  	  
save "c:\temp\allethn_sumCOD_0020_final_new.dta", replace

/////////////////////////////////////////////////////////////////
//4th step: Merge with population
/////////////////////////////////////////////////////////////////

**process population data**

clear
cd "I:\Charlotte R01\William Kerr_12-29-2020\stata codes\New Data setup"
import delimited ACS_popcounts_2000_2020.csv

gen edclass_num = .
replace edclass_num = 1 if edclass == "LEHS"
replace edclass_num = 2 if edclass == "SomeC"
replace edclass_num = 3 if edclass == "College"
drop edclass 
rename edclass_num edclass

gen race_num = .
replace race_num = 1 if race == "White"
replace race_num = 2 if race == "Black"
replace race_num = 3 if race == "Hispanic"
replace race_num = 4 if race == "Other"
drop race
rename race_num race

keep if state == "USA"
drop state

sort year age_gp sex edclass race
save ACS_popcounts_2000_2020.dta, replace

**merge with mortality data**

use "c:\temp\allethn_sumCOD_0020_final_new.dta", clear
sort year age_gp sex edclass race

merge 1:1 year age_gp sex edclass race using ACS_popcounts_2000_2020.dta
drop _merge

foreach var in T MVACC OPDPOI ALCPOI UIJ SIJ OTHJ COVID FLU OTHINF HEART CANCER ///
   STROKE RESP ALZH DM KIDNEY LIVER AUD OTHNCD REST {
   gen `var'rate   = (`var'mort/tpop)*100000
   }

label var Trate "All cause"
label var MVACCrate "Motor vehicle accidents"
label var OPDPOIrate "Opioid poisoning"
label var ALCPOIrate "Alcohol poisoning" 
label var UIJrate "Unintentional injuries"
label var SIJrate "Suicide"
label var OTHJrate "Other injuries"
label var COVIDrate "COVID-19"
label var FLUrate "Flu"
label var OTHINFrate "Other infectious"
label var HEARTrate "Heart disease"
label var CANCERrate "Cancer"
label var STROKErate "Stroke"
label var RESPrate "Lower respiratory disease"
label var ALZHrate "Alzheimer"
label var DMrate "Diabetes mellitus"
label var KIDNEYrate "Kidney"
label var LIVERrate "Liver"
label var AUDrate "Alcohol use disorder"
label var OTHNCDrate "Other chronic disease"
label var RESTrate "All others"


outsheet using US_mortality_rates_0020.csv , comma replace