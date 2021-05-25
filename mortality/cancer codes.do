///breast cancer C50
gen breast_cancer = 1 if inrange(icd10, "C500", "C509")

///Pancreatic cancer C25
gen pancreatic_cancer = 1 if inrange(icd10, "C250", "C259")

///Colorectal cancer
///Malignant neoplsm of colon C18, malignant neoplasm of rectogsigmoid junction C19 ///
///and malignant neoplsm of rectum C20?
// Change to C18.0 to C21.8 to include all colon and rectum cancers
gen colorectal_cancer = 1 if inrange(icd10, "C180", "C218") 

///Oesophageal cancer C15
gen oesophageal_cancer = 1 if inrange(icd10, "C150", "C159")

///Larnygeal cancer (larynx cancer?) C32
gen larynx_cancer = 1 if inrange(icd10, "C320", "C329")

///Liver cancer C22
gen liver_cancer = 1 if inrange(icd10, "C220", "C229")

///Oral cavity and pharynx cancer C03-C14
///Not including lip and tougue C00-C02???
// Should include C00 to C08 for Lip and oral cavity
gen oral_cancer = 1 if inrange(icd10, "C00", "C149")

///combine all cancer types above
gen cancer = 1 if inlist(1,breast_cancer,pancreatic_cancer,colorectal_cancer,oesophageal_cancer, ///
  larynx_cancer,liver_cancer,oral_cancer)

// Generate rest category
gen rest = .
replace rest = 1 if (lvdc == . & dm == . & ihd == . & str == . & hyphd == . ///
	& aud == . & uij == . & mvacc == . & ij == . & cancer == .)  
	
