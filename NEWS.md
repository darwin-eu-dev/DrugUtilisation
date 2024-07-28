# DrugUtilisation 0.7.0

* Deprecate dose specific functions: `addDailyDose`, `addRoute`, 
  `stratifyByUnit`.
  
* Deprecate drug use functions: `addDrugUse`, `summariseDrugUse`.

* Rename `dailyDoseCoverage` -> `summariseDoseCoverage`.

* Refactor of `addIndication` to create a categorical variable per window.

* New functionality `summariseProportionOfPatientsCovered`,
  `tableProportionOfPatientsCovered` and `plotProportionOfPatientsCovered`.

* Create `require*` functions.

* New functionality `summariseDrugRestart`, `tableDrugRestart` and 
  `plotDrugRestart`.

* New functionality `addDrugUtilisation`, `summariseDrugUtilisation` and 
  `tableDrugUtilisation`
