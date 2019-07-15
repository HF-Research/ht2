---
title: "Implementation"
output: 
  html_document:
    code_folding: hide
---



## 1. Define the Danish population
<details>
  <summary>Click to expand</summary>  

The first step is to determine which records in the CPR registry should be considered part of the Danish population for any given year. All disease/operation/medication data will be queried from this population.

The Danish CPR register contains data on all individuals who have received a CPR number. In order to construct an accurate estimate of the Danish population for any given year, we had to excluded the records of those who had died, immigrated, or were temporarily living outside of Denmark as of 1. January for the given year. Additionally, some records were excluded due to incomplete information. The records remaining were considered to comprise the Danish population for that year.  

Records were excluded from the dataset if they met any of the following criteria:  

* Contain a `NA` value for `bef_year`
* Contain a `NA` value for date of birth (`fdato`)
* Are under aged 35 as of 1. January
* Those with a registered death occurring before 1. January  

To remove all records who are not residents as of 1. January, records were excluded if the met any of the following criteria:

  * Emigrated from Denmark before 1. January
  * Immigrated to Denmark after 1. January
  * Were born outside of Denmark, but have no data on date of arrival in Denmark
  * Those temporarily living outside of Denmark as of 1. January. This estimate likely slightly under counts the true number of people temporarily living overseas, meaning a small number of records in our dataset might not truly reside in Denmark for the given year.



### Implementation in R

To implement the above exclusion in R, the following code was used, using the year 2016 as an example:

1. Read in the data from the CPR registry and death registry  
  <details>
    <summary>Expand to see code</summary>  
  
    ```r
      # library(data.table)
      # library(lubridate)
      # library(heaven)
      
      date_start <- as.Date("2016-01-01") # For this example, we will work on the year 2016
      path_pop <- "<path_to_cpr_registry>"
      
      pop_base <- importSAS(
        filename = path_pop,
        keep = c("pnr", "fdato", "bef_year", "sex", "first_ind", "last_ind", "last_ud", "ie_type", "opr_land"),
        where = "bef_year ne . & fdato ne .", # Ignore records with missing bef_year or fdato
        date.vars = c("fadto","first_ind","last_ind","last_ud"),
        character.vars = c("pnr", "sex")
      )
      
      # Because every records we imported has a valid bef_year, we no longer need this
      # variable - so we delete it
      pop_base[, bef_year := NULL]
      
      dead <-
        importSAS(
          filename = path_dead,
          keep = c("pnr", "doddato"),
          date.vars = "doddato",
          filter = pop_base[, .(pnr)] # Only care about deaths among those in our selected population
        )
      # Impose standard column name and order convention to be used for all outcome in
      # HjerteTal2
      dead[, event := "death"]
      setnames(dead, c("pnr","event_date","event"))
      setcolorder(dead, c("pnr","event_date","event"))
      setkey(dead, pnr)
      
      # In our project, kommune data is in a seperate dataset from the population data
      # (linked by PNR)
      kom_all <-
        importSAS(
          filename = path_kom,
          keep = c("pnr", "year", "kom"),
          filter = pop_base[, .(pnr)],
          character.vars = c("pnr", "kom"),
          where = "year between 2004 and 2016"
        )
      
      # We need the historical kommune data in a later step, but most we will only be
      # looking at the dataset of where people are living in 2016, so we make a
      # separate dataset for that
      kom <- kom_all[year == date_start, ]
      # Merge kommune data onto population data
      pop_base <- merge(pop_base, kom, all.x = TRUE, by = "pnr")
      pop_base[is.na(kom), kom := "Unknown"]
    ```
  
  </details>  

2. Remove those who died before 1. January
  <details>
    <summary>Expand to see code</summary>
    
    ```r
    died_before_start <- dead[event_date < date_start, pnr]
    pop_base <- pop_base[!(pnr %in% died_before_start), ]
    ```
  
  </details>

3. Remove those under 35
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    # define age as of 1. Jan
    pop_base[, age := floor(as.numeric(date_start - fdato) / 365.25)]
    
    pop_base <-
      pop_base[age >= age_minimum, ]
    ```
  
  </details>

4. Remove those who emigrated from Denmark before 1. January
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    emigrate <- pop_base[(last_ind < last_ud) |
                           (is.na(first_ind) & !is.na(last_ud)),
                         .(pnr, last_ud)]
    emigrate[, event := "emigrate"]
    setnames(emigrate, c("pnr","event_date","event"))
    setcolorder(emigrate, c("pnr","event_date","event"))
    
    emigrate_before <- emigrate[event_date < date_start, pnr]
    pop_base <- pop_base[!(pnr %in% emigrate_before), ]
    ```
  
  </details>

5. Remove those who immigrated to Denmark after 1. January
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    immigrate <- pop_base[(is.na(last_ud) &
                            !is.na(first_ind)),
                         .(pnr, last_ud)]
    immigrate[, event := "immigrate"]
    setnames(immigrate, c("pnr","event_date","event"))
    setcolorder(immigrate, c("pnr","event_date","event"))
    
    immigrate_before <- immigrate[event_date > date_start, pnr]
    pop_base <- pop_base[!(pnr %in% immigrate_before), ]
    ```
  
  </details>

6. Remove those born outside Denmark who have no arrival date and are not registered to any kommune
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    born_abroad_no_arrival <- pop_base[(ie_type == 2 |
                                         (is.na(ie_type) & opr_land != 5100)) &
                                         (is.na(first_ind) & kom == "Unknown"),
                                       pnr]
    pop_base <- pop_base[!(pnr %in% born_abroad_no_arrival), ]
    ```
  
  </details>

7. Remove those who are temporarily living outside Denmark as of 1. January
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    # Some records have a travel history that clearly shows they are not residents
    # as of 1. Jan
    expat <-
      pop_base[last_ind > last_ud &
                 between(date_start, last_ud, last_ind), pnr]
    pop_base <- pop_base[!(pnr %in% expat)]
    
    # Some records are potentially not residents of DK as of 1. Jan, but we cannot
    # determine this using travel data alone. Here we cross reference with the
    # kommune dataset to see if these records are registered in any kommune in DK
    tmp_abroad <- pop_base[kom == "Unknown" &
                             (last_ind > date_start) &
                             (first_ind != last_ind),
                           pnr]
    pop_base <- pop_base[!(pnr %in% tmp_abroad)]
    ```
  
  </details>

8. Remove those with corrupted travel data
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    corrupt_travel <- expat[((first_ind > last_ind) &
                        (first_in != last_ind)),
                     pnr]
    pop_base <- pop_base[!(pnr %in% corrupt_travel)]
    ```
  
  </details>
  
9. Remove "zombie" records. These are records that do not have a death date, nor immigration date, but at no longer living in DK (i.e. some records appear to be >120 year old). To identify these records we look at both their age and their kommune registration. If we see that they are very old in 2016 (>100) AND they have never been registered to a kommune at anytime in the previous 10 years, we remove the records
  <details>
    <summary>Expand to see code</summary>
  
    ```r
    no_current_kom <- pop_base[age >= 100 & kom == "Unknown", pnr]
    no_kom_hisotry <- kom_all[pnr %in% no_current_kom, 
                              ][, last_kom := max(year), by = "pnr"
                                ][last_kom < year(date_start - years(13)), pnr]
    pop_base <- pop_base[!(pnr %in% no_kom_hisotry), ]
    ```
  
  </details>
</details>

## 2. Add education data
<details>
  <summary>Click to expand</summary>  
The education level for each person was defined as the highest educational level
attained as of 1. January for the given year. If education data for any specific
perosne is missing for the year 2016, we take the most recent year's data, before 2016,
that is available for that person.

```r
# Imports education data for all years for each PNR record
edu_base <-
  importSAS(
    filename = path_edu,
    keep = c("pnr", "year", "udd_niveau_k"),
    filter = pop_base[, .(pnr)]
  )

# Remove any data from after 2016
edu_base <- edu_base[year <= year(date_start)]

# Keep only the most recent year's data for each person
setorder(edu_base, pnr, year)
edu_base <- unique(edu_base, by = "pnr")

setkey(edu_base, pnr)

# Classify education levels according to HjerteTal2's groupings
edu_base <- edu_grouping(edu_base, edu_labs = edu_labs)
edu_base[, udd_niveau_k := NULL] # delete raw variable


```


</details>

## 3. Add cause of death data
<details>
  <summary>Click to expand</summary>  
In order to know how many people died due to specific illnesses, we neeed to import data on the cause of death (COD). In addition to the primary COD, there are often contributing secondary CODs. If a specific illness is listed among either the primary, or any of the contributing CODs, it is counted towards that illness' mortality

1. Import and clean death registry data
  <details>
    <summary>Click to see code</summary>  
    
    ```r
    # Import dataset
    cod <-
      importSAS(
        filename = path_dead,
        keep = c(
          "pnr",
          "doddato",
          "c_dod_1a",
          "c_dod_1b",
          "c_dod_1c",
          "c_dod_1d",
          "c_dod_21",
          "c_dod_22",
          "c_dod_23",
          "c_dod_24"
        ),
        filter = pop_base[, .(pnr)],
        date.vars = "doddato"
      )
    # Standardize names
    cod_vars <- c(
      "pnr",
      "death_date",
      "cod1",
      "cod2",
      "cod3",
      "cod4",
      "cod5",
      "cod6",
      "cod7",
      "cod8"
    )
    setnames(
      cod,
     cod_vars 
    )
    
    # Clean the data. Change " " values to NA and add "D" to front of COD codes.
    # This "D" prefix is required because SKS codes used for diagnostic purposes
    # have the "D" prefixed to the ICD codes, but the codes as entered into the
    # death registry to not have this "D" prefix.
    cod[, (cod_vars) := lapply(.SD, function(i){
      i[i == " "] <- NA
      i[!is.na(i)] <- paste0("D", i[!is.na(i)])
      i
    })]
    
    ```
  
  </details>

2. Classify CODs due to the illness included in HjerteTal2. 
  <details>
      <summary>Click to see code</summary>  
      
      ```r
      # Convert 
      ```


</details>
