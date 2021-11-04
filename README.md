# Post Conflict Refugee Returns 

Analyses the conditions under which refugees have returned to their home countries after the conflict has ended. 

 - **Script:** `00a Custom Functions.R`
 - **Script:** `00b Historical Income Classification Thresholds.R`  
 - **Script:** `00c WB-UNHCR Country Name Lookup.R`  

## 1. Identify Source Countries & Conflicts  

 - 1.1 UNHCR's 51 major sources of refugees (1980-2018)[^1]  
 - 1.2 Identity all war / conflicts that ended sometime in 1989-2018[^2]  
 - **Script:** `01 Identify Sources and Conflicts.R`
    - Used the UCDP-PRIO Georeferenced Event Dataset to create an aggregated dataset with total number of deaths and violent incidents for every year and all countries that are major sources of refugees in the dataset differentiated by different types of violence (state,-based, non-state, and one-sided).  


[^1]: https://www.unhcr.org/5b27be547.pdf pg 63  
[^2]: https://ucdp.uu.se/downloads/index.html#ged_global  


## 2. Identify End of Conflict Year  

Takes aggregated conflict cases dataset and determines end of conflict year for all of them. 
 - A conflict is considered ended in a given year if:
    - If it is the last year for which high intensity conflict is recorded.  
    - If there is a gap of more than 2 years before high intensity conflict restarts.
 - High intensity conflict is defined as having more than 1000 deaths in a year due to any type of violence.
 - **Script:** `02 Identify End of Conflict.R`  

## 3. Create Dependent Variable: % refugee returned  

 - Takes UNHCR Data[^3] and prepares it to add to short-listed conflict cases.  
    - **Script:** `03a UNHCR Refugee Data.R`  
    - **Script:** `03b UNHCR Refugee Returned Data.R`  
- Adds UNHCR data to conflict cases and calculates % returned  
    - Additional Cases dropped where:
        - The refugee stock never rises to more than 10,000 in the interval between end of conflict and 10 years after the end.
        - The refugee stock is less than 1000 at the end of conflict.
    - UNHCR data gives the population *stock* (Number of refugees from a country in a year) and the *outflow* (Number of Refugees returned to a country in a year). 
    - A stock and flow model was created using this data: 
        - The stock of refugee population in a given year, S<sub>t</sub>, is the sum of refugee population in the previous year and the *"net flow"* of refugees in the given year, F<sub>t</sub>.
        - S<sub>t</sub> = S<sub>t-1</sub> + F<sub>t</sub>
        - The net flow in a given year is the difference between the new refugees, I<sub>t</sub>, and refugees that returned, O<sub>t</sub>. 
        - F<sub>t</sub> = I<sub>t</sub> - O<sub>t</sub>
        - Therefore, S<sub>t</sub> = S<sub>t-1</sub> + I<sub>t</sub> - O<sub>t</sub>  
        - Rearranging the above difference equation, we can calculate the new refugees displaced in a given year. 
        - I<sub>t</sub> = S<sub>t</sub> + O<sub>t</sub> - S<sub>t-1</sub>  
        - New refugees displaced in the 10 years after the end of conflict and the refugee population at the end of conflict are used to calculate a yearly baseline of refugees displaced since the end of conflict. 
        - The dependent variable, % refugees returned is calculated using this composite population. 
    - **Script:** `03c Adds UNHCR data to Conflict Cases.R`  

[^3]: http://popstats.unhcr.org/en/time_series  

## 4. Origin State Variables  

**Included:**  
 - Security: 
    - Number of years in the 10 years after the end of conflict (Year1-10) when annual death toll was more than the threshold (25, 500, 1000)  
 - Length of war:
    - Difference between Start Year and End of Conflict
 - Economic Condition:
    - Average GNI Per Capita over 10 (or available) years after the end of conflict 
    - Annualized change in GNI Per Capita over 10 (or available) years after the end of conflict 
 - **Script:** `04 Origin State Variables.R`  

**Not Included:**  
- Economic Growth: 
    - GDP per-capita[^4]  
- Human Development Index[^5]    
- Governance Capacity : 
    - Worldwide Governance Indicators[^6] 
- Democratization: 
    - Polity IV Score[^7] 
- Refugee Return Ministry  

[^4]: https://www.imf.org/external/pubs/ft/weo/2018/02/weodata/index.aspx
[^5]: http://hdr.undp.org/en/data
[^6]: http://info.worldbank.org/governance/wgi/#home
[^7]: http://www.systemicpeace.org/inscrdata.html

## 5. Host State Variables  

**Included:**  
 - Geography: 
    - % refugees at the end of conflict that are in host countries that share a land border with source country
        - **Script:** `05a Host State Variables - Geography.R`  
 - Economic conditions:
    - Distribution of refugees at the end of conflict living across host countries with different income levels 
        - Based on GNI per capita[^8] and income classification defined by World Bank[^9]
        - Proportion of refugees living in host countries for whom income data exists adds up to less than one in some cases. The only explanation for that could be that refugees are living in countries that are war-torn themselves and estimates of GNI are not availble. These countries are unlikely to be high-income. Therefore residual proportion is added to low-income variable.
        - **Script:** `05b Host State Variables - Economic conditions.R`  
 - Security:
    - % refugees at the end of conflict that are in host countries that are considered insecure.
        - A host country might be considered insecure if it has >=10,000 refugees in the year the conflict ends
        - A host country might be considered insecure if it has >= 1000 battle-related deaths in the year the conflict ends
        - A host country might be considered insecure if it has at least 5 years of low-level conflict (more than 25 battle related deaths annually) in the ten years after the end of conflict. 
        - **Script:** `05c Host State Variables - Security.R`  
 - Opportunity for employment:
    - % refugees at the end of conflict that are in host countries that have significant informal economies.
        - A host country is defined as Informal if the average proportion of informal economy is higher than the threshold (33%, 50%)
        - **Script:** `05d Host State Variables - Informal Economy.R`  
    - % refugees at the end of conflict that are in host countries with high unemployment rate[^11] 
        - A host country is defined as having high unemployment if the rate exceeds the threshold (5%, 10%, 15%)
        - **Script:** `05e Host State Variables - High Unemployment.R`  
 - Opportunity for education:
    - % refugees at the end of conflict that are in host countries with low education spending
        - A host country is defined as having low education spending if government spending on education as a percentage of GDP is less than the threshold (2%, 3%, 4%) 
        - **Script:** `05f Host State Variables - Low Education Spending.R`  

**Not Included:**  
 - Signatory to 1951 Convention  
    - % refugees at the end of conflict that are in host countries that are signatories to 1951 Convention[^15] 

[^8]: https://data.worldbank.org/indicator/NY.GNP.PCAP.CD
[^9]: http://databank.worldbank.org/data/download/site-content/OGHIST.xls 
[^10]: Hassan & Schneider 2016
[^11]: Clemens et al., “The Economic and Fiscal Effects of Granting Refugees Formal Labor Market Access,” p. 17
[^12]: https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
[^13]: https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS
[^14]: https://elibrary.worldbank.org/doi/pdf/10.1596/1813-9450-8773  
[^15]: https://www.unhcr.org/protect/PROTECTION/3b73b0d63.pdf

## 6. Analysis  
 - **Script:** `06a Visualization.R`
 - **Script:** `06b PCA.R`
 - **Script:** `More analysis.R`
 
