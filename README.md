## Park Equity  
Data analysis and visuals for NYCC 10.22.20 ['Oversight - Improving the Equity of Green Space throughout the City in Light of the COVID Epidemic'](https://legistar.council.nyc.gov/MeetingDetail.aspx?ID=806382&GUID=9E623EFF-9EC5-499E-B681-95FF4CAB9A08&Options=info|&Search=) hearing.


### Data Sources 
- [Walk-to-a-Park Service Area](https://data.cityofnewyork.us/Recreation/Walk-to-a-Park-Service-area/5vb5-y6cv)
- [Open Space (Parks)](https://data.cityofnewyork.us/Recreation/Open-Space-Parks-/g84h-jbjm)
- [Parks Properties](https://data.cityofnewyork.us/Recreation/Parks-Properties/enfh-gkve)
- [Waterfront Access Map: Publicly Owned Waterfront](https://data.cityofnewyork.us/City-Government/Waterfront-Access-Map-Data-Shapefile/388s-pnvc)
- [2010 Census Tracts](https://data.cityofnewyork.us/City-Government/2010-Census-Tracts/fxpq-c8ku)
- 2018 5-Year ACS Survey: *We used R package censusapi to get demographic data
  - [Income](https://data.census.gov/cedsci/table?q=income%20census%20tracts%20nyc&g=0500000US36005.140000,36047.140000,36061.140000,36081.140000,36085.140000&tid=ACSST5Y2018.S1901&hidePreview=true)
  - [Population](https://data.census.gov/cedsci/table?q=population%20census%20tracts%20nyc&g=0500000US36005.140000,36047.140000,36061.140000,36081.140000,36085.140000&tid=ACSDP5Y2018.DP05&hidePreview=true)
- Parks Closure Status Due to COVID-19:
  - [Athletic Facitilies](https://data.cityofnewyork.us/dataset/Parks-Closure-Status-Due-to-COVID-19-Athletic-Faci/g3xg-qtbc)
  - [Skate Parks](https://data.cityofnewyork.us/dataset/Parks-Closure-Status-Due-to-COVID-19-Skate-Parks/pvvr-75zk)
  - [Dog Runs](https://data.cityofnewyork.us/dataset/Parks-Closure-Status-Due-to-COVID-19-Dog-Runs/wswf-9pts)
  - [Adult Exercise Equipment](https://data.cityofnewyork.us/City-Government/Parks-Closure-Status-Due-to-COVID-19-Adult-Exercis/tkzt-zfpz)
  - [Playgrounds](https://data.cityofnewyork.us/dataset/Parks-Closure-Status-Due-to-COVID-19-Playgrounds/a4qt-mpr5)


### Methodology 

#### Summary & Intention
- Calculate how much square feet of park space individuals in NYC have access to at the census tract and zipcode level.
- See if there are geographic & income disparities in access to park space & the relation to COVID cases.


#### Parks & Open Spaces included in Analysis
From the [Open Space (Parks)](https://data.cityofnewyork.us/Recreation/Open-Space-Parks-/g84h-jbjm), [Parks Properties](https://data.cityofnewyork.us/Recreation/Parks-Properties/enfh-gkve) and [Waterfront Access Map](https://data.cityofnewyork.us/City-Government/Waterfront-Access-Map-Data-Shapefile/388s-pnvc) datasets, features that were within 50 ft of an access point from the [Walk-to-a-Park Service Area](https://data.cityofnewyork.us/Recreation/Walk-to-a-Park-Service-area/5vb5-y6cv) were included as the set of parks, open space and recreation areas for analysis. This excludes cemeteries. For more information on what parks were included in NYC Parks **Walk-to-a-Park Service Area** dataset, please view their [data dictionary](https://data.cityofnewyork.us/api/views/5vb5-y6cv/files/d1a297b8-4819-4bb4-93e8-a59293779abb?download=true&filename=Walk-to-a-park_DataDictionary_20170901.xlsx). 

#### 10 Minute Walking Distance
Using the access points from Walk-to-a-Park Service Area dataset, we created isochrone polygons or time-distance areas for each point. We used mapbox api for this process and selected a 10 minute walking distance parameter. The walking distance is set from the center of each census tract to the access point of each park. If the center of a census tract is within 10-minutes walking of any access point associated with a given park, it is designated as having access to that park. 

#### Census Tract

#### Zip Code
