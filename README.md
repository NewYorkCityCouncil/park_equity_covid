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

#### FIll
