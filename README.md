This is some code to extract temperature and precipitation data from the
Climatic Research Unit's gridded time series data (available
[here](http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22/cruts.1406251334.v3.22/)
(variables `pre` and `tmp`)).

The code in this repository extracts data from the following NetCDF files:

* `cru_ts3.22.1901.2013.pre.dat.nc`
* `cru_ts3.22.1901.2013.tmp.dat.nc`

(not included in the repo because they are close to 3 GB each). It then subsets
the data to match the locations and months of data from the
[Ecology Under Lake Ice](https://www.nceas.ucsb.edu/node/1625) project (data
also not included in the repo because it's not public yet).
