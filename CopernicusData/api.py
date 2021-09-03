# -*- coding: utf-8 -*-
"""
Created on Thu Aug 19 11:57:25 2021
@author: vhketola
"""
import cdsapi
import os
import numpy as np
import pandas as pd
from zipfile import ZipFile
import netCDF4
import shapefile

import rpy2
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri


###View these instructions when getting started with a new cds user:
###https://cds.climate.copernicus.eu/api-how-to
###You must also agree to the terms and conditions:
###https://cds.climate.copernicus.eu/cdsapp/#!/terms/cmip6-wps
c = cdsapi.Client()
###Specify the filepath here
file_path = os.getcwd() + "\\downloads\\" # using working directory
# file_path = "D:\\User\\Desktop\\Work\\downloads\\" # using (example) absolute path
###Specify the shapefile country here
shapename = "germany"
###Specify the parameters here
experiment = "ssp1_2_6" #1_2_6, 2_4_5, 5_8_5
model = "hadgem3_gc31_ll"
date = "2020-01-01/2060-12-31"


def createcsv(file_path, shapename, experiment, model, date):
    ###Determine whether to include monthly radiation data (daily not available)
    monthlyrad = True
    ###Variables with daily data to include
    variables = ["near_surface_air_temperature"
                 ,"near_surface_specific_humidity"
                 ,"precipitation"]
    
    ###Extract area for CMIP6 from the shapefile
    shape = shapefile.Reader(os.path.join(file_path,"attachments_WGS84//" + shapename + "//AOI//" + shapename + "_weather_aoi_WGS84.shp"))
    feature = shape.shapeRecords()[0]
    shapedata = feature.shape.__geo_interface__ #lon,lat format
    area = [-91, 181, 91, -181] # list with form North, West, South, East 
    for f in shapedata["coordinates"][0]: # set area equal to the shapefile bounds, rounded strictly
        if f[1] > area[0]:
            area[0] = int(np.ceil(f[1]))
        elif f[1] < area[2]:
            area[2] = int(np.floor(f[1]))
        if f[0] < area[1]:
            area[1] = int(np.floor(f[0]))
        elif f[0] > area[3]:
            area[3] = int(np.ceil(f[0]))
    # print(area)
    ###Check if area is large enough considering resolution of CMIP6 data
    if (abs(area[0] - area[2]) < 4) or (abs(area[3] - area[1]) < 4):
        area[0] += 1
        area[1] -= 1
        area[2] -= 1
        area[3] += 1
    print(area)

    ###Request data downloads
    def download(experiment, variables, model, date, area, monthlyrad):
        for variable in variables:
            try:
                c.retrieve(
                'projections-cmip6',
                {
                    'format': 'zip',
                    'temporal_resolution': 'daily',
                    'experiment': experiment,
                    'level': 'single_levels',
                    'variable': variable,
                    'model': model,
                    'date': date,
                    'area': area,
                },
                os.path.join(file_path, (variable + '_download.zip')))
            except:
                print("Error downloading data for " + variable + ", check that the area and time are correct.")
                continue
        ###if requested, include monthly radiation data
        if monthlyrad:
            try:
                c.retrieve(
                'projections-cmip6',
                {
                    'format': 'zip',
                    'temporal_resolution': 'monthly',
                    'experiment': experiment,
                    'level': 'single_levels',
                    'variable': 'surface_downwelling_shortwave_radiation',
                    'model': model,
                    'date': date,
                    'area': area,
                },
                os.path.join(file_path,'surface_downwelling_shortwave_radiation_download.zip'))
            except:
                print("Error downloading data for surface_downwelling_shortwave_radiation, check that the area and time are correct.")
    download(experiment, variables, model, date, area, monthlyrad)
    
    
    ###Extract all the zip files in the download location
    def unzip():
        for file in os.listdir(file_path):
            if file.endswith(".zip"):
                with ZipFile(os.path.join(file_path,file),"r") as z:
                    z.extractall(file_path)
    unzip()
    
    ###Create a concatenated dataframe for each variable and then merge
    def processdfs(varname):
        ###We add dataframes to a list and then concatenate them when done
        dfs = []
        for file in os.listdir(file_path):
            # print(varname)
            if file.endswith(".nc") and file.startswith(varname):
                f = netCDF4.Dataset(os.path.join(file_path,file))
                # print(f.variables.keys())
                variables = f.variables[varname]
                data = variables[:,:,:]
                
                ###We write a csv by creating a df with indexes:
                time_dim,lat_dim,lon_dim = variables.get_dims()
                time_var = f.variables[time_dim.name]
                times = netCDF4.num2date(time_var[:],time_var.units)
                lats = f.variables[lat_dim.name][:]
                lons = f.variables[lon_dim.name][:]
                
                colnames = ['time', 'lat', 'lon']
                idx = pd.MultiIndex.from_product([times,lats,lons], names=colnames)
                df = pd.DataFrame({varname:data.flatten()},index=idx)
                dfs.append(df)
        return pd.concat(dfs)
    def processcsv(shapename,experiment):
        first = True
        varnames = []
        dfs = []
        for file in os.listdir(file_path):
            if file.endswith(".nc"):
                varname = "" # assuming file naming format has variable name first
                for s in file:
                    if s == "_":
                        break
                    else:
                        varname += s
                if first:
                    varnames.append(varname)
                    first = False
                else:
                    if varname not in varnames: # create df for previous dataset if new dataset encountered
                        dfs.append(processdfs(varnames[0]))
                        varnames = [varname]
        dfs.append(processdfs(varnames[0])) # create df for previous dataset if all datasets have been encountered
        # print(dfs)
        try:
            dff = pd.concat(dfs, axis=1)
            dff.to_csv(os.path.join(file_path, shapename + "_" + experiment + ".csv"))
        except:
            dfs[0].to_csv(os.path.join(file_path,shapename + "_" + experiment + ".csv"))
    processcsv(shapename,experiment)
    
    def formatcsv(monthlyrad,shapename,experiment,date):
        df = pd.read_csv(os.path.join(file_path,shapename + "_" + experiment + ".csv"))
        df.insert(3,"date",pd.to_datetime(df["time"]).dt.date)
        
        ###Create climID for the different coordinates within the specified area
        clims = df.groupby(["date"]).count().iloc[0,0] + 1
        x = [i for i in range(1,clims)]
        for i in range(len(df) - len(x)):
            x.append(x[i])
        df.insert(0,"climID",x)
        df = df.sort_values(["climID","date"])
        
        ###Include CO2 data from csv
        ##Meinshausen et al. GMD 2017 (https://doi.org/10.5194/gmd-10-2057-2017); 
        ##Meinshausen et al., GMD, 2020 (https://doi.org/10.5194/gmd-2019-222)
        co2 = pd.read_csv(os.path.join(file_path,experiment + "_co2.csv"))
        # Start and end dates according to format: int(date[0:4])  int(date[11:15])
        
        ###For each row, CO2 is chosen according to year and latitude 
        co2 = co2.set_index("Year")
        co2 = co2.loc[int(date[0:4]):int(date[11:15]),["Northern Hemisphere","Southern Hemisphere"]]
        co2.index = pd.to_datetime(co2.index, format='%Y').date
        df = pd.merge(df,co2,left_on="date",right_index=True,how="left")
        df["Northern Hemisphere"] = df["Northern Hemisphere"].ffill()
        df["Southern Hemisphere"] = df["Southern Hemisphere"].ffill()
        df["Northern Hemisphere"] = df["Northern Hemisphere"].where(df["lat"] > 0)
        df["Southern Hemisphere"] = df["Southern Hemisphere"].where(df["lat"] <= 0)
        df["CO2"] = df["Northern Hemisphere"].fillna(df["Southern Hemisphere"])
        del df["Northern Hemisphere"]
        del df["Southern Hemisphere"]
        df = df.sort_values(["climID","date"])
        
        df["date"] = pd.to_datetime(df["date"]).dt.strftime("%m/%d/%Y")
        df = df.set_index("date")
        del df["time"]
        
        ###Shift all monthly data 
        if monthlyrad:
            dfn = df[["rsds","climID"]].dropna()
            dfn.index = (pd.to_datetime(df[["rsds","climID"]].dropna().index) + pd.tseries.offsets.MonthEnd(1)).strftime("%m/%d/%Y")
            del df["rsds"]
            df = pd.merge(df,dfn,left_on=["climID","date"],right_on=["climID","date"],how="left")
            df["rsds"] = df["rsds"].backfill()
            df = df.dropna(axis=0)
            df = df.reset_index()
        
        ###Create a climID df for the raster
        dftemp = df[["climID","lat","lon"]]
        vals = {"climID":[i for i in range(1,clims)]
                , "lat":[dftemp[dftemp["climID"]==i].iloc[0][1] for i in range(1,clims)]
                , "lon":[dftemp[dftemp["climID"]==i].iloc[0][2] for i in range(1,clims)]}
        dfraster = pd.DataFrame(vals)
        dfraster["y"] = dfraster["lat"]
        del dfraster["lat"]
        dfraster["x"] = dfraster["lon"]
        del dfraster["lon"]
        dfraster["z"] = dfraster["climID"]
        del dfraster["climID"]
        dfraster = dfraster[["x","y","z"]]
        
        ##Call R code with rpy2 to create the raster (check packages)
        utils = importr('utils')
        utils.chooseCRANmirror(ind=1)
        pandas2ri.activate()
        ro.globalenv['rdf'] = dfraster
        ro.globalenv['file'] = os.path.join(file_path, shapename + "_" + experiment + ".tiff")
        ro.r('''
                        library(raster)
                        print(rdf)
                        if (length(unique(rdf$y)) > 1) { # R throws an error if there are no unique y values
                                rdf <- rasterFromXYZ(rdf)
                                } else {
                                    rdf <- rasterFromXYZ(rdf, res=c(NA, 1))
                                    }
                        writeRaster(x=rdf, filename=file, overwrite = TRUE)
                        ''')
        
        ###Remove any unnecessary columns here
        del df["lat"]
        del df["lon"]
        del df["date"]
        df = df.set_index("climID")
        df.to_csv(os.path.join(file_path,shapename + "_" + experiment + ".csv"))
    formatcsv(monthlyrad,shapename,experiment,date)
    
    def convert(shapename,experiment): # Adapted from forPRELESinput.r by Xianglin Tian
        df = pd.read_csv(os.path.join(file_path,shapename + "_" + experiment + ".csv"))
        
        # The shortwave radiation (SW, W /m2) can be converted to PAR (mol/m2/day) 
        # External to the earth’s atmosphere, the ratio of PAR to total solar radiation is 0.44. 
        # Then we can use 4.56 (µmol/J or mol/MJ) to convert the unit as PRELES requests. 
        df["PAR"] = df["rsds"]*0.44*4.56/1e6*60*60*24
        
        # Temperature in the atmosphere. It has units of Kelvin (K). 
        # Temperature measured in kelvin can be converted to degrees Celsius (°C) by subtracting 273.15.
        df["TAir"] = df["tas"]-273.15
        
        # Precipetation from kg m-2 s-1 or mm s-1 to mm day-1
        df["Precip"] = df["pr"]*60*60*24
        
        ##' Convert specific humidity to relative humidity
        ##' from Bolton 1980 The computation of Equivalent Potential Temperature 
        ##' \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
        ##' @param qair specific humidity, dimensionless (e.g. kg/kg) ratio of water mass / total air mass
        ##' @param temp degrees C
        ##' @param press pressure in mb
        ##' @return rh relative humidity, ratio of actual water mixing ratio to saturation mixing ratio
        ##' @author David LeBauer in Stack
        def qair2rh(qair,temp,press=1013.25):
            es = 6.112 * np.exp((17.67 * temp)/(temp + 243.5))
            e = qair * press / (0.378 * qair + 0.622)
            rh = e / es
            rh[rh > 1] = 1
            rh[rh < 0] = 0
            return rh
        
        SVP = 610.7 * 10**(7.5*df["TAir"]/(237.3+df["TAir"]))
        RH = qair2rh(df["huss"],df["TAir"])
        df["VPD"] = SVP * (1-RH) / 1000
        
        del df["huss"]
        del df["pr"]
        del df["tas"]
        del df["rsds"]
        
        df = df.set_index("climID")
        df = df[["PAR", "TAir", "Precip", "VPD", "CO2"]]
        
        df.to_csv(os.path.join(file_path,shapename + "_" + experiment + ".csv"))
    convert(shapename, experiment)
    
    ###Remove leftover downloaded files
    def clean():
        for file in os.listdir(file_path):
            if file.endswith(".zip") or file.endswith(".nc") or file.endswith(".json") or file.endswith(".png"):
                os.remove(os.path.join(file_path,file))
    clean()

createcsv(file_path, shapename, experiment, model, date)
