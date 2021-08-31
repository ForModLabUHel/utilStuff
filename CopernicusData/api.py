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

###Specify a file path here
file_path = os.getcwd() + "\\downloads\\" # using working directory
#file_path = "D:\\User\\Desktop\\Work\\downloads\\" # using (example) absolute path

###Generating area from shapefiles
shape = shapefile.Reader(os.path.join(file_path,"attachments_WGS84//romania//AOI//romania_weather_aoi_WGS84.shp"))
feature = shape.shapeRecords()[0]
shapedata = feature.shape.__geo_interface__ #lon,lat format
area = [-91, 181, 91, -181] # list with form North, West, South, East 
for f in shapedata["coordinates"][0]: # set area equal to the shapefile bounds, rounded strictly
    if f[0] > area[0]:
        area[0] = int(np.ceil(f[0]))
    elif f[0] < area[2]:
        area[2] = int(np.floor(f[0]))
    if f[1] < area[1]:
        area[1] = int(np.floor(f[1]))
    elif f[1] > area[3]:
        area[3] = int(np.ceil(f[1]))
###Check if area is large enough considering resolution of CMIP6 data
if (abs(area[0] - area[2]) < 2) or (abs(area[3] - area[1]) < 2):
    area[0] += 1
    area[1] -= 1
    area[2] -= 1
    area[3] += 1
print(area)

###Specify parameters here or call function download() with them. Follow this example format.
experiment = "ssp1_2_6"
variables = ["near_surface_air_temperature","near_surface_specific_humidity","precipitation"]
model = "hadgem3_gc31_ll"
date = "2020-01-01/2060-12-31"
monthlyrad = True # Include monthly radiation data?

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
def processcsv():
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
        dff.to_csv(os.path.join(file_path,"data.csv"))
    except:
        dfs[0].to_csv(os.path.join(file_path,"data.csv"))
processcsv()

def formatcsv(monthlyrad):
    df = pd.read_csv(os.path.join(file_path,"data.csv"))
    df.insert(3,"date",pd.to_datetime(df["time"]))
    
    ###Create climID for the different coordinates within the specified area
    clims = df.groupby(["date"]).count().iloc[0,0] + 1
    x = [i for i in range(1,clims)]
    for i in range(len(df) - len(x)):
        x.append(x[i])
    df.insert(0,"climID",x)
    df = df.sort_values(["climID","date"])
    
    df["date"] = df["date"].dt.strftime("%m/%d/%Y")
    df = df.set_index("date")
    #df["doy"] = pd.to_datetime(df["time"]).dt.dayofyear
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
    
    ###Call R code with rpy2 to create the raster (check packages)
    utils = importr('utils')
    utils.chooseCRANmirror(ind=1)
    utils.install_packages("sp")
    utils.install_packages("lattice")   
    utils.install_packages("raster")
    pandas2ri.activate()
    ro.globalenv['rdf'] = dfraster
    ro.globalenv['file'] = os.path.join(file_path,"data.tiff")
    ro.r('''
                    library(raster)
                    rdf <- rasterFromXYZ(rdf)
                    writeRaster(x=rdf, filename=file)
                    ''')
        
    ###Remove any unnecessary columns here
    del df["lat"]
    del df["lon"]
    del df["date"]
    df = df.set_index("climID")
    df.to_csv(os.path.join(file_path,"data.csv"))
formatcsv(monthlyrad)

def clean():
    for file in os.listdir(file_path):
        if file.endswith(".zip") or file.endswith(".nc") or file.endswith(".json") or file.endswith(".png"):
            os.remove(file)
clean()
