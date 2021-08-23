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

###View these instructions when getting started with a new cds user:
###https://cds.climate.copernicus.eu/api-how-to
###You must also agree to the terms and conditions:
###https://cds.climate.copernicus.eu/cdsapp/#!/terms/cmip6-wps
c = cdsapi.Client()

###Specify a file path here
file_path = os.getcwd() + "\\downloads\\"
#file_path = "D:\\User\\Desktop\\Work\\downloads\\"

###Generating area from shapefiles
shape = shapefile.Reader(os.path.join(file_path,"attachments_WGS84//finland//AOI/AOI_FDTEP_T35VLK_VLJ_20210221_WGS84.shp"))
feature = shape.shapeRecords()[0]
shapedata = feature.shape.__geo_interface__ #lon,lat format
area = [-91, 181, 91, -181] # list with form North, West, South, East 
for f in shapedata["coordinates"][0]: # set area equal to the shapefile bounds, rounded accordingly
    if f[0] > area[0]:
        area[0] = int(np.ceil(f[0]))
    elif f[0] < area[2]:
        area[2] = int(np.floor(f[0]))
    if f[1] < area[1]:
        area[1] = int(np.floor(f[1]))
    elif f[1] > area[3]:
        area[3] = int(np.ceil(f[1]))
print(area)

###Specify parameters here or call function download() with them. Follow this example format.
experiment = "ssp1_2_6"
variables = ["near_surface_air_temperature","near_surface_specific_humidity","precipitation"]
model = "hadgem3_gc31_ll"
date = "2015-01-01/2060-12-31"
monthlyrad = True # Include monthly radiation data?

###Request data downloads
def download(experiment, variables, model, date, area, monthlyrad):
    for variable in variables:
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
    ###if requested, include monthly radiation data
    if monthlyrad:
        c.retrieve(
        'projections-cmip6',
        {
            'format': 'zip',
            'temporal_resolution': 'monthly',
            'experiment': experiment,
            'level': 'single_levels',
            'variable': 'surface_upwelling_shortwave_radiation',
            'model': model,
            'date': date,
            'area': area,
        },
        os.path.join(file_path,'surface_upwelling_shortwave_radiation_download.zip'))
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
        print(varname)
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
    print("returning")
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
    print(dfs)
    try:
        dff = pd.concat(dfs, axis=1)
        dff.to_csv(os.path.join(file_path,"data.csv"))
    except:
        dfs[0].to_csv(os.path.join(file_path,"data.csv"))
processcsv()
