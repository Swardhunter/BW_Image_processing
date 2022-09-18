# -*- coding: utf-8 -*-
"""
Created on Tue Apr 19 06:12:14 2022
Classification
@author: mskenawi
"""
import glob
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from osgeo import gdal,gdal_array
from osgeo.gdalconst import GA_Update 
import cv2 as cv
import os
import subprocess
import arcpy
from arcpy import env
from arcpy.sa import *
from arcpy.ia import *
import matlab.engine
import sys
import timeit
import time
from multiprocessing import Pool

global RA_RGB
def RA_RGB(Pthin,PTHOUT,Mosiac_Name):
    os.chdir(str(Pthin))
#Import Data 
    R1 = glob.glob("*.tif")
    globals()[f"1_OP_{Mosiac_Name}"] = []
    globals()[f"2_GLCM_{Mosiac_Name}"]  = []
#ARCPY ENVIRONMENT SETTINGS 
    env.workspace = PTHOUT
    env.rasterStatistics= 'STATISTICS'
    env.overwriteOutput = True
    #env.parallelProcessingFactor = "100%"
    env.outputCoordinateSystem = R1[0]
    env.compression = "LZ77"
    start_time = timeit.default_timer()
    print(start_time)
    if len(R1) > (64):
        pool = Pool(64)
        print("Cores used are ",64,flush=True)
    else:
        pool = Pool(len(R1))
        print("Cores used are ",len(R1),flush=True)
    TT = pool.map(PRO, R1)
    pool.close()
    print(timeit.default_timer() - start_time,flush=True)  
    for i in TT:
        #Get First Layers NP 
        Tex = gdal_array.OpenArray(i[0])
        #Set Metadatas 
        Tex.SetGeoTransform(i[2])
        Tex.SetProjection(i[3])
        Tex.SetMetadata(i[4])
        #SAME FOR GLCM 
        GLCM = gdal_array.OpenArray(i[1])
        #Set Metadatas 
        GLCM.SetGeoTransform(i[5])
        GLCM.SetProjection(i[3])
        GLCM.SetMetadata(i[4])
        globals()[f"1_OP_{Mosiac_Name}"].append(Tex)
        globals()[f"2_GLCM_{Mosiac_Name}"].append(GLCM)
    os.chdir(str(PTHOUT))
    print("Start Writing Output")
    print("Start Writing Output")
    start_time = timeit.default_timer()
    D1 = gdal.BuildVRT("Tex",  globals()[f"1_OP_{Mosiac_Name}"])
    gdal.Translate(f"1_OP_{Mosiac_Name}.tif", D1)
    print(timeit.default_timer() - start_time,flush=True)  
    start_time = timeit.default_timer()
    D2 = gdal.BuildVRT("GLCM",  globals()[f"2_GLCM_{Mosiac_Name}"])
    gdal.Translate(f"2_GLCM_{Mosiac_Name}.tif",D2)
    print(timeit.default_timer() - start_time,flush=True) 
    return 





global PRO
def PRO(INP):
    #os.chdir(str(Pthin))
    #Start Matlab
    eng = matlab.engine.start_matlab()
    #Define Matlab Path
    eng.addpath(r'D:\mskenawi\DropBox\PhD\LU_HP\Ecognition\Scripts\CLBP-source-code-master',nargout=0)
    I = gdal.Open(INP)
    gt = I.GetGeoTransform()
    pr = I.GetProjection()
    mt = I.GetMetadata()
    [RAW_Gray,CLBP_S_PCA,CLBP_S_MED,CLBP_MC_PCA] =  eng.TE_Color(rf'{Pthin}\{INP}',nargout = 4);
    HSI_OP = ColorspaceConversion(INP, "rgb_to_hsv")
    HSI_OPN = arcpy.RasterToNumPyArray(HSI_OP)
    RAW_Gray = np.array(RAW_Gray._data,dtype = np.uint8).reshape(RAW_Gray.size[::-1]).transpose()
    CLBP_S_PCA = np.array(CLBP_S_PCA._data,dtype = np.uint8).reshape(CLBP_S_PCA.size[::-1]).transpose()
    CLBP_MC_PCA = np.array(CLBP_MC_PCA._data,dtype = np.uint8).reshape(CLBP_MC_PCA.size[::-1]).transpose()
    CLBP_S_MED = np.array(CLBP_S_MED._data,dtype = np.uint8).reshape(CLBP_S_MED.size[::-1]).transpose()
    TEXT = np.stack((RAW_Gray,CLBP_S_PCA,CLBP_MC_PCA,CLBP_S_MED))
    RGB  = I.ReadAsArray()
    OP = np.concatenate((TEXT,RGB,HSI_OPN))
    #gdal_array.CopyDatasetInfo(I,OP)
#GLCM 
    os.system(fr'D:\mskenawi\DropBox\PhD\LU_HP\Ecognition\Scripts\OTB-7.4.0-Win64\bin\otbcli_HaralickTextureExtraction.bat -in "{INP}" -step 3 -parameters.xrad 7 -parameters.yrad 7 -parameters.xoff -1 -parameters.yoff -1 -parameters.nbbin 12 -texture advanced -out "{PTHOUT}/GLCM_ADV{INP[:-4]}.tif" float -ram 100000')
    os.system(fr'D:\mskenawi\DropBox\PhD\LU_HP\Ecognition\Scripts\OTB-7.4.0-Win64\bin\otbcli_HaralickTextureExtraction.bat -in "{INP}" -step 3 -parameters.xrad 7 -parameters.yrad 7 -parameters.xoff -1 -parameters.yoff -1 -parameters.nbbin 12 -texture simple -out "{PTHOUT}/GLCM_SIMP_{INP[:-4]}" float -ram 100000')
    R2 = glob.glob(str(PTHOUT)+fr'/*{INP[:-4]}*')   
    I1 = gdal.Open(R2[0])
    gt_GLCM = I1.GetGeoTransform()
    I1_N = gdal.Open(R2[0]).ReadAsArray()
    I1_N = I1_N[(0,1),:,:]
    I2 = gdal.Open(R2[1]).ReadAsArray()     
        #GET BANDS 2,3,5,6 Correlation, Cluster shade , Cluster prominance ,Contrast , Inerita 
    I2 = I2[(1,2,4,5),:,:]    
    I_GLCM = np.concatenate((I1_N,I2))
    #gdal_array.CopyDatasetInfo(I1,I_GLCM)
    I1 = None
    OP2 = [OP,I_GLCM,gt,pr,mt,gt_GLCM]
    for i in R2:
        os.remove(i)
    return OP2

Pthin = r'D:\mskenawi\one drive\RawData\Reservoir_T3\Rana_A1'
PTHOUT = r'D:\mskenawi\DropBox\PhD\LU_HP\Ecognition\WorkSpaces\Rana\T3\Data'
Mosiac_Name = "Rana_A1"

if __name__ == '__main__':
     #Start Matlab
    eng = matlab.engine.start_matlab()
    #Define Matlab Path
    eng.addpath(r'D:\mskenawi\DropBox\PhD\LU_HP\Ecognition\Scripts\CLBP-source-code-master',nargout=0)  
    #Define the OTB application for GLCM 
    subprocess.run(r"D:\mskenawi\DropBox\PhD\LU_HP\Ecognition\Scripts\OTB-7.4.0-Win64",shell = True);subprocess.run("otbApplicationLauncherCommandLine",shell = True)
    RA_RGB(Pthin,PTHOUT,Mosiac_Name)




