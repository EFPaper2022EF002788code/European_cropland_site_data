# -*- coding: utf-8 -*-

import numpy
import scipy
from scipy import stats
# from scipy.stats import *
import pandas
import xlrd
import xlwt
import math
import os
import re
from osgeo import gdal, gdalconst
from osgeo import ogr, osr
import netCDF4
import openpyxl
import datetime
import seaborn as sns
from collections import OrderedDict
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.font_manager as font_manager
from matplotlib.pyplot import MultipleLocator
from matplotlib.ticker import MaxNLocator, FormatStrFormatter
import matplotlib

# 代码计算的是站点在使用氮肥之后20天内SWC与albedo的相关性，并对农作五类型进行了区分，其中所有的变量都进行了remove seasonality处理

def doy2date(year, doy):
    month_leapyear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    month_notleap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    if year % 4 == 0 and (year % 100 != 0 or year % 400 == 0):
        for i in range(0, 12):
            if doy > month_leapyear[i]:
                doy -= month_leapyear[i]
                continue
            if doy <= month_leapyear[i]:
                month = i + 1
                day = doy
                break
    else:
        for i in range(0, 12):
            if doy > month_notleap[i]:
                doy -= month_notleap[i]
                continue
            if doy <= month_notleap[i]:
                month = i + 1
                day = doy
                break
    return month, day


#sitename = ['BE-Lon', 'DE-Kli', 'FR-Gri','FR-Lam','FR-Aur','CZ-KrP','DE-RuS']



file_dif = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\whole\constant\constant211\nogpp-up\predictors_pdp-DN-211-nogpp-average.csv'

dfa_dif = pandas.read_csv(file_dif, header = 0)
time00= numpy.array(dfa_dif['V1']).tolist()
albedo00= numpy.array(dfa_dif['V2']).tolist() # Standard Deviation  albedoS2  'albedomodis'
type00 = numpy.array(dfa_dif['V4']).tolist()


file_soil = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\whole\constant\constant211\baresoil\all-soilalbedo.csv'
dfa_soil = pandas.read_csv(file_soil, header = 0)
date_soil= numpy.array(dfa_soil['date']).tolist()
albedosoil= numpy.array(dfa_soil['albedo_soil']).tolist() # Standard Deviation  albedoS2  'albedomodis'
soil_ave = numpy.nanmedian(albedosoil)
unique_numbers = list(set(type00))
dift = []
difal=[]
diftype = []

for t in unique_numbers:
    time0 = [time00[i] for i in range(0,len(albedo00)) if type00[i]==t]
    albedo0 = [albedo00[i] for i in range(0,len(albedo00)) if type00[i]==t]
    type0 = [type00[i] for i in range(0,len(albedo00)) if type00[i]==t]

    timedif = [time0[i] for i in range(0, len(albedo0)-1)] #  if time0[i]<=200
    albedodif = [albedo0[i]-albedo0[-1] for i in range(0, len(albedo0)-1)]
    typedif = [type0[i] for i in range(0, len(albedo0)-1)]

    dift.append(timedif)
    difal.append(albedodif)
    diftype.append(typedif)

    data1 = {'time': timedif,
             'albedo': albedodif,
             'type': typedif,
             }
    # print(len(compare_time),len(compare_noon),len(compare_daily))
    df1 = pandas.DataFrame(data1,
                           columns=['time','albedo', 'type'])
    df1.to_csv(
            r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\whole\constant\constant211\nogpp-up' +'\\'+t+'-albedo-dif_finalpoint.csv')

#print(albedodif)
fig, ax3 = plt.subplots(figsize=(10, 8))
plt.subplots_adjust(left=0.15, bottom=0.12)

font = font_manager.FontProperties(style='normal', size=15)

ymajorFormatter = FormatStrFormatter('%1.3f')

for i in range(0,len(dift)):
    #'DN','Dsow','DHer','DHar','Dss','DTil'
    #'Dres','DN','DTil','DHer'
    # 'Kd','Ts','SWC','PA'
    if diftype[i][0] in ['DN','Dsow','DHer','DHar','DFug','DTil']:
        if diftype[i][0]=='DFug':
            color = 'red'
            label = 'D$_{Fug}$'
        elif diftype[i][0]=='DHer':
            color = 'blue'
            label = 'D$_{Her}$'
        elif diftype[i][0]=='DHar':
            color = 'darkgreen'
            label = 'D$_{Har}$'  # 'Tem$\mathregular{p_s}$ (℃)'
        elif diftype[i][0] == 'Dres':
            color = 'limegreen'
            label = 'D$_{res}$'
        elif diftype[i][0] == 'DN':
            color = 'orange'
            label = 'D$_{N}$'
        elif diftype[i][0] == 'DS':
            color = 'violet'
            label = 'D$_{S}$'
        elif diftype[i][0] == 'Dtababean':
            color = 'cyan'
            label = 'D$_{tababean}$'
        elif diftype[i][0] == 'Dgr':
            color = 'darkcyan'
            label = 'D$_{gr}$'
        elif diftype[i][0] == 'DTil':
            color = 'm'
            label = 'D$_{Til}$'
        elif diftype[i][0] == 'Dsow':
            color = 'gray'
            label = 'D$_{sow}$'
        elif diftype[i][0] == 'Dss':
            color = 'pink'
            label = 'D$_{ss}$'
        elif diftype[i][0] == 'DMan':
            color = 'coral'
            label = 'D$_{Man}$'
        elif diftype[i][0] == 'DIns':
            color = 'chocolate'
            label = 'D$_{Ins}$'
        ax3.scatter(dift[i], difal[i], c=color, label=label, marker='o',alpha=0.6,s=80)
    # ['winter wheat', 'maize', 'rapeseed','barley','triticale','Sugar Beet']
    #ax3.scatter(sm_new1[0], a_new1[0], c='', edgecolors='red', alpha=0.2, linewidths=3,  marker='o',s=30, zorder=5)  # winter wheat
    # ax3.plot(sm_p[0], al_p[0], color='red', linewidth=3, label='median', marker='o', markerfacecolor='red',
    #                                                    markersize=0, zorder=10)
    else:
        continue

ax3.set_xlim(-5, 250)
ax3.set_ylim(-0.02, 0.02)
ax3.set_ylabel('The changes in surface albedo', fontsize=20)
ax3.set_xlabel('Days since management practices', fontsize=20)  # gpp Ts Ta swc ws bowen ratio Kd pre THer TCa TN TTil THar
ax3.yaxis.set_major_formatter(ymajorFormatter)
ax3.legend(loc=1, ncol =2, fontsize=20, frameon=False)
ax3.axhline(y=0,color='silver', linestyle='--', lw=2, zorder=0)

# ax2 = ax3.twinx()
# #gpp_p1 = [gpp_p[i]+0.5 for i in range(0,len(gpp_p))]
# ax2.bar(gpp_p, num, fc='w',  linewidth=3, edgecolor = 'gray',alpha= 0.2,width=0.5, zorder=1)
ax3.tick_params(labelsize=20)
# ax2.set_ylim(0, 1000)
# #ax1.set_ylabel('Number of data', fontdict={'size': 16},labelpad=20)
# ax2.set_ylabel('The number of albedo data', fontdict={'size': 13},labelpad=15)

plt.text(x=0.01,  # 文本x轴坐标
         y=0.95,  # 文本y轴坐标
         s='(a)',  # 文本内容
         transform = ax3.transAxes,
         rotation=1,  # 文字旋转
         ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
         va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
         fontdict=dict(fontsize=20, color='black',
          #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                       weight='medium',  # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                       )  # 字体属性设置
         )
#
plt.grid(False)
plt.show()







