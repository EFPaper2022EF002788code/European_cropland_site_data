# -*- coding: utf-8 -*-

import numpy
import scipy
from scipy import stats
import pandas
import math
import datetime
import os
import xlrd
import xlwt
import re
from osgeo import gdal, gdalconst
from osgeo import ogr, osr
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import statsmodels.api as sm
from mpl_toolkits.axisartist.parasite_axes import HostAxes, ParasiteAxes
import matplotlib.font_manager as font_manager
from matplotlib.pyplot import MultipleLocator
from matplotlib.ticker import MaxNLocator, FormatStrFormatter
from matplotlib.pyplot import MultipleLocator
import matplotlib

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

def ymd(date):
    """

    :param date: extracted date from file
    :return: year, month , day
    """
    year='2004'
    month='01'
    day = '01'
    if '-' in date:
        year = date.split('-')[0]
        month = date.split('-')[1]
        day = date.split('-')[2]
    elif '/' in date:
        year = str(date.split('/')[2])
        if len(date.split('/')[0]) == 1:
            month = '0'+str(date.split('/')[0])
        elif len(date.split('/')[0]) == 2:
            month = str(date.split('/')[0])
        if len(date.split('/')[1]) == 1:
            day = '0'+str(date.split('/')[1])
        elif len(date.split('/')[1]) == 2:
            day = str(date.split('/')[1])
    else:
        year = date[0:4]
        month = date[4:6]
        day = date[6:8]
    return year, month,day


# RF = -(1/Nday)*S(SW_IN(d)*Ta(d)*Δa(d))  Δa(d) = a(d after management) - mean a (d before management)


file = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\whole\constant\constant211\predictors-whole-211.csv'
#,'DHer' 'DN','DFug','DHer','Dres','Dgr','DHar','DTil' 'Tmustard'  'DFug','DHer','Dres','Tmustard'
mm = ['DN','DFug','DHar','DHer','Dsow','DTil']
#mm = ['Dres','DN','DTil','DHer']
#mm = ['Kd','Ts','SWC','PA']
#mm = ['DN']
dfa = pandas.read_csv(file, header = 0)
time = numpy.array(dfa['time']).tolist()
site = numpy.array(dfa['site']).tolist()

# kd = numpy.array(dfa['Kd']).tolist()
# ts = numpy.array(dfa['Ts']).tolist()
# swc = numpy.array(dfa['SWC']).tolist()
# pa = numpy.array(dfa['PA']).tolist()

nitro = numpy.array(dfa['DN']).tolist()
fung = numpy.array(dfa['DFug']).tolist()
her = numpy.array(dfa['DHer']).tolist()
#res = numpy.array(dfa['Dres']).tolist()
# # man = numpy.array(dfa['DMan']).tolist()
# # #mus = numpy.array(dfa['Tmustard']).tolist()
# # gr = numpy.array(dfa['Dgr']).tolist()
har = numpy.array(dfa['DHar']).tolist()
til = numpy.array(dfa['DTil']).tolist()
# # ins = numpy.array(dfa['DIns']).tolist()
# #ss = numpy.array(dfa['Dss']).tolist()
sow = numpy.array(dfa['Dsow']).tolist()
# #S = numpy.array(dfa['DS']).tolist()

#
toa_radi1 = []
in_radi1 = []
time_radi1= []
site_zong1 = []
mana_zong1 = [[],[],[],[],[],[]]

file_radi = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\radiationforcing\radiation_extractionfromflux\hebing_pipei_radiance.csv'
#file_radi = r'E:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\Kd\hebing.csv'
dfa1 = pandas.read_csv(file_radi, header = 0)

time_radi = numpy.array(dfa1['time']).tolist()
site_radi = numpy.array(dfa1['site']).tolist()
totalradiance = numpy.array(dfa1['toaradi']).tolist()   # 'toaradi'  'totalradiance'
#diffuseradiance = numpy.array(dfa1['diffuseradiance']).tolist()
incomingradiance = numpy.array(dfa1['inradi']).tolist()   # 'inradi'  'incomingradiance'
#print(time_radi)
#     print('===============finish swin extraction ==================================')

for i in range(0,len(time)):
    if time[i] in time_radi:
        hang1 = time_radi.index(time[i])
        if site_radi[hang1] == site[i]:
            toa_radi1.append(totalradiance[hang1])
            in_radi1.append(incomingradiance[hang1])
            time_radi1.append(time[i])
            site_zong1.append(site[i])

            # mana_zong1[0].append(kd[i])
            # mana_zong1[1].append(ts[i])
            # mana_zong1[2].append(swc[i])
            # mana_zong1[3].append(pa[i])

            mana_zong1[0].append(nitro[i])
            mana_zong1[1].append(fung[i])
            mana_zong1[2].append(her[i])
            mana_zong1[3].append(har[i])
          #  # mana_zong1[6].append(S[i])
          #   # mana_zong1[9].append(ins[i])
          #  # mana_zong1[4].append(ss[i])
            mana_zong1[4].append(sow[i])
          #  mana_zong1[0].append(res[i])
          #   # mana_zong1[4].append(gr[i])
          #   # mana_zong1[5].append(man[i])
            mana_zong1[5].append(til[i])

            # print(mana[i])
            # print(mana_zong1)
        else:
            continue
    else:
        continue
#print(mana_zong1)
#print(len(time_radi1),len(mana_zong1))
# print(toa_radi)

inradi_day = []
toaradi_day = []
time_day = []
num_day = []
for k in range(0,len(mana_zong1)):
    inradi_sub = []
    toaradi_sub = []
    time_sub = []
    num_sub = []


    if k <20:

        for i in range(0,211):

            inradi = [in_radi1[j] for j in range(0,len(time_radi1)) if int(mana_zong1[k][j]) ==i]
            toaradi = [toa_radi1[j] for j in range(0,len(time_radi1)) if int(mana_zong1[k][j]) ==i]
            if len(inradi)>=1:

                inradi_sub.append(numpy.nanmean(inradi))
                toaradi_sub.append(numpy.nanmean(toaradi))
                time_sub.append(i)
                num_sub.append(len(inradi_sub))
            else:
                continue


    inradi_day.append(inradi_sub)
    toaradi_day.append(toaradi_sub)
    time_day.append(time_sub)
    num_day.append(num_sub)


file_dif = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\whole\constant\constant211\nogpp-up\predictors_pdp-DN-211-nogpp-average.csv'

dfa_dif = pandas.read_csv(file_dif, header = 0)
time00= numpy.array(dfa_dif['V1']).tolist()
albedo00= numpy.array(dfa_dif['V2']).tolist() # Standard Deviation  albedoS2  'albedomodis'
type00 = numpy.array(dfa_dif['V4']).tolist()


dift = []
difal=[]
difincom = []
difta = []
diftype = []

for k in range(0,len(mm)):
    time0 = [round(time00[i]) for i in range(0,len(albedo00)) if type00[i]==mm[k]]
    albedo0 = [albedo00[i] for i in range(0,len(albedo00)) if type00[i]==mm[k]]
    type0 = [type00[i] for i in range(0,len(albedo00)) if type00[i]==mm[k]]

    timedif = [time0[i] for i in range(0, len(albedo0)-1)] #if time0[i]<=200
    albedodif = [albedo0[i]-albedo0[-1] for i in range(0, len(albedo0)-1)]
    typedif = [type0[i] for i in range(0, len(albedo0)-1)]
    #print(len(inradi_day),len(timedif))
    deltaa = []
    inradi = []
    Ta = []
    timeaa = []
    typeaa = []
    for j in range(0,len(timedif)):
        if timedif[j] in time_day[k]:
            hh = time_day[k].index(timedif[j])
            deltaa.append(-(albedodif[j])*(inradi_day[k][hh]/toaradi_day[k][hh])*inradi_day[k][hh])
            inradi.append(inradi_day[k][hh])
            Ta.append(inradi_day[k][hh]/toaradi_day[k][hh])
            timeaa.append(timedif[j])
            typeaa.append(typedif[j])
    dift.append(timeaa)
    difincom.append(inradi)
    difta.append(Ta)
    difal.append(deltaa)
    diftype.append(typeaa)

    data1 = {'time': timeaa,
             'rf': deltaa,
             'inradi':inradi,
             'ta': Ta,
             'type': typeaa,
             }
    # print(len(compare_time),len(compare_noon),len(compare_daily))
    df1 = pandas.DataFrame(data1,
                           columns=['time', 'rf', 'inradi','ta','type'])
    # df1.to_csv(
    #          r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\fallow\constant\constant151\nogpp-up' +'\\'+mm[k]+'-albedo-RF-fallow-1123.csv')

#  print(deltaa)
    #rf1 = sum(deltaa)

# rf2 = (1/len(timedif))*sum(deltaa)
#
#print(diftype)
# print(gpp_p)
fig, ax3 = plt.subplots(figsize=(10, 8))
plt.subplots_adjust(left=0.15, bottom=0.12)

font = font_manager.FontProperties(style='normal', size=15)

ymajorFormatter = FormatStrFormatter('%1.2f')

#print(len(dift))
for i in range(0,len(dift)):
  #  print(dift[i])
  #['DN','DFug','DHar','DHer','Dsow','DTil']:
  #['Dres','DN','DTil','DHer']
    if diftype[i][0] in ['DN','DFug','DHar','DHer','Dsow','DTil']:
        if diftype[i][0]=='DFug':
            color = 'red'
            label = 'D$_{Fug}$'
        elif diftype[i][0]=='DHer':
            color = 'blue'
            label = 'D$_{Her}$'
        elif diftype[i][0]=='DHar':
            color = 'darkgreen'
            label = 'D$_{Har}$'
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
        else:
            continue
        ax3.scatter(dift[i], difal[i], c=color, label=label, marker='o', alpha=0.6, s=80)
    # else:
    #     continue


   # ax3.scatter(timedif, rf1, c=color, label=unique_numbers[0], marker='o', alpha=0.6, s=80)

ax3.set_xlim(-5, 250)
ax3.set_ylim(-7, 7)
ax3.set_ylabel('Radiative Forcing (W'+ ' $\mathregular{m^-}$' + '$\mathregular{^2}$)', fontsize=20)
ax3.set_xlabel('Days since management practices', fontsize=20)  # gpp Ts Ta swc ws bowen ratio Kd pre THer TCa TN TTil THar
ax3.yaxis.set_major_formatter(ymajorFormatter)
ax3.legend(loc=1, ncol=2, fontsize=20, frameon=False)
ax3.axhline(y=0,color='silver', linestyle='--', lw=2, zorder=0)
#ax3.axhline(y=rf1,color='black', linestyle='-', lw=2, zorder=0)

# ax2 = ax3.twinx()
# #gpp_p1 = [gpp_p[i]+0.5 for i in range(0,len(gpp_p))]
# ax2.bar(gpp_p, num, fc='w',  linewidth=3, edgecolor = 'gray',alpha= 0.2,width=0.5, zorder=1)
ax3.tick_params(labelsize=20)
# ax2.set_ylim(0, 1000)
# #ax1.set_ylabel('Number of data', fontdict={'size': 16},labelpad=20)
# ax2.set_ylabel('The number of albedo data', fontdict={'size': 13},labelpad=15)

plt.text(x=0.01,  # 文本x轴坐标
         y=0.95,  # 文本y轴坐标
         s='(b)',  # 文本内容
         transform = ax3.transAxes,
         rotation=1,  # 文字旋转
         ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
         va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
         fontdict=dict(fontsize=20, color='black',
          #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                       weight='medium',  # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                       )  # 字体属性设置
         )
# #
plt.grid(False)
plt.show()
