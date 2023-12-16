# -*- coding: utf-8 -*-

import numpy
import scipy
from scipy import stats
from scipy import interpolate as inter
from scipy import constants as Const
from scipy.signal import savgol_filter
import pandas
import math
import datetime
import os
import random
from random import sample
import xlrd
import xlwt
import copy
# from sklearn.preprocessing import LabelEncoder
# from sklearn.preprocessing import OneHotEncoder
# import statsmodels.api as sm
# from sklearn.model_selection import train_test_split
# from sklearn.model_selection import cross_val_score
# from sklearn.ensemble import RandomForestRegressor
# from sklearn import metrics
import palettable  # python颜色库

# from sklearn.metrics import r2_score
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.axisartist.parasite_axes import HostAxes, ParasiteAxes
import matplotlib.font_manager as font_manager
from matplotlib.pyplot import MultipleLocator
from matplotlib.ticker import MaxNLocator, FormatStrFormatter
from matplotlib.pyplot import MultipleLocator
import matplotlib
#
# # # =======================================importance heatmap
# =============================================================
day = 15


# p240 = [float(rf[i]) for i in range(0,len(rf)) if int(vari[i]) == 240]
# p270 = [float(rf[i]) for i in range(0,len(rf)) if int(vari[i]) == 270]
#print(numpy.mean(p30))
#label = ['0-30','0-60','0-90','0-120','0-150','0-180','0-210','0-240','0-270']
# color = ['sky blue','cerulean','light salmon','red orange']
color = ['windows blue', 'faded green']
fig = plt.figure(figsize=(20, 12))
fig.subplots_adjust(left=0.07, bottom=0.12, right=0.95, top=0.85, hspace=0.5, wspace=0.5)
# left, bottom, width, height = 0.08, 0.14, 0.85, 0.83
mana = ['DN','DFug','DHar','DHer','Dsow','DTil']
name1 = ['nitrogen fertilizer','Fungicide','Harvest','Herbicide','Sowing','Tillage']
llabel = ['(a)','(b)','(c)','(d)','(e)','(f)']
for i in range(1,7):
    filepath = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\whole\constant\constant211\nogpp-up' + '\\' + mana[i-1]+'-albedo-RF-1123.csv'
    # dfa = pandas.read_excel(file_albedo, sheet_name=site[snum])
    df = pandas.read_csv(filepath, header=0)  # path of file

    vari = (numpy.array(df['Periods']).tolist())
    rf = (numpy.array(df['RF']).tolist())

    p30 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 30]
    p60 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 60]
    p90 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 90]
    p120 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 120]
    p150 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 150]
    p180 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 180]
    p210 = [float(rf[j]) for j in range(0, len(rf)) if int(vari[j]) == 210]

    ax = fig.add_subplot(2, 3, i)
#p30,p60,p90,p120,p150,p180,p210,p240,p270
    ax.boxplot([p30,p60,p90,p120,p150,p180,p210], vert=True,showmeans=True, boxprops = {'color':'black'},
                 meanprops={'marker': 'D', 'markerfacecolor': 'gray', 'markersize': 8},
                 medianprops={'linestyle': '-', 'color': 'red', 'linewidth':3},showfliers=False,showbox=True)
    ax.set_xlim(0, 8)  #5 9
    ax.set_ylim(-7, 7)

    #ax.set_xlim(0, 9)
    name = ['30','60','90','120','150','180','210']
    ax.legend(loc=1, ncol=2, labelspacing=0.3, columnspacing=0.7, fontsize=18, frameon=False)
    ax.axhline(y=0,color='silver', linestyle='--', lw=2, zorder=0)
    ax.tick_params(labelsize=18)
    ax.set_xticklabels(name)
    ax.set_ylabel('Radiative Forcing (W'+ ' $\mathregular{m^-}$' + '$\mathregular{^2}$)', fontdict={'size':18})
    ax.set_xlabel('Days since ' + name1[i-1], fontsize=18)
    # nitrogen fertilizer  crop residues harvest herbicide
    ax1 = ax.twinx()
    ## tillage, ploughing, covercrop, residues, AA, BB, CC, nitrogen, herbicide, fungicide, Manure
    #num = [len(residues), len(covercrop), len(ploughing),len(tillage), len(CC),len(BB), len(fungicide), len(nitrogen)]
    #num = [len(CC),len(BB), len(fungicide), len(nitrogen)]

    num = [len(p30), len(p60), len(p90),len(p120), len(p150),len(p180), len(p210)]
                      # 1, 10    1, 6
    ax1.bar(x=list(range(1, 8)), height = num,  width=0.3, fc='w',  linewidth=2, edgecolor = 'black',alpha= 0.2, zorder=1)
    ax1.tick_params(labelsize=18)
    ax1.set_ylim(0, 500)
    ax1.set_xlim(0, 8)   # 0.5, 5.5 0.5, 9.5
    ax1.set_ylabel('The number of data', fontdict={'size': 18})

    plt.text(x=0.01,  # 文本x轴坐标
             y=0.90,  # 文本y轴坐标
             s=llabel[i-1],  # 文本内容
             transform = ax.transAxes,
             rotation=1,  # 文字旋转
             ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
             va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
             fontdict=dict(fontsize=20, color='black',
              #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                           weight='medium',  # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                           )  # 字体属性设置
             )

plt.grid(False)
plt.show()