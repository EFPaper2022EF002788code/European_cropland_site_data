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

#from sklearn.metrics import r2_score
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



filepath = r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\fallow\constant\constant151\testingforyear' + '\\' + 'predictors-fallow-151-testingforyear.csv'
# dfa = pandas.read_excel(file_albedo, sheet_name=site[snum])
df = pandas.read_csv(filepath, header=0)  # path of file

vari = numpy.array(df['number']).tolist()
x = numpy.array(df['CAdataset_out_of_box.albedo']).tolist()# rmse r2
y = numpy.array(df['pred1_rf_oob']).tolist()
#0:11

from scipy.stats import gaussian_kde
fig= plt.figure(figsize=(10, 10))
# fig, ax1 = plt.subplots(figsize=(10, 6))
left, bottom, width, height = 0.15, 0.15, 0.8, 0.8
ax = fig.add_axes([left, bottom, width, height])        #'RdYlGn_r' 'RdYlGn'
ys = 'RdYlGn'

linear_model=numpy.polyfit(x,y,1)
linear_model_fn=numpy.poly1d(linear_model)
tau, p_value = stats.kendalltau(x,y)
print(p_value)
#print(linear_model_fn[0],linear_model_fn[1])
ax.plot((0,1),(0,1), transform=ax.transAxes, ls='--',c='lightgray',linewidth = 3,zorder =1)
ax.plot(x,linear_model_fn(x),color='black', linewidth = 3,zorder =2,label = 'fitting model')
ax.scatter(x,y,c='b', marker='o', s= 120, zorder =2,alpha = 0.4)
# sns.kdeplot(x=x, y=y, fill=True, cmap='Spectral_r', shade = True, shade_lowest = False,n_levels = 50,zorder =5)

ax.set_ylim(0, 0.3)
ax.set_xlim(0, 0.3)
ax.set_ylabel('Observed surface albedo', fontdict={'size': 24})

#ax.set_xticklabels(['(1-7)','(7-14)','(14-21)','(42-49)','(56-63)'],fontsize = 16)
ax.set_xlabel('Predicted surface albedo', fontdict={'size': 24})
ax.tick_params(labelsize=24)
#ax.axhline(y=0,color='silver', linestyle='--', lw=2, zorder=0)

ax.legend(loc=1, ncol=2, labelspacing=0.3, columnspacing=0.7, fontsize=24, frameon=False)

# ax.text(x=0.1,  # 文本x轴坐标
#          y=0.95,  # 文本y轴坐标
#          s='Y = 0.608'+'X + 0.066',  # 文本内容
#          transform=ax.transAxes,
#          rotation=1,  # 文字旋转
#          ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
#          va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
#          fontdict=dict(fontsize=20, color='black',
#                        #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
#                        weight='normal',
#                        # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'
#
#                        )  # 字体属性设置
#          )
ax.text(x=0.1,  # 文本x轴坐标
         y=0.95,  # 文本y轴坐标
         s='$\mathregular{R^2}$'+' = 0.23'+'   p < 0.01',  # 文本内容
         transform=ax.transAxes,
         rotation=1,  # 文字旋转
         ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
         va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
         fontdict=dict(fontsize=24, color='black',
                       #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                       weight='normal',
                       # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                       )  # 字体属性设置
         )
ax.text(x=0.1,  # 文本x轴坐标
         y=0.90,  # 文本y轴坐标
         s='RMSE = 0.03',  # 文本内容
         transform=ax.transAxes,
         rotation=1,  # 文字旋转
         ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
         va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
         fontdict=dict(fontsize=24, color='black',
                       #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                       weight='normal',
                       # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                       )  # 字体属性设置
         )

ax.text(x=0.1,  # 文本x轴坐标
         y=0.85,  # 文本y轴坐标
         s='Number = 81',  # 文本内容
         transform=ax.transAxes,
         rotation=1,  # 文字旋转
         ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
         va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
         fontdict=dict(fontsize=24, color='black',
                       #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                       weight='normal',
                       # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                       )  # 字体属性设置
         )

plt.text(x=0.01,  # 文本x轴坐标
         y=0.95,  # 文本y轴坐标
         s='(b)',  # 文本内容
         transform = ax.transAxes,
         rotation=1,  # 文字旋转
         ha='left',  # x=2.2是文字的左端位置，可选'center', 'right', 'left'
         va='baseline',  # y=8是文字的低端位置，可选'center', 'top', 'bottom', 'baseline', 'center_baseline'
         fontdict=dict(fontsize=24, color='black',
          #             family='monospace',  # 字体,可选'serif', 'sans-serif', 'cursive', 'fantasy', 'monospace'
                       weight='medium',  # 磅值，可选'light', 'normal', 'medium', 'semibold', 'bold', 'heavy', 'black'

                       )  # 字体属性设置
         )

plt.grid(False)
plt.show()
