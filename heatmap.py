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

filepath = r'E:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\extractpredictorsRF20220921\david\test20221029\9\test\fallow\constant\constant151\nogpp-up' + '\\' + 'predictors-fallow-151-noGPP-importance.csv'
# dfa = pandas.read_excel(file_albedo, sheet_name=site[snum])
df = pandas.read_csv(filepath, header=0)  # path of file

vari = (numpy.array(df['vari']).tolist())
IncMSE = (numpy.array(df['importance']).tolist())


#Purity = numpy.array(df['IncNodePurity']).tolist()
# DEGeb = numpy.array(df['DE-Geb']).tolist()
# DEKli = numpy.array(df['DE-Kli']).tolist()
# DERuS = numpy.array(df['DE-RuS']).tolist()  # rmse r2
# FRAur = numpy.array(df['FR-Aur']).tolist()
# FRGri = numpy.array(df['FR-Gri']).tolist()
# FRLam = numpy.array(df['FR-Lam']).tolist()
#0:11
# vv = 12
# data = {
#     'vari': vari,
#     'IncMSE': IncMSE,
#     'Purity': Purity,
# }

# ddd = pandas.DataFrame(data, columns=['vari', 'IncMSE', 'Purity'])
# ddarray = ddd.values

fig = plt.figure(figsize=(14,10))
# fig, ax1 = plt.subplots(figsize=(10, 6))
left, bottom, width, height = 0.1, 0.13, 0.85, 0.8
ax1 = fig.add_axes([left, bottom, width, height])        #'RdYlGn_r' 'RdYlGn'
ys = 'RdYlGn'
# lb = 'Importance score'
#nn = 'R2'

# sns.heatmap(data=ddarray, vmin=0, vmax=0.8, cmap=plt.get_cmap(ys), annot=True, fmt=".3f",
#             annot_kws={'size': 10, 'weight': 'normal', 'color': 'black'},
#             cbar=True,
#             cbar_kws={  # color bar的名称 #'label': 'R'+ '$\mathregular{^2}$',  RMSE
#                 'orientation': 'vertical',  # color bar的方向设置，默认为'vertical'，可水平显示'horizontal'
#                 "ticks": numpy.arange(0, 0.5, 0.025),  # color bar中刻度值范围和间隔  0, 1.2, 0.2
#                 "format": "%.1f",  # 格式化输出color bar中刻度值
#                 "pad": 0.01,  # color bar与热图之间距离，距离变大热图会被压缩
#             }, ax=ax1,
#
#             )
# #vari = ['SW_OUT', 'GPP', 'SWC', 'Amax', 'Ts', 'P', 'MF', 'IG', 'GW', 'H', 'OF', ]  # albedo
#
# vari = ['SW_IN','GPP','SWC','LE','T','F','N','H','T','HH','MA','P']  #Amax
# cax = plt.gcf().axes[-1]
# cax.tick_params(labelsize=14)
# cbar = ax1.collections[0].colorbar
# # cbar.set_label(lb,rotation = 270,fontsize=16)
# # cbar.set_label('RMSE', rotation=270, fontsize=16)
# ax1.set_yticklabels(vari, rotation=0, fontsize=14)
# ax1.set_xticklabels(['BE-Lon', 'CZ-KrP', 'DE-Geb', 'DE-Kli', 'DE-RuS', 'FR-Aur', 'FR-Gri', 'FR-Lam'], fontsize=14,
#                     rotation=30)
# ax1.set_title('Importance score' + '_albedo_' + str(day) + 'd', fontsize=14)
# # '_Amax_', '_albedo_'
# #plt.show()
#
# seq = numpy.array(df['seq'])
# ssee = numpy.array(df['ssee'])
#
# # RMSE   'R'+ '$\mathregular{^2}$'
#
# # plt.savefig(r'D:\PHD\OECHIDEE\preprocess\WINTER2020\randomforest_robustlinear\managementinfo20220719\fig'+'\\'+'Fitting performance of RF model-'+ i + '_'+ str(day)+'d_'+nn+'.png')
# print(list(seq), '\n',list(ssee))
#name = ['GPP','TFer.','Kd','THar.','Ts','TPlo.','SWC','TTil.','sitelabel','TypePes.','EFer.']
#name = ['GPP','T$_{N}$','Temp$_s$','PA','T$_{Fug}$','Clay ratio','SiteIden','T$_{Her}$','T$_{Ins}$','T$_{sow}$']
#colors = ['orangered','royalblue','orangered','orangered','royalblue','orangered','orangered','seagreen','royalblue','royalblue','royalblue']
name = ['Kd','Temp$_s$','SWC','ws','D$_{res}$','Sand$_{c}$','Clay$_{c}$','D$_{N}$','D$_{Til}$','D$_{Her}$','D$_{ss}$']
colors = ['orangered','orangered','orangered','orangered','royalblue','orangered','orangered','royalblue','royalblue','royalblue','royalblue']

#print(colors)
# [["hotpink"], "hotpink", "hotpink", "hotpink", "hotpink", "lightskyblue", "lightskyblue",
#                       "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue"]
#print(Purity)
bar1 = ax1.bar(vari, IncMSE,color='lightgrey',  # 判断大于0的为红色，负的为蓝色
               width=0.55,  # 柱形宽度
               align='center',  # 柱形的位置edge/center
               alpha=0.8, ls='-', lw=5, # 柱形透明度
               edgecolor=colors,  # 柱形边缘颜色
               )
ax1.set_ylabel('Importance score', fontsize=24)
ax1.set_ylim(0, 1)
ax1.set_xlim(-1, 11)
ax1.set_xticklabels(name, fontsize=24, rotation=30)
ax1.tick_params(labelsize=24)
from matplotlib.patches import Arc, Ellipse, Rectangle, Wedge

# ax1.add_patch(
#     Rectangle(
#         (4.4, 0.46),  # (x,y)
#         0.6,  # width
#         0.03, facecolor ='lightgrey',edgecolor='royalblue',lw=5)
# )
#
# ax1.add_patch(
#     Rectangle(
#         (4.4, 0.50),  # (x,y)
#         0.6,  # width
#         0.03, facecolor ='lightgrey',edgecolor='orangered',lw=5  # height
#     )
# )
#
# ax1.add_patch(
#     Rectangle(
#         (4.4, 0.42),  # (x,y)
#         0.6,  # width
#         0.03, facecolor ='lightgrey',edgecolor='seagreen',lw=5  # height
#     )
# )
#
# ax1.text(x=0.50,  # 文本x轴坐标
#          y=0.85,  # 文本y轴坐标
#          s='Features related to climate',  # 文本内容
#          transform=ax1.transAxes,
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
# ax1.text(x=0.50,  # 文本x轴坐标
#          y=0.78,  # 文本y轴坐标
#          s='Information of MPs',  # 文本内容
#          transform=ax1.transAxes,
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
# ax1.text(x=0.50,  # 文本x轴坐标
#          y=0.71,  # 文本y轴坐标
#          s='Site identity',  # 文本内容
#          transform=ax1.transAxes,
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
plt.text(x=0.01,  # 文本x轴坐标
         y=0.95,  # 文本y轴坐标
         s='(d)',  # 文本内容
         transform = ax1.transAxes,
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
