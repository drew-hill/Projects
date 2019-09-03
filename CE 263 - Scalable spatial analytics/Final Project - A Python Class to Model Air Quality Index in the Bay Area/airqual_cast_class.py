import requests
from bs4 import BeautifulSoup
import pandas as pd
import matplotlib
from matplotlib import pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as mcolors
import cv2
import shapely
from shapely.geometry import Point, Polygon, MultiPolygon, MultiPoint
from descartes.patch import PolygonPatch
from matplotlib.patches import Circle
import datetime
import numpy as np
from pylab import *
import seaborn as sns
import time
from scipy import spatial
from sklearn.gaussian_process import GaussianProcessRegressor
import os.path
import pickle
from sklearn.metrics import mean_squared_error





class aqi_cast:
    
    def __init__(self,bw,train_df,test_df,prediction_polygon_coords_df,wx_df,date_string):
        self.trn = train_df
        self.tst = test_df
        self.pred_set = prediction_polygon_coords_df
        self.bw = bw
        self.date_string = date_string
        self.wx = wx_df
        self.y_train = train_df.values[:,4]
        self.X_train = train_df.values[:,[1,2,3,7,8,9,10,11,12,13]]
        self.y_test = test_df.values[:,4]
        self.X_test = test_df.values[:,[1,2,3,7,8,9,10,11,12,13]]

 
        #Add time (hour and weekday) to pred_set
        self.pred_dflist = []
        for i in range(24):
            self.a = self.pred_set.copy()
            self.a['datetime'] = self.date_string + ' ' + str(0 + i) + ':00'
            self.pred_dflist.append(self.a)
        self.pred_set_time = pd.concat(self.pred_dflist)
        self.pred_set_time = self.pred_set_time.reset_index()
        for i in range(len(self.pred_set_time)):
            self.pred_set_time.ix[i, 'hour'] = pd.to_datetime(self.pred_set_time.ix[i, 'datetime']).hour    
        for i in range(len(self.pred_set_time)):
            self.pred_set_time.ix[i, 'weekday'] = pd.to_datetime(self.pred_set_time.ix[i, 'datetime']).dayofweek

        # Add weather to pred_set
        self.prediction_set = self.pred_set_time.merge(self.wx, left_on='datetime', right_on = 'datetime')
        
        # Rearrange Prediction_Set features to match test/training sets
        self.cols = ['lon','lat','hour','weekday','Visibility','DryBulbCelsius',
                'WetBulbCelsius','RelativeHumidity','WindDirection','StationPressure']
        self.prediction_set = self.prediction_set[self.cols]

        # instanstanciate gp process, and gp fit
        self.gp = GaussianProcessRegressor(alpha = self.bw, normalize_y = True,copy_X_train=False)
        self.gpfit = self.gp.fit(self.trn[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']].values, self.trn['aqi'].values)

        # predict on test set
        self.y_tst_pred = self.gpfit.predict(self.tst[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']].values)

        #r2
        self.score = self.gpfit.score(self.tst[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']].values,self.tst['aqi'].values)
        # predict on polygon samples
        self.X_pred = self.prediction_set[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']].values
        self.y_pred = self.gpfit.predict(self.X_pred)
        self.Xy = np.append(self.prediction_set,self.y_pred[...,None],1)


    def vis_predpolygon(self):
        self.points1 = MultiPoint(np.array(self.tst[['lon','lat']]))
        self.fig, self.ax = subplots()
        self.ax.scatter(self.prediction_set['lon'],self.prediction_set['lat'], alpha=.005)

        self.env1 = points1.buffer(0.28).simplify(0.25)
        self.patch1 = PolygonPatch(self.env1, facecolor='#6699cc', edgecolor='#6699cc', alpha=0.25, zorder=2)
        self.ax.add_patch(self.patch1)

        self.ax.scatter( x = self.tst.lon, y = self.tst.lat,c= self.tst['aqi'], cmap='RdYlBu_r')
        self.ax.set_aspect(1)

        #colors
        self.scalarmappaple = cm.ScalarMappable(
            norm=mcolors.Normalize(vmin=float(self.tst['aqi'].min()), vmax=float(self.tst['aqi'].max())),
            cmap='RdYlBu_r')
        self.scalarmappaple.set_array(self.tst['aqi'])
        plt.colorbar(self.scalarmappaple)

        self.ax.set_title('Daily Mean AQI by Station')
        self.ax.set_aspect(1)

        plt.show()

        
    def gptrain_CV_plot(self, showplot = True):
        self.pred = self.gpfit.predict(self.tst[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']])

        ## CV
        # predict on test set
        self.y_tst_pred = self.gpfit.predict(self.tst[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']].values)

        #r2
        self.score = self.gpfit.score(self.tst[['lon','lat','hour','weekday','Visibility','WetBulbCelsius','RelativeHumidity','StationPressure']].values,self.tst['aqi'].values)

        # RMSE on test data set
        self.RMSE = mean_squared_error(self.tst['aqi'].values, self.y_tst_pred)**0.5
        print 'r2 from random forest regression', round(self.score,4)
        print 'predictive RMSE from random forest regression:', round(self.RMSE,4)
        
        if showplot:
            # Plot observed vs. predicted
            plt.scatter(self.tst['aqi'].values, self.y_tst_pred)
            # m, b = np.polyfit(y_test, y_test_pred, 1)
            # plt.plot(y_test, m*y_test + b, color='red',alpha=0.5)
            # plt.xlim([7,14])
            # plt.ylim([7,14])
            plt.xlabel('Observed AQI')
            plt.ylabel('Predicted AQI')
            plt.title('Gaussian Process Regression\n Observed vs. Predicted')
            plt.plot()
 

    def pred_vis(self, showplot = False, animate = False):
        # Turn off interactive plotting 
        # (requires manual 'show'ing of plot)
        plt.ioff()
        
        ## Produce Bay Area Model for each hour of the day
        for i in range(24):
            self.hour = float(i)
            self.Xy_plot = self.Xy[self.Xy[:,2] == self.hour]

            ## Plot
            self.fig, self.ax = subplots()
            self.ax.scatter( x = self.Xy_plot[:,0], y = self.Xy_plot[:,1], c=self.Xy_plot[:,10], cmap='RdYlBu_r')
            self.ax.set_aspect(1)

            #colors
            self.scalarmappaple = cm.ScalarMappable(
                norm=mcolors.Normalize(vmin=0, vmax=100),
                cmap='RdYlBu_r')
            self.scalarmappaple.set_array(self.Xy_plot[:,10])
            plt.colorbar(self.scalarmappaple)

            # labels
            xlabel('longitude')
            ylabel('latitude')
            title('AQI')
            if showplot:
                plt.show()
            savefig('data/sim' + str(self.hour) + '.png', transparent=True);

#         if animate:
            
            
            
    def pred_kml_singlehour(self,hour, showplot=False):
        
        self.thishour = float(hour)
        self.Xy_plot = self.Xy[self.Xy[:,2] == self.thishour]

        ## Plot
        self.fig, self.ax = subplots()
        self.ax.scatter( x = self.Xy_plot[:,0], y = self.Xy_plot[:,1], c=self.Xy_plot[:,10], cmap='RdYlBu_r')
        self.ax.set_aspect(1)
        plt.axis('off')

        #colors
        self.scalarmappaple = cm.ScalarMappable(
            norm=mcolors.Normalize(vmin=0, vmax=100),
            cmap='RdYlBu_r')
        self.scalarmappaple.set_array(self.Xy_plot[:,10])

        #remove whitespace around image
        subplots_adjust(top = 1, bottom = 0, right = 1, left = 0,
                      hspace = 0, wspace = 0)
        margins(0,0)
        gca().xaxis.set_major_locator(NullLocator())
        gca().yaxis.set_major_locator(NullLocator())

        if showplot:
            plt.show()
        savefig('data/overlayimage' + str(self.thishour) + '.png', transparent=True)

        
        ## open image and create KML
        self.text_file = open("/Users/lawsonhill/Box Sync/Current Coursework/CE263 - Scalable Spatial Analytics/Assignments/Final/data/overlay%s.kml" % str(hour),"w")
            # write to that file
        self.text_file.write("""<?xml version="1.0" encoding="UTF-8"?>
        <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
        <GroundOverlay>
            <name>Final Proj overlay</name>
            <color>dbffffff</color>
            <Icon>
                <href>/Users/lawsonhill/Box Sync/Current Coursework/CE263 - Scalable Spatial Analytics/Assignments/Final/data/overlayimage%s.0.png</href>
                <viewBoundScale>0.75</viewBoundScale>
            </Icon>
            <LatLonBox>
                <north>38.65070295401754</north>
                <south>36.80158110126777</south>
                <east>-121.37669410126777</east>
                <west>-122.95028508631127</west>
            </LatLonBox>
        </GroundOverlay>
        </kml>""" % str(hour))
            # close file
        self.text_file.close()