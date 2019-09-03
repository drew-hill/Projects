from pylab import *
import numpy as np
import pandas as pd
from scipy.spatial.distance import pdist, squareform
from scipy import spatial
from sklearn.gaussian_process import GaussianProcessRegressor
# from geopy.distance import great_circle
import matplotlib
from matplotlib import pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as mcolors
from sklearn.model_selection import train_test_split

np.random.seed(1)



class GPthis_CV:
    
    def __init__(self, train, test, bw, simulation_grid):
        self.train = np.array(train)
        self.test = np.array(test)
        self.grid = np.array(Xgrid)
        self.mu = np.mean(self.train[:,[2]])
        self.sigma = np.std(self.train[:,[2]])
        self.bw = bw
        
        # instanstanciate gp process, and gp fit
        self.gp = GaussianProcessRegressor(alpha = bw, normalize_y = True)
        self.gpfit = self.gp.fit(self.train[:,[0,1]],self.train[:,[2]])
        
        # produce K (covariance) and L (cholesky decomposition)
        self.K_test = self.gpfit.predict(self.test[:,[0,1]], return_cov=True)[1]
        self.L_test = np.linalg.cholesky(self.K_test + .001*np.eye(self.K_test.shape[0]))
        self.K_train = self.gpfit.predict(self.train[:,[0,1]], return_cov=True)[1]
        self.L_train = np.linalg.cholesky(self.K_train + .001*np.eye(self.K_train.shape[0]))
        
        # random single-fold cross-validating
        self.train_train, self.train_test = train_test_split(train, test_size = 0.15, random_state =0)
        self.gp_cv = GaussianProcessRegressor(alpha = bw, normalize_y = True)
        self.gpfit_cv = self.gp.fit(self.train_train[:,[0,1]],self.train_train[:,[2]])
        self.cv_score = self.gpfit_cv.score(self.train_test[:,[0,1]], self.train_test[:,[2]])
        
    def predict(self):
        self.pred = self.gpfit.predict(self.test[:,[0,1]])
        return self.pred
    
    def predicted_cov(self):
        self.cov_matrix = self.gpfit.predict(self.test[:,[0,1]], return_cov=True)[1]
        return self.cov_matrix
    
    def pred_score(self):
        self.score = self.gpfit.score(self.train[:,[0,1]], self.train[:,[2]])
        return self.score
    
    def simulate(self,N):
        return self.sigma*np.random.normal(0,1,N) + self.mu
    
    def grid_pred(self):
        self.K_pg = self.gpfit.predict(self.grid, return_cov=True)[1]
        self.L_pg = np.linalg.cholesky(self.K_pg + .001*np.eye(self.K_pg.shape[0]))
        
        # Predict using prediction algorithm SciKit
        self.sim1 = self.gpfit.predict(self.grid)
        # Predict using formulat from Problemset
#         self.sim1 = self.mu + self.L_pg
    
        self.sim_grid = np.concatenate((self.grid,self.sim1),axis=1)
        return self.sim_grid
    
    def visualize_train(self, filename, show=False):
        plt.hist(self.train[:,[2]], facecolor='green', alpha=0.5)
        plt.title("Hist: $\mu=%.2f$, $\sigma=%.2f$" % (self.mu, self.sigma)) 
        plt.savefig(filename)
        if show: 
            plt.show()
            
    def visualize_sim(self, filename, N, show=False):
        x = self.simulate(N)
        pred = self.gpfit.predict(self.test[:,[0,1]])
        plt.hist(x, facecolor='green', alpha=0.5)
        plt.title("Hist: $\mu=%.2f$, $\sigma=%.2f$" % (mean(x), std(x))) 
        plt.savefig(filename)
        if show: 
            plt.show()
            
    def visualize_grid_pred(self):
        # redo grid simulation
        self.sim1 = self.gpfit.predict(self.grid)
        self.sim_grid = np.concatenate((self.grid,self.sim1),axis=1)
        
        # create plots
        fig, ax = subplots()
        ax.scatter( x = self.sim_grid[:,1], y = self.sim_grid[:,0], c=self.sim_grid[:,2], cmap='RdYlBu_r')
        ax.set_aspect(1)
        
        #colors
        scalarmappaple = cm.ScalarMappable(
            norm=mcolors.Normalize(vmin=self.sim_grid[:,2].min(), vmax=self.sim_grid[:,2].max()),
            cmap='RdYlBu_r')
        scalarmappaple.set_array(self.sim_grid[:,2])
        plt.colorbar(scalarmappaple)

        # labels
        xlabel('longitude')
        ylabel('latitude')
        title('Rain (mm)')
        savefig('data/sim.png', transparent=True);
        
    def kml_output(self):
        # redo grid simulation
        self.sim1 = self.gpfit.predict(self.grid)
        self.sim_grid = np.concatenate((self.grid,self.sim1),axis=1)
        
        # create plots
        fig, ax = subplots()
        ax.scatter( x = self.sim_grid[:,1], y = self.sim_grid[:,0], c=self.sim_grid[:,2], cmap='RdYlBu_r')
        ax.set_aspect(1)
        plt.axis('off')
        
        #colors
        scalarmappaple = cm.ScalarMappable(
            norm=mcolors.Normalize(vmin=self.sim_grid[:,2].min(), vmax=self.sim_grid[:,2].max()),
            cmap='RdYlBu_r')
        scalarmappaple.set_array(self.sim_grid[:,2])
        
        # remove white space around image
        subplots_adjust(top = 1, bottom = 0, right = 1, left = 0, 
            hspace = 0, wspace = 0)
        margins(0,0)
        gca().xaxis.set_major_locator(NullLocator())
        gca().yaxis.set_major_locator(NullLocator())

        # save
        savefig('data/sim_noaxis.png', transparent=True, bbox_inches = 'tight', pad_inches= 0);
        
        ## Generate KML file
            # create and open file called overlay.kml
        text_file = open("data/overlay.kml","w")
            # write to that file
        text_file.write("""<?xml version="1.0" encoding="UTF-8"?>
        <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
        <GroundOverlay>
            <name>CE263N Problem3 overlay</name>
            <color>dbffffff</color>
            <Icon>
                <href>/Users/Lawson/Box Sync/Current Coursework/CE263 - Scalable Spatial Analytics/Assignments/Problem 3/data/sim_noaxis.png</href>
                <viewBoundScale>0.75</viewBoundScale>
            </Icon>
            <LatLonBox>
                <north>39.3</north>
                <south>38.3</south>
                <east>-120.0</east>
                <west>-121.0</west>
            </LatLonBox>
        </GroundOverlay>
        </kml>""")
            # close file
        text_file.close()