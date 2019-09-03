#!/usr/bin/env python -W ignore::DeprecationWarning
import numpy as np
import pickle 

from matplotlib import pyplot as plt
from pylab import *


### this will be replaced with the real test image ###
im_test = plt.imread('parking_test.png')
###


# This function MUST take locations (loc) and an image (im) 
# as input parameters and return the feature vector
def my_feature_vector(loc, im, size = 10):
  w = size
  # a patch of the size w cenetered at loc is extracted as a feature vector
  patch = im[loc[1]-w:loc[1]+w, loc[0]-w:loc[0]+w]
  p = np.array(patch).flatten()
  return p 
  

## 10 preview test locations
### these will be replaced with the real set of 100 test locations ###
test_locs_labs = np.load('test_locations_and_labels.np')

test_locs   = test_locs_labs[:,0:2]
test_labels = test_locs_labs[:,2]

X_test = []
for loc in test_locs:
  X_test.append( my_feature_vector(loc, im_test) )


###### 
# Data handling to suit my growing (i.e. limited) knowledge of class/method/function creation

## load classifier
my_classifier = pickle.load(open('classifier_DrewHill.pickle')) 


def drew_predict(X):
    X_i = np.empty((0,3), float)

    for i in xrange(np.array(X).shape[0]):
        row_num_newarray = np.array(X).shape[0]/ 4
        a = np.array(X)
        a = a.reshape(row_num_newarray,4)
        b = np.delete(a, 3 , 1)     
    #     car = np.array([test_locs_labs[i,2]] * row_num_newarray)        
    #     c = np.insert(b, 3,car, 1)
        
        x_i = np.empty((0,), int)
        
        for i in xrange(b.shape[0]):
            boop = my_classifier.predict(b[i])
            x_i = np.append(x_i, boop, 0)
   
        predicted = int(round(np.mean(x_i)))

        return predicted
    return predicted

## perform classification

score = 0
for i, xtest in enumerate(X_test): 
  
  predicted = drew_predict(xtest)
  
  if (test_labels[i] == 1.0)&(predicted == 1.0):
     score = score + 2
  
  if (test_labels[i] == 1.0)&(predicted == 0.0):
     score = score - 0.5
  
  if (test_labels[i] == 0.0)&(predicted == 1.0):
     score = score - 0.5
  
  if (test_labels[i] == 0.0)&(predicted == 0.0):
     score = score + 0.25
     
  print test_labels[i], predicted, score 

print 'You final Score is: %.2f' % score