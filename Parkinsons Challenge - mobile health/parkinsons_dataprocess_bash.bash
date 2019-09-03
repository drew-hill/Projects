
###### BASH Script
## to be performed in Unix

# navigate to directory

cd ~/Parkinsons\ Data/MJFF-Data


# untar a single file matching 'accel' pattern to another dir

tar -xvjf HumDynLog_APPLE_LGE_LGE_A0000028AF9C96_20111220_115329_20111220_120000.tar.bz2 -C ~/Parkinsons\ Data/revised-drew-r '*accel*'


# Loop through all tar files and un tar only those files matching pattern '*accel*'  (i.e. 
# accelerometry-related files); simultaneously, move every relevant file to a new directory

for file in ~/Parkinsons\ Data/MJFF-Data/*

do

  tar -xvjf "$file" -C ~/Parkinsons\ Data/accel_only '*accel*'

done



# Loop through all tar files and un tar only those files matching pattern '*gps*'  (i.e. 
# gps-related files); simultaneously, move every relevant file to a new directory

for file in ~/Parkinsons\ Data/MJFF-Data/*

do

  tar -xvjf "$file" -C ~/Parkinsons\ Data/gps_only '*gps*'

done



