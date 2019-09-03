import requests
from bs4 import BeautifulSoup
import pandas as pd
import datetime
import numpy as np



## list of dates to be included
numdays = 75
dateList = []
base = datetime.datetime(2016, 11,30)
dateList = [base - datetime.timedelta(days=x) for x in range(0, numdays)]


for date in dateList:    
    ## Inputs
    url = 'http://www.baaqmd.gov/about-air-quality/current-air-quality/air-monitoring-data?DataViewFormat=daily&DataView=aqi&StartDate=' + str(date)[0:10] + '&ParameterId=316'

    ## Scrape
    # Scrape the HTML at the url
    r = requests.get(url)
    # crate beautiful soup object
    soup = BeautifulSoup(r.text, 'lxml')
    # Create an object of all HTML tables on the page
    tables = soup.find_all('table')


    ###
    ### Northern Zone
    ###

    # find the table that includes Coast & Central Bay
    # by identifying the table with this name
    td_list = soup.find_all('table')
    i = 0
    for elem in td_list:
        if 'Sebastopol' in elem.text:
            ind = i
        i += 1

    nz_data= tables[ind]

    ## Create dataframe for coastal zone table

    df = []

    for i in xrange(3,8):
        row_marker = 0  
        new_table = pd.DataFrame(columns=range(0,26), index = [i-3]) 
        for row in nz_data.find_all('tr')[0:i]:
            column_marker = 0
            columns = row.find_all('td')
            for column in columns:
                new_table.iat[row_marker,column_marker] = column.get_text()
                column_marker += 1
        # create list of rows
        df.append(new_table)

    # append all rows together
    northern = pd.concat(df)



    ###
    ### Coastal & Central Bay
    ###

    # find the table that includes Coast & Central Bay
    # by identifying the table with this name
    td_list = soup.find_all('table')
    i = 0
    for elem in td_list:
        if 'Oakland' in elem.text:
            ind = i
        i += 1

    coastal_data= tables[ind]

    ## Create dataframe for coastal zone table

    df = []

    for i in xrange(3,9):
        row_marker = 0  
        new_table = pd.DataFrame(columns=range(0,26), index = [i-3]) 
        for row in coastal_data.find_all('tr')[0:i]:
            column_marker = 0
            columns = row.find_all('td')
            for column in columns:
                new_table.iat[row_marker,column_marker] = column.get_text()
                column_marker += 1
        # create list of rows
        df.append(new_table)

    # append all rows together
    coastal = pd.concat(df)



    ###
    ### Eastern Zone
    ###

    # find the table that includes Coast & Central Bay
    # by identifying the table with this name
    td_list = soup.find_all('table')
    i = 0
    for elem in td_list:
        if 'Concord' in elem.text:
            ind = i
        i += 1

    eas_data= tables[ind]

    ## Create dataframe for coastal zone table

    df = []

    for i in xrange(3,5):
        row_marker = 0  
        new_table = pd.DataFrame(columns=range(0,26), index = [i-3]) 
        for row in eas_data.find_all('tr')[0:i]:
            column_marker = 0
            columns = row.find_all('td')
            for column in columns:
                new_table.iat[row_marker,column_marker] = column.get_text()
                column_marker += 1
        # create list of rows
        df.append(new_table)

    # append all rows together
    eastern = pd.concat(df)


    ###
    ### South Central Bay
    ###

    # find the table that includes Coast & Central Bay
    # by identifying the table with this name
    td_list = soup.find_all('table')
    i = 0
    for elem in td_list:
        if 'Redwood' in elem.text:
            ind = i
        i += 1

    red_data= tables[ind]

    ## Create dataframe for coastal zone table

    df = []

    for i in xrange(3,4):
        row_marker = 0  
        new_table = pd.DataFrame(columns=range(0,26), index = [i-3]) 
        for row in red_data.find_all('tr')[0:i]:
            column_marker = 0
            columns = row.find_all('td')
            for column in columns:
                new_table.iat[row_marker,column_marker] = column.get_text()
                column_marker += 1
        # create list of rows
        df.append(new_table)

    # append all rows together
    south_cen = pd.concat(df)


    ###
    ### Santa Clara Valley
    ###

    # find the table that includes Coast & Central Bay
    # by identifying the table with this name
    td_list = soup.find_all('table')
    i = 0
    for elem in td_list:
        if 'Jackson' in elem.text:
            ind = i
        i += 1

    sc_data= tables[ind]

    ## Create dataframe for coastal zone table

    df = []

    for i in xrange(3,6):
        row_marker = 0  
        new_table = pd.DataFrame(columns=range(0,26), index = [i-3]) 
        for row in sc_data.find_all('tr')[0:i]:
            column_marker = 0
            columns = row.find_all('td')
            for column in columns:
                new_table.iat[row_marker,column_marker] = column.get_text()
                column_marker += 1
        # create list of rows
        df.append(new_table)

    # append all rows together
    santaclara = pd.concat(df)


    # ####
    # #### combine all dataframes
    # ####
    baaqmd = pd.concat([northern, coastal, eastern, south_cen, santaclara])

    # set index
    baaqmd.index = range(len(baaqmd))

    # set column names
    baaqmd.columns = ['station'] + range(24) + ['mean']

    # remove odd strings from station name
    baaqmd['station'] = baaqmd['station'].map(lambda x: x.lstrip('\n').rstrip('\n*'))
    
    # produce coordinates for merge
    # manually add columns with lat and longitude
        # taken from the 2013 BAAQMD Network Plan
    blat = [38.310942, 38.273744, 37.972310, 38.403765, 38.102507, 
            37.865000, 37.793624, 37.743065, 37.814781, 37.765946, 
            37.960400, 37.936013, 37.687526, 37.482934, 36.999571, 
            37.348497, 37.338000]
    blon = [-122.296189, -122.274590 ,-122.520004, -122.818294, 
            -122.237976,  -122.303000, -122.263376, -122.169935, 
            -122.282347, -122.399044, -122.356811, -122.026154, 
            -121.784217, -122.203500, -121.574684, -121.894898, 
            -121.850000]
    bsta = [u'Napa', u'Napa Valley College ', u'San Rafael', u'Sebastopol',
         u'Vallejo', u'Berkeley Aquatic Park', u'Laney College Fwy',
         u'Oakland East', u'Oakland West',
         u'San Francisco - Arkansas St.', u'San Pablo - Rumrill', 
         u'Concord', u'Livermore - Rincon Ave.', u'Redwood City',
         u'Gilroy', u'San Jose - Jackson St.', u'San Jose Fwy']
    coordinates = pd.DataFrame({'station': bsta, 'lon': blon, 'lat': blat})    
    
    # merge lon and lat with their respective stations
    baaqmd = baaqmd.merge(coordinates, left_on='station', right_on = 'station')

    ## make long -- remove mean
    baaqmd_long = pd.melt(baaqmd, id_vars=['station','lon','lat'], var_name='hour', value_name='aqi')
    baaqmd_long = baaqmd_long[baaqmd_long.hour != 'mean']
    
    # make date variable
    baaqmd_long['day'] = str(date)[0:10]
    
    # convert hour + day columns into single datetime column for wx 
    # merging purposes; retain individual columsn for prediction
    baaqmd_long['hour'] = baaqmd_long['hour'].astype(str)
    baaqmd_long['datetime'] = baaqmd_long['day'] + ' ' + baaqmd_long['hour'] + ':00'
    
    ## Write to file
    with open('data/baaqmd_long', 'a') as f:
        baaqmd_long.to_csv(f, header = False, index = False)