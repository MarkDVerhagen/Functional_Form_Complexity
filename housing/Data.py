import pandas as pd
import os


class Data(object):
    """This class generates the dataset used for analysis"""
    
    def __init__(self, regions=[], data_dir=os.path.join('data', 'raw'),
                 filename='tranall2011_19.pkl', merge_lsoa = False, temp = True):
        print("Generating dataset")
        self.regions = regions
        self.merge_lsoa = merge_lsoa
        self.data_dir = data_dir
        self.file_path = os.path.join(self.data_dir, filename)

        self.read_data()
        if temp:
            self.generate_temp()
        
        if self.merge_lsoa:
            print("Merging LSOA data for London area")
            self.lsoa_set = pd.read_excel(os.path.join(self.data_dir, 'housing_travel_imd_crime.xlsx'))
            self.data = self.data.merge(self.lsoa_set, on = 'lsoa11')
    
    def read_data(self):
        self.data = pd.read_pickle(self.file_path)
        if self.regions:
            print('Dataset based on the following regions: ' + ' '.join(self.regions))
            self.data = self.data.loc[self.data['rgn11nm'].isin(self.regions)]
    
    def generate_temp(self):
        self.data['year'] = pd.to_numeric(self.data['dateoftransfer'].str.slice(0, 4))
        self.data['month'] = pd.to_numeric(self.data['dateoftransfer'].str.slice(5, 7))

