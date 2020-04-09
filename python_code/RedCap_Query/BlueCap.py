############################## IMPORTS #######################################
# Imports

import pandas as pd
import BlueCrypt
import matplotlib.pyplot as plt
import re
import tkinter as tk
from redcap import Project, RedcapError

##############################################################################

root = tk.Tk()

##############################################################################


class QueryDatabase:
############################### INIT METHOD ##################################
    def __init__(self, fields=None, id_filter='baby', del_blank=None,
                 icd=None):

        if del_blank is None:
            self.del_blank = []
        else:
            self.del_blank = del_blank

        if fields is None:
            self.fields = []
        else:
            self.fields = fields

        self.icd = icd
        if icd is not None:
            if icd == 'icd9':
                self.icd_format = ['^\d{3}\.?(?<=\.)(\d{0,3})?$',
                                   '^E\d{3}\.?(?<=\.)\d?$',
                                   '^V\d{2}\.?(?<=\.)(\d{0,2})?$']
                self.icd_format = re.compile('|'.join(self.icd_format))

            elif icd == 'icd10':
                self.icd_format = re.compile('^[A-TV-Z]\d{2}\.?(?<=\.)(\d{0,3})?([ADS])?$')

            else:
                self.icd_format = None
                print("The value passed for the \'icd\' argument was invalid. "
                      "You may want to correct this, then re-run your query again.")

        self.api_token = BlueCrypt.decry(BlueCrypt.rc_filepath)
        self.url = BlueCrypt.rc_url
        self.id_filter = id_filter
        self.project = Project(self.url, self.api_token)

############################ ACCESSORY METHODS ###############################

    def show_fields(self):
        Field_Dict = {}
        for i, item in enumerate(self.project.field_names):
            Field_Dict.update({i: item})

        for key, value in Field_Dict.items():
            print(str(key) + ": " + value)

    def select_ids(self, df, Rows):

        if self.id_filter == 'baby':
            r = re.compile(r'Baby-[0-9]+')
            for elem in Rows:
                re_check = re.match(r, elem[0])
                if re_check:
                    df.append(elem)
            return df

        elif self.id_filter == 'mother':
            r = re.compile(r'Mother-[0-9]+')
            for elem in Rows:
                re_check = re.match(r, elem[0])
                if re_check:
                    df.append(elem)
                return df

        elif self.id_filter == 'both':
            pass

        else:
            raise ValueError("The \'grouping\' argument that has been passed "
                             "is invalid")

    def remove_blanks(self, df):
        Temp = []
        Indexes = []

        for i, elem in enumerate(df):
            if i == 0:
                for ii, x in enumerate(elem):
                    for y in self.del_blank:
                        if x == y:
                            Temp.append(ii)
            for _ in elem:
                for t in Temp:
                    if elem[t] == '':
                        if i not in Indexes:
                            Indexes.append(i)
                        break
                    elif elem[t] == 'NA':
                        if i not in Indexes:
                            Indexes.append(i)
                        break

        for index in sorted(Indexes, reverse=True):
            del df[index]

        return df

    def match_icd(self, df):
        Temp = []
        Indexes = []
        df0 = []
        re_icd = re.compile('.*icd.*$')

        for i, elem in enumerate(df):
            if i == 0:
                for ii, x in enumerate(elem):
                    if re.match(re_icd, x) is not None:
                        Temp.append(ii)
                        Indexes.append(i)
            for _ in elem:
                for t in Temp:
                    if re.match(self.icd_format, elem[t]) is not None:
                        if i not in Indexes:
                            Indexes.append(i)
                        break

        for i, elem in enumerate(df):
            for x in Indexes:
                if i == x:
                    df0.append(elem)
                    Indexes.remove(x)
                    break
        print(df0)
        return df0

############################## EXPORT METHOD #################################

    def export(self):
        query = self.project.export_records(fields=self.fields, format='csv')
        query = query.split('\n')

        Rows = []
        df = []

        for elem in query:
            columns = elem.split(',')
            del columns[1:4]
            Rows.append(columns)
        df.append(Rows[0])

        df = self.select_ids(df, Rows)
        df = self.remove_blanks(df)
        if self.icd is not None:
            df = self.match_icd(df)

        df = pd.DataFrame(df[1:], columns=df[0])
        return df




if __name__ == "__main__":
    test = QueryDatabase()
    print(test.api_token)