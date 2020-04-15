import numpy as np
import tkinter as tk
import os

from tkinter import filedialog

def load_doc(filename):
    file = open(filename, 'r')
    text = file.read()
    file.close()
    return text


def save_list(List, filename):
    data = '\n'.join(List)
    file = open(filename, 'w+')
    file.write(data)
    file.close()

def load_npy(path=None):
    if path is None:
        root = tk.Tk()
        root.withdraw()
        path = filedialog.askopenfilename(filetypes=(("Numpy Files", "*.npy"),
                                                     ("All Files", "*.*")))
    data = np.load(path, allow_pickle=True)
    return data


def retrieve_files(filetype, directory=None):

    if directory is None:
        root = tk.Tk()
        root.withdraw()
        directory = filedialog.askdirectory()

    working_files = list()
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(filetype):
                working_files.append(str(directory + '/' + file))
    return working_files


# create regex to find pat id
# use regex to match filenames from tkinter
# put matched items into new list
# save list to text file

if __name__ == "__main__":
    np_data = load_npy()
    print(np_data)




