import numpy as np
import tkinter as tk

from tkinter import filedialog

def read_doc(filename):
    file = open(filename, 'r')
    text = file.read()
    file.close()
    return text


def save_list(List, filename):
    data = '\n'.join(List)
    file = open(filename, 'w+')
    file.write(data)
    file.close()

def read_npy(path=None):
    if path is None:
        root = tk.Tk()
        root.withdraw()
        path = filedialog.askopenfilename(filetypes=(("Numpy Files", "*.npy"),
                                                     ("All Files", "*.*")))
    data = np.load(path, allow_pickle=True)
    return data


# create regex to find pat id
# use regex to match filenames from tkinter
# put matched items into new list
# save list to text file

if __name__ == "__main__":
    np_data = read_npy()
    print(np_data)







