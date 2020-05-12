import numpy as np
import tkinter as tk
import os
import shutil
import time

from tkinter import filedialog

#####################################TO DO'S##################################

# create timer function
# Finish sentiment analysis
# Count number of files for everything


def load_doc(filename):
    file = open(filename, 'r')
    text = file.read()
    file.close()
    return text


def save_list(List, filename, save_dest=None):
    print(filename)
    if save_dest is not None:
        filename = save_dest + filename
        if not os.path.exists(save_dest):
            os.makedirs(save_dest)
        data = '\n'.join(List)
        file = open(filename, 'w+')
        file.write(data)
        file.close()
    else:
        data = '\n'.join(List)
        file = open(filename, 'w+')
        file.write(data)
        file.close()

def load_npy(paths=None):
    if paths is None:
        root = tk.Tk()
        root.withdraw()
        paths = filedialog.askopenfilenames(filetypes=(("Numpy Files", "*.npy"),
                                                     ("All Files", "*.*")))

    concat_list = list()
    for path in paths:
        data = np.load(path, allow_pickle=True)
        concat_list.extend(data)

    return concat_list


def retrieve_files(filetype, directory=None, count=False):

    if directory is None:
        root = tk.Tk()
        root.withdraw()
        directory = filedialog.askdirectory()

    working_files = list()
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith(filetype):
                working_files.append(str(directory + '/' + file))

    if count:
        counter(working_files)
    return working_files


def counter(obj):
    if isinstance(obj, (list, dict, tuple)):
        count = 0
        for _ in obj:
            count += 1
        print("Number of elements: ", count)
    elif isinstance(obj, str):
        words = obj.split(' ')
        for elem in words:
            count = 0
            if elem is obj:
                for _ in elem:
                    count += 1
                print("String is one word. There are %d characters in the word." % count)
            else:
                count += 1
                print("There are %d words in the string." % count)
    else:
        pass


def timer(method):
    def elapsed(*args, **kwargs):
        start = time.time()
        result = method(*args, **kwargs)
        end = time.time()
        time_elapsed = end-start
        print(time_elapsed)
        return result
    return elapsed


def copy_to_new_dir(files, destination):
    for file in files:
        shutil.copy(file, destination)

# create regex to find pat id
# use regex to match filenames from tkinter
# put matched items into new list
# save list to text file

if __name__ == "__main__":
    load_npy()
    # print(np_data)




