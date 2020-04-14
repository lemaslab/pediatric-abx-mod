import tkinter as tk
import re
import os

from tkinter import filedialog
from nltk import pos_tag
##############################################################################


def reDuction(items, paths):
    files = []
    for item in items:
        regex = re.compile(item)
        for path in paths:
            if re.search(regex, path):
                files.append(path)
            else:
                continue
    return files

def retrievefiles(filetype, path=None):

    if path is None:
        root = tk.Tk()
        root.withdraw()
        path = filedialog.askdirectory()

    working_files = []
    for root, dirs, files in os.walk(path):
        for file in files:
            if file.endswith(filetype):
                working_files.append(str(path + '/' + file))
    return working_files


def reduce(files, keywords):
    reduced_files = []
    for file in files:
        marker = False
        with open(file, 'r') as f:
            s_tokenize = f.readlines()
            w_tokenize = []

            for i, sent in enumerate(s_tokenize):
                s_tokenize[i] = sent.lstrip()
                w_tokenize.append(re.findall(r"[\w'-]+|[.,!?;]", s_tokenize[i]))

            for i, words in enumerate(w_tokenize):
                tagged = pos_tag(words)

            for elem in tagged:
                for word in keywords:
                    if elem[0] == word and marker is False:
                        marker = True

            if marker is True:
                reduced_files.append(file)
    return reduced_files

