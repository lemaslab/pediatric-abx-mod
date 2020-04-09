

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


# create regex to find pat id
# use regex to match filenames from tkinter
# put matched items into new list
# save list to text file








