

def read_doc(filename):
    file = open(filename, 'r')
    text = file.read()
    file.close()
    return text


def save_list(List, filename):
    data = '\n'.join(List)
    file = open(filename, 'w')
    file.write(data)
    file.close()









