import re
import operator
import string
import commons


from nltk.corpus import stopwords
##############################################################################


def clean_doc(doc, stop_words=True, alpha=True):

    tokens = doc.split()
    table = str.maketrans('', '', string.punctuation)
    tokens = [word.translate(table) for word in tokens]
    if alpha is True:
        tokens = [word for word in tokens if word.isalpha()]
    if stop_words is True:
        stop_words = set(stopwords.words('english'))
        tokens = [word for word in tokens if word not in stop_words]
    tokens = [word for word in tokens if len(word) > 1]
    return tokens


def bagging(files):
    vocab = {}
    for file in files:
        with open(file, 'r') as f:
            s_tokenize = f.readlines()
            s_tokenize = [x.lower() for x in s_tokenize]

            for i, sent in enumerate(s_tokenize):
                s_tokenize[i] = sent.lstrip()
                w_tokenize = re.findall(r"[\w'-]+|[.,!?;]", s_tokenize[i])
                w_tokenize = ' '.join([str(elem) for elem in w_tokenize])
            w_tokenize = clean_doc(w_tokenize)
            for i, elem in enumerate(w_tokenize):
                if re.match("[.,!?;]", elem):
                    continue
                elif elem not in vocab and elem.isalpha():
                    vocab[elem] = 1
                elif elem in vocab:
                    vocab[elem] += 1
    return vocab


def filter_and_reduce(dict, minimum):
    tokens = sorted(dict.items(), key=operator.itemgetter(1), reverse=True)
    tokens = [key for key, value in tokens if not value <= minimum]
    return tokens


def tokenize_doc(path, vocab):
    doc = commons.load_doc(path)
    tokens = clean_doc(doc)
    tokens = [word for word in tokens if word in vocab]
    return ' '.join(tokens)




















# def create_matrix(rows, columns):
#     # print(files)
#     # rows = []
#     # for i, file in enumerate(files):
#     #     if i == 0:
#     #         rows = filefrompath(file, rows)
#     #     else:
#     #         rows = filefrompath(file, rows)
#     #
#     #     if file is files[-1]:
#     #         return rows
#     #     else:
#     #         continue
#
#
#     # Set columns to words
#     # Set rows to filenames
#     # date of birth
#     # born in hospital
#     # one prenatal visit in 1st month
#     # USE REDCAP
#     # if date of birth and health and wellness visit within first 60 days
#
#     # move everything to UF health repo
#
#     #UFRC
#     pass

# def build_matrix(columns, rows, newfilename=None):
#     if newfilename is None:
#         newfilename = 'matrix.txt'
#     with open(newfilename, 'w+') as nf:
#         for i, x in enumerate(columns):
#             if i == 0:
#                 nf.write('\t')
#             elif x == columns[-1]:
#                 nf.write(x + '\n')
#                 break
#             elif i:
#                 nf.write(x + '\t')
#
#         for i, y in enumerate(rows):
#             text = read_doc(y)
#             #insert regex here; save equal to 'doc'
#             nf.write(doc + '\t')
#
#             for x in columns:
#
#                 # If we pass 'rows' as a full file path, we can use regex to name the rows
#             # I need to read the files to determine whether they should have a 1 or 0 for each point.
#             #don't forget the 'if fill is None' statement
#             if

