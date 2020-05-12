import re
import bow

from commons import counter, timer
from nltk import pos_tag
##############################################################################


@timer
def re_duction(items, paths):
    files = []
    for item in items:
        regex = re.compile(item)
        for path in paths:
            if re.search(regex, path):
                files.append(path)
            else:
                continue
    counter(files)
    return files


@timer
def reduce_by_keywords(files, keywords):
    reduced_files = list()
    for file in files:
        marker = False
        with open(file, 'r') as f:
            s_tokenize = f.readlines()
            w_tokenize = list()

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
    counter(reduced_files)
    return reduced_files


def process_docs(paths, vocab, training=True):
    lines = list()
    for path in paths:
        line = bow.tokenize_doc(path, vocab)
        lines.append(line)
    return lines


