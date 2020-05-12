import pre_processing as prep
import bow
import commons
from __init__ import current_date, current_time, dest
from tensorflow.keras.preprocessing.text import Tokenizer
keywords = ['feed', 'feeding', 'fed', 'breast', 'breastfeed', 'breastfeeding',
            'breastfed', 'breast-feed', 'breast-feeding', 'breast-fed',
            'bottle-feed', 'bottle-feeding', 'bottle-fed', 'bottle', 'nurse',
            'nursing', 'nursed', 'lactate', 'lactating', 'lactation']


def main(files=None, vocab=None, save_ids=False, save_vocab=False):
    if files is None:
        files = commons.retrieve_files('.txt', count=True)
        np_data = commons.load_npy()
        files = prep.re_duction(np_data, files)
        files = prep.reduce_by_keywords(files, keywords)
        if save_ids:
            commons.save_list(files, current_date+'-'+current_time+'_'+"reduced_patIDs.txt", save_dest=dest)
        commons.copy_to_new_dir(files, dest+"Med_Notes_reduced")
    else:
        pass
        #open file and convert to list, set equal to files

    if vocab is None:
        vocab = bow.bagging(files)
        vocab = bow.filter_and_reduce(vocab, 2)
        if save_vocab:
            commons.save_list(vocab, current_date+'-'+current_time+'_'+"breastfeeding_vocab.txt", save_dest=dest)
    else:
        vocab = bow.bagging(files)
        vocab = bow.filter_and_reduce(vocab, 2)

    vocab = set(vocab)
    return vocab


if __name__ == '__main__':
    vocab = main(save_ids=True, save_vocab=True)
# Make a directory in a specified path
# I want to save vocab here
# Save using DATE-TIME_breastfeeding_vocab.txt
# I want to make a directory inside of that one for the revised file list
# Not just a txt file with path list, copy paths to folder


    # positive_lines = prep.process_docs(commons.retrieve_files('.txt'), vocab)
    # negative_lines = prep.process_docs(commons.retrieve_files('.txt'), vocab)
    #
    # print(len(positive_lines), len(negative_lines))
    #
    # tokenizer = Tokenizer()
    # docs = positive_lines + negative_lines
    # tokenizer.fit_on_texts(docs)
    #
    # Xtrain = tokenizer.texts_to_matrix(docs, mode='freq')
    # print(Xtrain.shape)

