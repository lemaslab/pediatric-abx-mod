import pre_processing as prep
import bow
import commons

keywords = ['feed', 'feeding', 'fed', 'breast', 'breastfeed', 'breastfeeding',
            'breastfed', 'breast-feed', 'breast-feeding', 'breast-fed',
            'bottle-feed', 'bottle-feeding', 'bottle-fed', 'bottle', 'nurse',
            'nursing', 'nursed', 'lactate', 'lactating', 'lactation']


def main(files=None, vocab=None, save_ids=False, save_vocab=False):
    if files is None:
        files = commons.retrieve_files('.txt')
        np_data = commons.load_npy()
        files = prep.reDuction(np_data, files)
        files = prep.reduce(files, keywords)
        if save_ids:
            commons.save_list(files, "reduced_patIDs.txt")
    else:
        #open file and convert to list, set equal to files

    if vocab is None:
        vocab = bow.bagging(files)
        vocab = bow.filter_and_reduce(vocab, 2)
        if save_vocab:
            commons.save_list(vocab, 'breastfeeding_vocab.txt')
    else:
        #same as above for vocab

    vocab = set(vocab)


if __name__ == '__main__':
    main(save_ids=True, save_vocab=True)

    # positive_lines = prep.process_docs(commons.retrieve_files('.txt'), vocab)
    # negative_lines = prep.process_docs(commons.retrieve_files('.txt'), vocab)
    #
    # print(len(positive_lines), len(negative_lines))


