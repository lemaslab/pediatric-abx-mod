import pre_processing as prep
import bow
import commons as coms

keywords = ['feed', 'feeding', 'fed', 'breast', 'breastfeed', 'breastfeeding',
            'breastfed', 'breast-feed', 'breast-feeding', 'breast-fed',
            'bottle-feed', 'bottle-feeding', 'bottle-fed', 'bottle', 'nurse',
            'nursing', 'nursed', 'lactate', 'lactation']

if __name__ == '__main__':

    files = prep.retrievefiles('.txt')
    np_data = coms.read_npy()
    files = prep.reDuction(np_data, files)
    files = prep.reduce(files, keywords)
    # coms.save_list(files, "reduced_patIDs.txt")
    vocab = bow.bagging(files)
    vocab = bow.filter_and_reduce(vocab, 2)
    coms.save_list(vocab, 'breastfeeding_vocab.txt')

    # test = bow.create_matrix(files, vocab)
    # print(test)

#come up with criteria for being in cohort
    #1 prenata
    #1 postnatal
    #birth weight
    #
