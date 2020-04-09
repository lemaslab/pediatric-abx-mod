import pre_processing
import bow
import commons

keywords = ['feed', 'feeding', 'fed', 'breast', 'breastfeed', 'breastfeeding',
            'breastfed', 'breast-feed', 'breast-feeding', 'breast-fed',
            'bottle-feed', 'bottle-feeding', 'bottle-fed', 'bottle', 'nurse',
            'nursing', 'nursed', 'lactate', 'lactation']

if __name__ == '__main__':
    files = pre_processing.retrievefiles('.txt')
    print(len(files))
    files = pre_processing.reduce(files, keywords)
    print(len(files))
    vocab = bow.bagging(files)
    vocab = bow.filter_and_reduce(vocab, 2)
    print(vocab)
    print(len(vocab))
    commons.save_list(vocab, 'breastfeeding_vocab.txt')

    # test = bow.create_matrix(files, vocab)
    # print(test)

#come up with criteria for being in cohort
    #1 prenata
    #1 postnatal
    #birth weight
    #
