#! /usr/bin/python3

import nltk
import numpy as np
import sklearn.feature_extraction.text as sk_txt
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn.metrics import f1_score
import sklearn_crfsuite as sk_crf
from sklearn_crfsuite import metrics
import sys
import pickle


# grid-search
# More Feature Engineering!


# read document
# save per line: List(token, label, pos-tag)
def read_tokens_from_input_file(path):
    with open(path, "r", encoding='latin-1') as f:
        tokens = get_tokens_from_input_file(f.readlines())
    return tokens


def get_tokens_from_input_file(lines):
    words = []
    tags = []
    pos_tags = []
    # reading words / tags
    for i, line in enumerate(lines):
        if i % 10000 == 0:
            print(i, "lines of total:", len(lines))
        if len(line) > 0:
            found_tokens = nltk.word_tokenize(line)
            if len(found_tokens) == 2:
                words.append(found_tokens[0])
                tags.append(found_tokens[1])
    # reading pos-tags
    for pos_tag in nltk.pos_tag(words):
        pos_tags.append(pos_tag[1])
    return np.array([words, tags, pos_tags])


# Generate Features:
# tfidf
def get_tfidf(data, is_train=True):
    if is_train:
        bow = sk_txt.CountVectorizer(ngram_range=(1, 2), stop_words='english', lowercase=True)
        tfidf = sk_txt.TfidfTransformer()
        bow_t = bow.fit_transform(data[0], data[1])
        tfidf_t = tfidf.fit_transform(bow_t)
        pickle.dump(bow, open('bow.model', 'wb'))
        pickle.dump(tfidf, open('tfidf.model', 'wb'))
    else:
        bow = pickle.load(open('bow.model', 'rb'))
        tfidf = pickle.load(open('tfidf.model', 'rb'))
        bow_t = bow.transform(data[0])
        tfidf_t = tfidf.transform(bow_t)
    return tfidf_t


# Extract features
# word before, word behind
# POS-Tag before, POS-Tag after
# is_BOS / is_EOS / is_IOS
# contains numbers
# contains special characters
# ..
def generate_feature_dicts(tokens, tfidf):
    x = tokens[0]
    pos = tokens[2]

    features = []

    for i, w in enumerate(x):
        feat = {
            'word.lower': w.lower(),
            'isupper': w.isupper(),
            'isdigit': w.isdigit(),
            'postag': pos[i],
        }
        if i > 0:
            last_w = x[i - 1]
            last_pos = pos[i - 1]
            feat.update(
                {
                    'last_w': last_w.lower(),
                    'last_isupper': last_w.isupper(),
                    'last_postag': last_pos
                }
            )
        if i < len(x) - 1:
            next_w = x[i + 1]
            next_pos = pos[i + 1]
            feat.update(
                {
                    'next_w': next_w.lower(),
                    'next_isupper': next_w.isupper(),
                    'next_postag': next_pos
                }
            )
        features.append(feat)

    return features


def get_features(tokens, is_train=True):
    tfidf = get_tfidf(tokens, is_train=is_train)
    features = generate_feature_dicts(tokens, tfidf)
    return features


# train model
def train(trainpath, modelname):
    # extract features
    train_tokens = read_tokens_from_input_file(trainpath)
    train_features = get_features(train_tokens)
    # fitting
    crf = sk_crf.CRF(algorithm='lbfgs', c1=0.1, c2=0.1, max_iterations=100, all_possible_transitions=True)
    crf.fit([train_features], [train_tokens[1]])
    print('should dump model here')
    pickle.dump(crf, open(modelname, 'wb'))


def annotate_doc(path, labels, outpath):
    nf = open(outpath, 'w')
    i = 0
    with open(path, "r", encoding='latin-1') as f:
        for j, line in enumerate(f.readlines()):
            found_tokens = nltk.word_tokenize(line)
            if len(found_tokens) == 2:
                nf.write("{}\t{}\n".format(found_tokens[0], labels[0][i]))
                i += 1
            else:
                nf.write(line)
    nf.close()


# predict new data
def predict(testpath, outpath, modelname):
    # extract features
    test_tokens = read_tokens_from_input_file(testpath)
    test_features = get_features(test_tokens, is_train=False)
    # predicting
    crf = pickle.load(open(modelname, 'rb'))
    label_pred = crf.predict([test_features])
    annotate_doc(testpath, label_pred, outpath)


def fscore_crf(y, y_pred, labels):
    labels.remove('O')
    return metrics.flat_f1_score(y, y_pred, average='weighted', labels=labels)


def k_cross_validate(x_tr, y_tr, pos_tr, do_annotating=False):
    k_fold = KFold(n_splits=10)
    crf = sk_crf.CRF(algorithm='lbfgs', c1=0.1, c2=0.1, max_iterations=100, all_possible_transitions=True)
    scores = []

    for i, (train, validate) in enumerate(k_fold.split(x_tr)):
        x_tr_cv, x_val_cv = x_tr[train], x_tr[validate]
        y_tr_cv, y_val_cv = y_tr[train], y_tr[validate]
        pos_tr_cv, pos_val_cv = pos_tr[train], pos_tr[validate]

        f_tr = get_features([x_tr_cv, y_tr_cv, pos_tr_cv])
        f_val = get_features([x_val_cv, y_val_cv, pos_val_cv], is_train=False)

        crf.fit([f_tr], [y_tr_cv])
        pred = crf.predict([f_val])

        scores.append(fscore_crf([y_val_cv], pred, list(crf.classes_)))
        print(str(i+1)+'-fold Score:', scores)

        if do_annotating:
            original = [x_val_cv.tolist(), y_val_cv.tolist()]
            prediction = [x_val_cv.tolist(), pred[0]]
            o = open('validation/'+str(i+1)+'fold_original.iob', "w")
            p = open('validation/'+str(i+1)+'fold_prediction.iob', "w")
            for j, _ in enumerate(x_val_cv.tolist()):
                o.write("{}\t{}\n".format(original[0][j], original[1][j]))
                p.write("{}\t{}\n".format(prediction[0][j], prediction[1][j]))
            o.close()
            p.close()
    print('Average Score:', np.mean(scores))


def validate(path, do_annotating=False):
    tokens = read_tokens_from_input_file(path)
    if do_annotating:
        k_cross_validate(tokens[0], tokens[1], tokens[2], do_annotating=True)
    else:
        x_tr, x_ts, y_tr, y_ts, pos_tr, pos_ts = train_test_split(tokens[0], tokens[1], tokens[2], test_size=0.3)
        k_cross_validate(x_tr, y_tr, pos_tr)
        # train/predict x_ts/y_ts
        f_tr = get_features([x_tr, y_tr, pos_tr])
        crf = sk_crf.CRF(algorithm='lbfgs', c1=0.1, c2=0.1, max_iterations=100, all_possible_transitions=True)
        crf.fit([f_tr], [y_tr])
        f_ts = get_features([x_ts, y_ts, pos_ts], is_train=False)
        y_pred = crf.predict([f_ts])
        print("F1-Score on Testset:", fscore_crf([y_ts], y_pred, list(crf.classes_)))


# train: python3 crf_ner.py train data/training5_annotated.iob crf.model
# annotate: python3 crf_ner.py annotate crf.model data/training5_not_annotated.iob data/training5_predicted.iob
def main():
    # train model
    if sys.argv[1] == 'train':
        if len(sys.argv) != 4:
            print('Wrong number of arguments! (I need datapath and modelpath)')
            return False
        trainpath = sys.argv[2]
        modelpath = sys.argv[3]
        train(trainpath, modelpath)
        print('Model successfully trained!')

    # predict
    if sys.argv[1] == 'annotate':
        if len(sys.argv) != 5:
            print('Wrong number of arguments!')
            return False
        modelname = sys.argv[2]
        testpath = sys.argv[3]
        outpath = sys.argv[4]
        predict(testpath, outpath, modelname)
        print('Document annotated!')

    if sys.argv[1] == 'validate':
        path = sys.argv[2]
        validate(path)

    if sys.argv[1] == 'validate_annotate':
        path = sys.argv[2]
        validate(path, do_annotating=True)


if __name__ == '__main__':
    main()
