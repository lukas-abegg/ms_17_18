#! /usr/bin/python3
import os
import re
import ssl
import string
import sys

import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

import numpy as np
import pandas as pd

from sklearn.base import TransformerMixin, BaseEstimator
from sklearn.decomposition import TruncatedSVD
from sklearn.externals import joblib
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix
from sklearn.metrics import f1_score, recall_score, precision_score
from sklearn.model_selection import KFold
from sklearn.model_selection import train_test_split
from sklearn.pipeline import Pipeline, make_pipeline, make_union
from sklearn.preprocessing import Normalizer


class ItemSelector(BaseEstimator, TransformerMixin):
    def __init__(self, column):
        self.column = column

    def fit(self, x, y=None):
        return self

    def transform(self, x, y=None):
        return x[self.column]


def main():
    if len(sys.argv) < 2:
        # print('Please specifiy the mode you want to use (train / classify)')
        model_name = "model"
        ham_directory = "../email-korpus/train-ham"
        spam_directory = "../email-korpus/train-spam"
        train_model(model_name, ham_directory, spam_directory)
        print('---- Training sucessfull ----')
    elif sys.argv[1].lower() == 'train' and len(sys.argv) == 5:
        model_name = sys.argv[2]
        ham_directory = sys.argv[3]
        spam_directory = sys.argv[4]
        train_model(model_name, ham_directory, spam_directory)
        print('---- Training sucessfull, model saved ----')
    elif sys.argv[1].lower() == 'classify':
        model_name = sys.argv[2]
        email_directory = sys.argv[3]
        result_file = sys.argv[4]
        classify_mails(model_name, email_directory, result_file)
        print('---- Prediction completed ----')
    else:
        print(sys.argv[1].lower())
        print('Oops, something went wrong, please check if you called the script correctly :)')


def train_model(model_name, ham_directory, spam_directory):
    nltk.download('stopwords')
    df_ham = make_df(ham_directory, 0)
    df_spam = make_df(spam_directory, 1)
    print("---------    Preprocessing done   ---------")

    # Create Test and Training data
    df_final = pd.concat([df_ham, df_spam])
    df_final = df_final.sample(frac=1).reset_index(drop=True)

    x = df_final[['body', 'subject', 'sender']]
    y = df_final.label.values

    # body pipe
    body_pipe = make_pipeline(
        ItemSelector('body'),
        TfidfVectorizer(encoding='latin1', lowercase=False, ngram_range=(1, 4))
    )

    # subject pipe
    subject_pipe = make_pipeline(
        ItemSelector('subject'),
        TfidfVectorizer(encoding='latin1', lowercase=False, ngram_range=(1, 4))
    )

    # sender pipe
    sender_pipe = make_pipeline(
        ItemSelector('sender'),
        TfidfVectorizer(encoding='latin1', lowercase=False, ngram_range=(1, 4))
    )

    feature_union = make_union(body_pipe, subject_pipe, sender_pipe)

    lr_tfidf = Pipeline([
        ('union', feature_union),
        ('svd', TruncatedSVD(n_components=500)),
        ('norm', Normalizer(copy=False)),
        ('clf', LogisticRegression(random_state=0, C=100.0, penalty="l2"))
    ])

    x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.3, random_state=42)
    x_train = pd.DataFrame(x_train, columns=['body', 'subject', 'sender'])
    x_test = pd.DataFrame(x_test, columns=['body', 'subject', 'sender'])

    lr_tfidf.fit(x_train, y_train)

    print("Train data:  -------------------")
    print_scores(lr_tfidf, x_train, y_train)
    print("Test data:   -------------------")
    print_scores(lr_tfidf, x_test, y_test)
    print("Crossvalidation data:   -------------------")
    cross_valid(lr_tfidf, x, y)

    # Train on the complete corpus
    lr_tfidf = lr_tfidf.fit(x, y)

    # Save trained model
    joblib.dump(lr_tfidf, '{}.clf'.format(model_name))


def classify_mails(model_name, email_directory, result_file):
    df = make_df(email_directory, 2)  # label gets dropped later anyway
    x = df[['body', 'subject', 'sender']]
    model_name = model_name + ".clf"
    clf = joblib.load(model_name)
    y = clf.predict(x)
    my_list = y.tolist()
    my_list = ['SPAM' if elem == 1 else 'HAM' for elem in my_list]
    filenames = os.listdir(email_directory)
    my_file = open(result_file, 'w')

    for i, item in enumerate(my_list):
        my_file.write("{} \t {}\n".format(filenames[i], item))
    my_file.close()


def convert_files(directory):
    root = "./"
    # build directory path
    directory_path = os.path.join(root, directory)

    for mail in os.listdir(directory_path):
        file_path = directory_path + "/" + mail
        with open(file_path, "r", encoding='latin-1') as m:
            mail_dict = parse_message(m)
            yield mail_dict


def parse_message(msg):
    body = ''
    email = {'body': '', 'subject': '', 'sender': '', 'label': ''}
    in_body = False
    exclude_terms = ['URL:', 'Date:', 'Return-Path:']

    for line in msg:
        if line == '\n':
            in_body = True
            continue

        if any(term in line for term in exclude_terms):
            continue

        # get rid of html markup
        line = re.sub('<[^>]*>', '', line)

        if in_body:
            # get rid of stopwords
            line = tokenize(line)
            body += line.strip()
            email['body'] = body
        elif line.startswith('From:'):
            sender = line.strip()
            sender = sender.replace('"', '')
            sender = sender[5:]
            email['sender'] = sender
        elif line.startswith('Subject:'):
            # get rid of stopwords
            line = tokenize(line)
            subject = line.strip()
            subject = subject[8:]
            email['subject'] = subject

        # Optionally an else branch could extract more features
    return email


def tokenize(text):
    exclude = set(string.punctuation)
    stop_words = set(stopwords.words('english'))

    tokens = word_tokenize(text)

    text = []
    for word in tokens:
        if len(word) > 3 and (word not in stop_words
                              and word not in exclude):
            text.append(word)

    text = ' '.join(word for word in text)
    return text


def calc_scores(lr_tfidf, x, y):
    y_pred = lr_tfidf.predict(x)

    accuracy = lr_tfidf.score(x, y)
    f1 = f1_score(y, y_pred, average=None)
    recall = recall_score(y, y_pred, average=None)
    precision = precision_score(y, y_pred, average=None)
    return accuracy, f1, recall, precision


def print_scores(lr_tfidf, x, y):
    (accuracy, f1, recall, precision) = calc_scores(lr_tfidf, x, y)

    print("SPAM und HAM getrennt:")
    print("----------------------")
    print("Accuracy in Test: " + str(accuracy))
    print("Precision in Test: " + str(precision))
    print("Recall in Test: " + str(recall))
    print("F1 in Test: " + str(f1))

    print("Mean der beiden Klassen:")
    print("----------------------")
    print("Accuracy in Test: " + str(np.mean(accuracy)))
    print("Precision in Test: " + str(np.mean(precision)))
    print("Recall in Test: " + str(np.mean(recall)))
    print("F1 in Test: " + str(np.mean(f1)))


def cross_valid(lr_tfidf, x, y):
    k_fold = KFold(n_splits=10)
    confusion_lr = np.array([[0, 0], [0, 0]])
    acc_lr = []

    for i, (train, validate) in enumerate(k_fold.split(x)):
        x_tr, x_val = x.values[train], x.values[validate]
        y_tr, y_val = y[train], y[validate]

        x_tr = pd.DataFrame(x_tr, columns=['body', 'subject', 'sender'])
        x_val = pd.DataFrame(x_val, columns=['body', 'subject', 'sender'])

        lr_tfidf.fit(x_tr, y_tr)

        acc_lr.append(lr_tfidf.score(x_val, y_val))
        confusion_lr += confusion_matrix(y_val, lr_tfidf.predict(x_val))

    print('Confusion - LR')
    print(confusion_lr)
    acc_lr = np.asarray(acc_lr)
    print('Mean-Accuracy LR: {}'.format(np.mean(acc_lr)))
    print('Standard Deviation LR: {}'.format(np.std(acc_lr)))
    print('Variance LR: {}'.format(np.var(acc_lr)))


def make_df(path, label):
    rows = []
    index = []
    for i, text in enumerate(convert_files(path)):
        rows.append({'body': text['body'], 'subject': text['subject'], 'sender': text['sender'], 'label': label})
        index.append(i)

    df = pd.DataFrame(rows, index=index)
    return df


if __name__ == '__main__':
    try:
        _create_unverified_https_context = ssl._create_unverified_context
    except AttributeError:
        pass
    else:
        ssl._create_default_https_context = _create_unverified_https_context
    main()
