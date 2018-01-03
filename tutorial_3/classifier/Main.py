import os
import re
from nltk.corpus import stopwords
import pandas as pd
import numpy as np
import nltk

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
import sklearn.pipeline
from sklearn.preprocessing import Normalizer
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
from sklearn.metrics import confusion_matrix

import codecs

root = "../"


def make_df(path, label):
    rows = []
    index = []
    for i, text in enumerate(convert_files(path)):
        rows.append({'body': text['body'], 'label': label})  # 'subject': text['subject'], 'sender' : text['sender'],
        index.append(i)

    df = pd.DataFrame(rows, index=index)
    return df


def convert_files(directory):
    # build directory path
    directory_path = os.path.join(root, directory)

    for mail in os.listdir(directory_path):
        file_path = directory_path + "/" + mail
        with codecs.open(file_path, "r", encoding='utf-8', errors='ignore') as m:
            mail_dict = parse_message(unicode(m.read()))
            yield mail_dict


def parse_message(msg):
    body = ''
    email = {}
    # email['subject'] = ''
    in_body = False
    exclude_terms = ['URL:', 'Date:', 'Return-Path:']
    sw = stopwords.words("english")

    for line in msg:
        if line == '\n':
            in_body = True
            continue

        if any(term in line for term in exclude_terms):
            continue

        # get rid of html markup
        line = re.sub('<[^>]*>', '', line)

        # Sget rid of stopwords
        line = ' '.join([word for word in line.split() if word.lower() not in sw])

        if in_body:
            body += line.strip()
            email['body'] = body
    #         elif line.startswith('From:'):
    #             sender = line.strip()
    #             sender = sender.replace('"', '')
    #             sender = line[5:]
    #             email['sender'] = sender
    #         elif line.startswith('Subject:'):
    #             subject = line.strip()
    #             subject = line[8:]
    #             email['subject'] = subject

    # Optionally an else branch could extract more features

    return email


df_ham = make_df('email-korpus/train-ham', 0)
df_spam = make_df('email-korpus/train-spam', 1)

# Create test and training data
df_final = pd.concat([df_ham, df_spam])
df_final = df_final.sample(frac=1).reset_index(drop=True)
X = df_final.body.values
y = df_final.label.values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

vectorizer = TfidfVectorizer(binary=False, max_df=0.2, max_features=6000,
                                     min_df=7, stop_words='english', norm='l1',
                                     use_idf=True, smooth_idf=False, analyzer='word', ngram_range=(1, 4))

k_fold = KFold(n_splits=10)
confusion_knn = np.array([[0, 0], [0, 0]])

acc_knn = []

for i, (train, validate) in enumerate(k_fold.split(X_train)):
    X_tr, X_val = X_train[train], X_train[validate]
    y_tr, y_val = y_train[train], y_train[validate]

    vectorizer.fit(X_tr, y_tr)

    x_train_tfidf = vectorizer.fit_transform(X_tr, y_tr)

    print("Actual number of tfidf features: %d" % x_train_tfidf.get_shape()[1])
    print("Performing dimensionality reduction using LSA")

    # Project the tfidf vectors onto the first 150 principal components.
    # Though this is significantly fewer features than the original tfidf vector,
    # they are stronger features, and the accuracy is higher.
    svd = TruncatedSVD(90)
    lsa = sklearn.pipeline.make_pipeline(svd, Normalizer(copy=False))

    #   Run SVD on the training data, then project the training data.
    x_train_lsa = lsa.fit_transform(x_train_tfidf)
    explained_variance = svd.explained_variance_ratio_.sum()
    print("Explained variance of the SVD step: {}%".format(int(explained_variance * 100)))

    knn = KNeighborsClassifier(n_neighbors=13)
    knn.fit(x_train_lsa, y_tr)

    x_test_tfidf = vectorizer.transform(X_val)
    x_test_lsa = lsa.transform(x_test_tfidf)

    acc_knn.append(knn.score(x_test_lsa, y_val))
    confusion_knn += confusion_matrix(y_val, vectorizer.predict(x_test_lsa))

acc_knn = np.asarray(acc_knn)

print('Average Accuarcy')
print('KNN: {}'.format(np.mean(acc_knn)))

print('Standard-deviation')
print('KNN: {}'.format(np.std(acc_knn)))

print('Variance')
print('KNN: {}'.format(np.var(acc_knn)))

print('Confusion - KNN')
print(confusion_knn)
