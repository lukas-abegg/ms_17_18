import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
import sklearn.pipeline
from sklearn.preprocessing import Normalizer
from utils.Preprocessing import Preprocessing
from AbstractExperiment import AbstractExperiment


class TfidfLsaExperiment(AbstractExperiment):

    def set_k_of_kfold(self, k):
        self.k_of_kfolds = k

    def set_classifier(self, classifier):
        self.classifier = classifier

    def preprocessed_data(self, emails):
        preprocessing = Preprocessing()
        df = preprocessing.build_dataframes(emails)
        data, data_headers, dictionary, y_train_raw = preprocessing.pre_process(df)
        texts = data.body.tolist()
        subjects = data.subject.tolist()

        print("Amount of emails: " + str(len(texts)))

        x_train_raw = []
        for text, subject in zip(texts, subjects):
            if not isinstance(text, type(None)):
                if not isinstance(subject, type(None)):
                    x_train_raw.append(" ".join([subject, Preprocessing.tokenize(text)]))
            else:
                x_train_raw.append(Preprocessing.tokenize(text))

        print("--> Done preprocessing.")
        vectorizer = TfidfVectorizer(binary=False, max_df=0.2, max_features=6000,
                                     min_df=7, stop_words='english', norm='l1',
                                     use_idf=True, smooth_idf=False, analyzer='word', ngram_range=(1, 4))

        print(x_train_raw)
        x_train_tfidf = vectorizer.fit_transform(x_train_raw)
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

        # Now apply the transformations to the test data as well.
        return np.array(x_train_lsa), np.array(y_train_raw), dictionary
