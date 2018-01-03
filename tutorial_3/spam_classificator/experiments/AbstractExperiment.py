import numpy as np
from sklearn.model_selection import StratifiedKFold
from sklearn.metrics import f1_score
from sklearn.metrics import accuracy_score
import cPickle as pickle
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split

from utils.EmailParser import EmailParser


class AbstractExperiment(object):

    classifier = ""
    k_of_kfolds = ""

    # these methods have to be overwritten in each experiment
    def preprocessed_data(self, emails):
        """This method has to return [X , Y] after preprocessing"""
        raise NotImplementedError('subclasses must override getPreprocessedData()!')

    def set_classifier(self):
        """This method has to return k"""
        raise NotImplementedError('subclasses must override get_classifier()!')

    def set_k_of_kfold(self):
        """This method has to return k"""
        raise NotImplementedError('subclasses must override get_k_of_kfold()!')

    def get_k_of_kfold(self):
        return self.k_of_kfolds

    # these methods are the same for every experiment, do not overwrite them
    def get_classifier_instance(self):
        return self.classifier

    def run(self, cross_validation, path):
        parser = EmailParser(path)
        emails = parser.parse_emails()
        if cross_validation:
            self.__execute_with_kfold_cross_validation(self.get_k_of_kfold(), emails)
        else:
            self.__execute_without_kfold(emails)

    def train(self, x_train, y_train):
        classifier = self.get_classifier_instance()
        classifier.fit(x_train, y_train)

    def predict(self, x_test):
        classifier = self.get_classifier_instance()
        return classifier.predict(x_test)

    def __execute_without_kfold(self, emails):
        """This method returns a vector with predictions for all documents"""
        x, y, dict_data = self.preprocessed_data(emails)
        print(str(len(x)))
        x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.33, random_state=42)

        self.train(x_train, y_train)
        predictions = self.predict(x_test)

        self.__serialize_results([x_test, y_test, predictions, dict_data])
        self.__print_analysis()

    def __execute_with_kfold_cross_validation(self, k, emails):
        """This method returns a vector with predictions for all documents"""
        x, y, dict_data = self.preprocessed_data(emails)
        skf = StratifiedKFold(n_splits=k, shuffle=True)

        all_predictions = np.zeros(y.shape)
        kfold_iterations_data = []

        iteration = 0

        for train_index, test_index in skf.split(x, y):
            print "Iteration ", iteration, ":", " Train ", train_index.shape[0], " Test ", test_index.shape[0]
            iteration += 1

            x_train, x_test = x[train_index, :], x[test_index, :]
            y_train, y_test = y[train_index], y[test_index]

            self.train(x_train, y_train)
            prediction = self.predict(x_test)

            kfold_iterations_data.append([test_index, y_test, prediction])
            all_predictions[test_index] = prediction

        self.__serialize_results([kfold_iterations_data, all_predictions, y, dict_data])
        self.__print_analysis()

    def __serialize_results(self, data):
        filename = "results/"+self.__class__.__name__+".p"
        print "Serializing to " + filename
        p = pickle.Pickler(open(filename, "wb"))
        p.fast = True
        p.dump(data)

    def __deserialize_results(self):
        filename = "results/"+self.__class__.__name__+".p"
        print "Deserialize from " + filename
        p = pickle.Unpickler(open(filename, "rb"))
        data = p.load()
        return data

    def __print_analysis(self):
        kfold_iterations_data, all_predictions, y, dict_data = self.__deserialize_results()

        iteration = 0

        for iteration_data in kfold_iterations_data:
            print "Iteration", iteration, ":"

            f1_score_value = f1_score(iteration_data[3], iteration_data[4], average=None)
            print "F1-Score: ",  f1_score_value
            print " Accuracy: ", accuracy_score(iteration_data[1], iteration_data[2])

            iteration += 1

        print "*----------------------------------------------------*"
        print " Overall analysis:"
        print "*----------------------------------------------------*"

        print " F1-Score: ", f1_score(y, all_predictions, average=None)
        print " Accuracy: ", accuracy_score(y, all_predictions)
        print " Confusion Matrix ", dict_data.keys()
        print confusion_matrix(y, all_predictions, dict_data.values())



