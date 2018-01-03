import pandas as pd
import string
import nltk


class Preprocessing:

    def __init__(self):
        pass

    __columns = ["label", "sender", "receiver", "subject", "body"]
    __dict = {'SPAM': True, 'HAM': False}

    def build_dataframes(self, emails):
        df = pd.DataFrame(emails)
        df.columns = self.__columns
        df['label'].replace(self.__dict, inplace=True)
        return df

    # clean up unicode in body
    @staticmethod
    def __decode(row):
        row = row['body']
        return row.encode('utf-8').decode('ascii', 'ignore')

    @staticmethod
    def tokenize(text):
        numbers = set(string.digits)
        whitespace = set(string.whitespace)
        exclude = set(string.punctuation)

        sent_tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')
        if len(text):
            sentences = sent_tokenizer.tokenize(text)

        text = []
        for word in sentences:
            if len(word) > 3:
                word = ''.join(ch for ch in word if (ch not in exclude
                                                     and ch not in numbers
                                                     and ch not in whitespace))
                if len(word) > 3:
                    text.append(word)

        text = ' '.join(word for word in text)
        return text

    @staticmethod
    def __gen_label_dict(df):
        labels = {}
        i = 0
        for label in df['label']:
            if label not in labels:
                labels[label] = i
                i = i + 1
        return labels

    @staticmethod
    def __gen_label_list(df, labels):
        label_list = []
        for label in df['label']:
            label_list.append(labels[label])
        return label_list

    # body = utf-8-encoded
    def pre_process(self, df):
        df['body'] = df.apply(self.__decode, axis=1)
        headers = list(df.columns.values)
        label_dict = self.__gen_label_dict(df)
        label_list = self.__gen_label_list(df, label_dict)

        return df, headers, label_dict, label_list

