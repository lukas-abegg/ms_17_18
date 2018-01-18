#! /usr/bin/python3
import sys
import string
import ssl
import math
from typing import List

import pickle

import nltk
from nltk import SnowballStemmer
from nltk import edit_distance, pos_tag
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize


class Token:
    def __init__(self, value: str, tag: str, pos_tagging: str):
        self.value = value
        self.tag = tag
        self.pos_tagging = pos_tagging


class NERTagger:
    __tag_no_entity = "O"

    def __init__(self, *args):
        if len(args) == 4:
            self.dictionary = args[0]
            self.stopwords_list = args[1]
            self.punctuation = args[2]
            self.pos_tagging = args[3]
        else:
            self.__load_dictionaries()

    def save_dictionaries(self):
        pickle.dump(self.dictionary, open("dictionary.p", "wb"))
        pickle.dump(self.stopwords_list, open("stopwords_list.p", "wb"))
        pickle.dump(self.punctuation, open("punctuation.p", "wb"))
        pickle.dump(self.pos_tagging, open("pos_tagging.p", "wb"))

    def __load_dictionaries(self):
        self.dictionary = pickle.load(open("dictionary.p", "rb"))
        self.stopwords_list = pickle.load(open("stopwords_list.p", "rb"))
        self.punctuation = pickle.load(open("punctuation.p", "rb"))
        self.pos_tagging = pickle.load(open("pos_tagging.p", "rb"))

    def tag_tokens(self, words: List[Token]):
        tagged_words = []
        for word in words:
            if self.__check_no_entity(word):
                tagged_words.append(Token(word.value, self.__tag_no_entity, word.pos_tagging))
            else:
                tag = self.__check_rules(word)
                tagged_words.append(Token(word.value, tag, word.pos_tagging))
        return tagged_words

    def __check_no_entity(self, word: Token) -> bool:
        if word.value in self.stopwords_list:
            return True
        elif word.value in self.punctuation:
            return True
        else:
            return False

    def __check_rules(self, word: Token) -> str:
        keys = list(self.dictionary.keys())

        if word.pos_tagging in self.pos_tagging:
            if word.value in keys:
                return self.dictionary[word.value]
            else:
                found, tag = self.__check_distance(word, keys)
                # if found:
                #     return tag
                # else:
                #     found, tag = self.__check_word_stems(word, keys)
        else:
            tag = self.__tag_no_entity
        return tag

    def __check_distance(self, word: Token, keys) -> (bool, str):
        # calculate edit-distances
        distances = list(map(lambda x: edit_distance(word.value, x), keys))

        # get index where distance is minimal - conflict resolution happens here as we just pick first index_min
        index_min = min(range(len(distances)), key=distances.__getitem__)

        if distances[index_min] < round(math.floor(len(word.value)) * 0.9):
            return True, self.dictionary[keys[index_min]]
        else:
            return False, self.__tag_no_entity

    def __check_word_stems(self, word, keys) -> (bool, str):
        stemmer = SnowballStemmer("english")
        stemmed_word = stemmer.stem(word.value)
        stemmed_keys = list(map(lambda x: stemmer.stem(x), keys))

        if stemmed_word in stemmed_keys:
            idx = stemmed_keys.index(stemmed_word)
            return True, self.dictionary[keys[idx]]

        return False, self.__tag_no_entity


def main():
    if len(sys.argv) < 2:
        input_file = "./../data/uebung4-training.iob"
        train_tagger(input_file)
        print('---- Training for NER-Tagger is finished ----')
    elif len(sys.argv) == 3:
        input_file = sys.argv[1]
        output_file = sys.argv[2]
        tag_tokens(input_file, output_file)
        print('---- NER-Tagger finished, see result in output file ----')
    else:
        print(sys.argv[1].lower())
        print('Oops, something went wrong, please check if you called the script correctly :)')


def train_tagger(input_file):
    gold_std, pos_taggings = build_dict_from_input_file(input_file)
    additional_dictionary = build_additional_dict()
    dictionary = concatenate_dicts(gold_std, additional_dictionary)
    stopwords_list = build_stopwords()
    punctuation = set(string.punctuation)
    tagger = NERTagger(dictionary, stopwords_list, punctuation, pos_taggings)
    tagger.save_dictionaries()


def tag_tokens(input_file, output_file):
    tokens = read_tokens_from_input_file(input_file)
    tagger = NERTagger()
    tagged_list = tagger.tag_tokens(tokens)
    write_annotations_to_file(tagged_list, output_file)


def build_dict_from_input_file(path) -> (dict, set):
    entities_from_file = {}
    pos_taggings_from_file = []
    with open(path, "r", encoding='latin-1') as f:
        for token in get_tokens_from_input_file(f.readlines()):
            if token.tag != "O":
                entities_from_file[token.value] = token.tag
                pos_taggings_from_file.append(token.pos_tagging)
    return entities_from_file, set(pos_taggings_from_file)


def build_additional_dict():
    entities_from_file = {}
    path = "./../data/dictionary/human-genenames.txt"
    with open(path, "r", encoding='latin-1') as f:
        for token in get_tokens_from_list(f.readlines()):
            entities_from_file[token] = "B-protein"
    return entities_from_file


def concatenate_dicts(gold_std, additional_dict):
    return dict(list(gold_std.items()) + list(additional_dict.items()))


def build_stopwords():
    nltk.download('stopwords')
    stopwords_list = []
    path = "./../data/dictionary/english_stop_words.txt"
    with open(path, "r", encoding='latin-1') as f:
        for token in get_tokens_from_list(f.readlines()):
            stopwords_list.append(token)
    return set(stopwords_list).union(set(stopwords.words('english')))


def read_tokens_from_input_file(path):
    with open(path, "r", encoding='latin-1') as f:
        tokens = get_tokens_from_input_file(f.readlines())
    return tokens


def write_annotations_to_file(annotations: List[Token], output_file):
    my_file = open(output_file, 'w')
    for token in annotations:
        my_file.write("{}\t{}\n".format(token.value, token.tag))
    my_file.close()


def get_tokens_from_list(lines):
    tokens = []
    for line in lines:
        if len(line) > 0:
            found_tokens = word_tokenize(line)
            if len(found_tokens) == 1:
                tokens.append(found_tokens[0])
    return tokens


def get_tokens_from_input_file(lines) -> List[Token]:
    words = []
    tags = []
    pos_taggings = []
    tokens = []
    # reading words / tags
    for line in lines:
        if len(line) > 0:
            found_tokens = word_tokenize(line)
            if len(found_tokens) == 2:
                words.append(found_tokens[0])
                tags.append(found_tokens[1])
    # reading pos-tags
    for pos_tagging in pos_tag(words):
        pos_taggings.append(pos_tagging[1])
    # concatenate all
    for i in range(len(words)):
        tokens.append(Token(words[i], tags[i], pos_taggings[i]))
    return tokens


if __name__ == '__main__':
    try:
        _create_unverified_https_context = ssl._create_unverified_context
    except AttributeError:
        pass
    else:
        ssl._create_default_https_context = _create_unverified_https_context
    main()
