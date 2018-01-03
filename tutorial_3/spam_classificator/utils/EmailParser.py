import codecs
from os import listdir
from os.path import isfile, join
import email
from BeautifulSoup import BeautifulSoup


class Email:
    label = ""
    sender = ""
    receiver = ""
    subject = ""
    body = ""

    def __init__(self, label, mail):
        self.label = label
        self.sender = mail["from"]
        self.sender = mail["to"]
        self.sender = mail["subject"]
        text = ""
        if mail.is_multipart():
            for payload in mail.get_payload():
                text = text + payload.get_payload()
        else:
            text = mail.get_payload()
        self.body = self.parse_body(text)

    @staticmethod
    def parse_body(body):
        soup = BeautifulSoup(body)
        if len(body) > 0:
            return ' '.join(soup.findAll(text=True))
        else:
            return ' '

    def to_row(self):
        return [self.label, self.sender, self.receiver, self.subject, self.body]


class EmailParser:
    __path_ham = "train-ham/"
    __path_spam = "train-spam/"
    __path_testing = "testing"
    __path_basic = ""

    def __init__(self, path=""):
        self.__path_basic = path

    @staticmethod
    def __read_email(filename):
        with codecs.open(filename, encoding="utf-8", errors='ignore') as f:
            content = email.message_from_string(f.read())
        return content

    def __read_emails(self, path, label):
        emails = []
        for f in listdir(path):
            if isfile(join(path, f)):
                parsed = Email(label, self.__read_email(join(path, f)))
                emails.append(parsed.to_row())
        return emails

    def parse_emails(self):
        ham_path = self.__path_basic + "/" + self.__path_ham
        spam_path = self.__path_basic + "/" + self.__path_spam

        spams = self.__read_emails(spam_path, "SPAM")
        hams = self.__read_emails(ham_path, "HAM")

        return hams + spams
