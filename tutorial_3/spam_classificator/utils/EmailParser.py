from os import listdir
from os.path import isfile, join
import mailparser
from BeautifulSoup import BeautifulSoup


class Email:
    label = ""
    sender = ""
    receiver = ""
    subject = ""
    body = ""

    def __init__(self, label, mail):
        self.label = label
        if "From" in mail.headers:
            self.sender = mail.headers["From"]
        if "To" in mail.headers:
            self.sender = mail.headers["To"]
        if "Subject" in mail.headers:
            self.sender = mail.headers["Subject"]
        if not isinstance(mail.text_plain, type(None)):
            self.body = self.parse_body(mail.body)

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
    __path_ham = "testing/train-ham"
    __path_spam = "testing/train-spam"
    __path_testing = "testing"
    __path_basic = ""

    def __init__(self, path=""):
        self.__path_basic = path

    @staticmethod
    def __read_email(filename):
        content = mailparser.parse_from_file(filename)
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
        testing_path = self.__path_basic + "/" + self.__path_testing

        hams = self.__read_emails(ham_path, "HAM")
        spams = self.__read_emails(spam_path, "SPAM")
        testing = self.__read_emails(testing_path, "TEST")

        return hams + spams
