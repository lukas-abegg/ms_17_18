from experiments.TfidfLsaExperiment import TfidfLsaExperiment
from sklearn.neighbors import KNeighborsClassifier

exp1 = TfidfLsaExperiment()
exp1.set_k_of_kfold(5)
exp1.set_classifier(KNeighborsClassifier(n_neighbors=13))
exp1.run(True, "/home/lukas/git-projects/ms_2017_18/tutorial_3/email-korpus/testing")
