# -*- coding: utf-8 -*-
"""
Created on Sat Dec 19 09:16:16 2015

@author: Sang
"""

import nltk
from nltk.book import *

#text preparation
text="The new international standard details the requirements, capabilities and uses of cloud-based big data, with an eye toward ensuring that its benefits can be achieved on a global scale. It also outlines how cloud computing systems can be leveraged to provide big-data services."

#text tokenization
text_tokens=nltk.word_tokenize(text)

#create nltk text
nltk_text = nltk.Text(text_tokens)

#text normalization
words = [w.lower() for w in nltk_text]

print(words)
print(nltk.pos_tag(words))

""" results:
[('the', 'DT'), ('new', 'JJ'), ('international', 'JJ'), ('standard', 'JJ'), ('details', 'NNS'), ('the', 'DT'), ('requirements', 'NNS'), (',', ','), ('capabilities', 'NNS'), ('and', 'CC'), ('uses', 'NNS'), ('of', 'IN'), ('cloud-based', 'JJ'), ('big', 'JJ'), ('data', 'NNS'), (',', ','), ('with', 'IN'), ('an', 'DT'), ('eye', 'NN'), ('toward', 'IN'), ('ensuring', 'VBG'), ('that', 'IN'), ('its', 'PRP$'), ('benefits', 'NNS'), ('can', 'MD'), ('be', 'VB'), ('achieved', 'VBN'), ('on', 'IN'), ('a', 'DT'), ('global', 'JJ'), ('scale', 'NN'), ('.', '.'), ('it', 'PRP'), ('also', 'RB'), ('outlines', 'VBZ'), ('how', 'WRB'), ('cloud', 'JJ'), ('computing', 'NN'), ('systems', 'NNS'), ('can', 'MD'), ('be', 'VB'), ('leveraged', 'VBN'), ('to', 'TO'), ('provide', 'VB'), ('big-data', 'JJ'), ('services', 'NNS'), ('.', '.')]
"""
"""
nltk.help.upenn_tagset('RB')
RB: adverb
    occasionally unabatingly maddeningly adventurously professedly
    stirringly prominently technologically magisterially predominately
    swiftly fiscally pitilessly ...
"""

"""
searching text:
A concordance view shows us every occurrence of a given word, together with some
context.
"""
nltk_text.concordance("cloud")

#counting words / tokens
len(nltk_text)

#counting # of unique words
len(set(nltk_text))

#counting # of a specific word
nltk_text.count('data')

#Frequency distribution
fdist1 = FreqDist(nltk_text)
#couting # of a specific word
fdist1['data']  # 1

# draw a plot of the distribution
fdist1.plot(50, cumulative=False)

"""
Other examples

fdist = FreqDist(samples) Create a frequency distribution containing the given samples
fdist.inc(sample) Increment the count for this sample
fdist['monstrous'] Count of the number of times a given sample occurred
fdist.freq('monstrous') Frequency of a given sample
fdist.N() Total number of samples
fdist.keys() The samples sorted in order of decreasing frequency
for sample in fdist: Iterate over the samples, in order of decreasing frequency
fdist.max() Sample with the greatest count
fdist.tabulate() Tabulate the frequency distribution
fdist.plot() Graphical plot of the frequency distribution
fdist.plot(cumulative=True) Cumulative plot of the frequency distribution
fdist1 < fdist2 Test if samples in fdist1 occur less frequently than in fdist2
"""

#lemmatization
wnl = nltk.WordNetLemmatizer()
lem_results = [wnl.lemmatize(t) for t in nltk_text]
