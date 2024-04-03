import stanza
import nltk
from stanza.pipeline.core import DownloadMethod
from stanza.models.constituency.parse_tree import Tree as ParseTree

import json
from json import dumps
import argparse
import sys
import os

# set language to english

LANG = 'en'

model = stanza.Pipeline(
    LANG,
    processors='tokenize, pos, constituency',
    download_method=DownloadMethod.REUSE_RESOURCES
)

def text2tree(text):
    corpus = model(text)
    return [remove_punkt(sentence.constituency) for sentence in corpus.sentences]

def remove_punkt(tree):
    return ParseTree(tree.label, [remove_punkt(child) for child in tree.children if child.label != '.' and child.label != ','])

def tree2dict(tree):
    return {
        'node': tree.label,
        'children': [tree2dict(child) for child in tree.children]
    }

def text2json(text):
    trees = text2tree(text)
    # print(trees)
    # print(nltk.Tree.fromstring(str(trees[0])))
    # sys.exit(-1)
    return {
            'input': text, # text
            'output': [tree2dict(tree) for tree in trees] # grammar tree
        }

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", help="path to file", required=False)
    parser.add_argument("-t", "--text", help="text to parse", required=False)

    args = parser.parse_args()

    path2file = args.file
    text = args.text

    if not text and not path2file:
        print('Missing arguments! Either provide text or path to file!')
        sys.exit(-1)

    if text:
        print(text2tree(text))
        sys.exit(0)

    if not path2file:
        print('Missing argument -f!')
        sys.exit(-1)

    if not os.path.exists(path2file):
        print('File does not exist!')
        sys.exit(-1)

    dumpfile = path2file.split(".")[0] + "_grammar.json"

    with open(path2file, 'r') as f:
        data = f.readlines()

    grammars = []

    for text in data:
        grammars.append(text2json(text.replace('\n', '')))

    with open(dumpfile, 'w') as f:
        json.dump(grammars, f, indent=2)

if __name__ == "__main__":
    main()
    

