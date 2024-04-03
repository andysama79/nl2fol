import os
import sys
# sys.path.append('parser/src')
import json

import argparse
from parser.constructGrammar import text2json, text2tree

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", help="path to file", required=False)
    parser.add_argument("-t", "--text", help="text to parse", required=False)
    parser.add_argument("-j", "--json", help="store json output to file", required=False, type=bool, default=False)

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

    if args.json:
        with open(dumpfile, 'w') as f:
            json.dump(grammars, f, indent=4)
        print(f'Output grammar saved to {dumpfile}')
    
    

if __name__ == "__main__":
    main()