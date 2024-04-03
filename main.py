import os
import sys
# sys.path.append('parser/src')
import json
import shutil
import subprocess as sbp

import argparse
from parser.constructGrammar import text2json, text2tree

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", help="path to file", required=True)
    parser.add_argument("-j", "--json", help="store json output to file", required=False, type=bool, default=False)

    args = parser.parse_args()

    path2file = args.file

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
    
    outfile = 'output.txt' # logic

    with open(outfile, 'w') as f:
        f.write('') # clear file

    if os.path.exists('temp'):
        shutil.rmtree('temp')
    os.makedirs('temp')

    previous = ''

    for i, grammar in enumerate(grammars):
        path = 'temp/temp.json'
        with open(path, 'w') as f:
            json.dump(grammar, f, indent=4)
        
        # sbp.run(['./run.sh', path])
        sbp.call(['./run.sh', path])

        path = 'temp/_temp.json'
        with open(path, 'r') as f:
            data = json.load(f)
            # print(data)

        with open(outfile, 'a') as f:
            if previous == data['logic']:
                f.write('Cannot Parse\n')
            else:
                f.write(' '.join(data['logic']) + '\n')
        
        previous = data['logic']

    shutil.rmtree('temp')

    print(f'Output logic saved to {outfile}')
        
if __name__ == "__main__":
    main()