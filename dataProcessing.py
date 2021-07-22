import csv 
from collections import defaultdict

def ctree():
    """ One of the python gems. Making possible to have dynamic tree structure.

    """
    return defaultdict(ctree)


def build_leaf(name, leaf):
    """ Recursive function to build desired custom tree structure

    """
    res = {"name": name}

    # add children node if the leaf actually has any children
    if len(leaf.keys()) > 0:
        res["children"] = [build_leaf(k, v) for k, v in leaf.items()]

    if len(leaf.keys()) == 0:
        res["value"] = 1

    return res


def main():
    """ The main thread composed from two parts.

    First it's parsing the csv file and builds a tree hierarchy from it.
    Second it's recursively iterating over the tree and building custom
    json-like structure (via dict).

    And the last part is just printing the result.

    """
    tree = ctree()
    # NOTE: you need to have test.csv file as neighbor to this file
    # with open('/Users/nancyorgan/Documents/Coffee/flavors.csv') as csvfile:
    with open('/Users/nancyorgan/Documents/Coffee/customNotes.csv') as csvfile:   
        reader = csv.reader(csvfile)
        for rid, row in enumerate(reader):

            # skipping first header row. remove this logic if your csv is
            # headerless
            # if rid == 0:
            #     continue

            # usage of python magic to construct dynamic tree structure and
            # basically grouping csv values under their parents
            leaf = tree[row[0]]

            if row[3] == "NA":
                row = row[:-1]

            # print(type(row[3]))

            # print(row[3] == "NA")
            # print(row[:-1])

            print(row)
            # print(row[3:4])
            # print(row.pop)
            # print(row[0:3])
            # print(row[3:4] == 'NA')


            for cid in range(1, len(row)):
                
    

                leaf = leaf[row[cid]]
            # if leaf[row[3]] == "NA":
            #     for cid in range(1, 3):
            #         leaf = leaf[row[cid]]

    # building a custom tree structure
    res = []
    for name, leaf in tree.items():
        res.append(build_leaf(name, leaf))

    # printing results into the terminal
    import json
    with open('/Users/nancyorgan/Documents/personalsite/data/ProcessingTest.json', 'w') as f:
        json.dump(res, f)
    # print(json.dumps(res))


# so let's roll
main()