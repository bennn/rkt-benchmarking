"""
    2013-07-21:
        Find the number d<1000 for which
        1 / d has the longest recurring cycle in its decimal.
"""
def reciprocol(n):
    """ 2013-07-21: Generator for reciprocol of 1/n """
    curr = 0
    m = 10
    remainders = set([m])
    still_dividing = True
    while m:
        if m < n:
            yield curr
            curr = 0
            m = m * 10
            if m in remainders:
                m = 0
                yield -1
            else:
                remainders.add(m)
        else:
            curr += 1
            m -= n
            if m == 0: yield curr

def longest_reciprocol(d):
    """
    "   2013-07-29:
    "       Find the number 1/d with the longest reciprocol
    """
    longest_num = -1
    cycle_length = -1
    for n in xrange(2, d+1):
        length = 0
        for _ in reciprocol(n):
            length += 1
        if length > cycle_length:
            longest_num = n
            cycle_length = length
    return longest_num

# Everything below here is scratch work. Went towards a poor algorithm
# that found the solution by force

import re

def decimal_part(n):
    """
    "   2013-07-21:
    "       Get the decimal part of a number and express it as an integer.
    "       TODO there must be a cleaner way...
    """
    return int(str(n).split(".", 1)[1])

def find_cycle(s, limit=9000):
    """
        2013-07-21:
            Find a cycle inside the iterable `s`.
            Break after `limit` steps
    """
    ns = []
    for _ in xrange(0, limit):
        ns.append(str(next(s)))
    s = "".join(ns)
    # Ok, now we have a big string
    # for n in xrange(limit/3, 1, -1):
    for n in xrange(4, limit/3):
        subs = s[-n:]
        match = re.search(r"(%s){3}$" % subs, s)
        if match:
            return s, subs
    return "", "" # Controversial!

def print_decimals(n):
    """
    "   2013-07-21:
    "       [print_decimals n] prints the value of all decimal numbers 1/1 -> 1/n
    """
    with open("/home/ninian/code/euler/euler/26/26.tab", "w") as f:
        for i in xrange(1, n+1):
            print>>f, "\t".join([
                str(i),
                str(1./i),
                ])

def one_cycle(n):
    return (n[-2] == n[-3] == n[-4] == n[-5] == n[-6])

def two_cycle(n):
    return (n[-3:-1] == n[-5:-3] == n[-7:-5])

def three_cycle(n):
    return (n[-4:-1] == n[-7:-4] == n[-10:-7])

def print_filtered(n):
    """
    "   2013-07-21:
    "       [print_filtered n] prints some reciprocols,
    "       just the ones that passed a few simple filters
    """
    with open("/home/ninian/code/euler/euler/26/26.tab", "w") as f:
        for i in xrange(1, n+1):
            r = str(decimal_part(1. / i))
            if len(r) > 10 \
              and not one_cycle(r) \
              and not two_cycle(r) \
              and not three_cycle(r):
                print>>f, "\t".join([
                    str(i),
                    r,
                    ])

def filter_filtered(n):
    """
    " 2013-07-21: Get more digits from the reciprocols that
    "     passed the filter.
    """
    with open("26.tab", "r") as f:
        with open("26-2.tab", "w") as g:
            for line in f:
                n = int(line.split("\t", 1)[0])
                r = reciprocol(n)
                s, c = find_cycle(r)
                if len(c) > 5:
                    print>>g, "\t".join([str(n), c, s])

def getlongest(fname):
    with open(fname, "r") as f:
        longest = ""
        for line in f:
            num = line.split("\t")[1]
            if len(num) > len(longest):
                longest = num
    return longest

# 983
