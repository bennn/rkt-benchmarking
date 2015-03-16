chain_lengths = {}

def collatz_length(n):
    l = 1
    while n != 1:
        l += 1
        if n % 2 == 0:
            n = n / 2
        else:
            n = (n + n + n) + 1
    return l

def memo_collatz_length(n):
    if n in chain_lengths:
        return chain_lengths[n]
    else:
        r = collatz_length(n)
        chain_lengths[n] = r
        return r

def longest_collatz(limit):
    start = 1
    step = 1
    i = start
    c = 0
    longest_length = 0
    while i < limit:
        l = memo_collatz_length(i)
        if l > longest_length:
            c = i
            longest_length = l
        i += step
    return c
