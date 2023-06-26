def skips(l):
    r = []
    for i in range(len(l)):
        rl = [l[j] for j in range(i, len(l), i+1)]
        r.append(rl)
    return r


def skips2(l):
    return [[l[j] for j in range(i, len(l), i + 1)] for i in range(len(l))]

def main():
    r = skips("Hello!")
    print(r)
    r2 = skips2("Hello!")
    print(r2)

if __name__ == '__main__':
    print("This is a test")
    main()
