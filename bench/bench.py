i = 0
max = 1000000


def blackbox(x):
    if x < 0:
        exit(1)


while i < max:
    i += 1
    blackbox(i)

print(i)
