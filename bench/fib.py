c = [0,1]
i = 0
max = 100

while i < max:
    x = c[0] + c[1]
    c[0] = c[1]
    c[1] = x

    i += 1

print (c[0])
