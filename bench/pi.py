i = 0
max = 1000000
n = 0

while i < max:
    n += 1.0/(2*i+1) - 1.0/(2*i+3)
    i += 2

print(n*4)
