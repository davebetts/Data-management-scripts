numbers = range(1, 39)
print numbers
f = open('myfile', 'w')
while len(numbers) > 0 :
    f.write(str(numbers[0:9]) + '\n')
    numbers = numbers[10:]
f.close()
