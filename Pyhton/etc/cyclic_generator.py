prime = 13

for perspective_generator in range(prime):
    k = [pow(perspective_generator, k) % 13 for k in range(prime)]
    print(f'{perspective_generator} -- {k}')
    # for j in range(prime):

