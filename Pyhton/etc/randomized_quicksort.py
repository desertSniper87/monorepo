from random import randrange

def create_array(size_of_array: int) -> []:
    array = [randrange(1, 50) for i in range(size_of_array)]
    return array
        
array = create_array(10)

def quick_sort(array):
    size_of_array = len(array) - 1
    pivot_index = randrange(0, size_of_array)
    pivot = array[pivot_index]

    for index, i in enumerate(array):
        print(index, i)

quick_sort(array)

