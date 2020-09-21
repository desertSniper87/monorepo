def encrypt(key, value):
    t = str(key)[0]
    result = ""
    for i in range(1, len(str(value))+1):
        print(f"i --> {i}")
        if str(value)[i-1] == '1':
            t = int(bool(t) != bool(str(key)[i]))
        print(f't --> {t}')
        result += str(t)

    return result


def test(key, message, expected_result):
    val = "{0:05b}".format(i)
    res = encrypt(val, message)
    print(f'{val} --> {res}')
    if res == expected_result:
        print(f'{val} --> {res}')
        print("found")


for i in range(32):    
    # test(i, "0110", "0011")
    # test(i, "0101", "1010")
    test(i, "1110", "0110")
