#!/usr/bin/env python3
#
# This is a simple script to encrypt a message using AES
# with CBC mode in Python 3.
# Before running it, you must install pycryptodome:
#
# $ python -m pip install PyCryptodome
#
# Author.: José Lopes
# Date...: 2019-06-14
# License: MIT
##


from hashlib import md5
from base64 import b64decode
from base64 import b64encode

from Crypto.Cipher import AES
from Crypto.Random import get_random_bytes
from Crypto.Util.Padding import pad, unpad


class AESCipher:
    def __init__(self, key):
        self.key = md5(key.encode('utf8')).digest()

    def encrypt(self, data, iv=None):
        if iv is None:
            iv = get_random_bytes(AES.block_size)
        self.cipher = AES.new(self.key, AES.MODE_CBC, iv)
        return b64encode(iv + self.cipher.encrypt(pad(data.encode('utf-8'), 
            AES.block_size)))

    def decrypt(self, data):
        raw = b64decode(data)
        self.cipher = AES.new(self.key, AES.MODE_CBC, raw[:AES.block_size])
        return self.cipher.decrypt(raw[AES.block_size:]), AES.block_size


if __name__ == '__main__':
    # print('TESTING ENCRYPTION')
    # msg = input('Message...: ')
    # pwd = input('Password..: ')
    # print('Ciphertext:', AESCipher(pwd).encrypt(msg).decode('utf-8'))

    # print('\nTESTING DECRYPTION')
    # cte = input('Ciphertext: ')
    # pwd = input('Password..: ')
    # print('Message...:', AESCipher(pwd).decrypt(cte).decode('utf-8'))
    msg = "Pay Bob 100$"
    # for i in range(10000):
        # print (AESCipher().encrypt(data=msg, iv=i))
    c1 = "20814804c1767293b99f1d9cab3bc3e7"
    c2 = "ac1e37bfb15599e5f40eef805488281d"

    output = "IIFIBMF2cpO5nx2cqzvD56weN7+xVZnl9A7vgFSIKB0="
    

    # print('Message...:', AESCipher(c1).decrypt(c2)[0].decode('utf-8'))
    pwd = "ABC"
    while(True):
        iv = get_random_bytes(AES.block_size)
        print('Message...:', AESCipher(iv).decrypt(output).decode('utf-8'))
