from aes import AESCipher
from key import Key

def main():

    testKey = Key(128)
    testKey.generate()
    AES_test = AESCipher("Hello World! This will be an encrypted message using AES 128 bit encryption. xo",testKey)
    print(testKey._key)
    '''
    test1 = [
    [0x00, 0xAB, 0x89, 0x7b],
    [0x63, 0x7c, 0x77, 0x7b],
    [0x63, 0x7c, 0x77, 0x7b],
    [0x63, 0x7c, 0x77, 0x7b]
    ]
    test = [
    [0, 1, 2, 3],
    [4, 5, 6, 7],
    [8, 9, 10, 11],
    [12, 13, 14, 15]
    ]

    sub bytes test

    AES_test._sub_bytes(test)
    print(f"after sub bytes : {test}")
    AES_test._inv_sub_bytes(test)
    print(f"after inv sub bytes: {test}")

    print("test before:")
    for line in test:
        print(line)
    AES_test._shift_rows(test)
    print("test after:")
    for line in test:
        print(line)
    '''

    test = [
    [1, 1, 1, 1],
    [2, 2, 2, 2],
    [3, 3, 3, 3],
    [4, 4, 4, 4]
    ]
    test = AES_test._mix_columns(test)

if __name__ == "__main__":
    main()
