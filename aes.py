from key import Key
import sys
from collections import deque
from copy import copy

#sys.getsizeof("Hello World! This will be an encrypted message using AES 128 bit encryption. xo")
#TODO:
    # X Sub bytes
    # O Inv sub bytes
    # O Key scheduler
    #
class AESCipher:
    def __init__(self, plain_text: str, key: Key):
        # Define the S-box
        # this S-box was generated using ChatGPT
        self.S_BOX = [
            [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76],
            [0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0],
            [0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15],
            [0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75],
            [0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84],
            [0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf],
            [0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8],
            [0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2],
            [0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73],
            [0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb],
            [0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79],
            [0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08],
            [0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a],
            [0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e],
            [0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf],
            [0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]
        ]
        self.MIX_COL_MATRIX = [
            [0x2,0x3,0x1,0x1],
            [0x1,0x2,0x3,0x1],
            [0x1,0x1,0x2,0x3],
            [0x3,0x1,0x1,0x2]
        ]
        self._plain_text = plain_text
        self._key = key

        if self._key._keySize != sys.getsizeof(plain_text):
            raise Exception("Plain text must match the key size. Use 'sys.getsizeof(plain_text)' to ensure the text is either 128, 192, or 256 bits.")


        if self._key._keySize == 128:
            self._rounds = 10
        elif self._key._keySize == 192:
            self._rounds = 12
        elif self._key._keySize == 256:
            self.rounds = 14
        else:
            raise Exception("Error when determining rounds.")


    def encrypt(self):
        #Round 0 transformation
        #self._add_round_key()

        #Round 1-N
        #for i in range(self._rounds):
            #self._sub_bytes()
            #self._shift_rows()
            #self._mix_columns()
            #self._add_round_key()
        #return cipher_text
        pass



    def decrypt(self, Key):
        pass

    def _schedule_key(self, key):
        pass
    def _expand_key(self):
        pass
    #encryption methods

    def _add_round_key(self, round_key):
        return self._key ^ round_key

    def _sub_bytes(self, data):
        new_data = [[None, None, None, None]]*4
        # takes 4x4 byte data matrix as input
        #
        # outputs another matrix but with the substituted values from the S-box
        # As of 3/12/24 this works correctly...

        for row_index in range(len(data)):
            for byte_index in range(len(data[row_index])):
                #need upper and lower bytes for index into S-box
                upper = (data[row_index][byte_index] >> 4) & 0x0F #bit shift bitwise& 0b00001111 ***row***
                lower = data[row_index][byte_index] & 0x0F # ***column

                #print(f"row: {row_index} byte = {byte_index} {hex(data[row_index][byte_index])} swapped with {hex(self.S_BOX[upper][lower])}")
                new_data[row_index][byte_index] = self.S_BOX[upper][lower]
        return new_data
    def _shift_rows(self, data):
        #NEEDS TO RETURN A NEW DATA
        for i in range(1,len(data)):
            shift = -i

            row = deque(data[i])
            row.rotate(shift)
            data[i] = list(row)

    def _mix_columns(self, data):
        #newlist = [expression for item in iterable if condition == True]
        cols = [self._get_column(data, i) for i in range(len(data[0]))]
        mixed_cols = []

        for col in cols:
            mixed_cols.append(self._mix_column(col))

        return  self._cols_to_rows(mixed_cols)
        #[self._get_column(cols, i) for i in range(len(cols[0]))]

    def _cols_to_rows(self, columns):
        num_rows = len(columns[0])  # Number of rows is the length of each column
        num_cols = len(columns)     # Number of columns is the length of the list

        # Initialize an empty matrix with the appropriate dimensions
        rows = [[0] * num_cols for _ in range(num_rows)]

        # Populate the matrix by iterating through each column and assigning values to rows
        for i, column in enumerate(columns):
            for j, value in enumerate(column):
                rows[j][i] = value

        return rows

    def _mix_column(self, col):
        a, b, c, d = col
        print(f"a,b,c,d = {a} {b} {c} {d}")
        return [
            self._mc_dbl(a ^ b) ^ b ^ c ^ d,   # 2a + 3b + c + d
            self._mc_dbl(b ^ c) ^ c ^ d ^ a,   # 2b + 3c + d + a
            self._mc_dbl(c ^ d) ^ d ^ a ^ b,   # 2c + 3d + a + b
            self._mc_dbl(d ^ a) ^ a ^ b ^ c    # 2d + 3a + b + c
        ]
    #inverted methods for decryption

    def _inv_shift_rows(self, data):
        for i in range(1,len(data)):
            shift = i
            row = deque(data[i])
            row.rotate(shift)
            data[i] = list(row)

    def _inv_sub_bytes(self,data):
        #takes in a data block that contains values from the S-box.
        #if a certain value is in the S-box, then the new data is the index of the value within the S-box. so for example,
        # 0x63 is in the s-box -> the index of 0x63 would be 0x00, the first 0 is the upper 4 bits and the second 0 is the bottom 4 bits.
        # Required functionality:
            # - Iterate through data and swap values back.
        for row_index in range(len(data)):
            for byte_index in range(len(data[row_index])):
                new_data = None
                #find S-box value
                for s_row_index in range(len(self.S_BOX)):
                    try:
                        new_data = (s_row_index << 4 | self.S_BOX[s_row_index].index(data[row_index][byte_index])) #row and column of the value in the S-box.
                    except:
                        pass
                if new_data is not None:
                    data[row_index][byte_index] = new_data



    def _inverse_mix_columns(self, data):
        cols = [self._get_column(data, i) for i in range(len(data[0]))]
        unmixed_cols = []

        for col in cols:
            unmixed_cols.append(self._inverse_mix_column(col))

        return self._cols_to_rows(unmixed_cols)

    def _inverse_mix_column(self, col):
        a, b, c, d = col
        # Undo the operations performed in _mix_column function
        # | 14 11 13 9 | [a]
        # | 9 14 11 13 | [b]
        # | 13 9 14 11 | [c]
        # | 11 13 9 14 | [d]
        return [
            self._mult(a,14) ^ self._mult(b,11) ^ self._mult(c, 13) ^ self._mult(d, 9),
            self._mult(a, 9) ^ self._mult(b, 14) ^ self._mult(c, 11) ^ self._mult(d, 13),
            self._mult(a, 13) ^ self._mult(b, 9) ^ self._mult(c, 14) ^ self._mult(d, 11),
            self._mult(a, 11) ^ self._mult(b, 13) ^ self._mult(c, 9) ^ self._mult(d, 14),

        ]

    def _mc_dbl(self, a):

        '''
        src: https://crypto.stackexchange.com/questions/71204/how-are-these-aes-mixcolumn-multiplication-tables-calculated

        static inline uint8_t dbl(uint8_t a) {
            return (a << 1) ^ (0x11b & -(a >> 7));
        }
        '''
        #print(f"a: {a} -> dbl: {(a << 1) ^ (0x11 & -(a >> 7))}")

        return (a << 1) ^ (0x11 & -(a >> 7))

    def _mult(self, x, multiplier):
        multipliers = {9,11,13,14}

        if multiplier in multipliers:
            if multiplier == 9:
                return self._mc_dbl(self._mc_dbl(self._mc_dbl(x))) ^ x
            elif multiplier == 11:
                return self._mc_dbl(self._mc_dbl(self._mc_dbl(x)) ^ x ) ^ x
            elif multiplier == 13:
                return  self._mc_dbl(self._mc_dbl(self._mc_dbl(x) ^ x)) ^ x
            elif multiplier == 14:
                return self._mc_dbl(self._mc_dbl(self._mc_dbl(x) ^ x) ^ x)
        else:
            raise Exception("Invalid multiplier.")
        return 0

    def _get_column(self, matrix, i):
        return [row[i] for row in matrix]




sbox = [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
		0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
		0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
		0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
		0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
		0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
		0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
		0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
		0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
		0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
		0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
		0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
		0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
		0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
		0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
		0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]

Rcon = [0x00000000, 0x01000000, 0x02000000,
		0x04000000, 0x08000000, 0x10000000,
		0x20000000, 0x40000000, 0x80000000,
		0x1b000000, 0x36000000]

def keyExpansion(self, key):
	#prep w list to hold 44 tuples
	w = [()]*44

	#fill out first 4 words based on the key
	for i in range(4):
		w[i] = (self._key[4*i], self._key[4*i+1], self._key[4*i+2], self._key[4*i+3])

	#fill out the rest based on previews words, rotword, subword and rcon values
	for i in range(4, 44):
		#get required previous keywords
		temp = w[i-1]
		word = w[i-4]

		#if multiple of 4 use rot, sub, rcon etc
		if i % 4 == 0:
			x = RotWord(temp)
			y = SubWord(x)
			rcon = Rcon[int(i/4)]

			temp = hexor(y, hex(rcon)[2:])

		#creating strings of hex rather than tuple
		word = ''.join(word)
		temp = ''.join(temp)

		#xor the two hex values
		xord = hexor(word, temp)
		w[i] = (xord[:2], xord[2:4], xord[4:6], xord[6:8])

	return w

#takes two hex values and calculates hex1 xor hex2
def hexor(self, hex1, hex2):
	#convert to binary
	bin1 = hex2binary(hex1)
	bin2 = hex2binary(hex2)

	#calculate
	xord = int(bin1, 2) ^ int(bin2, 2)

	#cut prefix
	hexed = hex(xord)[2:]

	#leading 0s get cut above, if not length 8 add a leading 0
	if len(hexed) != 8:
		hexed = '0' + hexed

	return hexed

#takes a hex value and returns binary
def hex2binary(self, hex):
	return bin(int(str(hex), 16))


#takes from 1 to the end, adds on from the start to 1
def RotWord(self, word):
	return word[1:] + word[:1]


#selects correct value from sbox based on the current word
def SubWord(self,word):
	sWord = ()

	#loop throug the current word
	for i in range(4):

		#check first char, if its a letter(a-f) get corresponding decimal
		#otherwise just take the value and add 1
		if word[i][0].isdigit() == False:
			row = ord(word[i][0]) - 86
		else:
			row = int(word[i][0])+1

		#repeat above for the seoncd char
		if word[i][1].isdigit() == False:
			col = ord(word[i][1]) - 86
		else:
			col = int(word[i][1])+1

		#get the index base on row and col (16x16 grid)
		sBoxIndex = (row*16) - (17-col)

		#get the value from sbox without prefix
		piece = hex(self.S_BOX[sBoxIndex])[2:]

		#check length to ensure leading 0s are not forgotton
		if len(piece) != 2:
			piece = '0' + piece

		#form tuple
		sWord = (*sWord, piece)

	#return string
	return ''.join(sWord)
