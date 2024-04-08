import secrets

class Key:
    def __init__(self, keySize):

        if keySize == 128:
            self._keySize = keySize
        elif keySize == 192:
            self._keySize = keySize
        elif keySize == 256:
            self._keySize = keySize
        else:
            raise Exception("Key size invalid.")

    def generate(self):
        #generate hex key based on size (converts to bits since its hex)
        self._key = secrets.token_hex(self._keySize // 4)
        self._byte_arr_key = [self._key[i:i+2] for i in range(0, len(self._key), 2)]
