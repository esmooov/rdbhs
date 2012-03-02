## Format layout

An rdb file begins with "REDIS0003" encoded as a bytestring. [9 bytes]

------

For each database in the dump

- The first byte is the SELECT_DB opcode (0xfe) [1 byte]
- The second byte is the length-encode database number (See below) [1-4 byte(s)]
- For each key-value pair in the database:
    - If the object has an expire time:
        - The first byte is the EXPIRETIME opcode (0xfd) or, in Redis 2.6+, EXPIRETIME_MS opcode (0xfc)
        - What follows is the expire time in seconds or, in Redis 2.6+, in ms (See below)
    - The first byte is the object type (See below for list of object type opcodes)
    - Then, the string-encoded key for the object (See below)
    - Then, the object itself, specially encoded for its type (See [below](#special-encodings))

------

An rdb file ends with the EOF opcode (0xff) [1 byte]

======

## Length-encoding

The first two most-significant bits hold the encoding type

- Less than 64 (00)
- Less than 16,384 (01)
- 32-bit integer (10)
- Special encoding (11)

The remaining bits hold the length to be encoded (marked below as X's)

- (00) | XXXXXX [1 byte total]
- (01) | XXXXXX XXXXXXXX [2 bytes total]
- (10) | 000000 XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX [5 bytes total]
- (11) | XXXXXX [1 byte total]

<a id="special-encodings">
## Special-encodings for length
</a>
