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
    - Then, the object itself, specially encoded for its type (See below)

------

An rdb file ends with the EOF opcode (0xff) [1 byte]

======

## Length-encoding

The first two most-significant bits hold the encoding type

- Less than 64 (00)
- Less than 16,384 (01)
- 32-bit integer (10)
- Special encoding (11)

The remaining bits hold the length (or, in the last case, encoding type) to be encoded (marked below as X's)

- (00) | XXXXXX [1 byte total]
- (01) | XXXXXX XXXXXXXX [2 bytes total]
- (10) | 000000 XXXXXXXX XXXXXXXX XXXXXXXX XXXXXXXX [5 bytes total]
- (11) | XXXXXX [1 byte total]



## String objects

The first 1-5 bytes of a string object contains the length of the string object and whether or not it is an encoded string, as described in the length-encoding section above.

If the string is an encoded type with either a 8, 16, or 32-bit encoding type, the string is loaded as an integer, as described above in the integer-loading section, and converted to a string.

If the string is encoded with both bits set, it is loaded as an lzf compressed string, as described below.

If the string is not an encoded type, it is loaded as a bytestring consisting of the next [length] bytes.

## Special-encodings for length

If a length-encoded byte has its most-significant two bits set, the remaining 6 bytes will contain either a 0,1,2 or 3.

- A zero means the value following the length is encoded as an 8-bit integer.
- A one means the value following the length is encoded as an 16-bit integer.
- A two means the value following the length is encoded as an 32-bit integer.
- A three means the value following the length is encoded as an lzf compressed string.

## Ziplist

(Note: I was unable to find whether the endiannesses listed below as little-endian encodings were always litle-endian or whether they are stored as host byte order.)

Ziplists are space-efficient special encodings for lists and sorted sets. The max number of members and max size for the ziplist encoding is set in the conf file that the Redis server reads when starting.

The first 4 bytes store the number of bytes in the ziplist

The next 4 bytes store the offset (in bytes) to the end of the last entry in the list

The next 2 bytes store the number of members of the ziplist

The remaining n-1 bytes of the ziplist store a sequence of members. If the ziplist is encoding a sorted set, the members should be parsed as value, score pairs.

The structure of every zip list members is as follows:

- First, is the number of bytes of the previous member in the ziplist. If this byte is less than 0xfe, the length is stored as a single byte. If the first byte is set to 0xfe, the next four bytes will hold the length of the previous member.

- Next, is the encoding and length of the current member of the ziplist. This is similar, though not exactly the same, to the way the lengths and encodings of RDB objects are stored.
  - If the first two most-significant bits are set to zero (00), the ziplist member is encoded as an n-bytes long string where n is the value of the remaining 6 bits. (00|XXXXXX)
  - If the first two most-significant bits are set to one (01), the ziplist member is encoded as an n-bytes long string where n is the conjunction of remaining 6 bits shifted left 8 bytes and the next byte (01|XXXXXX XXXXXXXX)
  - If the first two most-significant bits are set to two (10), the ziplist member is encoded as an n-bytes long string where n is the conjunction of remaining 6 bits shifted left 16 bytes and the next two bytes (01|XXXXXX XXXXXXXX XXXXXXXX).
  - If the first two most-significant bits are set to three and the next two bits are set to zero (1100), the ziplist member is encoded as an int16t (2 bytes).
  - If the first two most-significant bits are set to three and the next two bits are set to one (1101), the ziplist member is encoded as an int32t (4 bytes).
  - If the first two most-significant bits are set to three and the next two bits are set to two (1110), the ziplist member is encoded as an int64t (8 bytes).

- Finally, comes the value specified by the encoding and length

Every ziplist terminates with a 0xff byte

[13 00 00 00] -> Little-endian 32-bit length (in bytes) of ziplist

[0e 00 00 00] -> little-endian 32-bit offset (in bytes) to the end of the last entry in the list

[02 00]       -> little-endian 16-bit number of list entries

[00]          -> number of bytes of previous entry

[c0]          -> value encoding

[01 00]       -> 16-bit value

[04]          -> number of bytes of previous entry

[c0]          -> value encoding

[01 00]       -> 16-bit value

[ff]          -> end of ziplist

## Intset

[02 00 00 00]  -> Little-endian 32-bit length encoding, in bytes, of members of the intset

[01 00 00 00]  -> Little-endian 32-bit number of elements in the intset

[01 00]        -> One little-endian member, two-bytes long, as specified in the encoding above
