import binq

TEST_ADDR = 0x1000
TEST_CODE = b"\x48\xc7\xc0\x64\x00\x00\x00\x04\xc8\x48\x89\xc3\xc3"

db = binq.Database()
db.add_func(TEST_ADDR, TEST_CODE)
db.analyze()
db.test_search()
