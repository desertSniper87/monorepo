import unittest

from lillys_homework import *

class HomeWorkTest(unittest.TestCase):
    def testMain(self):
        self.assertEqual(lilyshomework(), secret)

def main():
        unittest.main()

if __name__ == "__main__":
    main()
