import unittest
import decimal

from cobolio import parser


class TestParser(unittest.TestCase):
    def test_cobol_data_to_dict_pic_x(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC X(6).
            05   FILLER REDEFINES MY-FIELD-1.
                 10  MY-FIELD-1-1       PIC X(3).
                 10  MY-FIELD-1-2       PIC X(3).
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        rec_data = b'123456'
        rec_dict = parser.loads(rec_data, layout, input_encoding='utf8')
        print(rec_dict)
        self.assertEqual('123', rec_dict['MY_FIELD_1_1'])
        self.assertEqual('123', rec_dict['MY_FIELD_1_1'])
        self.assertEqual('456', rec_dict['MY_FIELD_1_2'])

    def test_cobol_data_to_dict_pic_9_display(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC 9(6).
            05   MY-FIELD-2             PIC S9(6).
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'123456123456'
        rec_dict = parser.loads(rec_data, layout, input_encoding='utf8')
        print(rec_dict)
        self.assertEqual(rec_dict['MY_FIELD_1'], '123456')  # unsigned
        self.assertEqual(rec_dict['MY_FIELD_2'], '+123456')  # signed

        # try ebcdic encoding - does not support signed neg in ascii
        rec_data = b'\xf1\xf2\xf3\xf4\xf5\xf6\xf1\xf2\xf3\xf4\xf5\xd6'  # negative value
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(rec_dict['MY_FIELD_1'], '123456')  # unsigned
        self.assertEqual(rec_dict['MY_FIELD_2'], '-123456')  # signed

    def test_cobol_data_to_dict_pic_9_comp_unsigned(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC 9(9) COMP.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'\x00\x00\x00\xff'  # ff = 255
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(255, int(rec_dict['MY_FIELD_1']))  # unsigned

    def test_cobol_data_to_dict_pic_9_comp_signed(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC S9(9) COMP.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'\x00\x00\x00\x01'
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(1, int(rec_dict['MY_FIELD_1']))  # signed positive

        rec_data = b'\xff\xff\xff\xff'  # ff = -1
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(-1, int(rec_dict['MY_FIELD_1']))  # signed positive

    def test_cobol_data_to_dict_pic_9_comp3_unsigned(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC 9(7) COMP-3.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'\x10\x11\x23\x1F'  # 1011231
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(1011231, int(rec_dict['MY_FIELD_1']))

    def test_cobol_data_to_dict_pic_9_comp3_signed(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC S9(7) COMP-3.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'\x10\x11\x23\x1F'  # 1011231
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(1011231, int(rec_dict['MY_FIELD_1']))

        rec_data = b'\x10\x11\x23\x1D'  # -1011231
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(-1011231, int(rec_dict['MY_FIELD_1']))

    def test_cobol_data_to_dict_pic_9_comp3_decimal_unsigned(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC 9(7)V99 COMP-3.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'\x10\x11\x23\x19\x9F'  # 1011231.99
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(decimal.Decimal('+1011231.99'), decimal.Decimal(rec_dict['MY_FIELD_1']))

    def test_cobol_data_to_dict_pic_9_comp3_decimal_signed(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC S9(7)V99 COMP-3.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
        rec_data = b'\x10\x11\x23\x19\x9D'  # -1011231.99
        rec_dict = parser.loads(rec_data, layout)
        print(rec_dict)
        self.assertEqual(decimal.Decimal('-1011231.99'), decimal.Decimal(rec_dict['MY_FIELD_1']))

    def test_cobol_data_to_dict_occurs(self):
        copybook = """
        01  MY-COPY.
            05   FIELD-1  OCCURS 5 TIMES.
                 10  FIELD-1-1          PIC X(3).
                 10  FIELD-1-2          PIC X(3).
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        rec_data = b'AAA111BBB222CCC333DDD444EEE555'
        print(f'layout={layout}')
        rec_dict = parser.loads(rec_data, layout, input_encoding='utf8')
        print(f'rec_dict={rec_dict}')
        self.assertEqual('AAA', rec_dict.get('FIELD_1_1_1'))
        self.assertEqual('111', rec_dict.get('FIELD_1_2_1'))
        self.assertEqual('EEE', rec_dict.get('FIELD_1_1_5'))
        self.assertEqual('555', rec_dict.get('FIELD_1_2_5'))

    @unittest.expectedFailure
    def test_cobol_data_to_dict_occurs_depending(self):
        """
        This test is broken - OCCURS DEPENDING ON does not seem to work
        """
        copybook = """
        01  MY-COPYBOOK.
            05 FIELD-VALUES           PIC 9(5) COMP-3.
            05 FIELD-1 OCCURS 10 TIMES DEPENDING ON FIELD-VALUES.
               10  FIELD-1-1          PIC X(3).
               10  FIELD-1-2          PIC X(3).
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        rec_data = b'\x00\x00\x5FAAA111BBB222CCC333DDD444EEE555'
        rec_dict = parser.loads(rec_data, layout, input_encoding='utf8')
        print(f'rec_dict={rec_dict}')
        self.assertEqual('AAA', rec_dict.get('FIELD_1_1_1'))
        self.assertEqual('111', rec_dict.get('FIELD_1_2_1'))
        self.assertEqual('EEE', rec_dict.get('FIELD_1_1_5'))
        self.assertEqual('555', rec_dict.get('FIELD_1_2_5'))


class TestFunctions(unittest.TestCase):
    def test_add_decimal_point(self):
        self.assertEqual(1, parser.add_decimal_point(1, 0))
        self.assertEqual(0.1, parser.add_decimal_point(1, 1))
        self.assertEqual('1', parser.add_decimal_point('1', 0))
        self.assertEqual('0.1', parser.add_decimal_point('01', 1))

    def test_unpack_zd(self):
        self.assertEqual('', parser.unpack_zd('', 0), 'No value passed')

    def test_uppack_comp(self):
        self.assertEqual('+0', parser.unpack_comp('', 2, 0), 'No value passed')

    def test_uppack_comp3(self):
        self.assertEqual('', parser.unpack_comp3('', 0), 'No value passed')

    def test_parser_pic_9_comp3_unsigned_decimal(self):
        copybook = """
        01  MY-COPY.
            05   MY-FIELD-1             PIC 9(9)V99 COMP-3.
        """
        rec_len, layout = parser.copybook_to_layout(copybook)
        print(rec_len, layout)
