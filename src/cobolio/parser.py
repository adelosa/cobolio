"""
The cobolio module provides mainframe file record parsing functions.

The process to parse a file is performed using 2 functions:

* :code:`copybook_to_layout`: converts a COBOL copybook to a layout list
* :code:`loads`: parse mainframe data using a layout list returning a python dict

Layout list
-----------

    The layout list is a list of tuples. Each tuple in the list describes a field.

    The following is a description of the tuples in the layout list:

  .. list-table::
    :widths: 10 30 60
    :header-rows: 1

    * - position
      - name
      - description
    * - 1
      - data_name
      - A pythonised name of the copybook field name
    * - 2
      - offset
      - Position in record where field starts
    * - 3
      - length
      - The data length of the field
    * - 4
      - disp_length
      - The length of the field when displayed
    * - 5
      - usage
      - The type and structure of the field
         - COMP: Binary (2,4 or 8 bytes)
         - COMP-2: Floating point (8 bytes)
         - COMP-3: Zoned packed decimal
         - COMP-4: See COMP
         - COMP-5: Binary (2,4 or 8 bytes)
         - DISPLAY: Zoned decimal or char
    * - 6
      - sign
      - Indicates if the field if signed
         - SIGNED: Is signed
         - UNSIGNED: Is unsigned or scale does not apply
    * - 7
      - scale
      - Indicates number of decimal places - digits after decimal point


Parsing mainframe data
----------------------

The parsing function is modelled on the python standard json library.

The functions convert mainframe file record bytes to a python dictionary.
Each dictionary keys represent a field in the record.

Example code
------------

Read a mainframe record with a COBOL copybook layout returning a dict::

    >>> import binascii
    >>> import cobolio
    >>> copybook = '''
        01  COPY.
            05   FIELD-1             PIC X(5).
    '''
    >>> rec_len, layout = cobolio.copybook_to_layout(copybook)
    >>> rec_len
    5
    >>> layout
    [('FIELD_1', 0, 5, 5, 'DISPLAY', 'UNSIGNED', 0)]
    >>> data = binascii.unhexlify('f1f2f3f4f5')
    >>> out_dict = cobolio.loads(data, layout)
    >>> out_dict
    {'FIELD_1': '12345'}

"""

import codecs
import logging

import cobolio.config as cfg
from cobolio.CopybookTokenizer import CopybookTokenizer
from cobolio.CopybookParser import CopybookParser

LOGGER = logging.getLogger(__name__)


def copybook_to_layout(copybook: str) -> (int, list[tuple]):
    """
    Get layout list and record length for a COBOL copybook

    :param copybook: str containing COBOL copybook
    :return: record_length (int), layout list (list of tuples)

    """

    # convert copybook to dict format
    copybook_tokens = CopybookTokenizer(copybook)
    copybook_parser = CopybookParser()
    parse_dict = copybook_parser.copybook_parser(copybook_tokens)

    layout = []

    lrecl = 0
    if parse_dict[0].get('lrecl_max'):
        lrecl = parse_dict[0]['lrecl_max']

    for field_spec_key in parse_dict:
        field_spec = parse_dict[field_spec_key]
        if field_spec.get('usage'):
            usage = field_spec['usage']
        else:
            usage = 'DISPLAY'

        data_name = field_spec['data_name'].replace('-', '_')
        offset = field_spec['offset']

        if field_spec.get('storage_length'):
            length = field_spec['storage_length']
        else:
            length = 0

        if field_spec.get('disp_length'):
            disp_length = field_spec['disp_length']
        else:
            disp_length = 0

        sign = 'UNSIGNED'
        if field_spec.get('signed'):
            sign = 'SIGNED'

        if field_spec.get('scale'):
            scale = field_spec['scale']
        else:
            scale = 0

        # Add the field spec to the layout
        if length > 0:
            layout.append((data_name, offset, length, disp_length, usage, sign, scale))

    return lrecl, layout


def loads(record_bytes: bytes, layout: list, input_encoding: str = None) -> dict:
    """
    Convert mainframe record to python dict using supplied layout

    :param record_bytes: Bytes for record to be loaded
    :param layout: Layout list used to parse the data into a dictionary
    :param input_encoding: Encoding of the record data.

        All `standard python codecs
        <https://docs.python.org/3/library/codecs.html?highlight=encode#standard-encodings>`_ will be accepted
        but because of the encoding of numeric data on mainframe files, you should only use mainframe based
        encoding schemes. See `EBCDIC Code Pages
        <https://en.wikipedia.org/wiki/EBCDIC#Code_pages_with_Latin-1_character_sets>`_

        Default input encoding is **cp37**.

    :return: Dict containing record data

    """
    decoder = codecs.getdecoder(input_encoding if input_encoding else cfg.codepage)
    output = dict()
    for name, start, size, disp_size, usage, sign, scale in layout:
        field = record_bytes[start:start + size]
        if usage == 'COMP-3':
            output[name] = unpack_comp3(field, scale)
        elif usage == 'COMP':
            output[name] = unpack_comp(field, disp_size, scale)
        else:
            disp = list(decoder(field))[0]
            if sign == 'SIGNED':
                disp = unpack_zd(disp, scale)
            if not disp.isprintable():
                disp = __handle_non_printable(disp, '.')  # Replaces non-printable char with '.'
                LOGGER.debug('0x{}'.format(str(field.hex()).upper()))  # Sending the hex value of EBCDIC data
            output[name] = disp

    return output


def add_decimal_point(number, scale):
    if scale > 0:
        if isinstance(number, (int, float, complex)):
            result = int(number) / int('1'.ljust(scale + 1, '0'))
        else:
            result = '{}.{}'.format(number[:-scale], number[-scale:])
    else:
        result = number
    return result


def unpack_zd(data, scale):
    if not data:
        return ""
    last_hexbyte = data[-1].encode(cfg.codepage).hex()
    zd_sign = {'a': '+', 'b': '-', 'c': '+', 'd': '-', 'e': '+', 'f': '+'}
    sign = zd_sign.get(last_hexbyte[0].lower(), '+')
    unpacked_val = '{}{}{}'.format(sign, data[:-1], last_hexbyte[1])
    return add_decimal_point(unpacked_val, scale)


def unpack_comp(data, disp_size, scale):
    if data:
        comp_dec = int.from_bytes(data, byteorder='big', signed=True)
    else:
        comp_dec = 0
    comp_dec = f'{comp_dec:+0{disp_size}.0f}'
    # return comp_field_replace(add_decimal_point(comp_dec, scale))
    return add_decimal_point(comp_dec, scale)


def unpack_comp3(data, scale):
    if not data:
        return ""

    hexbytes = data.hex()
    if hexbytes[-1].lower() in ('b', 'd', 'B', 'D'):
        unpacked = "-{}".format(hexbytes[:-1])
    else:
        unpacked = "+{}".format(hexbytes[:-1])
    return add_decimal_point(unpacked, scale)
    # return comp_field_replace(add_decimal_point(unpacked, scale))


def __handle_non_printable(data, mask):
    out_data = []
    for char in data:
        if char.isprintable():
            out_data.append(char)
        else:
            out_data.append(mask)
    return ''.join(out_data)


def __comp_field_replace(field):

    field = field.replace('.', '', 1)
    field = field.replace('-', '', 1)
    field = field.replace('+', '', 1)
    field = field.replace(',', '')
    if not field.isnumeric() and field:
        field = '0x{}'.format(str(field.hex()).upper())  # Sending the hex value of EBCDIC data
    return field
