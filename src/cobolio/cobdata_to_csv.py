import codecs
import csv
import logging

import cobolio.config as cfg
from cobolio.CopybookTokenizer import CopybookTokenizer
from cobolio.CopybookParser import CopybookParser

LOGGER = logging.getLogger(__name__)


def yield_records(file, rec_size):
    rec_bytes = file.read(rec_size)
    while rec_bytes:
        yield rec_bytes
        rec_bytes = file.read(rec_size)


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


def handle_non_printable(data, mask):
    out_data = []
    for char in data:
        if char.isprintable():
            out_data.append(char)
        else:
            out_data.append(mask)
    return ''.join(out_data)


def cobol_data_to_dict(rec_data, layout, input_encoding=cfg.codepage):
    decoder = codecs.getdecoder(input_encoding)
    output = dict()
    for name, start, size, disp_size, usage, sign, scale in layout:
        field = rec_data[start:start + size]
        if usage == 'COMP-3':
            output[name] = unpack_comp3(field, scale)
        elif usage == 'COMP':
            output[name] = unpack_comp(field, disp_size, scale)
        else:
            disp = list(decoder(field))[0]
            if sign == 'SIGNED':
                disp = unpack_zd(disp, scale)
            if not disp.isprintable():
                disp = handle_non_printable(disp, '.')  # Replaces non-printable char with '.'
                LOGGER.debug('0x{}'.format(str(field.hex()).upper()))  # Sending the hex value of EBCDIC data
            output[name] = disp

    return output


def convert_cobol_data_to_csv(cobol_file, rec_length, layout, output_file, **kwargs):

    input_encoding = kwargs.get('in_encoding', cfg.codepage)
    LOGGER.debug(f'input_encoding={input_encoding}')

    field_names = [field_spec[0] for field_spec in layout]
    LOGGER.debug(f'field_list={field_names}')
    out_file = csv.DictWriter(output_file, field_names, delimiter=',', quoting=csv.QUOTE_NONNUMERIC)
    out_file.writeheader()

    for rec_data in yield_records(cobol_file, rec_length):
        out_file.writerow(cobol_data_to_dict(rec_data, layout))


def comp_field_replace(field):

    field = field.replace('.', '', 1)
    field = field.replace('-', '', 1)
    field = field.replace('+', '', 1)
    field = field.replace(',', '')
    if not field.isnumeric() and field:
        field = '0x{}'.format(str(field.hex()).upper())  # Sending the hex value of EBCDIC data
    return field


def copybook_to_layout(copybook):

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
