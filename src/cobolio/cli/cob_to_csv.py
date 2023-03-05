import argparse
import logging

from cobolio import cobdata_to_csv
from cobolio.cli import print_banner, add_version


def cob_to_csv(input_copybook, input_datafile, output_datafile, **kwargs):

    # get COBOL copybook file
    with open(input_copybook, 'r') as copybook_file:
        copybook_data = copybook_file.read()

    # Convert copybook to layout format, get record length
    rec_len, layout = cobdata_to_csv.copybook_to_layout(copybook_data)

    output_encoding = kwargs.get('out_encoding')
    with open(input_datafile, 'rb') as inputFile, open(
            output_datafile, 'w', newline='', encoding=output_encoding) as output_file:
        cobdata_to_csv.convert_cobol_data_to_csv(inputFile, rec_len, layout, output_file, **kwargs)


def cli_entry():
    cli_run(**vars(cli_parser().parse_args()))


def cli_run(**kwargs):
    print_banner('cob_to_csv', kwargs)

    if kwargs.get('debug'):
        logging.basicConfig(level=logging.DEBUG)

    if not kwargs.get('out-csv'):
        kwargs['out-csv'] = kwargs['in-data'] + '.csv'

    cob_to_csv(kwargs.get('in-copybook'), kwargs.get('in-data'), kwargs.get('out-csv'), **kwargs)


def cli_parser():
    parser = argparse.ArgumentParser(prog='cob_to_csv', description='COBOL file to csv')
    parser.add_argument('in-copybook')
    parser.add_argument('in-data')
    parser.add_argument('-o', '--out-csv')
    parser.add_argument('--in-encoding')
    parser.add_argument('--out-encoding')
    parser.add_argument('--debug', action='store_true')
    add_version(parser)

    return parser


if __name__ == '__main__':
    cli_entry()
