import os
import tempfile
import unittest

from cobolio.cli import cob_to_csv

script_path = os.path.realpath(__file__)
script_dir = os.path.dirname(script_path)
project_dir = os.path.split(script_dir)[0]


def print_stream(stream, description):
    stream.seek(0)
    data = stream.read()
    print('***' + description + '***')
    print(data)
    stream.seek(0)


class TestCobToCsvCli(unittest.TestCase):

    def test_run_files_original(self):
        # Convert TEST.EMP.RECORD.FILE.dat to csv using copybook EMPREC01.cpy
        input_copybook = os.path.join(project_dir, 'data', 'EMPREC01.cpy')
        input_datafile = os.path.join(project_dir, 'data', 'TEST.EMP.RECORD.FILE.dat')
        with tempfile.NamedTemporaryFile(mode='w') as output_datafile:
            print(output_datafile.name)
            cob_to_csv.cob_to_csv(input_copybook, input_datafile, output_datafile.name)
            with open(output_datafile.name, 'r') as reader:
                print_stream(reader, 'output csv')

    def test_run_files_output_latin1(self):
        # Convert TEST.EMP.RECORD.FILE.dat to csv using copybook EMPREC01.cpy
        input_copybook = os.path.join(project_dir, 'data', 'EMPREC01.cpy')
        input_datafile = os.path.join(project_dir, 'data', 'TEST.EMP.RECORD.FILE.dat')
        with tempfile.NamedTemporaryFile(mode='w') as output_datafile:
            print(output_datafile.name)
            cob_to_csv.cob_to_csv(input_copybook, input_datafile, output_datafile.name, out_encoding='latin1')
            with open(output_datafile.name, 'r') as reader:
                print_stream(reader, 'output csv')

    def test_run_files_input_cp500(self):
        # Convert TEST.EMP.RECORD.FILE.dat to csv using copybook EMPREC01.cpy
        input_copybook = os.path.join(project_dir, 'data', 'EMPREC01.cpy')
        input_datafile = os.path.join(project_dir, 'data', 'TEST.EMP.RECORD.FILE.dat')
        with tempfile.NamedTemporaryFile(mode='w') as output_datafile:
            print(output_datafile.name)
            cob_to_csv.cob_to_csv(input_copybook, input_datafile, output_datafile.name, in_encoding='cp500')
            with open(output_datafile.name, 'r') as reader:
                print_stream(reader, 'output csv')
