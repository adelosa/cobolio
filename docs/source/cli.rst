==================
Command line tools
==================

The following command line tools are installed as part of the package

cob_to_csv
==========
Converts mainframe files to CSV format using a COBOL copybook

.. code-block:: text

    usage: cob_to_csv [-h] [-o OUT_CSV] [--in-encoding IN_ENCODING] [--out-encoding OUT_ENCODING]
                      [--debug] [--version] in-copybook in-data

    COBOL file to csv

    positional arguments:
      in-copybook
      in-data

    optional arguments:
      -h, --help            show this help message and exit
      -o OUT_CSV, --out-csv OUT_CSV
      --in-encoding IN_ENCODING
      --out-encoding OUT_ENCODING
      --debug
      --version             show program's version number and exit
