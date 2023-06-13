.. include:: <isonum.txt>
=============
Developer API
=============

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

Quickstart
==========

Getting layout from COBOL copybook
----------------------------------

For any processing, you will need a layout list.
A layout list can be generated from a COBOL copybook.

.. code-block:: python

    # get COBOL copybook file into variable
    with open(input_copybook, 'r') as copybook_file:
        copybook_data = copybook_file.read()

    # Convert copybook to layout format, record length
    rec_length, layout = cobolio.copybook_to_layout(copybook_data)


Extract mainframe file to CSV
-----------------------------
The following example assumes a fixed record length
mainframe file where all records use the same structure.

.. code-block:: text
   :caption: Example file data

    AAAAA11111
    BBBBB22222

.. code-block:: cobol
   :caption: COBOL copybook

   000000 01 COPYBOOK.
   000010   05  CB-FIELD-1           PIC X(5).
   000020   05  CB-FIELD-2           PIC X(5).

.. code-block:: python
   :caption: Python program

   """
   read cb
   get layout list, reclen using copybook_to_layout
   open binary input
   open file output, wrap DictWriter
   while input.read reclen bytes
   parse using loads funct
   write using DictWriter
   """
   def some_func():
       print('hello')

Processing multi-record files
-----------------------------
This file structure is where a single logical
record can be made up of one or more records
in the input file.

For example a customer file where each customer is provided
as 3 records in the file - a name record (N), address record (A)
and contact record (C).

.. code-block:: text
   :caption: Example customer file data

    000001 N MR   JAMES   CAMERON
    000001 A 100 MAIN ST GOODSVILLE
    000001 C jcameron@aliens.com
    000002 N MR   BENNY   HILLS
    000002....

.. code-block:: cobol
   :caption: Copybook layouts for 3 record formats

   000000 01 NAME-RECORD.
   000010    05 CUST-NUMBER       PIC 9(6).
   000020    05 FILLER            PIC X.
   000030    05 REC-TYPE          PIC X.
   000040    05 FILLER            PIC X.
   000050    05 CUST-NAME         PIC X(25).

   000000 01 ADDRESS-RECORD.
   000010    05 CUST-NUMBER       PIC 9(6).
   000020    05 FILLER            PIC X.
   000030    05 REC-TYPE          PIC X.
   000040    05 FILLER            PIC X.
   000050    05 CUST-ADDR         PIC X(25).

   000000 01 CONTACT-RECORD.
   000010    05 CUST-NUMBER       PIC 9(6).
   000020    05 FILLER            PIC X.
   000030    05 REC-TYPE          PIC X.
   000040    05 FILLER            PIC X.
   000050    05 CUST-CONTACT      PIC X(25).

Variable length records
-----------------------
Variable length records have a Record Descriptor Word (RDW)
at the start of each record that provides the length of
any given record.

Variable content records
------------------------


cobolio functions
=================
.. automodule:: cobolio.parser
   :members: copybook_to_layout, loads
   :undoc-members:
   :show-inheritance:
