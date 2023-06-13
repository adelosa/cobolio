=======
cobolio
=======

A Python package for processing mainframe data files using COBOL copybooks.

.. image:: https://img.shields.io/pypi/l/cobolio.svg
        :target: https://pypi.org/project/cobolio
        :alt: License
.. image:: https://img.shields.io/pypi/v/cobolio.svg
        :target: https://pypi.org/project/cobolio
        :alt: Version
.. image:: https://img.shields.io/pypi/wheel/cobolio.svg
        :target: https://pypi.org/project/cobolio
        :alt: Wheel
.. image:: https://img.shields.io/pypi/implementation/cobolio.svg
        :target: https://pypi.org/project/cobolio
        :alt: Implementation
.. image:: https://img.shields.io/github/issues/adelosa/cobolio
        :target: https://github.com/adelosa/cobolio/issues
        :alt: Status
.. image:: https://img.shields.io/pypi/dm/cobolio.svg
        :target: https://pypi.org/project/cobolio
        :alt: Downloads per month
.. image:: https://img.shields.io/pypi/pyversions/cobolio.svg
        :target: https://pypi.org/project/cobolio
        :alt: Python versions

.. readme_install

Install
=======
Install using pip::

    pip install cobolio


Usage
=====

Documentation is hosted at `GitHub <https://adelosa.github.io/cobolio>`_.

Roadmap
=======
* dumps function -- reverse processing (dict -> bytes)

Limitations
===========

No support for :code:`OCCURS DEPENDING ON` clause in copybook.

Acknowledgements
================

The original code was taken from AWS samples repo called 
`unlock mainframe data files on aws <https://github.com/aws-samples/unlock-mainframe-data-files-on-aws>`_.

This project repackages this functionality as a python package
and provide pythonic interfaces for programmatic use.

It also provides a CLI for processing files for end users.

