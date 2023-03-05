from cobolio import __version__


def print_banner(command_name, parms):
    message = f'{command_name} (cobolio {__version__})'
    print(message)
    print('parameters:')
    for parm_key in parms:
        if parms[parm_key]:
            print(f' -{parm_key}:{parms[parm_key]}')


def add_version(parser):
    version_text = (f'%(prog)s (cobolio {__version__})\n'
                    f'(C)Copyright 2023 Anthony Delosa\n')
    parser.add_argument('--version', action='version', version=version_text)
