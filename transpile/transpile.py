import argparse
import logging
import logging.config
import subprocess
import yaml

from argparse import ArgumentParser
from pathlib import Path


logger = logging.getLogger(__name__)

def configure_logging(parent, args):
    """
    Configure logging (level, format, etc.) for this module

    @param{parent} -- directory where 'transpile.yml' resides
    @param{args}   -- result of ArgumentParser().parse_args()
    """
    with open(Path(parent, 'transpile.yml'), 'r') as config:
        params = yaml.load(config)

        logging.config.dictConfig(params['logging'])

        if args.verbose == 0:
            pass  # use level specified by the config file
        elif args.verbose == 1:
            logger.setLevel(logging.INFO)
        else:
            logger.setLevel(logging.DEBUG)

        if args.quiet:
            logging.disable(logging.CRITICAL)
            # Message below should not reach the user
            logger.critical('logging is active despite --quiet')

def transpile():
    """
    Call transcrypt to transpile boolean.py into JavaScript
    """

    fpath = Path(__file__).resolve()
    parent = fpath.parent

    parser = ArgumentParser(
        prog=fpath.name,
        description="Transpile boolean.py into JavaScript"
    )

    # file path to boolean.py, usually ../boolean/boolean.py
    bpath = Path(parent.parent, 'boolean', 'boolean.py')
    parser.add_argument(
        '--src', nargs=1, default=[bpath],
        help='start transpilation from here'
    )

    # path to produced javascript output
    jpath = Path(parent.parent, '__javascript__')
    parser.add_argument(
        '--dst', nargs=1, default=[jpath],
        help='store produced javascript here'
    )

    parser.add_argument(
        '-v', '--verbose', action='count',
        help='print more output information'
    )

    parser.add_argument(
        '-q', '--quiet', action='store_true',
        help='do not print anything to the console'
    )

    # see https://docs.python.org/3.3/library/argparse.html#nargs
    parser.add_argument(
        'args', nargs=argparse.REMAINDER,
        help='pass these arguments to transpiler'
    )

    args = parser.parse_args()

    configure_logging(parent, args)

    # User can specify `--silent` to suppress command line output
    # So, delay debug calls until we know the desired verbosity level
    logger.debug('transpile() call')

    logger.debug('.parse_args() call')
    logger.debug('src : ' + str(args.src))
    logger.debug('dst : ' + str(args.dst))
    logger.debug('args: ' + str(args.args))
    logger.debug('.parse_args() done')


    logger.debug('transpile() done')

if __name__ == "__main__":
    transpile()
