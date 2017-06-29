import yaml
import logging
import logging.config

from pathlib import Path
from argparse import ArgumentParser

logger = logging.getLogger(__name__)

def configure_logging(parent):
    with open(Path(parent, 'transpile.yml'), 'r') as config:
        params = yaml.load(config)

        logging.config.dictConfig(params['logging'])

def transpile():
    """
    Call transcrypt to transpile boolean.py into JavaScript
    """
    fpath = Path(__file__).resolve()

    configure_logging(fpath.parent)

    logger.debug('transpile() call')

    parser = ArgumentParser(
        prog=fpath.name,
        description="Transpile boolean.py into JavaScript"
    )

    # file path to boolean.py, usually ../boolean/boolean.py
    bpath = Path(fpath.parent, 'boolean', 'boolean.py')
    parser.add_argument(
        'src', nargs='?', default=[bpath],
        help='start transpilation from here'
    )

    # path to produced javascript output
    jpath = Path(fpath.parent, '__javascript__')
    parser.add_argument(
        'dst', nargs=1, default=[jpath],
        help='store produced javascript here'
    )

    logger.debug('transpile() done')

if __name__ == "__main__":
    transpile()
