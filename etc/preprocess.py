# import ast
# import logging
# import logging.config
# import yaml


# logger = logging.getLogger(__name__)

# def preprocess(args):
#     """
#     Transform python code so that transcrypt can transpile it
#     """
#     logger.debug('preprocess() call')

#     with open(args.src, 'r') as src:
#         ast_tree = ast.parse(src.read())

#         preprocess_super(ast_tree)

#     logger.debug('preprocess() done')

# def preprocess_super(ast_tree):
#     for item in ast_tree.body:
#         if isinstance(item, ast.ClassDef):
#             preprocess_classdef(item)

# def preprocess_classdef(ast_class):
#     name = ast_class.name

#     if len(ast_class.bases) == 0:
#         logger.debug(f'class {name} has no base classes')

#         return
#     elif len(ast_class.bases) != 1:
#         logger.warning(f'class {name} has > 1 base classes')
#         logger.warning('transpiling will most likely fail')

#         return

#     for item in ast_class.body:
#         if isinstance(item, ast.FunctionDef):
#             preprocess_functiondef(item)

# def preprocess_funcitondef(ast_func):
#     name = ast_func.name

#     if name != '__init__':
#         logger.debug(f'def {name} is not __init__, skipping')

#         return

#     for item in ast_func.body:
#         try:
#             value = item.value.func.value

#             func  = item.value.func.value.func

#             if func.id == 'super' and len(value) == 2:


# if __name__ == "__main__":
    # with open('transpile.yml', 'r') as config:
    #     params = yaml.load(config)

    # class Args:
    #     def __init__(self):
    #         self.src = 'simple_class.py'

    # args = Args()

    # preprocess(args)
