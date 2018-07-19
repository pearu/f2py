'''This file provides utilities to create a fortran parser suitable
for a particular standard'''
# pylint: disable=eval-used

import inspect
import sys


def get_module_classes(input_module):
    ''' Return all classes local to a module

    :param module input_module: the module containing the classes
    :return: a `list` of classes contained in the module

    '''
    module_cls_members = []
    module_name = input_module.__name__
    # first find all classes in the module. This includes imported
    # classes.
    all_cls_members = inspect.getmembers(sys.modules[module_name],
                                         inspect.isclass)
    # next only keep classes that are specified in the module
    for cls_member in all_cls_members:
        if cls_member[1].__module__ == module_name:
            module_cls_members.append(cls_member)
    return module_cls_members


class ParserFactory(object):
    '''Creates a parser suitable for the specified fortran standard'''

    def create(self, std=None):
        '''Creates a class hierarchy suitable for the specified fortran
        standard

        :param std string: the fortran standard. Choices are 'f2003' or
                           'f2008'. 'f2003' is the default.
        :return: a Program class (not object) for use with the fortran reader
        :rtype: :py:class:`fparser.two.Fortran2003.Program`

        For example:

        >>> from fparser.two.parser import ParserFactory
        >>> f2003_parser = ParserFactory().create()
        >>> f2003_parser = ParserFactory().create(std='f2003')
        >>> f2008_parser = ParserFactory().create(std='f2008')

        '''
        # find all relevant classes in our Fortran2003 file as we
        # always need these
        from fparser.two import Fortran2003
        f2003_cls_members = get_module_classes(Fortran2003)
        if not std:
            # default to f2003
            std = "f2003"
        if std == "f2003":
            # we already have our required list of classes so call _setup
            # to setup our class hierarchy
            self._setup(f2003_cls_members)
            # the class hierarchy has been set up so return the top
            # level class that we start from when parsing fortran code
            return Fortran2003.Program
        elif std == "f2008":
            # we need to find all relevent classes in our Fortran2003
            # and Fortran2008 files and then ensure that where classes
            # have the same name we return the Fortran2008 class
            # i.e. where Fortran2008 extends Fortran2003 we return
            # Fortran2008.
            # First find all Fortran2008 classes
            from fparser.two import Fortran2008
            f2008_cls_members = get_module_classes(Fortran2008)
            # next add in Fortran2003 classes if they do not already
            # exist as a Fortran2008 class
            f2008_class_names = [i[0] for i in f2008_cls_members]
            for local_cls in f2003_cls_members:
                if local_cls[0] not in f2008_class_names:
                    f2008_cls_members.append(local_cls)
            # we now have our required list of classes so call _setup
            # to setup our class hierarchy
            self._setup(f2008_cls_members)
            # the class hierarchy has been set up so return the top
            # level class that we start from when parsing fortran
            # code. Fortran2008 does not extend the top level class so
            # we return the Fortran2003 one.
            return Fortran2003.Program
        else:
            print "unsupported standard {0}".format(std)
            exit(1)

    def _setup(self, input_classes):
        '''Perform some Python magic to create the connections between classes
        and populate the baseclass with this information. This has
        been lifted from the original implementation and no attempt
        has been made to tidy up the code, other than making it
        conformant to the coding rules.

        :param list input_classes: a list of tuples each containing a
        class name and a class

        '''

        __autodoc__ = []
        base_classes = {}

        import logging
        import fparser.two.Fortran2003
        class_type = type(fparser.two.Fortran2003.Base)

        for clsinfo in input_classes:
            clsname = "{0}.{1}".format(clsinfo[1].__module__, clsinfo[0])
            cls = eval(clsname)
            # ?? classtype is set to Base so why have issubclass?
            if isinstance(cls, class_type) and \
               issubclass(cls, fparser.two.Fortran2003.Base) \
               and not cls.__name__.endswith('Base'):
                base_classes[cls.__name__] = cls
                if len(__autodoc__) < 10:
                    __autodoc__.append(cls.__name__)

        #
        # OPTIMIZE subclass_names tree
        #

        if 1:  # Optimize subclass tree:

            def _rpl_list(clsname):
                if clsname not in base_classes:
                    error_string = 'Not implemented: {0}'.format(clsname)
                    logging.getLogger(__name__).debug(error_string)
                    return []
                # remove this code when all classes are implemented
                cls = base_classes[clsname]
                if 'match' in cls.__dict__:
                    return [clsname]
                bits = []
                for names in getattr(cls, 'subclass_names', []):
                    list1 = _rpl_list(names)
                    for names1 in list1:
                        if names1 not in bits:
                            bits.append(names1)
                return bits

            for cls in list(base_classes.values()):
                if not hasattr(cls, 'subclass_names'):
                    continue
                opt_subclass_names = []
                for names in cls.subclass_names:
                    for names1 in _rpl_list(names):
                        if names1 not in opt_subclass_names:
                            opt_subclass_names.append(names1)
                if not opt_subclass_names == cls.subclass_names:
                    cls.subclass_names[:] = opt_subclass_names

        # Initialize Base.subclasses dictionary:
        for clsname, cls in list(base_classes.items()):
            subclass_names = getattr(cls, 'subclass_names', None)
            if subclass_names is None:
                message = '%s class is missing subclass_names list' % (clsname)
                logging.getLogger(__name__).debug(message)
                continue
            try:
                bits = fparser.two.Fortran2003.Base.subclasses[clsname]
            except KeyError:
                fparser.two.Fortran2003.Base.subclasses[clsname] = bits = []
            for name in subclass_names:
                if name in base_classes:
                    bits.append(base_classes[name])
                else:
                    message = '{0} not implemented needed by {1}'. \
                              format(name, clsname)
                    logging.getLogger(__name__).debug(message)

        if 1:
            for cls in list(base_classes.values()):
                # subclasses = fparser.two.Fortran2003.Base.subclasses.get(
                #     cls.__name__, [])
                # subclasses_names = [c.__name__ for c in subclasses]
                subclass_names = getattr(cls, 'subclass_names', [])
                use_names = getattr(cls, 'use_names', [])
                # for name in subclasses_names:
                #     break
                #     if name not in subclass_names:
                #         message = ('%s needs to be added to %s '
                #                    'subclasses_name list'
                #                    % (name, cls.__name__))
                #         logging.getLogger(__name__).debug(message)
                # for name in subclass_names:
                #     break
                #     if name not in subclasses_names:
                #         message = '%s needs to be added to %s '
                #         'subclass_name list' % (name, cls.__name__)
                #         logging.getLogger(__name__).debug(message)
                for name in use_names + subclass_names:
                    if name not in base_classes:
                        message = ('%s not defined used '
                                   'by %s' % (name, cls.__name__))
                        logging.getLogger(__name__).debug(message)
