from __future__ import absolute_import
from __future__ import print_function
from six.moves import map
from six.moves import range

__all__ = ['SetupPy']

import os
import re
import sys
from numpy.distutils.exec_command import exec_command
from numpy.distutils.misc_util import yellow_text, red_text, cyan_text
from .base import Component
from .utils import FileSource

def write_files(container):
    s = ['creating files and directories:']
    for filename, i in list(container.label_map.items()):
        content = container.list[i]
        d,f = os.path.split(filename)
        if d and not os.path.isdir(d):
            s.append('  %s/' % (d))
            if not Component._generate_dry_run:
                os.makedirs(d)
        s.append('  %s' % (filename))
        if not Component._generate_dry_run:
            overwrite = True
            if os.path.isfile(filename):
                overwrite = False
                f = open(filename, 'r')
                i = 0
                for line in f:
                    if 'is generated using ExtGen tool' in line:
                        overwrite = True
                        break
                    i += 1
                    if i>5: break
                if not overwrite:
                    s[-1] += ' - unknown file exists, skipping'
                else:
                    s[-1] += ' - extgen generated file exists, overwriting'
            if overwrite:
                f = open(filename,'w')
                f.write(content)
                f.close()
    return '\n'.join(s)


class SetupPy(Component):

    """
    >>> from __init__ import *
    >>> s = SetupPy('SetupPy_doctest')
    >>> s += PyCModule('foo')
    >>> s,o = s.execute('build_ext', '--inplace')
    >>> assert s==0,`s`
    >>> import SetupPy_doctest as mypackage
    >>> print mypackage.foo.__doc__ #doctest: +ELLIPSIS
    This module 'foo' is generated with ExtGen from NumPy version...

    """
    template_setup_py_start = '''\
def configuration(parent_package='', top_path = ''):
    from numpy.distutils.misc_util import Configuration
    config = Configuration('',parent_package,top_path)'''
    template_setup_py_end = '''\
    return config
if __name__ == "__main__":
    from numpy.distutils.core import setup
    setup(configuration=configuration)
'''
    template = '%(SourceWriter)s'

    container_options = dict(
      SourceWriter = dict(user_defined_str = write_files),
      TMP = dict()
    )

    component_container_map = dict(
        FileSource = 'SourceWriter',
        ExtensionModule = 'TMP',
    )

    def initialize(self, build_dir, *components, **options):
        self.name = self.path = build_dir
        if not self.path:
            self.setup_py = setup_py = Component.PySource('extgen_setup.py')
            self.init_py = init_py = Component.PySource('extgen__init__.py')
        else:
            self.setup_py = setup_py = Component.PySource('setup.py')
            self.init_py = init_py = Component.PySource('__init__.py')

        setup_py += self.template_setup_py_start

        self += init_py
        self += setup_py

        list(map(self.add, components))

        return self

    def finalize(self):
        self.setup_py += self.template_setup_py_end

    def execute(self, *args):
        """
        Run generated setup.py file with given arguments.
        """
        if not args:
            raise ValueError('need setup.py arguments')
        self.debug(self.generate(dry_run=False))
        cmd = [sys.executable,'setup.py'] + list(args)
        self.debug('entering %r directory' % (self.path))
        self.debug('executing command %r' % (' '.join(cmd)))
        try:
            r = exec_command(cmd, execute_in=self.path, use_tee=False)
        except:
            self.info('leaving %r directory' % (self.path))
            raise
        else:
            try:
                self._show_warnings_errors(r[1])
            except Exception as msg:
                #print msg
                self.warning(r[1])
            if not r[0]:
                self.debug('SUCCESS!')
            else:
                self.warning('FAILURE')
            self.debug('leaving %r directory' % (self.path))
        return r

    def _show_warnings_errors(self, output):
        warning_match = re.compile(r'(?P<filename>.*?):(?P<lineno>\d+)(:\d+)*:\s*warning:(?P<message>.*)').match
        error_match = re.compile(r'(?P<filename>.*?):(?P<lineno>\d+)(:\d+)*:\s*error:(?P<message>.*)').match
        messages = {}
        files = {}
        types = {}
        for line in output.splitlines():
            r = warning_match(line)
            t = ''
            if r: t = 'warning'
            else:
                r = error_match(line)
                if r: t = 'error'
            if r:
                fn = os.path.normpath(os.path.join(self.path, r.group('filename')))
                n = int(r.group('lineno'))-1
                m = r.group('message')
                if n in messages:
                    if m not in messages[n]:
                        messages[n].append(m)
                else:
                    types[n] = t
                    files[n] = fn
                    messages[n] = [m]
        if not messages:
            return
        keys = list(messages.keys())
        keys.sort()
        for n in keys:
            m = messages[n]
            fn = files[n]
            t = types[n]
            if t=='warning':
                color_text = yellow_text
            elif t=='error':
                color_text = red_text
            print(fn)
            f = open(fn,'r')
            content = f.readlines()
            f.close()
            print('%s: compile %s message:' % (fn, t))
            for i in range(n-3, n+3):
                if i<0 or i>=len(content): continue
                if i==n:
                    print('%d: %s <-- %s' % (i+1, color_text(content[i].rstrip()), cyan_text('\n'.join(m))))
                else:
                    print('%d: %s' % (i+1, content[i].rstrip()))

def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()
