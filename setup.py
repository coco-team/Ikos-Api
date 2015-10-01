import os
import sys
import glob
from subprocess import check_call
from setuptools import setup, Extension


def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()


def find_lib(patterns, search_dirs):
    for search_dir in search_dirs:
        for root, dirs, files in os.walk(search_dir):
            for lib in patterns:
                for f in files:
                    if 'lib' + lib in f:
                        return lib

    return False

# download the IKOS official release
IKOS_URL = 'http://ti.arc.nasa.gov/m/opensource/downloads/ikos/ikos_arbos.0.1.tar.gz'
IKOS_PATH = 'ikos_arbos.0.1/ikos'

if not os.path.exists('ikos-core/ikos'):
    sys.stdout.write('downloading ikos..\n')
    check_call(['curl', '-fLC', '-', '--retry', '3', '--retry-delay', '3', '-o', 'ikos.tar.gz', IKOS_URL])
    check_call(['tar', '-x', '-f', 'ikos.tar.gz'])
    check_call(['patch', '-p1', '-d', IKOS_PATH], stdin=open('ikos-core/ikos.patch'))
    check_call(['ln', '-f', '-s', os.path.abspath(IKOS_PATH), 'ikos-core/ikos'])

# find boost_python
libraries = ['gmp', 'gmpxx', 'pthread']
include_dirs = ['ikos-core']
library_dirs = []

search_library_dirs = ['/usr/local/lib64', '/usr/lib64', '/usr/local/lib', '/usr/lib']

if sys.platform.lower() == 'darwin':
    include_dirs.append('/opt/local/include')
    library_dirs.append('/opt/local/lib')
    search_library_dirs.append('/opt/local/lib')

_version = sys.version_info
boost_python_patterns = [
    'boost_python-py%s%s' % (_version[0], _version[1]),
    'boost_python-mt-py%s%s' % (_version[0], _version[1]),
    'boost_python-py%s' % _version[0],
    'boost_python-mt-py%s' % _version[0],
    'boost_python%s%s' % (_version[0], _version[1]),
    'boost_python%s' % _version[0],
]

sys.stdout.write('looking for boost_python.. ')
sys.stdout.flush()
boost_python = find_lib(boost_python_patterns, search_library_dirs)

if not boost_python:
    sys.stdout.write('not found, will try -lboost_python\n')
    boost_python = 'boost_python'
else:
    sys.stdout.write(boost_python + '\n')

libraries.append(boost_python)

apicore = Extension('ikos.apicore', ['PyIkos/ikos/apicore.cpp'],
                    libraries=libraries,
                    include_dirs=include_dirs,
                    library_dirs=library_dirs,
                    extra_compile_args=['-O1', '-Wall', '-Wno-non-virtual-dtor', '-Wno-unused-function', '-Wno-deprecated', '-ffast-math'],
                    depends=glob.glob('ikos-core/*/*.hpp'))

setup(
    name='PyIkos',
    version='1.0',
    author='Maxime Arthaud',
    author_email='maxime@arthaud.me',
    license='MIT',
    url='https://bitbucket.org/lememta/ikos-api',
    description='Python API for IKOS (Inference Kernel for Open Static Analyzers).',
    long_description=read('README.md'),
    classifiers=[
        'Development Status :: 5 - Production/Stable',
        'Environment :: Console',
        'Intended Audience :: Developers',
        'Intended Audience :: Science/Research',
        'Operating System :: MacOS',
        'Operating System :: POSIX',
        'Operating System :: Unix',
        'Programming Language :: C++',
        'Programming Language :: Python',
        'Topic :: Scientific/Engineering',
    ],
    packages=['ikos'],
    package_dir={'': 'PyIkos'},
    ext_modules=[apicore]
)
