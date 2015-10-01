APPNAME = 'ikos-api'
VERSION = '1.0'

top = '.'
out = 'build'

def options(opt):
    opt.load('compiler_cxx boost')
    opt.recurse('PyIkos')
    opt.recurse('OIkos')

    opt.parser.set_defaults(jobs=1)
    opt.add_option('--no-python', action='store_true', default=False, help='Do not build PyIkos')
    opt.add_option('--no-ocaml', action='store_true', default=False, help='Do not build OIkos')


def configure(conf):
    conf.env.ikos_python = not conf.options.no_python
    conf.env.ikos_ocaml = not conf.options.no_ocaml

    # for Mac users
    conf.env.append_value('INCLUDES', ['/opt/local/include'])
    conf.env.append_value('LINKFLAGS', ['-L/opt/local/lib'])

    conf.env.IKOS_CXXFLAGS = ['-Wall', '-Wno-non-virtual-dtor', '-Wno-unused-function', '-Wno-deprecated', '-ffast-math']
    conf.env.IKOS_LINKFLAGS = ['-Wl,--no-as-needed', '-lgmp', '-lgmpxx', '-lpthread']

    # settings for IKOS sources
    conf.find_program('curl', var='CURL')
    conf.find_program('tar', var='TAR')
    conf.find_program('patch', var='PATCH')
    conf.find_program('ln', var='LN')
    conf.env.IKOS_URL = 'http://ti.arc.nasa.gov/m/opensource/downloads/ikos/ikos_arbos.0.1.tar.gz'
    conf.env.IKOS_PATH = 'ikos_arbos.0.1/ikos'

    # check
    conf.load('compiler_cxx boost')

    if conf.env.ikos_python:
        conf.recurse('PyIkos')
        conf.options.boost_python = conf.env.PYTHON_VERSION.replace('.', '')[:2]
        conf.check_boost(lib='python')
    else:
        conf.check_boost()

    conf.check_cc(lib='gmp')
    conf.check_cxx(lib='gmpxx')
    conf.check_cxx(lib='pthread')

    if conf.env.ikos_ocaml:
        conf.recurse('OIkos')


def build(bld):
    # download IKOS sources
    bld(rule="${CURL} -fLC - --retry 3 --retry-delay 3 -o '${TGT[0].abspath()}' '${IKOS_URL}'", target='ikos.tar.gz')
    bld(rule="${TAR} -x -f '${SRC[0].abspath()}'", source='ikos.tar.gz', target=bld.env.IKOS_PATH)
    bld(rule="${PATCH} -p1 -d '${SRC[0].abspath()}' < '${SRC[1].abspath()}' && "
             "${LN} -f -s '${SRC[0].abspath()}' '${SRC[1].parent.abspath()}'",
        name='IKOS_CORE',
        includes='ikos-core',
        export_includes='ikos-core',
        lib=['gmp', 'gmpxx', 'pthread'],
        use=['BOOST'],
        source=[bld.env.IKOS_PATH, 'ikos-core/ikos.patch'],
        )

    if bld.env.ikos_python:
        bld.recurse('PyIkos')
    if bld.env.ikos_ocaml:
        bld.recurse('OIkos')
