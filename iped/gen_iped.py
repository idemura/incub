from gen_make import MakeFile

m = MakeFile(['pthread', 'cityhash'])
m.cxx_bin('iped', ['iped.cxx'])
m.cxx_mod('iped.cxx',
        ['base.cxx',
         'flags.cxx',
         'template.cxx',
         'mongoose',
         'pthread'])
m.cxx_mod('base.cxx', ['cityhash'])
m.cxx_mod('flags.cxx', ['base.cxx'])
m.cxx_mod('template.cxx', ['base.cxx'])
m.generic_clib('mongoose',
        ['mongoose.c',
         'mongoose.h'],
        ['MONGOOSE_NO_FILE_SYSTEM'])
m.cxx_test('base_test.cxx', ['base.cxx'])
m.cxx_test('flags_test.cxx', ['flags.cxx'])
m.cxx_test('template_test.cxx', ['template.cxx'])
m.set_default(['iped'])
m.write()
# print(m.makefile())

