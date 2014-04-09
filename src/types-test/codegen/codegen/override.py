# -*- Mode: Python; py-indent-offset: 4 -*-

# this file contains code for loading up an override file.  The override file
# provides implementations of functions where the code generator could not
# do its job correctly.

import sys, string, fnmatch
import re

import_pat = re.compile(r'\s*import\s+(\S+)\.([^\s.]+)\s+as\s+(\S+)')

class Overrides:
    def __init__(self, filename=None):
        self.modulename = None
	self.ignores = {}
	self.glob_ignores = []
	self.overrides = {}
        self.overridden = {}
	self.kwargs = {}
        self.noargs = {}
        self.startlines = {}
        self.override_attrs = {}
        self.override_field_void_pointer_copy = {} # call a specific function for copying a void *field
        self.override_slots = {}
        self.headers = ''
        self.override_include_start = {}
        self.override_include_public = {}
        self.override_include_private = {}
        self.override_type = {} # inserted verbatim in type definition 
        self.override_save_load = {} # override the save load for a class 
        self.override_size = {} # override the size for a class 
        self.override_equal = {} # override the eq not eq for a class 
        self.override_create = {} # override the create for a class 
        self.override_intcreate = {} # override the create interface code
        self.override_implements = {} # inserted verbatim for implemented interfaces 
        self.override_destroy = {} # inserted verbatim before standard destroy
        self.override_internal_methods = '' # inserted verbatim in type structure 
        self.override_internal_methods_protos = '' # inserted verbatim before type structure
        self.override_int_create_final = {} # inserted verbatim in  create/load/full_copy
        self.override_print = {} # override the print function for a class 
        self.override_info = {} # override the info function for a class 
        self.override_path_extract = {} # override the path_extract field  for a class 
        self.override_loop = {} # override the loop field  for a class 
        self.init = ''
        self.last = ''
        self.imports = []
        self.copyright = ''

	if filename: self.handle_file(filename)

    def handle_file(self, filename):
        fp = open(filename, 'r')
	# read all the components of the file ...
        bufs = []
        startline = 1
        lines = []
        line = fp.readline()
        linenum = 1
        while line:
            if line == '%%\n' or line == '%%':
                if lines:
                    bufs.append((string.join(lines, ''), startline))
                startline = linenum + 1
                lines = []
            else:
                lines.append(line)
            line = fp.readline()
            linenum = linenum + 1
        if lines:
            bufs.append((string.join(lines, ''), startline))
	if not bufs: return

	for buf, startline in bufs:
	    self.__parse_override(buf, startline, filename)

    def __parse_override(self, buffer, startline, filename):
	pos = string.find(buffer, '\n')
	if pos >= 0:
	    line = buffer[:pos]
	    rest = buffer[pos+1:]
	else:
	    line = buffer ; rest = ''
	words = string.split(line)
	if words[0] == 'ignore' or words[0] == 'ignore-' + sys.platform :
	    for func in words[1:]: self.ignores[func] = 1
	    for func in string.split(rest): self.ignores[func] = 1
	elif words[0] == 'ignore-glob' or words[0] == 'ignore-glob-' + sys.platform :
	    for func in words[1:]: self.glob_ignores.append(func)
	    for func in string.split(rest):
		self.glob_ignores.append(func)
	elif words[0] == 'override':
	    func = words[1]
	    if 'kwargs' in words[1:]:
		self.kwargs[func] = 1
            elif 'noargs' in words[1:]:
		self.noargs[func] = 1
	    self.overrides[func] = rest
            self.startlines[func] = (startline + 1, filename)
        elif words[0] == 'override-attr':
            attr = words[1]
            self.override_attrs[attr] = rest
            self.startlines[attr] = (startline + 1, filename)
        elif words[0] == 'override-field-void-pointer-copy':
            attr = words[1]
            self.override_field_void_pointer_copy[attr] = rest
            self.startlines[attr] = (startline + 1, filename)
        elif words[0] == 'override-slot':
            slot = words[1]
            self.override_slots[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'headers':
            self.headers = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.headers, startline + 1, filename, rest)
        elif words[0] == 'copyright':
            self.copyright = '#line %d "codegen/%s"\n%s' % \
                           ( startline + 1, filename, rest)
            self.copyright = '%s' % rest 
        elif words[0] == 'override-type':
            slot = words[1]
            self.override_type[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-implements':
            # override code for all the implemented interfaces 
            type = words[1]
            self.override_implements[type] = rest
            slot = '%s_implements' % ( type )
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-save-load':
            # override code for all the implemented interfaces 
            type = words[1]
            self.override_save_load[type] = rest
            slot = '%s_save_load' % ( type )
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-size':
            # override code for all the implemented interfaces 
            type = words[1]
            self.override_size[type] = rest
            slot = '%s_size' % ( type )
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-equal':
            # override code for all the implemented interfaces 
            type = words[1]
            self.override_equal[type] = rest
            slot = '%s_equal' % ( type )
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-create':
            # override code for all the implemented interfaces 
            type = words[1]
            self.override_create[type] = rest
            slot = '%s_create' % ( type )
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-intcreate':
            # override code for all the implemented interfaces 
            type = words[1]
            self.override_intcreate[type] = rest
            slot = '%s_intcreate' % ( type )
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-destroy-prelim':
            slot = words[1]
            self.override_destroy[slot] = rest
            # take care to use a different name as in type override 
            stn = 'destroy_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
        elif words[0] == 'override-print':
            slot = words[1]
            self.override_print[slot] = rest
            # take care to use a different name as in type override 
            stn = 'print_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
        elif words[0] == 'override-info':
            slot = words[1]
            self.override_info[slot] = rest
            # take care to use a different name as in type override 
            stn = 'info_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
        elif words[0] == 'override-path-extract':
            slot = words[1]
            self.override_path_extract[slot] = rest
            # take care to use a different name as in type override 
            stn = 'path_extract_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
        elif words[0] == 'override-loop':
            slot = words[1]
            self.override_loop[slot] = rest
            # take care to use a different name as in type override 
            stn = 'loop_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
        elif words[0] == 'override-int-create-final':
            slot = words[1]
            self.override_int_create_final[slot] = rest
            # take care to use a different name as in type override 
            stn = 'int_create_final_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
            #self.override_int_create_final = '%s\n#line %d "codegen/%s"\n%s' % \
            # (self.override_int_create_final, startline + 1, filename, rest)
        elif words[0] == 'include-public':
            # we add '.include' to the slot for unicity
            slot = words[1]+'.include_public' # 
            self.override_include_public[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'include-start':
            # we add '.include' to the slot for unicity
            slot = words[1]+'.include_start' # 
            self.override_include_start[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'include-private':
            # we add '.include' to the slot for unicity
            slot = words[1]+'.include_private' # 
            self.override_include_private[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override-internal-methods':
            self.override_internal_methods = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.override_internal_methods, startline + 1, filename, rest)
        elif words[0] == 'override-internal-methods-protos':
            self.override_internal_methods_protos = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.override_internal_methods_protos, startline + 1, filename, rest)
        elif words[0] == 'init':
            self.init = '%s\n#line %d "codegen/%s"\n%s' % \
                        (self.init, startline + 1, filename, rest)
        elif words[0] == 'last':
            self.last = '%s\n#line %d "codegen/%s"\n%s' % \
                        (self.last, startline + 1, filename, rest)
        elif words[0] == 'modulename':
            self.modulename = words[1]
        elif words[0] == 'import':
            for line in string.split(buffer, '\n'):
                match = import_pat.match(line)
                if match:
                    self.imports.append(match.groups())

    def is_ignored(self, name):
	if self.ignores.has_key(name):
	    return 1
	for glob in self.glob_ignores:
	    if fnmatch.fnmatchcase(name, glob):
		return 1
	return 0
    def is_overriden(self, name):
	return self.overrides.has_key(name)
    def is_already_included(self, name):
        return self.overridden.has_key(name)
    def override(self, name):
        self.overridden[name] = 1
        return self.overrides[name]
    def getstartline(self, name):
        return self.startlines[name]
    def wants_kwargs(self, name):
	return self.kwargs.has_key(name)
    def wants_noargs(self, name):
	return self.noargs.has_key(name)
    def attr_is_overriden(self, attr):
        return self.override_attrs.has_key(attr)
    def attr_override(self, attr):
        return self.override_attrs[attr]
    def field_void_pointer_copy_is_overriden(self, attr):
        return self.override_field_void_pointer_copy.has_key(attr)
    def field_void_pointer_copy_override(self, attr):
        return self.override_field_void_pointer_copy[attr]
    def slot_is_overriden(self, slot):
        return self.override_slots.has_key(slot)
    def slot_override(self, slot):
        return self.override_slots[slot]
    def include_public_is_overriden(self, slot):
        return self.override_include_public.has_key(slot)
    def include_public_override(self, slot):
        return self.override_include_public[slot]
    def include_start_is_overriden(self, slot):
        return self.override_include_start.has_key(slot)
    def include_start_override(self, slot):
        return self.override_include_start[slot]
    def include_private_is_overriden(self, slot):
        return self.override_include_private.has_key(slot)
    def include_private_override(self, slot):
        return self.override_include_private[slot]
    def get_headers(self):
        return self.headers
    def get_include(self):
        return self.include
    def part_type_is_overriden(self, slot):
        return self.override_type.has_key(slot)
    def get_override_type(self,slot):
        return self.override_type[slot]
    def part_implements_is_overriden(self, slot):
        return self.override_implements.has_key(slot)
    def get_override_implements(self,slot):
        return self.override_implements[slot]
    def part_save_load_is_overriden(self, slot):
        return self.override_save_load.has_key(slot)
    def get_override_save_load(self,slot):
        return self.override_save_load[slot]
    def part_size_is_overriden(self, slot):
        return self.override_size.has_key(slot)
    def get_override_size(self,slot):
        return self.override_size[slot]
    def part_equal_is_overriden(self, slot):
        return self.override_equal.has_key(slot)
    def get_override_equal(self,slot):
        return self.override_equal[slot]
    def part_create_is_overriden(self, slot):
        return self.override_create.has_key(slot)
    def get_override_create(self,slot):
        return self.override_create[slot]
    def part_intcreate_is_overriden(self, slot):
        return self.override_intcreate.has_key(slot)
    def get_override_intcreate(self,slot):
        return self.override_intcreate[slot]
    def part_destroy_is_overriden(self, slot):
        return self.override_destroy.has_key(slot)
    def get_override_destroy(self,slot):
        return self.override_destroy[slot]
    def part_print_is_overriden(self, slot):
        return self.override_print.has_key(slot)
    def get_override_print(self,slot):
        return self.override_print[slot]
    def part_info_is_overriden(self, slot):
        return self.override_info.has_key(slot)
    def get_override_info(self,slot):
        return self.override_info[slot]
    def part_path_extract_is_overriden(self, slot):
        return self.override_path_extract.has_key(slot)
    def get_override_path_extract(self,slot):
        return self.override_path_extract[slot]
    def part_loop_is_overriden(self, slot):
        return self.override_loop.has_key(slot)
    def get_override_loop(self,slot):
        return self.override_loop[slot]
    def part_int_create_final_is_overriden(self, slot):
        return self.override_int_create_final.has_key(slot)
    def get_override_int_create_final(self,slot):
        return self.override_int_create_final[slot]
    def get_init(self):
        return self.init
    def get_last(self):
        return self.last
    def get_imports(self):
        return self.imports
    def get_copyright(self):
        return self.copyright

