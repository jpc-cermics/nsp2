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
        self.override_slots = {}
        self.headers = ''
        self.override_include = {}
        self.override_include_private = {}
        self.override_type = {} # inserted verbatim in type definition 
        self.override_save_load = {} # override the save load for a class 
        self.override_implements = {} # inserted verbatim for implemented interfaces 
        self.override_destroy = {} # inserted verbatim before standard destroy
        self.override_internal_methods = '' # inserted verbatim in type structure 
        self.override_internal_methods_protos = '' # inserted verbatim before type structure
        self.override_destroy_prelim = '' # inserted verbatim before standard destroy
        self.override_int_create_final = '' # inserted verbatim in int_xx_create 
        self.init = ''
        self.last = ''
        self.imports = []
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
        elif words[0] == 'override-slot':
            slot = words[1]
            self.override_slots[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'headers':
            self.headers = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.headers, startline + 1, filename, rest)
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
        elif words[0] == 'override_destroy_prelim':
            slot = words[1]
            self.override_destroy[slot] = rest
            # take care to use a different name as in type override 
            stn = 'destroy_%s' % slot
            self.startlines[stn] = (startline + 1, filename)
        elif words[0] == 'include':
            # we add '.include' to the slot for unicity
            slot = words[1]+'.include' # 
            self.override_include[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'include_private':
            # we add '.include' to the slot for unicity
            slot = words[1]+'.include_private' # 
            self.override_include_private[slot] = rest
            self.startlines[slot] = (startline + 1, filename)
        elif words[0] == 'override_internal_methods':
            self.override_internal_methods = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.override_internal_methods, startline + 1, filename, rest)
        elif words[0] == 'override_internal_methods_protos':
            self.override_internal_methods_protos = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.override_internal_methods_protos, startline + 1, filename, rest)
        elif words[0] == 'override_int_create_final':
            self.override_int_create_final = '%s\n#line %d "codegen/%s"\n%s' % \
                           (self.override_int_create_final, startline + 1, filename, rest)
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
    def slot_is_overriden(self, slot):
        return self.override_slots.has_key(slot)
    def slot_override(self, slot):
        return self.override_slots[slot]
    def include_is_overriden(self, slot):
        return self.override_include.has_key(slot)
    def include_override(self, slot):
        return self.override_include[slot]
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
    def part_destroy_is_overriden(self, slot):
        return self.override_destroy.has_key(slot)
    def get_override_destroy(self,slot):
        return self.override_destroy[slot]
    def get_init(self):
        return self.init
    def get_last(self):
        return self.last
    def get_imports(self):
        return self.imports
