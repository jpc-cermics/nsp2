# -*- Mode: Python; py-indent-offset: 4 -*-
import sys
import string
import traceback
import keyword

class VarList:
    """Nicely format a C variable list"""
    def __init__(self):
	self.vars = {}
    def add(self, ctype, name):
	if self.vars.has_key(ctype):
	    self.vars[ctype] = self.vars[ctype] + (name,)
	else:
	    self.vars[ctype] = (name,)
    def __str__(self):
	ret = []
	for type in self.vars.keys():
	    ret.append('  ')
	    ret.append(type)
	    ret.append(' ')
	    ret.append(string.join(self.vars[type], ', '))
	    ret.append(';\n')
	if ret:
            ret.append('\n')
            return string.join(ret, '')
	return ''

class WrapperInfo:
    """A class that holds information about variable defs, code
    snippets, etcd for use in writing out the function/method
    wrapper."""
    def __init__(self):
        self.varlist = VarList()
        self.parsestr = ''
        self.opts = 'false'
        self.parselist = []
        self.types =[]
        self.codebefore = []
        self.attrcodebefore = []
        self.codeafter = []
        self.attrcodeafter = []
        self.arglist = []
        self.kwlist = []
        self.tylist = []
    def get_parselist(self):
        return string.join(self.parselist, ', ')
    def get_codebefore(self):
        return string.join(self.codebefore, '')
    def get_attrcodebefore(self):
        return string.join(self.attrcodebefore, '')
    def get_codeafter(self):
        return string.join(self.codeafter, '')
    def get_attrcodeafter(self):
        return string.join(self.attrcodeafter, '')
    def get_arglist(self):
        return string.join(self.arglist, ', ')
    def get_varlist(self):
        return str(self.varlist)
    def get_kwlist(self):
        if self.opts == 'true':
            ret = '  nsp_option opts[] = {%s };\n' % \
                  string.join(self.kwlist + [ '\n\t{NULL,t_end,NULLOBJ,-1}' ], ', ')
            if not self.get_varlist():
                ret = ret + '\n'
            return ret
        else:
            return ''
        
        
    def get_tylist(self):
        r = string.join(self.tylist , ', ')
        if len(r)== 0 and self.opts == 'false':
            ret = ''
        else:
            ret = '  int_types T[] = {%s' % r
            if self.opts == 'true':
                if len(r)== 0: 
                    ret = ret + 'new_opts'
                else:
                    ret = ret + ',new_opts'
            ret = ret + ',t_end};\n'
            if not self.get_varlist():
                ret = ret + '\n'
        return ret
    
    def add_parselist(self, codes, parseargs, keywords):
        self.parsestr = self.parsestr + codes
        for arg in parseargs:
            self.parselist.append(arg)
        if codes == '|':
            self.opts = 'true'
            self.parselist.append('opts')
        if self.opts == 'true':
            for kw in keywords:
                if keyword.iskeyword(kw):
                    kw = kw + '_'
                self.kwlist.append('\n\t{"%s",%s,NULLOBJ,-1}' % (kw,codes) )
        else:
            for kw in keywords:
                if keyword.iskeyword(kw):
                    kw = kw + '_'
                self.tylist.append('%s' % (codes) )

class ArgType:
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	"""Add code to the WrapperInfo instance to handle
	parameter."""
	raise RuntimeError, "write_param not implemented for %s" % \
              self.__class__.__name__
    def write_return(self, ptype, ownsreturn, info):
	"""Adds a variable named ret of the return type to
	info.varlist, and add any required code to info.codeafter to
	convert the return value to a python object."""
	raise RuntimeError, "write_return not implemented for %s" % \
              self.__class__.__name__
    def attr_write_return(self, ptype, ownsreturn, info):
	"""Adds a variable named ret of the return type to
	info.varlist, and add any required code to info.codeafter to
	convert the return value to a python object."""
	raise RuntimeError, "write_return not implemented for %s" % \
              self.__class__.__name__

class NoneArg(ArgType):
    def write_return(self, ptype, ownsreturn, info):
        info.codeafter.append('  return 0;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.attrcodeafter.append('  return NULLOBJ;')

class StringArg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
            if pdflt != 'NULL': pdflt = '"' + pdflt + '"'
	    info.varlist.add('char', '*' + pname + ' = ' + pdflt)
	else:
	    info.varlist.add('char', '*' + pname)
	info.arglist.append(pname)
	if pnull:
            info.add_parselist('string', ['&' + pname], [pname])
	else:
            info.add_parselist('string', ['&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        if ownsreturn:
	    # have to free result ...
	    info.varlist.add('gchar', '*ret')
            info.codeafter.append('  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;\n' +
                                  '  g_free(ret);\n  return 1;')
	else:
	    info.varlist.add('const gchar', '*ret')
            info.codeafter.append('  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;\n'
                                  '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info): 
        info.varlist.add('NspObject', '*nsp_ret')
        if ownsreturn:
	    # have to free result ...
	    info.varlist.add('gchar', '*ret')
            info.attrcodeafter.append('  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);\n  g_free(ret);\n  return nsp_ret;')
	else:
	    info.varlist.add('const gchar', '*ret')
            info.attrcodeafter.append('  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);\n  return nsp_ret;')
            

class UCharArg(ArgType):
    # allows strings with embedded NULLs.
    # XXXXX : to be implemented ...
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('guchar', '*' + pname + ' = "' + pdflt + '"')
	else:
	    info.varlist.add('guchar', '*' + pname)
        #FIXME info.varlist.add('int', pname + '_len')
	info.arglist.append(pname)
	if pnull:
            info.add_parselist('string', ['&' + pname],
                               [pname])
	else:
            info.add_parselist('string', ['&' + pname],
                               [pname])

class CharArg(ArgType):
    # a char argument is an int at nsp level 
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('int', pname + " = '" + pdflt + "'")
	else:
	    info.varlist.add('int', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
        
class GUniCharArg(ArgType):
    param_tmpl = '  %(name)s = (gunichar)nsp_%(name)s;\n'
    dflt_tmpl = ('  if (nsp_%(name)s != NULL) {\n'
                 '      if (nsp_%(name)s[1] != 0) {\n'
                 '          Scierror( "%(name)s should be a 1 character unicode string");\n'
                 '          return RET_BUG;\n'
                 '      }\n'
                 '      %(name)s = (gunichar)nsp_%(name)s[0];\n'
                 '   }\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('gunichar', pname + " = '" + pdflt + "'")
            info.codebefore.append(self.dflt_tmpl % {'name':pname})
	else:
	    info.varlist.add('gunichar', pname)
            info.codebefore.append(self.param_tmpl % {'name':pname})
        info.varlist.add('int', 'nsp_' + pname + ' = 0')
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gunichar', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)== FAIL)return RET_BUG;\n  return 1;')
        
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gunichar', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')

class IntArg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('int', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('int', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( IntScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')


class IntPointerArg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('int', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('int', pname)
	info.arglist.append('&' + pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( IntScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', '*ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) *ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', '*ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) *ret);')


        
class BoolArg(IntArg):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('int', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('int', pname)
	info.arglist.append(pname)
        info.add_parselist('s_bool', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( BoolScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.codeafter.append('  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
        info.attrcodeafter.append('  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);\n  return nsp_ret;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);\n  return nsp_ret;')

class TimeTArg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('time_t', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('time_t', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('time_t', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('time_t', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')

class ULongArg(ArgType):
    dflt = '  if (nsp_%(name)s)\n' \
           '      %(name)s = PyLong_AsUnsignedLong(nsp_%(name)s);\n'
    before = '  %(name)s = PyLong_AsUnsignedLong(nsp_%(name)s);\n'
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('gulong', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('gulong', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( ULongScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gulong', 'ret')
        info.codeafter.append(' if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;\n' +
                              '  return 1;');
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gulong', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')

class Int64Arg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('gint64', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('gint64', pname)
	info.arglist.append(pname)
        info.add_parselist('L', ['&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gint64', 'ret')
        info.codeafter.append('  return PyLong_FromLongLong(ret);')

    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gint64', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')

class UInt64Arg(ArgType):
    dflt = '  if (nsp_%(name)s)\n' \
           '      %(name)s = PyLong_AsUnsignedLongLong(nsp_%(name)s);\n'
    before = '  %(name)s = PyLong_AsUnsignedLongLong(nsp_%(name)s);\n'
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        if pdflt:
            info.varlist.add('guint64', pname + ' = ' + pdflt)
            info.codebefore.append(self.dflt % {'name':pname})            
        else:
            info.varlist.add('guint64', pname)
            info.codebefore.append(self.before % {'name':pname})            
        info.varlist.add('NspObject', "*nsp_" + pname + ' = NULL')
        info.arglist.append(pname)
        info.add_parselist('obj_check', ['&PyLong_Type', '&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('guint64', 'ret')
        info.codeafter.append('  return PyLong_FromUnsignedLongLong(ret);')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('guint64', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
        
class DoubleArg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('double', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('double', pname)
        info.arglist.append(pname)
        info.add_parselist('s_double', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( DoubleScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('double', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('double', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')

class GSList(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add('GSListdouble', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('GSListdouble', pname)
        info.arglist.append(pname)
        info.add_parselist('GSLists_double', ['&' + pname], [pname])
        info.attrcodebefore.append('GSList  if ( DoubleScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GSListdouble', 'ret')
        info.codeafter.append('GSList  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GSListdouble', 'ret')
        info.varlist.add('GSListNspObject', '*nsp_ret')
        info.attrcodeafter.append('GSList  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')


        
class FileArg(ArgType):
    nulldflt = ('  if ( IsNOne(nsp_%(name)s)\n'
                '      %(name)s = NULL;\n'
                '  else if (nsp_%(name)s && IsFile(nsp_%(name)s)\n'
                '      %(name)s = PyFile_AsFile(nsp_%(name)s);\n'
                '  else if (nsp_%(name)s) {\n'
                '      Scierror( "%(name)s should be a file object or None");\n'
                '      return RET_BUG;\n' 
                '  }')
    null = ('  if (nsp_%(name)s && PyFile_Check(nsp_%(name)s)\n'
            '      %(name)s = PyFile_AsFile(nsp_%(name)s);\n'
            '  else if (nsp_%(name)s != Py_None) {\n'
            '      Scierror( "%(name)s should be a file object or None");\n'
            '      return RET_BUG;\n'
            '  }\n')
    dflt = ('  if (nsp_%(name)s)\n'
            '      %(name)s = PyFile_AsFile(nsp_%(name)s);\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pnull:
	    if pdflt:
		info.varlist.add('FILE', '*' + pname + ' = ' + pdflt)
		info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
		info.codebefore.append(self.nulldflt % {'name':pname})
	    else:
		info.varlist.add('FILE', '*' + pname + ' = NULL')
		info.varlist.add('NspObject', '*nsp_' + pname)
		info.codebefore.append(self.null & {'name':pname})
            info.arglist.append(pname)
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
	else:
	    if pdflt:
		info.varlist.add('FILE', '*' + pname + ' = ' + pdflt)
		info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
		info.codebefore.append(self.dflt % {'name':pname})
		info.arglist.append(pname)
	    else:
		info.varlist.add('NspObject', '*' + pname)
		info.arglist.append('PyFile_AsFile(' + pname + ')')
            info.add_parselist('obj_check', ['&PyFile_Type', '&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
	info.varlist.add('FILE', '*ret')
        info.codeafter.append('  if (ret == NULL) return NULL;\n' +
                              '  return PyFile_FromFile(ret, "", "", fclose);\n')
                
    def attr_write_return(self, ptype, ownsreturn, info):
	info.varlist.add('FILE', '*ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  if (ret == NULL) return NULL;\n' +
                                  '  return PyFile_FromFile(ret, "", "", fclose);\n')
        
class EnumArg(ArgType):
    enum = ('  if (nspg_enum_get_value(%(typecode)s, nsp_%(name)s, &%(name)s)== FAIL)\n'
            '      return RET_BUG;\n')
    def __init__(self, enumname, typecode):
	self.enumname = enumname
	self.typecode = typecode
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add(self.enumname, pname + ' = ' + pdflt)
	else:
	    info.varlist.add(self.enumname, pname)
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	info.codebefore.append(self.enum % { 'typecode': self.typecode,
                                             'name': pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname]);
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gint', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gint', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
                                

class FlagsArg(ArgType):
    flag = ('  if (%(default)snspg_flags_get_value(%(typecode)s, nsp_%(name)s, &%(name)s)==FAIL)\n'
            '      return RET_BUG;\n')
    def __init__(self, flagname, typecode):
	self.flagname = flagname
	self.typecode = typecode
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pdflt:
	    info.varlist.add(self.flagname, pname + ' = ' + pdflt)
            default = "nsp_%s && " % (pname,)
	else:
	    info.varlist.add(self.flagname, pname)
            default = ""
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
        info.codebefore.append(self.flag % {'default':default,
                                            'typecode':self.typecode,
                                            'name':pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('guint', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('guint', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')

class ObjectArg(ArgType):
    # should change these checks to more typesafe versions that check
    # a little further down in the class heirachy.
    nulldflt = ('  if ( nsp_%(name)s != NULL ) {\n'
                '    if ( Is%(type)s((NspObject *)nsp_%(name)s))\n'
                '      %(name)s = %(cast)s(nsp_%(name)s->obj);\n' 
                '    else if (! IsNone((NspObject *)nsp_%(name)s)) {\n' 
                '         Scierror( "%(name)s should be a %(type)s or None");\n'
                '         return RET_BUG;\n'
                '    }\n'
                '  }\n') 
    null = ('  if ( Is%(type)s((NspObject *)nsp_%(name)s))\n'
            '      %(name)s = %(cast)s(nsp_%(name)s->obj);\n'
            '  else if ( ! IsNone((NspObject *) nsp_%(name)s))  {\n'
            '      Scierror( "%(name)s should be a %(type)s or None");\n'
            '      return RET_BUG;\n'
            '  }\n')
    dflt = '  if (nsp_%(name)s)\n' \
           '      %(name)s = %(cast)s(nsp_%(name)s->obj);\n'
    def __init__(self, objname, parent, typecode):
	self.objname = objname
	self.cast = string.replace(typecode, '_TYPE_', '_', 1)
        self.parent = parent
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pnull:
	    if pdflt:
		info.varlist.add(self.objname, '*' + pname + ' = ' + pdflt)
		info.varlist.add('NspGObject', '*nsp_' + pname + ' = NULL')
		info.codebefore.append(self.nulldflt % {'name':pname,
                                                        'cast':self.cast,
                                                        'type':self.objname}) 
	    else:
		info.varlist.add(self.objname, '*' + pname + ' = NULL')
		info.varlist.add('NspGObject', '*nsp_' + pname)
		info.codebefore.append(self.null % {'name':pname,
                                                    'cast':self.cast,
                                                    'type':self.objname}) 
            info.arglist.append(pname)
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
	else:
	    if pdflt:
		info.varlist.add(self.objname, '*' + pname + ' = ' + pdflt)
		info.varlist.add('NspGObject', '*nsp_' + pname + ' = NULL')
		info.codebefore.append(self.dflt % {'name':pname,
                                                    'cast':self.cast}) 
		info.arglist.append(pname)
                info.add_parselist('obj_check', ['&nsp_type_%s' % string.lower(self.objname),
                                         '&nsp_' + pname], [pname])
	    else:
		info.varlist.add('NspGObject', '*' + pname)
		info.arglist.append('%s(%s->obj)' % (self.cast, pname))
                info.add_parselist('obj_check', ['&nsp_type_%s' % string.lower(self.objname),
                                          '&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*': ptype = ptype[:-1]
        info.varlist.add(ptype, '*ret')
        if ownsreturn:
            info.varlist.add('NspObject', '*nsp_ret')
            info.codeafter.append('  nsp_type_%(name)s = new_type_%(name)s(T_BASE);\n' 
                                  '  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,' 
                                  '(NspTypeBase *) nsp_type_%(name)s))== NULL) return RET_BUG;\n' 
                                  '  g_object_unref(ret);\n'  
                                  '  MoveObj(stack,1,nsp_ret);\n  return 1;' %  {'name': string.lower(self.objname)} )
        else:
            info.varlist.add('NspObject', '*nsp_ret')
            info.codeafter.append('  nsp_type_%(name)s = new_type_%(name)s(T_BASE);\n'  
                                  '  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,' 
                                  '(NspTypeBase *) nsp_type_%(name)s))== NULL) return RET_BUG;\n'  
                                  '  MoveObj(stack,1,nsp_ret);\n  return 1;' %   {'name': string.lower(self.objname)} )            
    def attr_write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*': ptype = ptype[:-1]
        info.varlist.add(ptype, '*ret')
        if ownsreturn:
            info.varlist.add('NspObject', '*nsp_ret')
            info.attrcodeafter.append('  nsp_type_%(name)s = new_type_%(name)s(T_BASE);\n' 
                                  '  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,'
                                  '(NspTypeBase *) nsp_type_%(name)s))== NULL) return NULL;\n'  
                                  '  g_object_unref(ret);\n'  
                                  '  return nsp_ret;' %  {'name': string.lower(self.objname)} )

        else:
            info.attrcodeafter.append('  nsp_type_%(name)s = new_type_%(name)s(T_BASE);\n'  
                                      '  return (NspObject *) gobject_create(NVOID,(GObject *)ret,' 
                                  '(NspTypeBase *) nsp_type_%(name)s);' %  {'name': string.lower(self.objname)} )


class BoxedArg(ArgType):
    # haven't done support for default args.  Is it needed?
    check = ('  if (nspg_boxed_check(nsp_%(name)s, %(typecode)s))\n'
             '      %(name)s = nspg_boxed_get(nsp_%(name)s, %(typename)s);\n'
             '  else {\n'
             '      Scierror( "%(name)s should be a %(typename)s");\n'
             '      return RET_BUG;\n'
             '  }\n')
    null = ('  if (nspg_boxed_check(nsp_%(name)s, %(typecode)s))\n'
            '      %(name)s = nspg_boxed_get(nsp_%(name)s, %(typename)s);\n'
            '  else if (! IsNone(nsp_%(name)s)) {\n'
            '      Scierror("%(name)s should be a %(typename)s or None");\n'
            '      return RET_BUG;\n'
            '  }\n')
    def __init__(self, ptype, typecode):
	self.typename = ptype
	self.typecode = typecode
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pnull:
            info.varlist.add(self.typename, '*' + pname + ' = NULL')
	    info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	    info.codebefore.append(self.null % {'name':  pname,
                                                'typename': self.typename,
                                                'typecode': self.typecode})
	else:
            info.varlist.add(self.typename, '*' + pname + ' = NULL')
	    info.varlist.add('NspObject', '*nsp_' + pname)
	    info.codebefore.append(self.check % {'name':  pname,
                                                 'typename': self.typename,
                                                 'typecode': self.typecode})
        if ptype[-1] == '*':
            typename = ptype[:-1]
            if typename[:6] == 'const-': typename = typename[6:]
            if typename != self.typename:
                info.arglist.append('(%s *)%s' % (ptype[:-1], pname))
            else:
                info.arglist.append(pname)
        else:
            info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*':
            info.varlist.add(self.typename, '*ret')
            ret = 'ret'
        else:
            info.varlist.add(self.typename, 'ret')
            ret = '&ret'
            ownsreturn = 0 # of course it can't own a ref to a local var ...
        info.varlist.add('NspObject','*nsp_ret');
        ret_tmpl = '  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,%(typecode)s, %(ret)s, %(copy)s, TRUE,\n' \
                   '                                             (NspTypeBase *) %(nsptype)s))== NULL)\n'\
                   '    return RET_BUG;\n' \
                   '  MoveObj(stack,1,nsp_ret);\n' \
                   '  return 1;'
        
        info.codeafter.append(ret_tmpl %
                              { 'typecode': self.typecode,
                                'ret': ret,
                                'copy': ownsreturn and 'FALSE' or 'TRUE',
                                'nsptype': 'nsp_type_'+ string.lower(self.typename) })
    def attr_write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*':
            info.varlist.add(self.typename, '*ret')
            ret = 'ret'
        else:
            info.varlist.add(self.typename, 'ret')
            ret = '&ret'
            ownsreturn = 0 # of course it can't own a ref to a local var ...
        ret_tmpl = '  /* nspg_boxed_new handles NULL checking */\n' \
                   '  return (NspObject *) gboxed_create(NVOID,%(typecode)s, %(ret)s, %(copy)s, TRUE,(NspTypeBase *) %(nsptype)s);'
        info.attrcodeafter.append(ret_tmpl %
                              { 'typecode': self.typecode,
                                'ret': ret,
                                'copy': ownsreturn and 'FALSE' or 'TRUE',
                                'nsptype': 'nsp_type_'+ string.lower(self.typename) })

class CustomBoxedArg(ArgType):
    # haven't done support for default args.  Is it needed?
    null = ('  if (%(check)s(nsp_%(name)s))\n'
            '      %(name)s = %(get)s(nsp_%(name)s);\n'
            '  else if ( ! IsNone(nsp_%(name)s)) {\n'
            '      Scierror( "%(name)s should be a %(type)s or None");\n'
            '      return RET_BUG;\n'
            '  }\n')
    def __init__(self, ptype, pytype, getter, new):
	self.pytype = pytype
	self.getter = getter
        self.checker = 'Py' + ptype + '_Check'
	self.new = new
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pnull:
            info.varlist.add(ptype[:-1], '*' + pname + ' = NULL')
	    info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	    info.codebefore.append(self.null % {'name':  pname,
                                                'get':   self.getter,
                                                'check': self.checker,
                                                'type':  ptype[:-1]})
	    info.arglist.append(pname)
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
	else:
	    info.varlist.add('NspObject', '*' + pname)
	    info.arglist.append(self.getter + '(' + pname + ')')
            info.add_parselist('obj_check', ['&' + self.pytype, '&' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add(ptype[:-1], '*ret')
        info.codeafter.append('  if (ret == NULL) return RET_BUG;\n' +
                              '  return ' + self.new + '(ret);')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add(ptype[:-1], '*ret')
        info.attrcodeafter.append('  return (ret == NULL) ? NULL : ' + self.new + '(ret);')
        
class PointerArg(ArgType):
    # haven't done support for default args.  Is it needed?
    check = ('  if (nspg_pointer_check(nsp_%(name)s, %(typecode)s))\n'
             '      %(name)s = nspg_pointer_get(nsp_%(name)s, %(typename)s);\n'
             '  else {\n'
             '      Scierror( "%(name)s should be a %(typename)s");\n'
             '      return RET_BUG;\n'
             '  }\n')
    null = ('  if (nspg_pointer_check(nsp_%(name)s, %(typecode)s))\n'
            '      %(name)s = nspg_pointer_get(nsp_%(name)s, %(typename)s);\n'
            '  else if ( ! IsNone(nsp_%(name)s) ) {\n'
            '      Scierror( "%(name)s should be a %(typename)s or None");\n'
            '      return RET_BUG;\n'
            '  }\n')
    def __init__(self, ptype, typecode):
	self.typename = ptype
	self.typecode = typecode
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pnull:
            info.varlist.add(self.typename, '*' + pname + ' = NULL')
	    info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	    info.codebefore.append(self.null % {'name':  pname,
                                                'typename': self.typename,
                                                'typecode': self.typecode})
	else:
            info.varlist.add(self.typename, '*' + pname + ' = NULL')
	    info.varlist.add('NspObject', '*nsp_' + pname)
	    info.codebefore.append(self.check % {'name':  pname,
                                                 'typename': self.typename,
                                                 'typecode': self.typecode})
        info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*':
            info.varlist.add(self.typename, '*ret')
            info.codeafter.append('  /* nspg_pointer_new handles NULL checking */\n' +
                                  '  return nspg_pointer_new(' + self.typecode + ', ret);')
        else:
            info.varlist.add(self.typename, 'ret')
            info.codeafter.append('  /* nspg_pointer_new handles NULL checking */\n' +
                                  '  return nspg_pointer_new(' + self.typecode + ', &ret);')
    def attr_write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*':
            info.varlist.add(self.typename, '*ret')
            info.attrcodeafter.append('  /* nspg_pointer_new handles NULL checking */\n' +
                                  '  return nspg_pointer_new(' + self.typecode + ', ret);')
        else:
            info.varlist.add(self.typename, 'ret')
            info.attrcodeafter.append('  /* nspg_pointer_new handles NULL checking */\n' +
                                  '  return nspg_pointer_new(' + self.typecode + ', &ret);')

class AtomArg(IntArg):
    atom = ('  if ( nsp_gdk_atom_from_object(nsp_%(name)s,&%(name)s)==FAIL) return RET_BUG;\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        info.varlist.add('GdkAtom', pname)
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	info.codebefore.append(self.atom % {'name': pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GdkAtom', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.codeafter.append('  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL);\n' +
                              '    return RET_BUG;\n' +
                              '  MoveObj(stack,1,nsp_ret);\n  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GdkAtom', 'ret')
        info.attrcodeafter.append('  return (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL);');

class GTypeArg(ArgType):
    gtype = ('  if ((%(name)s = nspg_type_from_object(nsp_%(name)s)) == FAIL)\n'
             '      return RET_BUG;\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        info.varlist.add('GType', pname)
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	info.codebefore.append(self.gtype % {'name': pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GType', 'ret')
        info.codeafter.append('  return nspg_type_wrapper_new(ret);')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GType', 'ret')
        info.attrcodeafter.append('  return nspg_type_wrapper_new(ret);')

# simple GError handler.

class GErrorArg(ArgType):
    handleerror = ('  if ( %(name)s != NULL ) {\n'
                   '    Scierror("%%s: gtk error\\n",NspFname(stack));\n'
                   '    return RET_BUG;\n  }\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos): 
        info.varlist.add('GError', '*' + pname + ' = NULL') 
        info.arglist.append('&' + pname)
        info.codeafter.append(self.handleerror % { 'name': pname })

class GtkTreePathArg(ArgType):
    # haven't done support for default args.  Is it needed?
    normal = ('  %(name)s = nsp_gtk_tree_path_from_nspobject(nsp_%(name)s);\n'
              '  if (!%(name)s) {\n'
              '      Scierror( "could not convert %(name)s to a GtkTreePath");\n'
              '      return RET_BUG;\n'
              '  }\n')
    null = ('  if ( ! IsNone(nsp_%(name)s)) {\n'
            '      %(name)s = nsp_gtk_tree_path_from_nspobject(nsp_%(name)s);\n'
            '      if (!%(name)s) {\n'
            '          Scierror( "could not convert %(name)s to a GtkTreePath");\n'
            '          return RET_BUG;\n'
            '      }\n'
            '  }\n')
    null = ('  if (PyTuple_Check(nsp_%(name)s))\n'
            '      %(name)s = nsp_gtk_tree_path_from_nspobject(nsp_%(name)s);\n'
            '  else if ( !IsNone(nsp_%(name)s)) {\n'
            '      Scierror( "%(name)s should be a GtkTreePath or None");\n'
            '      return RET_BUG;\n'
            '  }\n')
    freepath = ('  if (%(name)s)\n'
                '      gtk_tree_path_free(%(name)s);\n')
    def __init__(self):
        pass
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
	if pnull:
            info.varlist.add('GtkTreePath', '*' + pname + ' = NULL')
	    info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	    info.codebefore.append(self.null % {'name':  pname})
	    info.arglist.append(pname)
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
	else:
            info.varlist.add('GtkTreePath', '*' + pname)
	    info.varlist.add('NspObject', '*nsp_' + pname)
            info.codebefore.append(self.normal % {'name': pname})
	    info.arglist.append(pname)
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
        info.codeafter.append(self.freepath % {'name': pname})
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GtkTreePath', '*ret')
        if ownsreturn:
            info.codeafter.append('  if (ret) {\n'
                                  '      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\n'
                                  '      gtk_tree_path_free(ret);\n'
                                  '      return nsp_ret;\n'
                                  '  }\n'
                                  '  return RET_BUG;')
        else:
            info.codeafter.append('  if (ret) {\n'
                                  '      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\n'
                                  '      return nsp_ret;\n'
                                  '  }\n'
                                  '  return RET_BUG;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GtkTreePath', '*ret')
        if ownsreturn:
            info.attrcodeafter.append('  if (ret) {\n'
                                      '      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\n'
                                      '      gtk_tree_path_free(ret);\n'
                                      '      return nsp_ret;\n'
                                      '  }\n'
                                      '  return NULL;')
            
        else:
            info.attrcodeafter.append('  if (ret) {\n'
                                      '      NspObject *nsp_ret = nsp_gtk_tree_path_to_nspobject(ret);\n'
                                      '      return nsp_ret;\n'
                                      '  }\n'
                                      '  return NULL;')
						       
class GdkRectanglePtrArg(ArgType):
    normal = ('  if (!nsp_gdk_rectangle_from_object(nsp_%(name)s, &%(name)s))\n'
              '      return RET_BUG;\n')
    null =   ('  if (nsp_%(name)s == NULL)\n'
              '      %(name)s = NULL;\n'
              '  else if (nsp_gdk_rectangle_from_object(nsp_%(name)s, &%(name)s_rect))\n'
              '      %(name)s = &%(name)s_rect;\n'
              '  else\n'
              '          return RET_BUG;\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        if pnull:
            info.varlist.add('GdkRectangle', pname + '_rect = { 0, 0, 0, 0 }')
            info.varlist.add('GdkRectangle', '*' + pname)
            info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
            info.arglist.append(pname)
            info.codebefore.append(self.null % {'name':  pname})
        else:
            info.varlist.add('GdkRectangle', pname + ' = { 0, 0, 0, 0 }')
            info.varlist.add('NspObject', '*nsp_' + pname)
            info.add_parselist('obj', ['&nsp_' + pname], [pname])
            info.arglist.append('&' + pname)
            info.codebefore.append(self.normal % {'name':  pname})

class GdkRectangleArg(ArgType):
    def write_return(self, ptype, ownsreturn, info):
	info.varlist.add('GdkRectangle', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
	info.codeafter.append('  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE,&ret, TRUE, TRUE,NULL))==NULL)\n' +
                              '    return RET_BUG;\n  MoveObj(stack,1,nsp_ret);\n  return 1;');

    def attr_write_return(self, ptype, ownsreturn, info):
	info.varlist.add('GdkRectangle', 'ret')
        info.attrcodeafter.append('  return (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &ret, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkrectangle);')

class NspObjectArg(ArgType):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        info.varlist.add('NspObject', '*' + pname)
        info.add_parselist('obj', ['&' + pname], [pname])
        info.arglist.append(pname)
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add("NspObject", "*ret")
        if ownsreturn:
            info.codeafter.append('  if (ret == NULLOBJ ) return RET_BUG;\n'
                                  '  MoveObj(stack,1,ret);' )
        else:
            info.codeafter.append('  if (ret == NULLOBJ ) return RET_BUG;\n'
                                  '  MoveObj(stack,1,ret);' )
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add("NspObject", "*ret")
        if ownsreturn:
            info.attrcodeafter.append(' return ret;')
        else:
            info.attrcodeafter.append(' return ret;')

# added for nsp : matrix
# -------------------------
    
class NspMatArg(ArgType):
    def write_param_gen(self, ptype, pname, pdflt, pnull, psize,info, pos, nsp_type):
	if pdflt:
	    info.varlist.add('NspMatrix', '*' + pname + ' = ' + pdflt)
	else:
	    info.varlist.add('NspMatrix', '*' + pname)
	info.arglist.append(pname)
        info.add_parselist(nsp_type, ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( IsMat(O) == FAIL) return FAIL;\n')
        #pos gives the position of the argument
        if psize:
            info.codebefore.append(' %s << size %s\n' % (pname,psize) )
        info.codebefore.append(' %s << %d\n' % (pname,pos) )
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize,info,pos, 'mat')
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('NspMatrix', '*ret')
        info.codeafter.append('  if ( ret == NULLMAT) return RET_BUG;\n'
                              '  MoveObj(stack,1,NSP_OBJECT(ret));\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info):
        info.varlist.add('NspMatrix', '*ret')
        info.attrcodeafter.append('  return ret;')

class NspMatCopyArg(NspMatArg):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize,info,pos,'matcopy')

class NspDoubleArrayArg(NspMatArg):
    def write_param_gen(self, ptype, pname, pdflt, pnull, psize,info, pos, nsp_type):
	if pdflt:
	    info.varlist.add('NspMatrix', '*' + pname + ' = ' + pdflt)
	else:
	    info.varlist.add('NspMatrix', '*' + pname)
	info.arglist.append(pname+'->R')
        info.add_parselist(nsp_type, ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( IsMat(O) == FAIL) return FAIL;\n')
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize,info,pos,'mat')

class NspDoubleArrayCopyArg(NspDoubleArrayArg):
    def write_param(self, ptype, pname, pdflt, pnull, psize,info, pos):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize, info,pos,'matcopy')

class ArgMatcher:
    def __init__(self):
	self.argtypes = {}

    def register(self, ptype, handler):
	self.argtypes[ptype] = handler
    def register_enum(self, ptype, typecode):
        if typecode is None:
            typecode = "G_TYPE_NONE"
        self.register(ptype, EnumArg(ptype, typecode))
    def register_flag(self, ptype, typecode):
        if typecode is None:
            typecode = "G_TYPE_NONE"
	self.register(ptype, FlagsArg(ptype, typecode))
    def register_object(self, ptype, parent, typecode):
        oa = ObjectArg(ptype, parent, typecode)
        self.register(ptype, oa)  # in case I forget the * in the .defs
	self.register(ptype+'*', oa)
        if ptype == 'GdkPixmap':
            # hack to handle GdkBitmap synonym.
            self.register('GdkBitmap', oa)
            self.register('GdkBitmap*', oa)
    def register_boxed(self, ptype, typecode):
        if self.argtypes.has_key(ptype): return
        arg = BoxedArg(ptype, typecode)
        self.register(ptype, arg)
	self.register(ptype+'*', arg)
        self.register('const-'+ptype+'*', arg)
    def register_custom_boxed(self, ptype, pytype, getter, new):
        arg = CustomBoxedArg(ptype, pytype, getter, new)
	self.register(ptype+'*', arg)
        self.register('const-'+ptype+'*', arg)
    def register_pointer(self, ptype, typecode):
        arg = PointerArg(ptype, typecode)
        self.register(ptype, arg)
	self.register(ptype+'*', arg)
        self.register('const-'+ptype+'*', arg)

    def get(self, ptype):
        try:
            return self.argtypes[ptype]
        except KeyError:
            if ptype[:8] == 'GdkEvent' and ptype[-1] == '*':
                return self.argtypes['GdkEvent*']
            raise
    def object_is_a(self, otype, parent):
        if otype == None: return 0
        if otype == parent: return 1
        if not self.argtypes.has_key(otype): return 0
        return self.object_is_a(self.get(otype).parent, parent)

matcher = ArgMatcher()

arg = NoneArg()
matcher.register(None, arg)
matcher.register('none', arg)

arg = StringArg()
matcher.register('char*', arg)
matcher.register('gchar*', arg)
matcher.register('const-char*', arg)
matcher.register('char-const*', arg)
matcher.register('const-gchar*', arg)
matcher.register('gchar-const*', arg)
matcher.register('string', arg)
matcher.register('static_string', arg)

arg = UCharArg()
matcher.register('unsigned-char*', arg)
matcher.register('const-guchar*', arg)
matcher.register('guchar*', arg)

arg = CharArg()
matcher.register('char', arg)
matcher.register('gchar', arg)
matcher.register('guchar', arg)

arg = GUniCharArg()
matcher.register('gunichar', arg)

arg = IntArg()
matcher.register('int', arg)
matcher.register('gint', arg)
matcher.register('guint', arg)
matcher.register('short', arg)
matcher.register('gshort', arg)
matcher.register('gushort', arg)
matcher.register('long', arg)
matcher.register('glong', arg)
matcher.register('gsize', arg)
matcher.register('gssize', arg)
matcher.register('guint8', arg)
matcher.register('gint8', arg)
matcher.register('guint16', arg)
matcher.register('gint16', arg)
matcher.register('gint32', arg)

# nsp 
arg= NspMatArg()
matcher.register('mat', arg)

arg= NspMatCopyArg()
matcher.register('matcopy', arg)

arg = NspDoubleArrayArg()
matcher.register('double[]', arg)

arg = NspDoubleArrayCopyArg()
matcher.register('const double[]', arg)

arg = IntPointerArg()
matcher.register('int*', arg)
matcher.register('gint*', arg)

arg = BoolArg()
matcher.register('gboolean', arg)

arg = TimeTArg()
matcher.register('time_t', arg)

# If the system maxint is smaller than unsigned int, we need to use
# Long objects with PyLong_AsUnsignedLong
if sys.maxint >= (1L << 32):
    matcher.register('guint32', arg)
else:
    arg = ULongArg()
    matcher.register('guint32', arg)

arg = ULongArg()
matcher.register('gulong', arg)

arg = Int64Arg()
matcher.register('gint64', arg)
matcher.register('long-long', arg)

arg = UInt64Arg()
matcher.register('guint64', arg)
matcher.register('unsigned-long-long', arg)

arg = DoubleArg()
matcher.register('double', arg)
matcher.register('gdouble', arg)
matcher.register('float', arg)
matcher.register('gfloat', arg)

arg = FileArg()
matcher.register('FILE*', arg)

# enums, flags, objects

## matcher.register('GSList*', GSList())

matcher.register('GdkAtom', AtomArg())

matcher.register('GType', GTypeArg())
matcher.register('GtkType', GTypeArg())

matcher.register('GError**', GErrorArg())
matcher.register('GtkTreePath*', GtkTreePathArg())
matcher.register('GdkRectangle*', GdkRectanglePtrArg())
matcher.register('GtkAllocation*', GdkRectanglePtrArg())
matcher.register('GdkRectangle', GdkRectangleArg())
matcher.register('NspObject*', NspObjectArg())

matcher.register('GdkNativeWindow', ULongArg())

matcher.register_object('GObject', None, 'G_TYPE_OBJECT')

del arg
