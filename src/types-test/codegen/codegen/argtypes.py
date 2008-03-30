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
        self.attrcodecopy = []
        self.arglist = []
        self.kwlist = []
        self.tylist = []
        self.setobj = 'f'

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
    def get_attrcodecopy(self):
        return string.join(self.attrcodecopy, '')
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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
	"""Add code to the WrapperInfo instance to handle parameter."""
	raise RuntimeError, "write_param not implemented for %s" % \
              self.__class__.__name__

    def write_return(self, ptype, ownsreturn, info):
	"""Adds a variable named ret of the return type to
	info.varlist, and add any required code to info.codeafter to
	convert the return value to a python object."""
        return '  XXXXX write_return not implemented for %s\n' % self.__class__.__name__
	#raise RuntimeError, "write_return not implemented for %s" % self.__class__.__name__
        
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
	"""Add code for the setter handler of a field."""
	raise RuntimeError, "write_param not implemented for %s" % \
              self.__class__.__name__

    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
	"""Adds a variable named ret of the return type to
	info.varlist, and add any required code to info.codeafter to
	convert the return value to a python object."""
        return '  XXXXX attr_write_return not implemented for %s\n' % self.__class__.__name__
        #raise RuntimeError, "write_return not implemented for %s" % self.__class__.__name__

    def attr_write_copy(self, pname, left_varname,right_varname,byref, pdef , psize, pcheck):
	"""used when a variable is to be copied """
        return '  XXXXX write_copy not implemented for %s' % self.__class__.__name__
	#raise RuntimeError, "write_copy not implemented for %s" % self.__class__.__name__

    def attr_write_save(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be saved """
        return '  XXXXX attr_write_save not implemented for %s\n' % self.__class__.__name__
	# raise RuntimeError, "attr_write_save not implemented for %s" % \
    
    def attr_write_load(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return '  XXXXX attr_write_load not implemented for %s\n' % self.__class__.__name__

    def attr_write_info(self,pname, varname,byref):
	"""used when a field is to be reloaded """
        return '  XXXXX attr_write_info not implemented for %s\n' % self.__class__.__name__

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return '  XXXXX attr_write_print not implemented for %s\n' % self.__class__.__name__

    def attr_write_init(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return '  XXXXX attr_write_init not implemented for %s\n' % self.__class__.__name__

    def attr_check_null(self,pname, varname,byref):
	"""used to check if  a field is set """
        return '  XXXXX attr_write_check_null not implemented for %s\n' % self.__class__.__name__

    def attr_free_fields(self,pname, varname,byref):
	"""used to free allocated fields  """
        return '  XXXXX attr_free_fields not implemented for %s\n' % self.__class__.__name__

    def attr_equal_fields(self,pname, varname,byref, pdef , psize, pcheck):
	"""used to test fields equality  """
        return '  XXXXX attr_equal_fields not implemented for %s\n' % self.__class__.__name__ 

    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return '  XXXXX attr_write_defval not implemented for %s\n' % self.__class__.__name__ 

class NoneArg(ArgType):
    def write_return(self, ptype, ownsreturn, info):
        info.codeafter.append('  return 0;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.attrcodeafter.append('  return NULLOBJ;')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class StringArg(ArgType):

    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
        info.attrcodebefore.append('  if ((%s = nsp_string_object(O))==NULL) return FAIL;\n' % pname)
        info.attrcodebefore.append('  if ((%s = nsp_string_copy(%s)) ==NULL) return FAIL;\n' % (pname,pname))
        info.attrcodebefore.append('  nsp_string_destroy(&((%s *) self)->obj->%s);\n' % (upinfo,pname))

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

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
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck): 
        info.varlist.add('NspObject', '*nsp_ret')
        if ownsreturn:
	    # have to free result ...
	    info.varlist.add('gchar', '*ret')
            info.attrcodeafter.append('  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);\n  g_free(ret);\n  return nsp_ret;')
	else:
	    info.varlist.add('const gchar', '*ret')
            info.attrcodeafter.append('  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);\n  return nsp_ret;')


    def attr_free_fields(self,pname, varname,byref):
	"""used to free allocated fields  """
        return  '  nsp_string_destroy(&(%s->%s));\n' % (varname,pname)

            
    def attr_write_save(self,pname, varname,byref, pdef , psize, pcheck):
        return '  if (nsp_xdr_save_string(xdrs,%s->%s) == FAIL) return FAIL;\n' % (varname,pname)

    def attr_write_load(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return '  if (nsp_xdr_load_new_string(xdrs,&(%s->%s)) == FAIL) return NULL;\n'  % (varname,pname)
    
    def attr_write_copy(self, pname, left_varname,right_varname,byref, pdef , psize, pcheck):
	"""used when a variable is to be copied """
        if right_varname:
            return '  if ((%s->%s = nsp_string_copy(%s->%s)) == NULL) return NULL;\n' % (left_varname,pname,right_varname,pname)
        else:
            return '  if ((%s->%s = nsp_string_copy(%s)) == NULL) return NULL;\n' % (left_varname,pname,pname)

    def attr_write_info(self,pname, varname,byref):
	"""used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s=%%s\\n",%s->%s);\n' % (pname,varname,pname)

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
        """used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s=%%s\\n",%s->%s);\n' % (pname,varname,pname)

    def attr_write_init(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return  '  %s->%s = NULL;\n' % (varname,pname)

    def attr_check_null(self,pname, varname,byref):
	"""used to check if  a field is set """
        return '  if ( %s->%s == NULLSTRING) {Scierror("Error: field %s is to be set\\n");return RET_BUG;}\n' % (varname,pname,pname);

    def attr_equal_fields(self,pname, varname,byref, pdef , psize, pcheck):
	"""used to test fields equality  """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  if ( strcmp(A->%s,loc->%s) != 0) return FALSE;\n' % (pname,pname)

    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        str = '  if ( %s->%s == NULL) \n    {\n' % (varname,pname);    
        str = str + '     if (( %s->%s = nsp_string_copy("")) == NULL)\n       return FAIL;\n    }\n' \
            % (varname,pname)
        return str



class UCharArg(ArgType):
    # allows strings with embedded NULLs.
    # XXXXX : to be implemented ...
    def write_param(self,upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
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

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class CharArg(ArgType):
    # a char argument is an int at nsp level 
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('int', pname + " = '" + pdflt + "'")
	else:
	    info.varlist.add('int', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('int', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''
        
class GUniCharArg(ArgType):
    param_tmpl = '  %(name)s = (gunichar)nsp_%(name)s;\n'
    dflt_tmpl = ('  if (nsp_%(name)s != NULL) {\n'
                 '      if (nsp_%(name)s[1] != 0) {\n'
                 '          Scierror( "%(name)s should be a 1 character unicode string");\n'
                 '          return RET_BUG;\n'
                 '      }\n'
                 '      %(name)s = (gunichar)nsp_%(name)s[0];\n'
                 '   }\n')
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('gunichar', pname + " = '" + pdflt + "'")
            info.codebefore.append(self.dflt_tmpl % {'name':pname})
	else:
	    info.varlist.add('gunichar', pname)
            info.codebefore.append(self.param_tmpl % {'name':pname})
        info.varlist.add('int', 'nsp_' + pname + ' = 0')
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&nsp_' + pname], [pname])

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gunichar', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)== FAIL)return RET_BUG;\n  return 1;')
        
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('gunichar', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class IntArg(ArgType):
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('int', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('int', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( IntScalar(O,&' + pname + ') == FAIL) return FAIL;\n')

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('int', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')

    def attr_write_copy(self,pname, left_varname,right_varname,byref, pdef , psize, pcheck):
        if right_varname:
            return '  '+ left_varname + '->'+ pname +'='+ right_varname +'->'+ pname +';\n'
        else:
            return '  '+ left_varname + '->'+ pname +'='+ pname +';\n'

    def attr_write_save(self,pname, varname,byref, pdef , psize, pcheck):
        return '  if (nsp_xdr_save_i(xdrs, %s->%s) == FAIL) return FAIL;\n' % (varname,pname)
    
    def attr_write_load(self,pname, varname,byref, pdef , psize, pcheck):
        return '  if (nsp_xdr_load_i(xdrs, &%s->%s) == FAIL) return NULL;\n' % (varname,pname)

    def attr_write_info(self,pname, varname,byref):
	"""used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s=%%d\\n",%s->%s);\n' % (pname,varname,pname)

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
        """used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s=%%d\\n",%s->%s);\n' % (pname,varname,pname)

    def attr_write_init(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be initialized """
        if pdef == 'no': 
            return '  %s->%s = 0;\n' % (varname,pname)
        else: 
            return '  %s->%s = %s;\n' % (varname,pname,pdef)

    def attr_check_null(self,pname, varname,byref):
	"""used to check if  a field is set """
        return ''

    def attr_free_fields(self,pname, varname,byref):
        return  ''

    def attr_equal_fields(self,pname, varname,byref, pdef , psize, pcheck):
	"""used to test fields equality  """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  if ( A->%s != loc->%s) return FALSE;\n' % (pname,pname)
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''
    
class IntPointerArg(ArgType):
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('int', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('int', pname)
	info.arglist.append('&' + pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( IntScalar(O,&' + pname + ') == FAIL) return FAIL;\n')

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', '*ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) *ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('int', '*ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) *ret);')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''
        
class BoolArg(IntArg):
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('int', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('int', pname)
	info.arglist.append(pname)
        info.add_parselist('s_bool', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( BoolScalar(O,&' + pname + ') == FAIL) return FAIL;\n')

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('int', 'ret')
        info.codeafter.append('  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
        info.attrcodeafter.append('  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);\n  return nsp_ret;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('int', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);\n  return nsp_ret;')
    def attr_write_info(self,pname, varname,byref):
	"""used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s\t= %%s\\n", ( %s->%s == TRUE) ? "T" : "F" );\n' % (pname,varname,pname)

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
        """used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s\t= %%s\\n", ( %s->%s == TRUE) ? "T" : "F" );\n' % (pname,varname,pname)


    def attr_write_init(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be initialized """
        if pdef == 'no': 
            return '  %s->%s = TRUE;\n' % (varname,pname)
        else: 
            return '  %s->%s = %s;\n' % (varname,pname,pdef)

    def attr_check_null(self,pname, varname,byref):
	"""used to check if  a field is set """
        return ''

    def attr_free_fields(self,pname, varname,byref):
        return  ''
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class TimeTArg(ArgType):
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('time_t', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('time_t', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('time_t', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('time_t', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class ULongArg(ArgType):
    dflt = '  if (nsp_%(name)s)\n' \
           '      %(name)s = PyLong_AsUnsignedLong(nsp_%(name)s);\n'
    before = '  %(name)s = PyLong_AsUnsignedLong(nsp_%(name)s);\n'
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('gulong', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('gulong', pname)
	info.arglist.append(pname)
        info.add_parselist('s_int', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( ULongScalar(O,&' + pname + ') == FAIL) return FAIL;\n')

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gulong', 'ret')
        info.codeafter.append(' if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;\n' +
                              '  return 1;');
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('gulong', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class Int64Arg(ArgType):
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('gint64', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('gint64', pname)
	info.arglist.append(pname)
        info.add_parselist('L', ['&' + pname], [pname])

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gint64', 'ret')
        info.codeafter.append('  return PyLong_FromLongLong(ret);')

    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('gint64', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

class UInt64Arg(ArgType):
    dflt = '  if (nsp_%(name)s)\n' \
           '      %(name)s = PyLong_AsUnsignedLongLong(nsp_%(name)s);\n'
    before = '  %(name)s = PyLong_AsUnsignedLongLong(nsp_%(name)s);\n'
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        if pdflt:
            info.varlist.add('guint64', pname + ' = ' + pdflt)
            info.codebefore.append(self.dflt % {'name':pname})            
        else:
            info.varlist.add('guint64', pname)
            info.codebefore.append(self.before % {'name':pname})            
        info.varlist.add('NspObject', "*nsp_" + pname + ' = NULL')
        info.arglist.append(pname)
        info.add_parselist('obj_check', ['&PyLong_Type', '&nsp_' + pname], [pname])
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('guint64', 'ret')
        info.codeafter.append('  return PyLong_FromUnsignedLongLong(ret);')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('guint64', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''
        
class DoubleArg(ArgType):

    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('double', pname + ' = ' + pdflt)
	else:
	    info.varlist.add('double', pname)
        info.arglist.append(pname)
        info.add_parselist('s_double', ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( DoubleScalar(O,&' + pname + ') == FAIL) return FAIL;\n')
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('double', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('double', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.attrcodeafter.append('  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);\n  return nsp_ret;')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

    def attr_write_copy(self,pname, left_varname,right_varname,byref, pdef , psize, pcheck):
        if right_varname:
            return '  '+ left_varname + '->'+ pname +'='+ right_varname +'->'+ pname +';\n'
        else:
            return '  '+ left_varname + '->'+ pname +'='+ pname +';\n'

    def attr_write_save(self,pname, varname,byref, pdef , psize, pcheck):
        return '  if (nsp_xdr_save_d(xdrs, %s->%s) == FAIL) return FAIL;\n' % (varname,pname)
    
    def attr_write_load(self,pname, varname,byref, pdef , psize, pcheck):
        return '  if (nsp_xdr_load_d(xdrs, &%s->%s) == FAIL) return NULL;\n' % (varname,pname)

    def attr_write_info(self,pname, varname,byref):
	"""used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s=%%f\\n",%s->%s);\n' % (pname,varname,pname)

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
        """used when a field is to be reloaded """
        return  '  Sciprintf1(indent+2,"%s=%%f\\n",%s->%s);\n' % (pname,varname,pname)

    def attr_write_init(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be initialized """
        if pdef == 'no': 
            return '  %s->%s = 0.0;\n' % (varname,pname)
        else: 
            return '  %s->%s = %s;\n' % (varname,pname,pdef)

    def attr_check_null(self,pname, varname,byref):
	"""used to check if  a field is set """
        return ''

    def attr_free_fields(self,pname, varname,byref):
        return  ''

    def attr_equal_fields(self,pname, varname,byref, pdef , psize, pcheck):
	"""used to test fields equality  """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  if ( A->%s != loc->%s) return FALSE;\n' % (pname,pname)
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''


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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
	info.varlist.add('FILE', '*ret')
        info.codeafter.append('  if (ret == NULL) return NULL;\n' +
                              '  return PyFile_FromFile(ret, "", "", fclose);\n')
                
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
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
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add(self.enumname, pname + ' = ' + pdflt)
	else:
	    info.varlist.add(self.enumname, pname)
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	info.codebefore.append(self.enum % { 'typecode': self.typecode,
                                             'name': pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname]);
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('gint', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('gint', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''
                                

class FlagsArg(ArgType):
    flag = ('  if (%(default)snspg_flags_get_value(%(typecode)s, nsp_%(name)s, &%(name)s)==FAIL)\n'
            '      return RET_BUG;\n')
    def __init__(self, flagname, typecode):
	self.flagname = flagname
	self.typecode = typecode
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('guint', 'ret')
        info.codeafter.append('  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('guint', 'ret')
        info.attrcodeafter.append('  return nsp_new_double_obj((double) ret);')
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
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
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

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
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add(ptype[:-1], '*ret')
        info.codeafter.append('  if (ret == NULL) return RET_BUG;\n' +
                              '  return ' + self.new + '(ret);')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
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
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        if ptype[-1] == '*':
            info.varlist.add(self.typename, '*ret')
            info.codeafter.append('  /* nspg_pointer_new handles NULL checking */\n' +
                                  '  return nspg_pointer_new(' + self.typecode + ', ret);')
        else:
            info.varlist.add(self.typename, 'ret')
            info.codeafter.append('  /* nspg_pointer_new handles NULL checking */\n' +
                                  '  return nspg_pointer_new(' + self.typecode + ', &ret);')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        info.varlist.add('GdkAtom', pname)
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	info.codebefore.append(self.atom % {'name': pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GdkAtom', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
        info.codeafter.append('  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL);\n' +
                              '    return RET_BUG;\n' +
                              '  MoveObj(stack,1,nsp_ret);\n  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('GdkAtom', 'ret')
        info.attrcodeafter.append('  return (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL);');


class GTypeArg(ArgType):
    gtype = ('  if ((%(name)s = nspg_type_from_object(nsp_%(name)s)) == FAIL)\n'
             '      return RET_BUG;\n')
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        info.varlist.add('GType', pname)
	info.varlist.add('NspObject', '*nsp_' + pname + ' = NULL')
	info.codebefore.append(self.gtype % {'name': pname})
	info.arglist.append(pname)
        info.add_parselist('obj', ['&nsp_' + pname], [pname])
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('GType', 'ret')
        info.codeafter.append('  return nspg_type_wrapper_new(ret);')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('GType', 'ret')
        info.attrcodeafter.append('  return nspg_type_wrapper_new(ret);')

# simple GError handler.

class GErrorArg(ArgType):
    handleerror = ('  if ( %(name)s != NULL ) {\n'
                   '    Scierror("%%s: gtk error\\n",stack.fname);\n'
                   '    return RET_BUG;\n  }\n')
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref): 
        info.varlist.add('GError', '*' + pname + ' = NULL') 
        info.arglist.append('&' + pname)
        info.codeafter.append(self.handleerror % { 'name': pname })
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
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
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
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
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))
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
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
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
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
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

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

class GdkRectangleArg(ArgType):
    def write_return(self, ptype, ownsreturn, info):
	info.varlist.add('GdkRectangle', 'ret')
        info.varlist.add('NspObject', '*nsp_ret')
	info.codeafter.append('  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE,&ret, TRUE, TRUE,NULL))==NULL)\n' +
                              '    return RET_BUG;\n  MoveObj(stack,1,nsp_ret);\n  return 1;');

    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
	info.varlist.add('GdkRectangle', 'ret')
        info.attrcodeafter.append('  return (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &ret, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkrectangle);')

class NspObjectArg(ArgType):
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        info.varlist.add('NspObject', '*' + pname)
        info.add_parselist('obj', ['&' + pname], [pname])
        info.arglist.append(pname)
    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add("NspObject", "*ret")
        if ownsreturn:
            info.codeafter.append('  if (ret == NULLOBJ ) return RET_BUG;\n'
                                  '  MoveObj(stack,1,ret);\n  return 1;' )
        else:
            info.codeafter.append('  if (ret == NULLOBJ ) return RET_BUG;\n'
                                  '  MoveObj(stack,1,ret);\n  return 1;' )
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add("NspObject", "*ret")
        if ownsreturn:
            info.attrcodeafter.append(' return ret;')
        else:
            info.attrcodeafter.append(' return ret;')

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return  '        if ( %s->%s->type->pr(%s->%s,indent+2,"%s",rec_level+1)==FALSE) return FALSE;\n' % (varname,pname,varname,pname,pname)


# generic for NspXXX
# -------------------------
    
class NspGenericArg(ArgType):
    def __init__(self,fullname,shortname,nsp_arg_type):
	self.fullname = fullname
	self.shortname = shortname
        self.shortname_uc = string.upper(shortname)
        self.nsp_arg_type = nsp_arg_type
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
	if pdflt:
	    info.varlist.add('Nsp'+self.fullname, '*' + pname + ' = ' + pdflt)
	else:
	    info.varlist.add('Nsp'+self.fullname, '*' + pname)
        info.setobj = 't' 
	info.arglist.append(pname)
        info.add_parselist(self.nsp_arg_type, ['&' + pname], [pname])
        info.attrcodebefore.append('  if ( ! Is%s(O) ) return FAIL;\n' % self.shortname )
        info.attrcodebefore.append('  if ((%s = (Nsp%s *) nsp_object_copy_and_name(attr,O)) == NULL%s) return FAIL;\n' % (pname,self.fullname,self.shortname_uc))
        if byref == 't' :
            info.attrcodebefore.append('  if (((%s *) self)->obj->%s != NULL ) \n' % (upinfo,pname))
            info.attrcodebefore.append('    nsp_%s_destroy(((%s *) self)->obj->%s);\n' % ( string.lower(self.fullname),upinfo,pname))
        else:
            info.attrcodebefore.append('  if (((%s *) self)->%s != NULL ) \n' % (upinfo,pname))
            info.attrcodebefore.append('    nsp_%s_destroy(((%s *) self)->%s);\n' % ( string.lower(self.fullname),upinfo,pname))
        #pos gives the position of the argument
        if psize:
            info.codebefore.append('/*  %s << size %s*/\n' % (pname,psize) )
        info.codebefore.append('/* %s << %d */\n' % (pname,pos) )

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('Nsp'+self.fullname, '*ret')
        info.codeafter.append('  if ( ret == NULL'+self.shortname_uc+') return RET_BUG;\n'
                              '  MoveObj(stack,1,NSP_OBJECT(ret));\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('Nsp'+self.fullname, '*ret')
        info.attrcodeafter.append('  return (NspObject *) ret;')
        info.setobj = 't' 

    def attr_write_save(self,pname, varname,byref, pdef , psize, pcheck):
        return '  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(%s->%s)) == FAIL) return FAIL;\n' % (varname,pname)

    def attr_write_load(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return '  if ((%s->%s =(Nsp%s *) nsp_object_xdr_load(xdrs))== NULL%s) return NULL;\n' % (varname,pname,self.fullname,self.shortname_uc)

    def attr_write_copy(self, pname, left_varname,right_varname,byref, pdef , psize, pcheck):
	"""used when a variable is to be copied """
        if right_varname:
            str =  '  if ( %s->%s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n' % (right_varname,pname,left_varname,pname)
            str = str + '      if ((%s->%s = (Nsp%s *) nsp_object_copy_and_name("%s",NSP_OBJECT(%s->%s))) == NULL%s) return NULL;\n    }\n' % (left_varname,pname,self.fullname,pname,right_varname,pname,self.shortname_uc)
            return str
        else:
            str =  '  if ( %s == NULL )\n    { %s->%s = NULL;}\n  else\n    {\n' % (pname,left_varname,pname)
            str = str + '      if ((%s->%s = (Nsp%s *)  nsp_object_copy_and_name("%s",NSP_OBJECT(%s))) == NULL%s) return NULL;\n    }\n' % (left_varname,pname,self.fullname,pname,pname,self.shortname_uc)
            return str

    def attr_write_info(self,pname, varname,byref):
	"""used when a field is to be reloaded """
        return  '  nsp_object_info(NSP_OBJECT(%s->%s),indent+2,"%s",rec_level+1);\n' % (varname,pname,pname)

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        return  '  if ( %s->%s != NULL)\n    { if ( nsp_object_%s(NSP_OBJECT(%s->%s),indent+2,"%s",rec_level+1)== FALSE ) return FALSE ;\n    }\n' \
            % (varname,pname,print_mode,varname,pname,pname)

    def attr_write_init(self,pname, varname,byref, pdef, psize, pcheck ):
	"""used when a field is to be initialized """
        return '  %s->%s = NULL%s;\n' % (varname,pname,self.shortname_uc);

    def attr_check_null(self,pname, varname,byref):
	"""used to check if  a field is set """
        return '  if ( %s->%s == NULL%s) {Scierror("Error: field %s is to be set\\n");return RET_BUG;}\n' % (varname,pname,self.shortname_uc,pname);
    
    def attr_free_fields(self,pname, varname,byref):
	"""used to free allocated fields  """
        if byref == 't':
            str = '  ' 
        else:
            str = ''
        return  str + '  nsp_%s_destroy(%s->%s);\n' % (string.lower(self.fullname),varname,pname)

    def attr_equal_fields(self,pname, varname,byref, pdef , psize, pcheck):
	"""used to test fields equality  """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  if ( NSP_OBJECT(A->%s)->type->eq(A->%s,loc->%s) == FALSE ) return FALSE;\n' % (pname,pname,pname)

    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
	str = '/* defvalue: %s %s %s %s */\n' % (self.fullname, self.shortname,self.shortname_uc,self.nsp_arg_type)
        return str+ '/* [defvalue for %s %s %s]*/ \n' % (pname, varname,byref)
    

class NspGenericArgMat(NspGenericArg):
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        str = '  if ( %s->%s == NULL%s) \n    {\n' % (varname,pname,self.shortname_uc);    
        str = str + '     if (( %s->%s = nsp_matrix_create("%s",\'r\',0,0)) == NULL%s)\n       return FAIL;\n    }\n' \
            % (varname,pname,pname,self.shortname_uc)
        return str

class NspGenericArgBMat(NspGenericArg):
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        str = '  if ( %s->%s == NULL%s) \n    {\n' % (varname,pname,self.shortname_uc);    
        str = str + '     if (( %s->%s = nsp_bmatrix_create("%s",0,0)) == NULL%s)\n       return FAIL;\n    }\n' \
            % (varname,pname,pname,self.shortname_uc)
        return str

class NspGenericArgList(NspGenericArg):
    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        str = '  if ( %s->%s == NULL%s) \n    {\n' % (varname,pname,self.shortname_uc);    
        str = str + '     if (( %s->%s = nsp_list_create("%s")) == NULL%s)\n       return FAIL;\n    }\n' \
            % (varname,pname,pname,self.shortname_uc)
        return str

# added for nsp : matrix
# -------------------------
    
class NspMatArg(ArgType):
    def write_param_gen(self, ptype, pname, pdflt, pnull, psize,info, pos, nsp_type, byref):
	if pdflt:
	    info.varlist.add('NspMatrix', '*' + pname + ' = ' + pdflt)
	else:
	    info.varlist.add('NspMatrix', '*' + pname)
	info.arglist.append(pname)
        info.add_parselist(nsp_type, ['&' + pname], [pname])
        info.attrcodebefore.append('  if ((%s = (NspMatrix *) nsp_object_copy(O)) == NULLMAT) return FAIL;\n' % pname)
        #pos gives the position of the argument
        if psize:
            info.codebefore.append('/* %s << size %s*/\n' % (pname,psize) )
        info.codebefore.append(' /* %s << %d*/\n' % (pname,pos) )
    def write_param(self, upinfo,ptype, pname, pdflt, pnull, psize,info, pos, byref):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize,info,pos, 'mat',byref)

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

    def write_return(self, ptype, ownsreturn, info):
        info.varlist.add('NspMatrix', '*ret')
        info.codeafter.append('  if ( ret == NULLMAT) return RET_BUG;\n'
                              '  MoveObj(stack,1,NSP_OBJECT(ret));\n'
                              '  return 1;')
    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
        info.varlist.add('NspMatrix', '*ret')
        info.attrcodeafter.append('  return ret;')

class NspMatCopyArg(NspMatArg):
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize,info,pos,'matcopy', byref)

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param(upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

class NspDoubleArrayArg(NspMatArg):

    def write_param_gen(self, ptype, pname, pdflt, pnull, psize,info, pos, nsp_type, byref):
	if pdflt:
	    info.varlist.add('double', '*' + pname + ' = ' + pdflt)
	else:
	    info.varlist.add('double', '*' + pname)
	info.arglist.append(pname+'->R')
        info.add_parselist(nsp_type, ['&' + pname], [pname]) 
        
        info.attrcodebefore.append('  if ( ! IsMat(O) \n' \
                                   '       || ((NspMatrix *) O)->rc_type != \'r\' \n' \
                                   '       || ((NspMatrix *) O)->mn != %s ) \n' \
                                   '     return FAIL;\n' \
                                   '  memcpy(%s, ((NspMatrix *) O)->R,%s*sizeof(double));\n' % (psize,pname,psize) )

    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize,info,pos,'realmat', byref)

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
	"""used to build the field setter """
        info.varlist.add('NspMatrix', '*M=(NspMatrix *) O')
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        info.attrcodebefore.append('  if ( ! IsMat(O) || M->rc_type != \'r\' || M->mn != %s ) \n' \
                                   '     return FAIL;\n' \
                                   '  Mat2double(M);\n' \
                                   '  memcpy(%s, ((NspMatrix *) O)->R,%s*sizeof(double));\n' % (psize,pset_name,psize) )

    def attr_write_init(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        if pdef == 'no': 
            vdef = '{0}'
        else: 
            vdef = pdef
        return '  {\n' \
            '    double x_def[%s]=%s;\n' \
            '    memcpy(%s->%s,x_def,%s*sizeof(double));\n' \
            '  }\n' % (psize,pdef, varname,pname, psize)

    def attr_equal_fields(self,pname, varname,byref, pdef , psize, pcheck):
	"""used to test fields equality  """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  {\n' \
            '    int i;\n' \
            '    for ( i = 0 ; i < %s ; i++ )\n' \
            '      if ( A->%s[i] != loc->%s[i] ) return FALSE;\n' \
            '  }\n' % (psize, pname,pname)

    def attr_write_save(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be saved """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  if ( nsp_xdr_save_array_d(xdrs,M->%s,%s)  == FAIL) return FAIL;\n' % ( pname , psize) 

    def attr_write_load(self,pname, varname,byref, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        if byref == 't' :
            pname = 'obj->'+pname
        return '  if ( nsp_xdr_load_array_d(xdrs,M->%s,%s) == FAIL) return NULL;\n' % ( pname , psize) 

    def attr_free_fields(self,pname, varname,byref):
        return  ''

    def attr_write_copy(self,pname, left_varname,right_varname,byref, pdef , psize, pcheck):
        if right_varname:
            return '  memcpy('+ left_varname + '->'+ pname +','+right_varname +'->'+ pname +','+ psize+ '*sizeof(double));\n'
        else:
            return '  memcpy('+ left_varname + '->'+ pname +','+ pname +','+ psize+ '*sizeof(double));\n'

    def attr_write_defval(self,pname, varname,byref):
	"""used to give a default value  """
        return ''

    def attr_write_print(self,pname, varname,byref,print_mode, pdef , psize, pcheck):
	"""used when a field is to be reloaded """
        if print_mode == 'latex':
            tag = 'latex_' 
        else:
            tag = '' 
        return  '  if ( nsp_print_%sarray_double(indent+2,"%s",%s->%s,%s,rec_level) == FALSE ) return FALSE ;\n' \
            % (tag, pname,varname,pname,psize)

    def attr_write_return(self, ptype, ownsreturn, info,  pdef, psize, pcheck):
	"""used to set a field setter """
        info.varlist.add('double*', 'ret')
        info.attrcodeafter.append('  return NSP_OBJECT(nsp_matrix_create_from_array(NVOID,1,%s,ret,NULL));\n' % ( psize ))

class NspDoubleArrayCopyArg(NspDoubleArrayArg):
    def write_param(self,upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref):
        self.write_param_gen( ptype, pname, pdflt, pnull, psize, info,pos,'matcopy', byref)

    def attr_write_set(self,upinfo, ptype, pname, pdflt, pnull, psize, info, pos, byref):
        if byref == 't' :
            pset_name  ='((%s *) self)->obj->%s' % (upinfo,pname) 
        else:
            pset_name  ='((%s *) self)->%s' % (upinfo,pname) 
        self.write_param( upinfo, ptype, pname, pdflt, pnull, psize,info, pos, byref)
        info.attrcodebefore.append('  %s= %s;\n' % (pset_name,pname))

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

matcher.register('GDateYear', arg)
matcher.register('GDateDay', arg)

# nsp
#

arg = NspGenericArgMat('Matrix','Mat','mat')
matcher.register('NspMatrix*', arg)

arg = NspGenericArgBMat('BMatrix','BMat','bmat')
matcher.register('NspBMatrix*', arg)

arg = NspGenericArgList('List','List','list')
matcher.register('NspList*', arg)

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
matcher.register('boolean', arg)

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
