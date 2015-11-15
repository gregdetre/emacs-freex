#!/usr/bin/env python

### freex_sqlalchemy.py --- interface with the sqlite database
##
## Copyright (C) 2007 Per B. Sederberg, Greg Detre
##
## Author: Per B. Sederberg, Greg Detre
## Keywords: hypermedia
## Date: 
##
## This file is part of Emacs Freex.  It is not part of GNU
## Emacs.
##
## Emacs Freex is free software; you can redistribute it
## and/or modify it under the terms of the GNU General
## Public License as published by the Free Software
## Foundation; either version 2, or (at your option) any
## later version.
##
## Emacs Freex is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the
## implied warranty of MERCHANTABILITY or FITNESS FOR A
## PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public
## License along with Emacs Freex; see the file COPYING.  If
## not, write to the Free Software Foundation, Inc., 51
## Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
##
### Commentary:
##
### Contributors:


import os, sys
import collections
import datetime, time
import string, re
import random
import subprocess
# import numpy as n
# import occurrences
## do we still need numpy???
# from numpy import *
# import scipy as S
# from scipy import sparse
# from nltk_lite import stem
# import cPickle

# ,IPython.ultraTB  
# sys.excepthook = IPython.ultraTB.FormattedTB( \
#    mode='Verbose', color_scheme='Linux', call_pdb=1) 

############################################################
# if you want to run test cases interactively in ipython to
# debug them, use:
#
# from freex_sqlalchemy import * ; fsqa = create_fsqa_test()
#
# you should now just be able to call functions and it will
# run them on the test database
#
# if you want to run the real database interactively:
#
# $ cd Dropbox/elisp/freex/
# >>> from freex_sqlalchemy import * ; fsqa = create_fsqa('/Users/greg/Dropbox/freex/freex.db',False,False)

# if you want to test the regexes
# from freex_sqlalchemy import * ; fsqa = create_fsqa('/Users/greg/Dropbox/freex/freex.db',False,False); update_implicit_link_regexp_temp()

# for tags: src = child, dest = parent, ASSOC_TAG = 1


try:
    from Pymacs import lisp
except:
    print 'No Pymacs'

from sqlalchemy import *
try: from sqlalchemy.orm import *
except: pass

interactions = {}

fsqa = None


####################################
# Some default values
#
# maybe store these as lisp variables, so they're accessible
# from both emacs and python???

# use for assoc types
ASSOC_TAG = 1
ASSOC_IMPLICIT = 2
ASSOC_EXPLICIT = 3
ASSOC_EMBEDDED = 4

# these are like ENUMs that encode the action that just
# occurred in the actions table
ACTION_LOAD_NUGGET = 1
ACTION_SAVE_NUGGET = 2
ACTION_CLOSE_NUGGET = 3
ACTION_ADDALIAS = 4
ACTION_CREATE = 5
ACTION_LOAD_EMBEDDED = 6
ACTION_ADD_TAG_PARENT = 7
ACTION_INSERT = 8
ACTION_MODIFY = 9



############################################################
class Nugget(object):

    def __init__(self, content="",
                 created=datetime.datetime.now(),
                 filename=""):
        self.content = content
	self.created = created
        self.filename = filename

    def __repr__(self):
        return '[Nug id: %d, Filename: %s, Created: %s]' % (self.id, 
                                                            self.filename,
                                                            self.created)


############################################################
class Alias(object):

    def __init__(self,alias,
                 created=datetime.datetime.now()):
        self.alias = alias
	self.created = created

    def __repr__(self):
        return '[Alias: %s]' % (self.alias)



############################################################
class Action(object):
    
    def __init__(self,action):
	self.action = action
        
    def __repr__(self):
	return '[Action: %s]' % (self.action)



############################################################
class NuggetHistory(object):

    def __init__(self,action_id,date=datetime.datetime.now()):
	self.action_id = action_id
	self.date = date

    def __repr__(self):
	return '[NugId: %d, ActionId: %d, Date: (%d,%d,%d)]' % \
               (self.nugid,
                self.action_id,
                self.date.year,
                self.date.month,
                self.date.day)


    
############################################################
class AssocType(object):

    def __init__(self,assoc_type):
	self.assoc_type = assoc_type

    def __repr__(self):
	return '[Assoc Type: %s]' % (self.assoc_type)



############################################################
class TagParent(object):
    def __init__(self,nugid,created=datetime.datetime.now()):
        self.dest_nugid = nugid
        self.assoc_type_id = ASSOC_TAG
        self.created = created
    def __repr__(self):
        return '[TagParent: %d]' % (self.dest_nugid)



############################################################
class TagChild(object):
    def __init__(self,nugid,created=datetime.datetime.now()):
        self.src_nugid = nugid
        self.assoc_type_id = ASSOC_TAG
        self.created = created
    def __repr__(self):
        return '[TagChild: %d]' % (self.src_nugid)



############################################################
class ForwardExplicitLink(object):
    def __init__(self,nugid,created=datetime.datetime.now()):
        self.dest_nugid = nugid
        self.assoc_type_id = ASSOC_EXPLICIT
        self.created = created
    def __repr__(self):
        return '[ForwardExplicitLink: %d]' % (self.dest_nugid)



############################################################
class BackwardExplicitLink(object):
    def __init__(self,nugid,created=datetime.datetime.now()):
        self.src_nugid = nugid
        self.assoc_type_id = ASSOC_EXPLICIT
        self.created = created
    def __repr__(self):
        return '[BackwardExplicitLink: %d]' % (self.src_nugid)



############################################################
class ForwardImplicitLink(object):
    def __init__(self,nugid,created=datetime.datetime.now()):
        self.dest_nugid = nugid
        self.assoc_type_id = ASSOC_IMPLICIT
        self.created = created
    def __repr__(self):
        return '[ForwardImplicitLink: %d]' % (self.dest_nugid)



############################################################
class BackwardImplicitLink(object):
    def __init__(self,nugid,created=datetime.datetime.now()):
        self.src_nugid = nugid
        self.assoc_type_id = ASSOC_IMPLICIT
        self.created = created
    def __repr__(self):
        return '[BackwardImplicitLink: %d]' % (self.src_nugid)



############################################################
class Fsqa(object):

    # core member variables
    database_file = None
    database_dir = None
    # if you change the default freex file extension from
    # freex to something else, store the setting here. this
    # should not have a dot
    file_ext = None
    db = None
    metadata = None
    session = None
    # by default, this assumes that you're running stuff
    # within emacs. if you set this to false, then it should
    # ignore all the lisp.blah() calls (assuming they've
    # been written inside an if statement)
    #
    # don't use booleans because they don't get transmitted
    # well to elisp
    use_lisp = 1
    
    # tables
    nuggets = None
    aliases = None
    actions = None
    nugget_history = None
    assoc_types = None
    nugget_assocs = None


    ############################################################
    def __init__(self, database_file, use_lisp, in_memory, file_ext='freex'):

        # store the filename part
        self.database_file = os.path.basename(
            os.path.abspath(database_file))
        # store the directory part
        self.database_dir = os.path.dirname(
            os.path.abspath(database_file))

        if not len(self.database_file):
            raise 'Can''t use an empty database_file'

        db_fullfile = os.path.join(
            self.database_dir,self.database_file)

        self.use_lisp = use_lisp

        self.in_memory = in_memory

        self.file_ext = file_ext

        # see if the database exists, if not, initialize it
        if not os.path.isfile(db_fullfile):
            # must initialize the database
            runInit = True
        else:
            runInit = False

        if self.in_memory:
            self.db = create_engine('sqlite:///:memory:')
        else:
            # connect to that database
            self.db = create_engine('sqlite:///' + db_fullfile)
            # self.db.echo = True
        
        try: self.metadata = BoundMetaData(self.db)
        except: self.metadata = MetaData(self.db)

        # not sure how best to deal with sessions. making it global
        # seems easiest - but a bad idea
        self.session = create_session()

        if runInit:
            self.initialize_db()
        else:
            self.load_db()

        # create the mappers
        # self.create_mappers()

    

    ############################################################
    def initialize_db(self):

        # Creating tables
        self.nuggets = Table(
            'nuggets',self.metadata,
            # primary key integers automatically autoincrement
            Column('id',Integer,primary_key=True),
            Column('content',String),
            Column('created',DateTime,
                   nullable=False,default=datetime.datetime.now()),
            Column('filename',String,nullable=False,unique=True,index=True),
            )
        self.nuggets.create()

        self.aliases = Table(
            'aliases',self.metadata,
            Column('id',Integer,primary_key=True),
            Column('alias',String,unique=True,index=True),
            Column('nugid',Integer,ForeignKey('nuggets.id'),
                   nullable=False,index=True),
            Column('created',DateTime,
                   nullable=False,default=datetime.datetime.now())
            )
        self.aliases.create()
        
        self.actions = Table(
            'actions',self.metadata,
            Column('id',Integer,primary_key=True),
            Column('action',String(40),nullable=False),
            )
        self.actions.create()
    
        self.nugget_history = Table(
            'nugget_history',self.metadata,
            Column('id',Integer,primary_key=True),
            Column('nugid',Integer,ForeignKey('nuggets.id'),
                   nullable=False),
            Column('action_id',Integer,ForeignKey('actions.id'),
                   nullable=False),
            Column('date',DateTime,
                   nullable=False,default=datetime.datetime.now())
            )
        self.nugget_history.create()
    
        self.assoc_types = Table(
            'assoc_types',self.metadata,
            Column('id',Integer,primary_key=True),
            Column('assoc_type',String(40),nullable=False)
            )
        self.assoc_types.create()

        # for tags: src = child, dest = parent, ASSOC_TAG = 1
        self.nugget_assocs = Table(
            'nugget_assocs',self.metadata,
            Column('id',Integer,primary_key=True),
            Column('src_nugid',Integer,ForeignKey('nuggets.id'),
                   nullable=False,index=True),
            Column('dest_nugid',Integer,ForeignKey('nuggets.id'),
                   nullable=False,index=True),
            Column('assoc_type_id',Integer,ForeignKey('assoc_types.id'),
                   nullable=False,default=ASSOC_TAG),
            Column('created',DateTime,
                   nullable=False,default=datetime.datetime.now())
            )
        self.nugget_assocs.create()



        ############################################################
        # Inserting records
        i = self.actions.insert()
        i.execute({'id':ACTION_INSERT,'action':'Nugget Insert'},
                  {'id':ACTION_MODIFY,'action':'Nugget Modify'},
                  {'id':ACTION_ADDALIAS,'action':'Nugget Add Alias'})
        
        i = self.assoc_types.insert()
        i.execute({'id':ASSOC_TAG,'assoc_type':'Assoc Tag'},
                  {'id':ASSOC_IMPLICIT,'assoc_type':'Assoc Implicit'},
                  {'id':ASSOC_EXPLICIT,'assoc_type':'Assoc Explicit'},
                  {'id':ASSOC_EMBEDDED,'assoc_type':'Assoc Embedded'})
        
        i = self.nuggets.insert()

        i = self.nugget_history.insert()

        i = self.aliases.insert()

        i = self.nugget_assocs.insert()



    ############################################################
    def create_mappers(self):
        # Mapping to objects

        # if we don't have this, it'll try to map things
        # that are already mapped and cause a problem
        clear_mappers()

        mapper(Alias,self.aliases)
        mapper(Action,self.actions)
        mapper(AssocType,self.assoc_types)
        
        mapper(NuggetHistory,self.nugget_history,properties={
                'action': relation(Action) #,cascade="all, delete-orphan")
                })

#         mapper(NuggetNoAssoc,self.nuggets,properties={
#                 'aliases': relation(Alias, 
#                                     cascade="all, delete-orphan"),
#                 'nugget_history': relation(NuggetHistory,
#                                            cascade="all,delete-orphan")
#                 })

        mapper(TagParent,self.nugget_assocs,properties={
                'assoc_type': relation(AssocType),
#                 'parent_nugget': relation(NuggetNoAssoc, 
#                                           primaryjoin=self.nugget_assocs.c.dest_nugid==NuggetNoAssoc.c.id,
#                                           cascade="all,delete-orphan"),
                })
        mapper(TagChild,self.nugget_assocs,properties={
                'assoc_type': relation(AssocType),
#                 'child_nugget': relation(NuggetNoAssoc, 
#                                          primaryjoin=self.nugget_assocs.c.src_nugid==NuggetNoAssoc.c.id,
#                                          cascade="all,delete-orphan"),
                })
        mapper(ForwardImplicitLink,self.nugget_assocs,properties={
                'assoc_type': relation(AssocType),
                })
        mapper(ForwardExplicitLink,self.nugget_assocs,properties={
                'assoc_type': relation(AssocType),
                })
        mapper(BackwardImplicitLink,self.nugget_assocs,properties={
                'assoc_type': relation(AssocType),
                })
        mapper(BackwardExplicitLink,self.nugget_assocs,properties={
                'assoc_type': relation(AssocType),
                })
        
        mapper(Nugget,self.nuggets,properties={
            'aliases': relation(Alias, 
                                cascade="all, delete-orphan"),
            'nugget_history': relation(NuggetHistory,
                                       cascade="all,delete-orphan"),
            'forward_implicit_links': relation(ForwardImplicitLink, 
                                               primaryjoin=and_(self.nuggets.c.id==ForwardImplicitLink.c.src_nugid,
                                                                self.nugget_assocs.c.assoc_type_id==ASSOC_IMPLICIT),
                                               cascade="all,delete-orphan"),
            'backward_implicit_links': relation(BackwardImplicitLink, 
                                                primaryjoin=and_(self.nuggets.c.id==BackwardImplicitLink.c.dest_nugid,
                                                                 self.nugget_assocs.c.assoc_type_id==ASSOC_IMPLICIT),
                                                cascade="all,delete-orphan"),
            'forward_explicit_links': relation(ForwardExplicitLink, 
                                               primaryjoin=and_(self.nuggets.c.id==ForwardExplicitLink.c.src_nugid,
                                                                self.nugget_assocs.c.assoc_type_id==ASSOC_EXPLICIT),
                                               cascade="all,delete-orphan"),
            'backward_explicit_links': relation(BackwardExplicitLink, 
                                                primaryjoin=and_(self.nuggets.c.id==BackwardExplicitLink.c.dest_nugid,
                                                                 self.nugget_assocs.c.assoc_type_id==ASSOC_EXPLICIT),
                                                cascade="all,delete-orphan"),
            'tag_parents': relation(TagParent, 
                                    primaryjoin=and_(self.nuggets.c.id==TagParent.c.src_nugid,
                                                     TagParent.c.assoc_type_id==ASSOC_TAG),
                                    cascade="all,delete-orphan"),
            'tag_children': relation(TagChild, 
                                     primaryjoin=and_(self.nuggets.c.id==TagChild.c.dest_nugid,
                                                      TagChild.c.assoc_type_id==ASSOC_TAG),
                                     cascade="all,delete-orphan")
            })
        


    ############################################################
    def load_db(self):

        # Loading already-created
        self.nuggets = Table('nuggets', self.metadata,
                        autoload=True)
        
        self.actions = Table('actions', self.metadata,
                        autoload=True)
    
        self.nugget_history = Table('nugget_history', self.metadata,
                               autoload=True)

        self.aliases = Table('aliases', self.metadata,
                        autoload=True)

        self.assoc_types = Table('assoc_types',self.metadata,
                            autoload=True)

        self.nugget_assocs = Table('nugget_assocs',self.metadata,
                              autoload=True)
    
    

############################################################
# exceptions
class FreexSqlalchemyError(Exception): pass

class NonexistentNugid(FreexSqlalchemyError): pass

class NonexistentAlias(FreexSqlalchemyError): pass

class NonexistentFilename(FreexSqlalchemyError): pass

class WildcardMatchesMultipleAliases(FreexSqlalchemyError): pass

class NeedsNoExtension(FreexSqlalchemyError): pass

class NeedsExtension(FreexSqlalchemyError): pass

class ShouldBeNoDot(FreexSqlalchemyError): pass

class ShouldBeList(FreexSqlalchemyError): pass

class CantBeList(FreexSqlalchemyError): pass

class ShouldBeStr(FreexSqlalchemyError): pass

class ShouldBeInt(FreexSqlalchemyError): pass

class ShouldBeNumber(FreexSqlalchemyError): pass
# for either int or ints that can be cast to str

class CantBeEmptyString(FreexSqlalchemyError): pass

class CantBeNone(FreexSqlalchemyError): pass

class UnfinishedTest(FreexSqlalchemyError): pass

class UnfinishedCode(FreexSqlalchemyError): pass

class NugidAlreadyExists(FreexSqlalchemyError): pass

class AliasAlreadyExists(FreexSqlalchemyError): pass

class FilenameAlreadyExists(FreexSqlalchemyError): pass

class NoDuplicatesAllowed(FreexSqlalchemyError): pass

class DBError(FreexSqlalchemyError): pass

class FailedTransaction(FreexSqlalchemyError): pass

class IllegalActionId(FreexSqlalchemyError): pass

class IncorrectListItemsType(FreexSqlalchemyError): pass



############################################################
# create a global instance of main Fsqa class. we . this is only necessary
# because i don't know how to create an instance of a class
# in elisp (though i haven't tried). instead, we've declared
# fsqa above to be a global variable, and now we can just
# provide this function as an entry point for elisp
def create_fsqa(database_file,
                use_lisp=1,
                in_memory=0,
                file_ext='freex'):

    global fsqa
    fsqa = Fsqa(database_file,use_lisp,in_memory,file_ext)
    return fsqa



############################################################
def create_fsqa_test():

    """
    This is for practicing in ipython. Creates an instance
    of FsqaTest, populates it, and returns just the fsqa
    member variable.
    """
    
    # from freex_sqlalchemy_test import *
    import freex_sqlalchemy_test
    
    fsqa_test = freex_sqlalchemy_test.FsqaTest()
    fsqa_test.setUp()
    fsqa_test.populate()

    return fsqa_test



############################################################
def get_fsqa():

    """
    Because I don't know how to access a class's member
    variable through pymacs, this provides access to the
    fsqa global variable through a function.
    """

    return fsqa



##############################################################
# implicit link stuph

# I really don't like globals, but pymacs is kinda forcing us into
# this
impLinkRegexp = None
# for the twostage implicit link matching
impLinkRegexpFirstn = None

# maps alias-truncated-to-n-characters to a list of
# untruncated aliases that start like that, e.g.
#   {abcde: [abcde1, abcde2, ...],}
firstn_to_aliases = None



##############################################################
def update_implicit_link_regexp_original(aliases=None):
    """
    Builds and compiles the implicit link regular expression.
    """
    # the global regexp
    global impLinkRegexp

    # only bother with the regexes if we're in emacs (i.e. use_lisp = True) 
    # AND emacs wants us to
    if fsqa.use_lisp and not lisp.freex_enable_implicit_links.value():
        # xxx - cheat and exit so it works without hyperlinks with
        # lame mac python 2.5 regex overflow bug
        impLinkRegexp = re.compile('', re.MULTILINE)
        return impLinkRegexp

    if aliases is None:
        # get and sort the aliases
        aliases = get_all_aliases()

    # add .lower() here and elsewhere if you want case-insensitive
    aliases = [re.escape(a) for a in aliases]

    # ensure that 'jubba wubba' comes before 'jubba'
    aliases.sort(reverse=True)

    # build the regexp string to be single words
    #
    # it used to look like this, which worked nicely, unless
    # there was a carriage return
    aliasRegexpStr = '\\b'+'\\b|\\b'.join(aliases)+'\\b'

    # we want to deal with the possibility that there are 0
    # or 1 spaces after a word, followed by 0 or 1 carriage
    # returns, followed by zero or more spaces, which is
    # what might happen if an implicit link was to span two
    # lines in an indented paragraph. that's what ' ?\n? *' does
    #
    # the \\b that gets
    # added later will ensure that there's a word boundary
    # of *some* kind. 
    aliasRegexpStr = aliasRegexpStr.replace('\\ ', ' ?\\\n? *')

    # for ages, it wasn't matching things like 'Smith &
    # Jones (2006)', because there was some problem with the
    # parentheses. i eventually realized that it matched the
    # first, but not the second parenthesis, because (i
    # think) the parenthesis was screwing with the \b (bare
    # word) separator. if you remove all the bare word
    # separators that follow closing parentheses, the sun
    # comes back out again
    aliasRegexpStr = aliasRegexpStr.replace( ')\\b', ')' )

    # compile the regexp
    # impLinkRegexp = re.compile(aliasRegexpStr, re.IGNORECASE|re.MULTILINE)
    impLinkRegexp = re.compile(aliasRegexpStr, re.MULTILINE)

    return aliasRegexpStr, impLinkRegexp


############################################################
def update_implicit_link_regexp_firstn(maxlen=4):
    """
    Stage 1 of get_all_matching_implicit_links_twostage().

    If you get an error because your regex is too big
    (thousands of nuggets), then set MAXLEN lower.

    Based on update_implicit_link_regexp_original()
    """
    global impLinkRegexpFirstn
    global firstn_to_aliases

    if fsqa.use_lisp and not lisp.freex_enable_implicit_links.value():
        impLinkRegexpFirstn = re.compile('', re.MULTILINE)
        return
    
    all_aliases = get_all_aliases()
    # add lower() here and elsewhere if you want case-insensitive
    # you need the strip(), otherwise e.g. '200 ' will come before '2006'
    trunc_aliases = [a[:maxlen].split()[0].strip() for a in all_aliases]
    # trunc_aliases = [a[:maxlen].strip() for a in all_aliases]
    esc_aliases = [re.escape(a) for a in trunc_aliases]
    uniq_aliases = list(set([a for a in esc_aliases]))
    uniq_aliases.sort(reverse=True)
    # some of the aliases might overlap, e.g. 'me t' might
    # greedily consume/block aliases beginning with 't', so
    # use lookaheads. see
    # http://stackoverflow.com/questions/11430863/how-to-find-overlapping-matches-with-a-regexp
    #
    # REMOVED unfortunately, this makes the regex too long
    # lookahead_aliases = ['(?=%s)' % a for a in uniq_aliases]
    #
    # N.B. doesn't require the trailing \\b at the end of
    # each disjunct, because we're not expecting the
    # truncated-n-alias to finish on a word boundary
    aliasRegexpStr = '\\b'+'|\\b'.join(uniq_aliases)
    aliasRegexpStr = aliasRegexpStr.replace('\\ ', ' ?\\\n? *')
    aliasRegexpStr = aliasRegexpStr.replace( ')\\b', ')' )
    impLinkRegexpFirstn = re.compile(aliasRegexpStr, re.MULTILINE)

    # see definition of FIRSTN_TO_ALIASES. can't be done in
    # a list/dict comprehension, because we need to keep
    # appending to existing keys. xxx maybe something in itertools?
    firstn_to_aliases = collections.defaultdict(list)
    assert len(trunc_aliases) == len(all_aliases)
    for trunc_a, all_a in zip(trunc_aliases, all_aliases):
        # key on tuple split by whitespace, e.g. 'AB
        # testing' -> ('AB','t'). this is so that short
        # first words get a clue from later words
        #
        # REMOVED unfortunately, this creates issues where
        # 'me t' can greedily consume any aliases beginning
        # with 't'...
        # firstn_to_aliases[tuple(trunc_a.split())] += [all_a]
        # firstn_to_aliases[trunc_a.split()[0]] += [all_a]
        firstn_to_aliases[trunc_a] += [all_a]

    return aliasRegexpStr, impLinkRegexpFirstn


############################################################
def get_all_matching_implicit_links_original(textToSearch):
    """ Return a list of (beg,end) tuples for all the matching implicit
    links in the provided string.  """
    global impLinkRegexp
    # make sure it's filled
    if impLinkRegexp is None:
	update_implicit_link_regexp_original()

    # get the start and endpoints for the matchranges
    matchranges = [list(match.span()) for match in impLinkRegexp.finditer(textToSearch)]

    # return the matchranges
    return matchranges
    


############################################################
def get_all_matching_implicit_links_twostage(textToSearch):
    """
    This breaks the implicit link finding into 2 stages:

    1) Create a relatively short regex based on the set of
    aliases-truncated-to-5-characters.

    2) Run this on the buffer, and compile a custom regex
    from the subset of aliases whose first 5 characters were
    found.

    The matches for this second regex are the implicit links.
    """
    # add .lower() here and elsewhere if you want case-insensitive
    # textToSearch = textToSearch.lower()

    global impLinkRegexpFirstn
    if impLinkRegexpFirstn is None:
	aliasRegexpStr, impLinkRegexpFirstn = update_implicit_link_regexp_firstn()

    # find the matchranges based on the first N characters
    matchranges_firstn = [list(match.span()) for match in impLinkRegexpFirstn.finditer(textToSearch)]

    # extract the aliases-truncated-to-maxlen from the text,
    # so we can burrow down to figure out exactly which of
    # the untruncated aliases match
    matched_aliases_firstn = [textToSearch[start:end] for start,end in matchranges_firstn]
    # POTENTIAL_FULL_ALIASES = list of all the aliases that
    # start with one of MATCHED_ALIASES_FIRSTN, so that we
    # can compile a regex of them
    potential_full_aliases = []
    for a_trunc in matched_aliases_firstn:
        # for this A_TRUNC alias-truncated-to-n-characters,
        # these are the list of potential full aliases. see
        # comment on keys for FIRSTN_TO_ALIASES re tuples
        # split by whitespace
        # current_potentials = firstn_to_aliases[tuple(a_trunc.split())]
        current_potentials = firstn_to_aliases[a_trunc]
        # current_potentials = firstn_to_aliases[a_trunc.split()[0]]
        # ignore starting strings (e.g. 'conversation', 'journal') where
        # there are too many potential matches to deal with (regex gets too big)
        #
        # if sum([len(a) for a in current_potentials]) < 5000:
            # xxx - actually, it would be better to call
            # UPDATE_IMPLICIT_LINK_REGEXP_FIRSTN recursively
            # to get a new batch here
            # potential_full_aliases += current_potentials
        potential_full_aliases += current_potentials
    potential_full_aliases = set(potential_full_aliases)

    wordset_aliases = get_all_matching_implicit_links_wordset(textToSearch, potential_full_aliases)

#     # xxx - ALL_ALIASES should just be a global, but construct it for now
#     aliases = []
#     for alias_set in firstn_to_aliases.values(): 
#         aliases += alias_set
#     aliases = set(aliases)
    # wordset_aliases = get_all_matching_implicit_links_wordset(textToSearch, aliases)

    if not wordset_aliases:
        # nothing matched the first N characters, so no point running the regex
        return []

    # now set up a custom regex for just the POTENTIAL_FULL_ALIASES
    # aliasRegexpStr, impLinkRegexp = update_implicit_link_regexp_original(aliases=potential_full_aliases)
    aliasRegexpStr, impLinkRegexp = update_implicit_link_regexp_original(aliases=wordset_aliases)

    matchranges = [list(match.span()) for match in impLinkRegexp.finditer(textToSearch)]

    return matchranges


############################################################
def get_all_matching_implicit_links_wordset(txt, aliases):
    """
    TXT is the text you want to search through.

    ALIASES is the list of potential (probably multi-word)
    aliases that you think might be in TXT.

    Designed to be run as a late-stage whittling for
    multi-word aliases whose first few characters are
    definitely in TXT.
    
    Returns FOUNDS, a subset of ALIASES. N.B. This is overly
    permissive - you still need to run a regex afterwards
    with the exact aliases to be certain they match, and to
    find the start/end boundaries.
    """
    # re_words = re.compile('\b.*\b', re.MULTILINE)
    # wordranges = [match.span() for match in re_words.finditer(txt)]
    # wordset = set([txt[start:end] for start,end in wordranges])

    def replace_punctuation_with_space(s):
        ## see http://stackoverflow.com/questions/265960/best-way-to-strip-punctuation-from-a-string-in-python
        # return s.translate(string.maketrans("",""), string.punctuation)
        #
        # Bleurgh. That only works for ascii. Updated for unicode: http://stackoverflow.com/questions/11692199/string-translate-with-unicode-data-in-python
        remove_punctuation_map = dict((ord(char), ord(' ')) for char in string.punctuation)
        return unicode(s).translate(remove_punctuation_map)

    txt_nopunc = replace_punctuation_with_space(txt)
    wordset = set(txt_nopunc.split())
    founds = set()
    for alias in aliases:
        if all([(w in wordset)
                for w in replace_punctuation_with_space(alias).split()]):
            founds.add(alias)
    return founds


############################################################
def get_contents_a(alias):
    """
    Returns the nugget's contents as a string. Alias cannot
    include an extension. Throws NonexistentAlias.
    """
    
    if not isinstance(alias,str):
        raise ShouldBeStr

    if has_ext(alias):
        raise NeedsNoExtension

    sel = '''
    select content from nuggets,aliases
    where nuggets.id = aliases.nugid and alias = "%s"
    ''' % alias

    content = fsqa.db.execute(sel).fetchone()
    if content==None:
        raise NonexistentAlias

    return str(content[0])



############################################################
def get_contents(nugid):
    """
    Return the nugget's contents as string. Nugid can be a
    string or int.
    """
    # interactions[get_contents] = ''

    if not isinstance(nugid,int):
        raise ShouldBeInt
    
    sel = 'select content from nuggets where id = %d' % \
          nugid

    content = fsqa.db.execute(sel).fetchone()

    if content==None:
        raise NonexistentNugid

    # for some reason, fetchone returns a tuple if it finds
    # something, even though there will only be one object
    return str(content[0])



############################################################
def get_aliases(nugid):
    # interactions[get_aliases] = ''

    if not exist_nugget(nugid):
        raise NonexistentNugid

    sel = 'select alias from aliases where nugid = %d' % \
          int(nugid)

    # get a list of unicode alias strings

    aliases = [x[0] for x in fsqa.db.execute(sel).fetchall()]

    # turn each unicode string into a normal string before
    # returning
    return [str(x) for x in aliases]


############################################################
def get_aliases_delim_a(alias):
    interactions[get_aliases_delim_a] = ''    
    return get_aliases_delim(get_nugid_from_alias(alias))


############################################################
def get_aliases_delim(nugid):
    # interactions[get_aliases_delim] = ''

    if nugid==None:
        raise CantBeNone
    
    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt
    
    return string.join(get_aliases(nugid),'; ')

    interactions[get_alias_from_nugid] = ''

    nugid = int(nugid)
    
    aliases = get_aliases(nugid)
    if len(aliases):
        return aliases[0]
    else:
        return '#%d' % nugid



############################################################
def get_alias_from_nugid(nugid):

    """
    Get the first alias for this nugid.
    """
    
    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt

    # xxx - this should just have its own sql call with a
    # fetchone()
    aliases = get_aliases(nugid)
    if len(aliases):
        return aliases[0]
    else:
        return '#%d' % nugid


############################################################
def get_tag_parents_delim_a(child_alias):
    # interactions[get_tag_parents_delim_a] = ''

    return get_tag_parents_delim( \
        get_nugid_from_alias(child_alias) )


############################################################
def get_tag_parents_delim(child_nugid):
    # interactions[get_tag_parents_delim] = ''

    parent_aliases = get_tag_parents_for_a( \
        get_alias_from_nugid(child_nugid) )

    return string.join(parent_aliases,'; ')



############################################################
def get_tag_parents_delim_slash(child_nugid, delim='/'):
    # interactions[get_tag_parents_delim] = ''

    parent_aliases = get_tag_parents_for_a( \
        get_alias_from_nugid(child_nugid) )

    parent_aliases_slash = string.join(parent_aliases,delim)
    # if there are already some entries, then put a slash on
    # the end to make it easy for users to add new tags
    if len(parent_aliases_slash):
       parent_aliases_slash += delim
    return parent_aliases_slash



############################################################
def get_tag_children_delim_a(parent_alias):
    # interactions[get_tag_children_delim_a] = ''

    return get_tag_children_delim( \
        get_nugid_from_alias(parent_alias) )



############################################################
def get_tag_children_delim(parent_nugid):
    # interactions[get_tag_children_delim] = ''

    child_aliases = get_tag_children_for_a( \
        get_alias_from_nugid(parent_nugid) )

    return string.join(child_aliases,'; ')



############################################################
def add_alias(nugid,alias):
    """Adds an alias to an existing nugget. Returns ALIAS if
    successful, else returns None. Will fail if ALIAS is
    empty. Raises an exception if there's no nugget NUGID,
    or if the alias already exists for that nugget, or for
    any other nugget."""
    interactions[add_alias] = ''

    if nugid==None:
        raise CantBeNone

    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt

    if alias==None:
        raise CantBeNone
    if not isinstance(alias,str):
        raise ShouldBeStr
    
    # die loudly if the nugget doesn't exist at all
    if not exist_nugget(nugid):
        raise NonexistentNugid

    # don't bother if it's just whitespace"
    if not len(string.strip(alias)):
        return None

    existing_aliases = get_aliases(nugid)
    # if the alias is in the list of existing
    # aliases, don't bother
    if alias in existing_aliases:
        return None

    try:
        ins = 'insert into aliases values (NULL,"%s",%d,"%s")' % \
              (alias,nugid,datetime.datetime.now())
        fsqa.db.execute(ins)
    except:
        raise DBError

    return alias



############################################################
def remove_alias(nugid,alias):

    # 070408
    if (nugid==None) | (alias==None):
        raise CantBeNone
    if not isinstance(alias,str):
        raise ShouldBeStr
    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt
    
    dlt = 'delete from aliases where nugid = %d and alias = "%s"' % \
          (int(nugid),alias)
    fsqa.db.execute(dlt)



############################################################
def remove_aliases(nugid):

    if nugid==None:
        raise CantBeNone
    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt
    
    dlt = 'delete from aliases where nugid = %d' % nugid
    fsqa.db.execute(dlt)



############################################################
def remove_tag_parents_from(child_nugid):
    dlt = 'delete from nugget_assocs where src_nugid = %d' % int(child_nugid)
    fsqa.db.execute(dlt)



############################################################
def remove_tag_children_from(dest_nugid):

    if dest_nugid==None: raise CantBeNone
    
    try: dest_nugid = int(dest_nugid)
    except: raise ShouldBeInt
    
    dlt = 'delete from nugget_assocs where dest_nugid = %d' % dest_nugid
    fsqa.db.execute(dlt)



############################################################
def put_aliases_delim_a(alias,aliases):

    put_aliases_delim(
        get_nugid_from_alias(alias),aliases)



############################################################
def put_aliases_delim(nugid,aliases):

    """
    Takes a semicolon-delimited string and replaces all of
    the existing aliases with this.

    The filename (without extension) is always an alias, and
    will remain, even if it's not on this list.

    Doesn''t return anything.
    """

    if nugid==None: raise CantBeNone
    if aliases==None: raise CantBeNone
    if not isinstance(aliases,str): raise ShouldBeStr
    try: nugid = int(nugid)
    except: raise ShouldBeInt

    # get rid of any carriage returns or extra spaces in the
    # semicolon-delimited list (this means that you can't
    # have aliases with multiple spaces though)
    #
    # the carriage returns probably got added by the
    # autofill, so make sure we add in a space in their
    # place so that the two lines don't get joined
    # together. we'll get rid of double spaces in a minute
    aliases = aliases.replace('\n',' ')
    # as long as there are any double spaces, keep on
    # getting rid of them
    #
    # ('find' will return -1 when it fails)
    while aliases.find('  ')!=-1:
        aliases = aliases.replace('  ',' ')

    # strip will get rid of leading and trailing whitespace
    # from each item of the recently-split list
    aliases_split = [x.strip() for x in aliases.split(';')]

    remove_aliases(nugid)

    for alias in aliases_split:
        add_alias(nugid,alias)

    # every nugget must have its filename (without
    # extension) as part of its list of aliases. if that
    # somehow got removed, add it in
    filename_no_ext = remove_ext(
        get_filename(nugid))
    if not filename_no_ext in aliases_split:
        add_alias(nugid,filename_no_ext)



############################################################
def add_nugget(filename,content):
    """Create a nugget containing CONTENT and return the
    nugid. Adds the filename (without extension) to the list
    of aliases. Creates an empty file called FILENAME if one
    doesn't already exist.    
    """
    # interactions[add_nugget] = ''

    if filename==None:
        raise CantBeNone
    
    # check that filename includes an extension
    # (e.g. 'freex')
    if not has_ext(filename):
        raise NeedsExtension

    # make sure there are no carriage returns in the filename
    filename = string.replace(filename,'\n',' ')
    # swap semi-colons for hyphens
    filename = string.replace(filename,';','-')
    # get rid of double spaces
    filename = string.replace(filename,'  ',' ')
    # strip away leading and trailing whitespace, though
    # there shouldn't be any
    filename = string.strip(filename)

    if len(remove_ext(filename))==0:
        raise CantBeNone
        
    # get rid of all the quotation marks, so that it doesn't
    # screw up the SQL command. this shouldn't be an issue,
    # but it is
    content = string.replace(content,'"','')
    content = str(content)

    ins = 'insert into nuggets values (NULL,"%s","%s","%s")' % \
          (content,datetime.datetime.now(),filename)
    fsqa.db.execute(ins)

    # try and open the file. if it fails (i.e. the file
    # doesn't exist), touch the file
    # freex_mode_dir = lisp.freex_mode_dir.value()
    try:
        # open( os.path.join(freex_mode_dir,filename), 'r')
        open( os.path.join(fsqa.database_dir,filename), 'r')
    except:
        # touch the file (use append, just in case)
        # newfile = open( os.path.join(freex_mode_dir,filename), 'a')
        newfile = open( os.path.join(fsqa.database_dir,filename), 'a')
        newfile.close()

    # to get the nugid of our new nugget, presumably the
    # last ID is the one we just added...
    sel = 'SELECT id FROM nuggets ORDER BY id DESC LIMIT 1'
    nugid = fsqa.db.execute(sel).fetchone()[0]

    # every nugget must have its filename (without
    # extension) as part of its list of aliases
    filename_no_ext = remove_ext(filename)
    add_alias(nugid,filename_no_ext)

    # only update the implicit link regex if the user
    # doesn't mind. for the moment, it's very slow, so
    # there's an option to stop it being updated
    # automatically. however, that flag is in emacs, and we
    # can only check it if there's an instance of emacs
    # running (i.e. use_lisp is true)
    if fsqa.use_lisp:
        if lisp.freex_fontify_update_implicit_link_regexp_often:
            # the user is willing to update the fontify
            # regex automatically
            lisp.freex_fontify_update_implicit_link_regexp()
        else:
            lisp.message('Not updating implicit link regexp')
    else:
        # not using lisp, so just call it directly
        update_implicit_link_regexp()

    return nugid



############################################################
def put_contents_a(alias,content):
    # interactions[put_contents_a] = ''

    put_contents(
        get_nugid_from_alias(alias),
        content)



############################################################
def put_contents(nugid,content):
    """Adds CONTENT to the NUGID file, and to the fsqa.db
    CONTENTS field.
    """
    # interactions[put_contents] = ''

    if nugid==None: raise CantBeNone
    
    try: nugid = int(nugid)
    except: raise ShouldBeNumber

    # updated to work with Pymacs 0.23's unicode
    try: content = str(content)
    except:
        import BeautifulSoup as bs
        content = str(bs.BeautifulSoup(content))

    # completely get rid of all the quotation marks
    content = string.replace(content,'"','')
    # get rid of double carriage returns, but add in a
    # period, so that fulltext searches don't lump together
    # the beginning and end of a paragraph
    content = string.replace(content,'\n\n',' . ')
    # completely get rid of single carriage returns
    content = string.replace(content,'\n',' ')
    # replace multiple spaces with a single one
    content = re.sub(' +', ' ', content)

    content = str(content)

    # write the contents to the fsqa.db entry
    upd = 'UPDATE nuggets SET content = "%s" WHERE id = %d' % \
          (content,nugid)
    fsqa.db.execute(upd)

    # write the contents out to the file
    # freex_mode_dir = lisp.freex_mode_dir.value()
    
    # filename = get_filename(nugid)
    # f = open( os.path.join(fsqa.database_dir,filename), 'w')
    # f.write(content)
    # f.close()



############################################################
def get_tag_parents_for_a(alias):
    
    """
    Gets all the tag parents that ALIAS has
    """

    nugid = get_nugid_from_alias(alias)

    parents = get_tag_parents_for(nugid)

    return [get_alias_from_nugid(x) for x in parents]



############################################################
def get_tag_parents_for(src_nugid):

    """
    Gets all the tags that NUGID belongs to
    """

    try:
        src_nugid = int(src_nugid)
    except:
        raise ShouldBeInt            

    sel = 'select dest_nugid from nugget_assocs where src_nugid = %d' \
          % src_nugid
    tag_parents = fsqa.db.execute(sel).fetchall()
    return [x[0] for x in tag_parents]



############################################################
def get_tag_children_for_a(parent_alias):

    # this should be called get_tag_children_a_for_a

    nugid = get_nugid_from_alias(parent_alias)

    children = get_tag_children_for(nugid)

    return map(get_alias_from_nugid,children)



############################################################
def filter_by_tag_parents(inp,
                          append_wildcard=True,
                          require_alias='',
                          second_order_tags=True,
                          sortby="alias",
                          display_sql=False):

    """
    This is the completion function for
    freex-meta-complete-alias that looks for any tag-parents
    delimited by '/', and uses them to filter down the list
    of aliases it returns.

    It also suggests possible tag-parents to use to whittle
    down the search.

    By default, appends a wildcard at the very end of the
    query, so that e.g. 'emacs' would include 'emacs lisp'
    as a match. Set this to false if you don't want to
    automatically append this wildcard. In that case, of
    course you could still query for 'emacs*' for the same
    effect.

    By default, this includes tags that are parents *or
    grandchildren* of the input tags.

    Set second_order_tags=False if you want it just to look
    for parents, rather than grandparents.

    Set sortby="created" if you want to sort the aliases by
    the date of creation. By default, sorts by alias
    alphabetically.

    Set display_sql=True if you'd like to see the SQL call
    that gets created.

    """
    
    # split the input string up into parts, based on '/' as
    # a delimiter (to make it feel like a filesystem). we're
    # not running string.strip on the parts, because there
    # shouldn't be whitespace around the '/' delimiters
    # during a completing-read
    inp_parts = string.split(inp,'/')

    # all the parts except the last one are
    # parent_aliases. if there are one or zero parts, then
    # there will be zero parent_aliases
    parent_aliases = inp_parts[:-1]

    last_alias_stem = inp_parts[-1]
    # trade all the * wildcards at the end for % wildcards
    last_alias_stem = last_alias_stem.replace('*','%')

    if append_wildcard:
        last_alias_stem = last_alias_stem + '%'

    if require_alias=='like':
        alias_req = 'AND alias LIKE "%s"' % last_alias_stem
    elif require_alias=='exact':
        alias_req = 'AND alias = "%s"' % last_alias_stem
    else:
        alias_req = ''

    if len(parent_aliases)==0:
        # if they didn't feed in any tags, just filter by the stub
        #
        # find all the aliases that match the stub%
        sel = """
        SELECT alias FROM aliases WHERE alias LIKE "%s"
        %s
        ORDER BY %s
        """ % (last_alias_stem, alias_req, sortby)

        if display_sql:
            print sel
        try:
            return [str(x[0]) for x in fsqa.db.execute(sel).fetchall()]
        except:
            return []

    if parent_aliases[-1]=='':
        # that means the user's input had a double-slash on
        # the end, which is the signal that they don't want
        # us to suggest_tag_parents
        do_suggest_tag_parents = False
        # and get rid of the last empty alias, because it'll
        # throw things off below
        parent_aliases = parent_aliases[:-1]

    else:
        do_suggest_tag_parents = True

    if not do_suggest_tag_parents:
       require_alias = ''

    # we're going to prepend this later to all the
    # completions we return so that it matches the user's
    # input, otherwise emacs won't show them in the completion
    # list
    preamb = string.join(parent_aliases,'/') + '/'

    # if the user fed in any "fulltext patterns" in quotes,
    # get a list of those, and remove them from the list of
    # parent_aliases
    ft_patterns, parent_aliases_no_ft = extract_fulltext_patterns(parent_aliases)

    if (len(parent_aliases_no_ft)==0):
        # if the only thing(s) the user fed in were fulltext
        # patterns, without any tags to filter by, then deal
        # with this specially
        #
        # we already dealt with the possibility that they
        # didn't feed in anything to query by

        # this is the list of fulltext patterns to search for, e.g.
        # content="%hello%" AND content="%world%"
        #
        # N.B. you need %%%s%% if you want to get %yourvar%
        content_q = string.join(
            ['content LIKE "%%%s%%"' % x for x in ft_patterns],
            ' AND ')
        
        sel = """
        SELECT nuggets.id, alias FROM nuggets, aliases WHERE 
          nuggets.id=aliases.nugid AND %s
          %s
          ORDER BY %s
        """ % (content_q, alias_req, sortby)

        tag_children = fsqa.db.execute(sel).fetchall()
        tag_children_nugids = [str(x[0]) for x in tag_children]
        tag_children_aliases = [str(x[1]) for x in tag_children]

        parent_nugids = []

    else:
        # the user fed in some tags to filter by (possibly as
        # well as some full text)
        
        # get the nugids of all the parent aliases
        parent_nugids = [get_nugid_from_alias(x) for x in parent_aliases_no_ft]

        # we want to get all the intersecting
        # tag_children_aliases of all the provided parents
        #
        # remember: src=child, dest=parent
        #
        # this is an example of the SELECT subqueries approach
        # that Ted Nadeau suggested for 'conversation/Ted Nadeau/':
        #
        # select * from nugget_assocs where \
        #   dest_nugid=4408 and src_nugid in
        #   (select src_nugid from nugget_assocs where dest_nugid=875);
        #
        # we know that there's at least one parent_alias
        # (because otherwise the function would have already
        # returned)
        #
        # this shouldn't have a semicolon, because that will be
        # added later
        #
        # this is the innermost subquery that we're going to
        # build layers around

        if second_order_tags:
            # this is the basic idea for finding tag
            # grandchildren. we OR this with the query that finds
            # tag children
            #
            # select alias from aliases where nugid in
            #   (select src_nugid from nugget_assocs where dest_nugid in
            #     (select src_nugid from nugget_assocs where dest_nugid = 4));
            #
            # get everything that's either a tag-parent or tag-grandparent
            base_q = """
            SELECT src_nugid FROM nugget_assocs WHERE
            (assoc_type_id=%i AND dest_nugid=%i)
            OR
            (assoc_type_id=%i AND dest_nugid IN 
            (SELECT src_nugid FROM nugget_assocs WHERE
            assoc_type_id=%i AND dest_nugid=%i))
            """ % (ASSOC_TAG, parent_nugids[0], \
                   ASSOC_TAG, ASSOC_TAG, parent_nugids[0])

        else: # no second order tags
            base_q = """
            SELECT src_nugid FROM nugget_assocs WHERE
            dest_nugid=%i AND assoc_type_id=%i
            """ % (parent_nugids[0], ASSOC_TAG)

        # the complete_q is going be built up layer by
        # layer. whatever happens, it's going to include the
        # base_q as its innermost layer
        complete_q = base_q

        # now keep on adding subquery layers to the base query
        # (ignoring the first parent alias)
        for pnugid in parent_nugids[1:]:

            if second_order_tags:

                # now, the complete query is going to have a new
                # outer layer that calls the previous complete query
                # as a subquery
                complete_q = """
                SELECT src_nugid FROM nugget_assocs WHERE
                (
                (assoc_type_id=%i AND dest_nugid=%i)
                OR
                (assoc_type_id=%i AND dest_nugid IN 
                (SELECT src_nugid FROM nugget_assocs WHERE
                assoc_type_id=%i AND dest_nugid=%i))
                )
                AND src_nugid IN (%s)
                """ % (ASSOC_TAG, pnugid, \
                       ASSOC_TAG, ASSOC_TAG, pnugid, 
                       complete_q)

            else:
                # now, the complete query is going to have a new
                # outer layer that calls the previous complete query
                # as a subquery
                complete_q = """
                SELECT src_nugid FROM nugget_assocs
                WHERE dest_nugid=%i
                AND assoc_type_id=%i
                AND src_nugid IN (%s)
                """ % (pnugid, ASSOC_TAG, complete_q)

        # finally, wrap the complete query one more time so that
        # it returns aliases rather than nugids
        if len(ft_patterns)>0:
            # we're dealing with both tags and fulltext

            # e.g. content LIKE "%hello%" AND content LIKE "%world%"
            content_q = string.join( \
                        ['content LIKE "%%%s%%"' % x for x in ft_patterns], \
                        ' AND ')
            
            complete_q = """
            SELECT nugid,alias FROM aliases, nuggets WHERE
            nuggets.id=aliases.nugid
            AND %s
            AND nugid IN (%s)
            %s
            ORDER BY %s
            """ % (content_q, complete_q, alias_req, sortby)

        else:
            # we're just dealing with tags
            complete_q = """
            SELECT nugid,alias FROM aliases WHERE nugid IN (%s)
            %s
            ORDER BY %s
            """ % (complete_q, alias_req, sortby)

        # get rid of all the carriage returns, and append a
        # semi-colon
        complete_q = string.replace(complete_q,'\n','') + ';'

        tag_children = fsqa.db.execute(complete_q).fetchall()
        tag_children_nugids = [str(x[0]) for x in tag_children]
        tag_children_aliases = [str(x[1]) for x in tag_children]

    # now it's time to suggest some tag parents (like
    # subdirectories, to filter the search further)
    #
    # don't bother trying to suggest tag parents if:
    #
    # - there are no tag-children, or only one (so there's
    # no whittling to be done)
    #
    # - the user doesn't want us to (signalled by a
    # double-slash at the end of their INP string - see
    # above)
    if (len(tag_children_aliases)<=1) | (do_suggest_tag_parents==False):
        stp = []

    else:
        # suggest away!

        # these are going to go into 'in (%s)' SQL queries
        union_strlist = '%s' % string.join(
            ['"%s"' % x for x in tag_children_nugids],
            ', ')

        # this is so we don't suggest back parents that the
        # user has already used to filter things by
        parent_nugid_list = string.join(
            [str(x) for x in parent_nugids],
            ', ')
        
        # now we want to find the tag-parents that are common to
        # all the tag-children, and suggest them as possible
        # ways to winnow down the search
        #
        # this is simpler than before. you have to be one of
        # the tag-children we just found, and then just give
        # me all your (distinct) tag-parents
        #
        # N.B. we're not filtering by 'alias LIKE
        # last_alias_stem' here, because the tag-parents'
        # aliases can take any form
        complete_q = """
        SELECT alias FROM aliases WHERE nugid IN
          (
          SELECT DISTINCT dest_nugid FROM nugget_assocs WHERE
            assoc_type_id=%i
            AND src_nugid IN (%s)
          )
        AND nugid NOT IN (%s)
        %s
        ORDER BY %s
        """ % (ASSOC_TAG, union_strlist, parent_nugid_list, alias_req, sortby)

        # get rid of all the carriage returns, and append a
        # semi-colon
        complete_q = string.replace(complete_q,'\n','') + ';'

        stp = fsqa.db.execute(complete_q).fetchall()
        stp = [str(x[0]) for x in stp]
        stp = [preamb + x + '/' for x in stp]

    suggestions = []
    
    # if the user specified that they don't want tag-parents
    # with a double-slash, then we need a double-slash on
    # the end of what we return
    #
    # xxx - it would be nice if we didn't reconstruct the
    # input, but just reused the INP string we were
    # fed. unfortunately, if they gave us a partial stub,
    # e.g. 'lecture/lec', then we'd have to remove that
    # stub, which would probably still be easier
    if not do_suggest_tag_parents:
        preamb = preamb + '/'

    # this next section still needs tidying up
    # tc = [get_aliases(x) for x in tag_children_nugids]
    tc = tag_children_aliases
    # tc = union_lists_no_duplicates(tc)
    tc = [preamb + x for x in tc]

    #     stp_to_remove = []
    #     for s in stp:
    #         if s in parent_aliases:
    #             stp_to_remove.append(s)
    #     for s in stp_to_remove:
    #         stp.remove(s)

    # stp = [get_aliases(x) for x in stp]
    # stp = union_lists_no_duplicates(stp)

    suggestions.extend(tc)

    if do_suggest_tag_parents:
        suggestions.extend(stp)

    if display_sql:
        print complete_q
        
    return suggestions



############################################################
def filter_by_tag_parents_fnames_only(inp,
                                      sortby="alias"):

    """ Takes the same kind of input as
    filter_by_tag_parents, e.g. 'parent1/parent2/', but
    returns filenames for each of the matches (no
    duplicates). Ignores tag-parent-suggestions.

    Update: at the time of writing, the ignoring
    tag-parent-suggestions isn't working right, so just be
    sure to have a double-slash in your query to use this.
    """

    #     # make sure that the last slash is a double-slash,
    #     # e.g. turn 'hello/cruel/world' into
    #     # 'hello/cruel//world'
    
    #     # if there's no double slash
    #     if inp.find('//') == -1:
    #         # if there is a double-slash
    #         pass
    
    #     # if there is at least one slash
    #     if inp.find('/'):
    #         # is there also a double-slash??
    #         if inp.find('//'):
    #             # if there is a double-slash, check it's the last
    #             # one
    #             if inp.find('//') > inp.find('/'):
    #                 # fine
    #                 pass
    #             else:
    #                 # hmmm. it looks as though there is a
    #                 # double-slash, but it's not the last one,
    #                 # e.g. 'hello//cruel/world'. let's fix that
    #                 #
    #                 # first, split up the slash-delimited list, treating
    #                 # double-slashes just like single-slashes
    #                 inp_split = [x for x in inp.split('/') if x != '']
    #                 # then rejoin all the pieces, making sure the
    #                 # last piece gets a double-slash
    #                 inp = string.join( inp_split[:-1], '/' ) + \
    #                       '//' + inp_split[-1]
    #         else:
    #             # there's at least one single slash, but no
    #             # double-slash. assume the user meant to append
    #             # a double-slash at the end
    #             inp = inp + '//'
    #     else:
    #         # there's just a single slash, so assume the user
    #         # meant to append a double-slash at the end
    #         inp = inp + '//'

    out = filter_by_tag_parents(inp,sortby=sortby)

    # turn the list of aliases into a unique list of
    # filenames
    #
    # each item in the list from filter_by_tag_parents has
    # all the tag-parents prepended, for completion
    # purposes, e.g. 'parent1/parent2/child'. We only care
    # about 'child', so we're going to throw away all but
    # the last part of each slash-delimited string in the
    # list.
    #
    # we need to make sure that we're getting the filename,
    # not just the first alias in the list for each of these
    #
    # convert into a set then back into a list to get rid of
    # duplicates
    #
    # update - i got rid of the list(set(x)), because i
    # don't think we need to do any special processing to
    # ensure the list is unique...
    children_fnames = uniquify_list([get_filename_a(x.split('/')[-1])
                                     for x in out])

    return children_fnames


############################################################
def edit_tag_parents_in_minibuffer_complete(inp):

    """

    This is the completion function for
    freex-meta-edit-tag-parents-in-minibuffer.

    It takes in a '/'-delimited string, and completes the
    last alias in the list, e.g. if fed in
    'alias1/alias2/ema' it might return
    ['alias1/alias2/emacs'] as the list of potential
    completions.

    It cribs heavily from filter_by_tag_parents.
    
    """

    lisp.message(inp)

    if len(inp)==0:
        # if the user fed us an empty string, just return all
        # aliases. that'll learn 'em
        return get_all_aliases()

    inp_parts = string.split(inp, '/')

    # we need to make sure not to prepend a '/' in the case
    # where there are no preceding aliases, e.g. if the user
    # typed 'emac', don't return '/emacs' as a suggestion
    if len(inp_parts)>1:
        # there are some preceding aliases
    
        # we're only going to complete the last in the list, but
        # we need to store the preceding ones, because we'll
        # have to string them back together after
        preceding_aliases = inp_parts[:-1]
        
        # reassemble things
        preamb = string.join(preceding_aliases,'/') + '/'

    else:
        # there are no preceding aliases

        preceding_aliases = []
        preamb = ''

    last_alias_stem = inp_parts[-1]
    # trade all the * wildcards for % wildcards
    last_alias_stem = last_alias_stem.replace('*','%')

    # list all the aliases that start with LAST_ALIAS_STEM,
    # e.g. 'select alias from aliases where alias like
    # "emac%";'
    complete_q = """
    SELECT alias FROM aliases WHERE alias LIKE "%s%%"
    """ % last_alias_stem

    # completions for the last alias
    last_alias_completions = fsqa.db.execute(complete_q).fetchall()
    last_alias_completions = [str(x[0]) for x in last_alias_completions]

    # full suggestions, each one comprising preceding
    # aliases and a possible completion for the last alias
    suggestions = [preamb + x
                   for x in last_alias_completions]

#     if fsqa.use_lisp:
#         lisp.message(str(suggestions))
#     else:
#         print suggestions
    
    return suggestions


############################################################
def intersect_tag_children_a_from_multiple_tag_parents_a(parent_aliases):
    """
    Returns the list of aliases for all the nuggets that
    are tag-children of all the PARENT_ALIASES.

    PARENT_ALIASES should be a string or list of strings,
    otherwise raises.

    Returns None for None or empty string.
    """

    # if they fed us empty air, don't get angry
    if not parent_aliases:
        return None

    if isinstance(parent_aliases,str):
        # give up if it's a blank string
        if len(parent_aliases)==0:
            return None
        # if it's a normal string, i.e. a single alias, then
        # just wrap it up as a list
        else:
            parent_aliases = [parent_aliases]

    if not isinstance(parent_aliases,list):
        raise ShouldBeList

    for alias in parent_aliases:
        if not isinstance(alias,str):
            raise ShouldBeStr
    
    # child_alias_lists = map(get_tag_children_for_a,parent_aliases)
    child_alias_lists = [get_tag_children_for_a(x) for x in parent_aliases]

    child_aliases = intersect_lists(child_alias_lists)

    return child_aliases



############################################################
def union_tag_parents_a_from_multiple_tag_children_a(child_aliases):

    """
    Find the tag-parents that all our tag-children have in common
    """
    
    # if they fed us empty air, don't get angry
    if not child_aliases:
        return None

    if isinstance(child_aliases,str):
        # give up if it's a blank string
        if len(child_aliases)==0:
            return None
        # if it's a normal string, i.e. a single alias, then
        # just wrap it up as a list
        else:
            child_aliases = [child_aliases]

    if not isinstance(child_aliases,list):
        raise ShouldBeList

    # parent_alias_lists = map(get_tag_parents_for_a,child_aliases)
    parent_alias_lists = [get_tag_parents_for_a(x) for x in child_aliases]

    parent_aliases = union_lists_no_duplicates(parent_alias_lists)

    return parent_aliases



############################################################
def union_lists_no_duplicates(lists):
    """
    Joins all the lists together, and then removes the
    duplicates by casting the big list into a set and back
    again.

    The order of the output is not determined.
    """

    if lists==None:
        raise CantBeNone
    if not isinstance(lists,list):
        raise ShouldBeList

    newlist = []
    for l in lists:
        
        if not isinstance(l,list):
            raise ShouldBeList
        
        newlist.extend(l)
    return list(set(newlist))



############################################################
def intersect_lists(lists):
    """Return items common to all the sublists in LISTS
    (where the number of sublists can be zero). The order
    that the intersected items get returned in is not
    defined. None of the items in the sublists can
    themselves be a list. All of the items returned will be
    unique."""

    if not isinstance(lists,list):
        raise ShouldBeList

    for l in lists:
        # check that all of the sublists in LISTS are lists
        if not isinstance(l,list):
            raise ShouldBeList
        
        # if any of the items in any of the sublists are
        # themselves a list, then raise an exception
        # if any( map(lambda x: isinstance(x,list),l) ):
        if any( [isinstance(x,list) for x in l] ):
            raise CantBeList

    if len(lists)==0:
        return []

    if len(lists)==1:
        return lists[0]

    # reduce runs some function cumulatively on the items in
    # a sequence. so for each item, it'll be running the
    # function on that item plus the results of all the
    # reduce calls so far...
    return list( reduce( \
        # this anonymous function takes two lists, turns
        # them both into sets, and then intersects them,
        # returning what's common to them both
        lambda l1,l2: set.intersection(set(l1),set(l2)), lists))



# def get_tag_children_for_multiple(parent_nugids):
#     """Returns the list of nugids for nuggets that are
#     tag-children of all of the PARENT_NUGIDS."""
#     # interactions[get_tag_children_for_multiple] = ''

#     # if they didn't feed in any parent_nugids, we can just
#     # return all the nugids right away
#     if not parent_nugids:
#         return get_all_ids()

#     if isinstance(parent_nugids,int):
#         parent_nugids = [parent_nugids];

#     # start with all of the nugids, and then whittle them
#     # down
#     child_nugids = set(get_all_ids())
#     for parent_nugid in parent_nugids:
#         if not parent_nugid:
#             continue

#         sel = 'select src_nugid from nugget_assocs where ' \
#               + 'dest_nugid = %d' % int(parent_nugid)

#         # these are the nuggets that are children of this parent
#         these_child_nugids = map(car, fsqa.db.execute(sel).fetchall() )

#         # only keep the ones that are also children of
#         # previous parents
#         child_nugids = child_nugids.intersection(these_child_nugids)

#     return list(child_nugids)


# def get_tag_parents_for_multiple(child_nugids):
#     """Returns the list of nugids for nuggets that are
#     tag-parents of all of the CHILD_NUGIDS."""
#     # interactions[get_tag_parents_for_multiple] = ''

#     # if they didn't feed in any child_nugids, we can just
#     # return all the nugids right away
#     if not child_nugids:
#         return get_all_ids()

#     if isinstance(child_nugids,int):
#         child_nugids = [child_nugids];

#     # start with all of the nugids, and then whittle them
#     # down
#     parent_nugids = set(get_all_ids())
#     for child_nugid in child_nugids:
#         if not child_nugid:
#             continue

#         # have i got this the wrong way round???
#         #         sel = 'select dest_nugid from nugget_assocs where ' \
#         #               + 'src_nugid = %d' % int(child_nugid)
#         sel = 'select src_nugid from nugget_assocs where ' \
#               + 'dest_nugid = %d' % int(child_nugid)

#         # these are the nuggets that are parents of this child
#         these_parent_nugids = map(car, fsqa.db.execute(sel).fetchall() )

#         # only keep the ones that are also children of
#         # previous children
#         parent_nugids = parent_nugids.intersection(these_parent_nugids)

#     return list(child_nugids)



############################################################
def get_tag_children_for(parent_nugid):

    """
    Gets all the nuggets that have the PARENT_NUGID
    tag-parent.
    """

    # 070312
    if parent_nugid==None:
        return None

    # 070413
    try:
        parent_nugid = int(parent_nugid)
    except:
        raise ShouldBeInt

    sel = 'select src_nugid from nugget_assocs where dest_nugid = %d' \
          % parent_nugid

    return [x[0] for x in fsqa.db.execute(sel).fetchall()]



############################################################
def exist_nugget(nugid):
    """Returns NUGID if a nugget with that nugid exists,
    otherwise None."""
    interactions[exist_nugget] = ''

    if nugid==None:
        raise CantBeNone

    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt

    sel = 'select id from nuggets where id = %d' % nugid
    return len(fsqa.db.execute(sel).fetchall())>0



############################################################
def exist_nugget_a(alias):
    """Returns ALIAS if a nugget with that alias exists,
    otherwise None."""
    # interactions[exist_nugget_a] = ''

    if isinstance(alias,unicode): alias = str(alias)

    if not isinstance(alias,str):
        if fsqa.use_lisp: lisp.message(str(alias.__class__))
        raise ShouldBeStr

    if len(alias)==0:
        return None
    
    try:
        get_nugid_from_alias(alias)
        return alias
    except:
        return None



############################################################
def remove_surplus_whitespace(s):
    
    """
    This finds any multi-space sequences or carriage returns
    in the string, and replaces them with a single space.
    """

    # find any sequences with at least one whitespace
    # character in string s, and replace each with a single
    # space
    return re.sub('\s+', ' ', s)


############################################################
def add_ext(filename):

    """
    Adds EXT to the FILENAME. Complement of REMOVE_EXT.
    """

    ext = fsqa.file_ext
    
    if not isinstance(ext,str):
        raise ShouldBeStr

    if ext[0]=='.':
        raise ShouldBeNoDot
    ext = '.' + ext

    if not isinstance(filename,str):
        raise ShouldBeStr

    if has_ext(filename):
        raise NeedsNoExtension
        
    return filename + ext


############################################################
def remove_ext(filename):
    """Remove EXT from the end of FILENAME. Complement of
    ADD_EXT."""

    if filename==None:
        raise CantBeNone

    ext = fsqa.file_ext

    if not isinstance(ext,str):
        raise ShouldBeStr
    if not isinstance(filename,str):
        raise ShouldBeStr

    if ext[0]=='.':
        raise ShouldBeNoDot
    ext = '.' + ext

    # e.g. if EXT=='.freex', this will become \.freex$. we
    # need to escape the dot, otherwise it will be treated
    # as a metacharacter, and the $ signifies the end of the
    # string
    ext_regexp = '\\' + ext + '$'

    if not re.search(ext_regexp,filename):
        raise NeedsExtension
        # raise 'file = %s, regexp = %s' % (filename, ext_regexp)

    return re.sub(ext_regexp,'',filename)



############################################################
def get_ext():

    """
    To make accessing the file_ext clss variable easier from
    pymacs.
    """

    return fsqa.file_ext



############################################################
def put_ext(ext):

    """
    To make accessing the file_ext clss variable easier from
    pymacs.
    """

    if not isinstance(ext,str):
        raise ShouldBeStr
    if ext[0]=='.':
        raise ShouldBeNoDot

    fsqa.file_ext = ext

    return fsqa.file_ext


############################################################
def has_ext(s):
    """
    Checks whether S ends in EXT. Returns an int (1 or 0),
    so that pymacs can understand the output.
    """

    # this will be true if there's an extension, otherwise
    # false
    # return re.search('.freex$',s) != None

    # if there's an extension, then REMOVE_EXT will remove
    # it and so we know that there's an extension. if not,
    # it'll raise an exception and so we can report false
    try:
        s2 = remove_ext(s)
        return 1
    except NeedsExtension:
        return 0



############################################################
def get_filename_a(alias):
    """
    Return the filename for the nugget that has ALIAS as an
    alias. Return None if no such nugget exists.
    """

    if alias==None:
        raise CantBeNone

    try:
        filename = get_filename(get_nugid_from_alias(alias))
        return filename
    except:
        return None


############################################################
def get_filename(nugid):
    """
    Return the filename for this NUGID. If the database
    burps, just return None.
    """

    if nugid==None:
        raise CantBeNone

    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt

    sel = 'select filename from nuggets where id = %d' % \
          nugid

    try:
        filename = str(fsqa.db.execute(sel).fetchone()[0])
    except:
        filename = None

    return filename


############################################################
def put_filename_a(alias,filename):

    nugid = get_nugid_from_alias(alias)
    if nugid:
        return put_filename(nugid,filename)
    else:
        return None



############################################################
def put_filename(nugid,filename):

    # 070410
    if (nugid==None) | (filename==None): raise CantBeNone
    try: nugid = int(nugid)
    except: raise ShouldBeInt
    if not isinstance(filename,str): raise ShouldBeStr

    if not has_ext(filename):
        raise NeedsExtension

    upd = 'update nuggets set filename="%s" where id = "%i"' % \
          ( filename,nugid )
    fsqa.db.execute(upd)



############################################################
def add_tag_child_to_tag_parent_a(child_nugid,parent_alias):
    # interactions[add_tag_child_to_tag_parent_a] = ''

    return add_tag_child_a_to_tag_parent_a(
        get_alias_from_nugid(child_nugid),parent_alias)



############################################################
def add_tag_child_a_to_tag_parent_a(child_alias,parent_alias):
    """Create a nugget-nugget association between
    CHILD_ALIAS and PARENT_ALIAS, creating nuggets for
    either of them if necessary. Returns (CHILD_NUGID,PARENT_NUGID) if
    successful, and None if it fails.

    Fails if the PARENT_ALIAS is already a tag-parent of the
    CHILD_ALIAS, if either of them are empty, or if
    PARENT_ALIAS == CHILD_ALIAS."""
    # interactions[add_tag_child_a_to_tag_parent_a] = ''

    # 070405
    if child_alias==None:
        return None
    if parent_alias==None:
        return None
    
    # if either alias is completely whitespace, then assume
    # it's a mistake and ignore it
    if not len(child_alias):
        return None
    if not len(parent_alias):
        return None

    # get both nugids, creating new nuggets if necessary
    child_nugid, existed_child = \
                 get_or_create_nugid_from_alias(child_alias)
    parent_nugid, existed_parent = \
                  get_or_create_nugid_from_alias(parent_alias)
    if not existed_child:
        msg = '[Created as tag-child for %s]' % parent_alias
        put_contents(child_nugid,msg)
    if not existed_parent:
        msg = '[Created as tag-parent for %s]' % child_alias
        put_contents(parent_nugid,msg)

    # can't tag a nugget with itself
    if child_nugid==parent_nugid:
        return None

    # don't tag them together if they've already been tagged
    # together
    # 
    # this does allow bidirectional tags though... maybe a
    # bad idea???
    if parent_nugid in get_tag_parents_for(child_nugid):
        return None

    ins = 'insert into nugget_assocs values ' + \
          '(NULL,%d,%d,%d,"%s")' % \
          (child_nugid,parent_nugid,
           ASSOC_TAG,
           datetime.datetime.now())
    fsqa.db.execute(ins)

    # this is just a sanity check. if we get this far in the
    # function, that means that all the conditions have
    # passed, and so we shouldn't ever be returning None for
    # either
    #
    # N.B. this is just a debugging check. if this is
    # causing a problem, just completely comment out the
    # next if statement and all will be well
    #
    # 070405
    if (child_nugid==None) | (parent_nugid==None):
        raise 'Shouldn''t be returning None'

    return child_nugid,parent_nugid


############################################################
def put_tag_parents_delim_a(child_alias,parent_aliases):
    # interactions[put_tag_parents_delim_a] = ''

    return put_tag_parents_delim(
        get_nugid_from_alias(child_alias),parent_aliases)



############################################################
def put_tag_parents_delim_slash(child_nugid,parent_aliases):

    """
    Stores a list of tag-parents fed in as a
    slash-delimited string, e.g. 'alias1/alias2/alias3'.

    All it really does is break up the slash-delimited list,
    glue it back together again with semicolons, and then
    feed that to PUT_TAG_PARENTS_DELIM.

    This is a temporary measure while we decide on the
    delimiter for tags (slashes, semicolons or hyphens...)
    
    """

    # strictly, we don't really need to do the stripping
    # here, because PUT_TAG_PARENTS_DELIM will strip each
    # component for us, but let's be neat anyway
    parent_aliases_list = [x.strip() for x in parent_aliases.split('/')]
    parent_aliases_semicolon_str = string.join(
        parent_aliases_list, ';')

    return put_tag_parents_delim(child_nugid, parent_aliases_semicolon_str)
        
        

############################################################
def put_tag_parents_delim(child_nugid,parent_aliases):
    """
    Replaces CHILD_NUGID nugget's existing tag_parents with
    PARENT_ALIASES, a semicolon-delimited string.
    """

    # just in case
    child_nugid = int(child_nugid)
    
    child_alias = get_alias_from_nugid(child_nugid)

    # start by removing all the existing tag_parents from
    # child_nugid
    remove_tag_parents_from(child_nugid)

    # get rid of any carriage returns or extra spaces in the
    # semicolon-delimited list (this means that you can't
    # have aliases with multiple spaces though)
    parent_aliases = parent_aliases.replace('\n',' ')
    # as long as there are any double spaces, keep on
    # getting rid of them
    while parent_aliases.find('  ')!=-1:
        parent_aliases = parent_aliases.replace('  ',' ')

    # split the semicolon-delimited list apart
    # parent_aliases = map(string.strip,
    #                      string.split(parent_aliases,';'))
    parent_aliases = [x.strip() for x in parent_aliases.split(';')]

    # for each of the tag-parents being added, create it if
    # necessary then add it as a tag-parent
    for parent_alias in parent_aliases:

        # add the new tag-parent
        add_tag_child_a_to_tag_parent_a(child_alias,parent_alias)

    return get_tag_parents_delim(child_nugid)



############################################################
def put_tag_parents_delim3(child_nugid,parent_aliases):
    """
    Third way notes.
    """
    
    # make sure child_nugid is int
    child_nugid = int(child_nugid)

    if not isinstance(parent_aliases,str):
        raise ShouldBeStr
    
    # split the semicolon-delimited list apart and remove
    # white space around each alias
    parent_aliases = [x.strip() for x in parent_aliases.split(';')]

    # get the nugids and aliases of the aliases that already have nugids
    full_parent_aliases = fsqa.session.query(Alias).select(Alias.c.alias.in_(*parent_aliases))
    existing_parent_nugids = [x.nugid for x in full_parent_aliases]
    existing_parent_aliases = [x.alias for x in full_parent_aliases]

    # get the list of aliases that do not have nugids
    new_aliases = filter(lambda x: x not in existing_parent_aliases, parent_aliases)
  
    # get the child nugget we'll be dealing with
    child_nug = fsqa.session.query(Nugget).select_by(id=child_nugid)[0]

    # loop over the current list of tag_parents
    for n,tag_parent in enumerate(child_nug.tag_parents):
        # see if it's in the new list of parent aliases
        if any([x in parent_aliases for x in tag_parent.dest_nugget.aliases]):
            # keep it and remove
            toremove
            pass
        else:
            # delete it from the list
            del child_nug.tag_parents[n]

    # find out the ones that are already in nugget_assocs
    s = select(fsqa.aliases.c.alias,\
                   (fsqa.nugget_assocs.c.src_nugid==child_nugid) & \
                   (fsqa.nugget_assocs.c.assoc_type_id==ASSOC_TAG) & \
                   (fsqa.nugget_assocs.c.dest_nugid==fsqa.aliases.c.nugid) & \
                   (fsqa.aliases.c.alias.in_(*parent_aliases)))
    alreadyThere = [x.alias for x in s.execute()]

    # pick the ones to add
    remaining_parents = filter(lambda x: x not in alreadyThere, parent_aliases)

    # Add in the missing tag parents
    # loop over the aliases that must be added
    for alias in remaining_parents:
        # see if the alias already exists
        if alias in existing_parent_aliases:
            # query for the nugget
            pass
        else:
            # add the new nugget
            pass

    # save all our updates to the child_nugget
    fsqa.session.save(child_nug)



############################################################
def put_tag_parents_delim2(child_nugid,parent_aliases):
    """
    Replaces CHILD_NUGID nugget's existing tag_parents with
    PARENT_ALIASES, a semicolon-delimited string.

    Replacement for put_tag_parents_delim
    """

    # make sure child_nugid is int
    child_nugid = int(child_nugid)

    # get the first alias of the nugid (will error if child_nugid does not exist)
    s = select([fsqa.aliases.c.alias],fsqa.aliases.c.nugid==child_nugid)
    child_alias = s.execute().fetchall()[0].alias

    if not isinstance(parent_aliases,str):
        raise ShouldBeStr
    
    # split the semicolon-delimited list apart and remove
    # white space around each alias
    parent_aliases = [x.strip() for x in parent_aliases.split(';')]
    # remove empty aliases
    parent_aliases = filter(lambda x: len(x)>0, parent_aliases)

    # remove all the obsolete tag parents
    # get a selection of all the nugids for the parent_aliases
    s = select([fsqa.aliases.c.nugid],fsqa.aliases.c.alias.in_(*parent_aliases))
    # use that selection in the deletion of the unused tag parents
    fsqa.nugget_assocs.delete((fsqa.nugget_assocs.c.src_nugid==child_nugid) & \
                              (fsqa.nugget_assocs.c.assoc_type_id==ASSOC_TAG) & \
                              ~(fsqa.nugget_assocs.c.dest_nugid.in_(s))).execute()
    
    # find out the parent_aliases that are already in nugget_assocs
    s = select([fsqa.aliases.c.alias],
               (fsqa.nugget_assocs.c.src_nugid==child_nugid) & \
               (fsqa.nugget_assocs.c.assoc_type_id==ASSOC_TAG) & \
               (fsqa.nugget_assocs.c.dest_nugid==fsqa.aliases.c.nugid) & \
               (fsqa.aliases.c.alias.in_(*parent_aliases)))
    alreadyThere = [x.alias for x in s.execute()]

    # get list of aliases not in nugget_assocs
    notThere = filter(lambda x: x not in alreadyThere, parent_aliases)

    # see which of the aliases to add are not yet in the database
    s = select([fsqa.aliases.c.alias],
               fsqa.aliases.c.alias.in_(*notThere))
    hasAlias = [x.alias for x in s.execute()]
    
    # get list of aliases whose nuggets we must create
    aliasExists = [x in hasAlias for x in notThere]

    # add in the tag_parents that are not there
    for n,alias in enumerate(notThere):
        # add the nugget for the alias if necessary
        if not aliasExists[n]:
            # must create
            nug = Nugget()
            nug.content = '[Created as tag-parent for %s]' % child_alias
            nug.filename = add_ext(alias)
            # add the alias
            nug.aliases.append(Alias(alias))
            # fill the nugget
            f = open( os.path.join(fsqa.database_dir,nug.filename), 'w')
            f.write(nug.content)
            f.close()
        else:
            # just select the Nugget that is not already added
            nug = fsqa.session.query(Nugget).select_by(alias=alias)[0]

        # make sure the child_nugid is not already there
        if not any([x.src_nugid == child_nugid for x in nug.tag_children]):
            # add the child nugget association
            nug.tag_children.append(TagChild(child_nugid))

            # save the new nugget
            fsqa.session.save(nug)

            # flush that session (I had to do this in the loop for some reason)
            fsqa.session.flush()



############################################################
def put_tag_children_delim_a(parent_alias,child_aliases):
    # interactions[put_tag_children_delim_a] = ''

    return put_tag_children_delim(
        get_nugid_from_alias(parent_alias),child_aliases)



############################################################
def put_tag_children_delim(parent_nugid,child_aliases):
    """Replaces PARENT_NUGID nugget's existing tag_children with
    CHILD_ALIASES, a semicolon-delimited string."""
    # interactions[put_tag_children_delim] = ''

    # just in case
    parent_nugid = int(parent_nugid)
    parent_alias = get_alias_from_nugid(parent_nugid)

    # start by removing all the existing tag_children from
    # parent_nugid
    remove_tag_children_from(parent_nugid)

    # split the semicolon-delimited list apart
    # child_aliases = map(string.strip,
    #                     string.split(child_aliases,';'))
    child_aliases = [x.strip() for x in child_aliases.split(';')]

    # for each of the tag-children being added, create it if
    # necessary then add it as a tag-parent
    for child_alias in child_aliases:

        # add the new tag-child
        add_tag_child_a_to_tag_parent_a(child_alias,parent_alias)

    return get_tag_children_delim(parent_nugid)



############################################################
def get_or_create_nugid_from_alias(al):
    """Tries to get the NUGID for alias AL. If that fails,
    i.e. the alias doesn't exist, then create a new nugget
    with that alias. Returns the NUGID either way, as well
    as a boolean for whether the nugget existed beforehand."""

    try:
        nugid = get_nugid_from_alias(al)
        if nugid:
            existed = True;
        else:
            # get_nugid_from_alias returns None if you feed
            # it None or an empty string
            existed = False
    except:
        filename = add_ext(al)
        nugid = add_nugget(filename,'')
        existed = False;

    return nugid,existed



############################################################
def get_nugid_from_alias(al, allow_multiple=False):
    """An alias of '#%d' is a special code for the nugid,
    so just return the %d part. Aliases with '#' in the name
    that aren't of this form will cause an exception."""

    # fail gracefully if we're fed empty air
    if not al:
        return None
    if not isinstance(al,str):
        raise ShouldBeStr
    if len(al)==0:
        return None

    # the alias shouldn't have an extension
    if has_ext(al):
        raise NeedsNoExtension

    # remove surplus whitespace (multiple spaces, carriage
    # returns etc.)
    al = remove_surplus_whitespace(al)

    # deal with wildcards
    al = al.replace('*', '%')

    # remove this code to deal with '#id' aliases - xxx
    if al[0] == '#':
        # if the alias matches the #%d formula
        if int(al[1:]):
            # check that %d is a real nugget
            if exist_nugget(int(al[1:])):
                # we're done - return that nugid
                return int(al[1:])
            else:
                raise NonexistentNugid

    sel = 'SELECT nugid FROM aliases WHERE alias LIKE "%s"' % al
    nugid = fsqa.db.execute(sel).fetchall()

    if len(nugid)>1:
        if allow_multiple:
            pass
        else:
            raise WildcardMatchesMultipleAliases

    if nugid:
        # fetchall returns a list of tuples. we already know
        # that there's only one item in the list, so we can
        # just grab it
        nugid = nugid[0][0]
    else:
        # raise 'Nug %s doesn''t exist' % al
        raise NonexistentAlias

    return nugid


############################################################
def get_nugid_from_filename(filename):

    if not has_ext(filename):
        raise NeedsExtension

    sel = 'select id from nuggets where filename = "%s"' % \
          filename
    nugid = (fsqa.db.execute(sel).fetchone())

    if not nugid:
        raise NonexistentFilename
    else:
        nugid = nugid[0]

    return nugid


############################################################
def get_all_ids():
    """Return a list of nugget ID numbers."""
    
    sel = 'select id from nuggets'

    ids = fsqa.db.execute(sel).fetchall()
    return [x[0] for x in ids]


############################################################
def get_all_aliases(nugids_to_exclude=None):
    """Return all the aliases in the database - useful for
    creating tab-completion lists."""
    
    sel = 'select alias from aliases'

    if nugids_to_exclude:

        if not isinstance(nugids_to_exclude,list):
            raise ShouldBeList

        # if any of the nugids_to_exclude are None, ignore
        # them
        while None in nugids_to_exclude:
            nugids_to_exclude.remove(None)

        # make sure they're all strings
        # nugids_to_exclude = map(str,nugids_to_exclude)
        nugids_to_exclude = [str(x) for x in nugids_to_exclude]

        sel = sel + \
              ' where nugid not in (' + \
              string.join(nugids_to_exclude,', ') + \
              ')'
    aliases = fsqa.db.execute(sel).fetchall()
    return [str(x[0]) for x in aliases]


############################################################
# def get_all_aliases_and_ids(nugids_to_exclude=None):
#     """Return a list of aliases and nugget id-strings (in '#%d'
#     form).
#     """
    
#     # need to remove any of the nugids_to_exclude from the
#     # result of get_all_ids
#     #
#     # [1,2,3] -> ['1','2','3']
#     # ids_str = map(str,get_all_ids())
#     ids_str = [str(x) for x in get_all_ids()]

#     # ['1','2','3'] -> ['#1','#2','#3']
#     # ids_str_hash = map(lambda x: '#' + x, ids_str)
#     ids_str_hash = ['#' + x for x in ids_str]

#     return ids_str_hash + get_all_aliases(nugids_to_exclude)



############################################################
def get_all_filenames():
    """Return all the unique filenames in the database. Useful
    for checking against the list of files in the directory
    to see if any fsqa.db entries are missing an actual file."""
    # interactions[get_all_filenames] = ''

    sel = 'select filename from nuggets'
    filenames = fsqa.db.execute(sel).fetchall()
    # turning this into a set implicitly throws away any
    # duplicates
    filenames = set( [str(x[0]) for x in filenames] )
    filenames.discard(None)

    return list(filenames)



############################################################
def change_filename_from(old,new):
    """
    Returns the nugid of the filename you're changing if
    successful, otherwise None.

    Only changes the FILENAME field in the database - does
    not affect the actual files at all.
    """

    if old==new:
        raise FilenameAlreadyExists

    if (old==None) | (new==None):
        raise CantBeNone

    if (not isinstance(old,str)) | (not isinstance(new,str)):
        raise ShouldBeStr

    if (old=='') | (new==''):
        raise CantBeEmptyString

    # check that the OLD filename does already
    # exist in the database
    nugid = get_nugid_from_filename(old)
    
    # we want to check that the NEW filename doesn't already
    # exist in the database. we want GET_NUGID_FROM_FILENAME
    # to fail...
    new_nugid = 0
    try:
        new_nugid = get_nugid_from_filename(new)
    except:
        pass
    if new_nugid:
        raise FilenameAlreadyExists

    try:
        # change the filename field for this nugget
        upd = 'update nuggets set filename="%s" where filename = "%s"' % \
              (new,old)    
        fsqa.db.execute(upd)

        # the new filename requires a new alias (though this
        # won't be the primary alias)
        add_alias(nugid,remove_ext(new))
        # remove the old one too
        remove_alias(nugid,remove_ext(old))

        return nugid

    except:
        return None



############################################################
def get_nugids_sql(sel):
    """Returns a list of NUGIDS that match the SQL
    statement SEL - designed for statements that start with
    'select id from nuggets...'."""
    # automatically putting all the tables in the FROM
    # statement is ugly...
    ids = fsqa.db.execute(sel).fetchall()
    return [x[0] for x in ids]



############################################################
def remove_nugget_a(alias):

    if not isinstance(alias,str):
        raise ShouldBeStr

    try:
        return remove_nugget(get_nugid_from_alias(alias))
    except NonexistentNugid:
        raise NonexistentAlias
    


############################################################
def remove_nugget(nugid):

    if nugid==None:
        raise CantBeNone

    try:
        nugid = int(nugid)
    except:
        raise ShouldBeInt

    # delete all the nugget's aliases
    remove_aliases(nugid)

    dlt = 'delete from nuggets where id = %d' % nugid
    fsqa.db.execute(dlt)

    # when we delete the nugget, we also have to delete all
    # its nugget-nugget associations
    #
    # i should combine the src_nugid and dest_nugid queries into 1
    dlt = 'delete from nugget_assocs where src_nugid = %d' % nugid
    fsqa.db.execute(dlt)
    dlt = 'delete from nugget_assocs where dest_nugid = %d' % nugid
    fsqa.db.execute(dlt)

    # get rid of its history information
    dlt = 'delete from nugget_history where nugid = %d' % nugid
    fsqa.db.execute(dlt)


############################################################
def remove_all_nuggets():
    for id in get_all_ids():
        remove_nugget(id)



############################################################
# only gets used in the lisp code - remove eventually - xxx
def remove_nugget_with_filename(filename):
    sel = 'select id from nuggets where filename = "%s"' % \
          filename
    nugid = fsqa.db.execute(sel).fetchone()[0]

    remove_nugget(nugid)



############################################################
def add_timestamp_a(alias,action,
                    dt=datetime.datetime.now()):
    # interactions[add_timestamp_a] = ''
    nugid = get_nugid_from_alias(alias)

    return add_timestamp(nugid,action,dt)



############################################################
def add_timestamp(nugid,action,
                  dt=datetime.datetime.now()):
    # interactions[add_timestamp] = ''

    if not nugid:
        return

    # check ACTION is an integer
    if action!=int(action):
        raise 'Action id must be an integer'

    if action<0 | action>max_action:
        raise IllegalActionId

    ins = 'insert into nugget_history values ' + \
          '(NULL,%d,%d,"%s")' % \
          (int(nugid), # nug id of relevant nugget
           int(action), # what kind of action was performed
           dt) # present time

    fsqa.db.execute(ins)

    return dt



############################################################
def get_last_modtime_as_dt(nugid):

    """
    Returns a datetime object timestamp of the last
    modification time for this nugget.

    If there is no modtime, returns ''.
    """
    
    try: nugid = int(nugid)
    except: raise ShouldBeInt
    
    if not exist_nugget(nugid):
        raise NonexistentNugid
    
    try:
        sel = 'select max(date) from nugget_history ' + \
              'where action_id = 2 ' + \
              'and nugid = %d' % int(nugid)
        modtime = fsqa.db.execute(sel).fetchone()[0]
    except:
        return None
    
    if not modtime:
        return None

    return dt_from_str(modtime)


    
############################################################
def get_last_modtime(nugid):
    
    """
    Returns a 'yy-mm-dd hh-mm-ss' timestamp string of the
    last modification time for this nugget.
    """
    
    # xxx - should be renamed to get_last_modtime_as_str

    modtime = get_last_modtime_as_dt(nugid)

    if modtime==None:
        modtime = ''
    else:
        modtime = str(modtime)
        # strip away the microseconds
        # modtime = modtime[:-7]

    return modtime
   


############################################################
def dt_from_str(dts):

    """Takes in a string timestamp, and returns a datetime
    object, e.g.

    dt = datetime.datetime.now()
    dt == dt_from_str( str(dt) )
    """

    # str(datetime.datetime.now()) = '2007-04-09 02:07:47.293374'
    #
    # strip away the last few milliseconds digits, cos we
    # don't need them and i couldn't figure out how to feed
    # them into STRPTIME
    dts = dts[0:19]

    fmt = '%Y-%m-%d %H:%M:%S'
    # this returns a time object,but everything else is in
    # datetime, and there don't appear to be any standard
    # ways to get from one to the other. argh.
    t = time.strptime(dts,fmt)
    # so we'll use this hack from
    # http://seehuhn.de/comp/pdate to turn the time object
    # into a datetime object
    dt = datetime.datetime(*t[:6])

    return dt


############################################################
def list_unaliased():
    """
    Nuggets should always have at least their filenames as
    aliases, but just occasionally, something goes
    wrong. Run this to see if there are any nuggets that
    don't have aliases.
    """

    ids = get_all_ids()

    for id in ids:
        a = get_alias_from_nugid(id)
        if a==None:
            print nugid
            continue
        if a[0]=='#':
            print str(id) + ': ' + a


############################################################
def count_untagged():
    """
    Counts how many untagged nuggets there are.
    """

    ids = get_all_ids()
    untagged = []

    for id in ids:
        tag_parents = get_tag_parents_for(id)

        if not tag_parents:
            untagged.append(id)

    return len(untagged)



############################################################
def choose_random_untagged():
    """
    Returns the filename of the first nugget it finds that
    doesn't have any tag-parents.

    This function should really be just a stub that users
    can override in some freex_sqlalchemy_custom.py somehow,
    but I haven't set that up.
    """

    ids = get_all_ids()

    # shuffle them, so we get a different one each time
    random.seed()
    random.shuffle(ids)

    for id in ids:
        tag_parents = get_tag_parents_for(id)
        alias = get_alias_from_nugid(id)

        try:
            alias_as_num = int(alias)
        except:
            alias_as_num = None

        # don't bother tagging things that already have lots
        # of tags
        if len(tag_parents)>1:
            continue
        
        # i have a lot of aliases in the form 'yymmdd' that
        # correspond to dates. i don't want to tag these. so
        # i'm going to automatically exclude 6-digit aliases
        if len(alias)==6 and alias_as_num:
            continue

        # 'old_notes' was applied automatically. if
        # something's just tagged with 'old_notes', then it
        # still needs human-applied tags to be useful
        if len(tag_parents)==1 and tag_parents != ['old_notes']:
            continue

        # ok. it needs to be tagged
        return get_filename(id)

    return None



############################################################
def any(lst):

    """Returns True if any the items in lst are True. I
    think this functionality has been added in python 2.5,
    but I want it now."""

    if not isinstance(lst,list):
        raise ShouldBeList

    for it in lst:
        if it:
            return True

    return False


############################################################
def all(lst):

    """Returns True if all the items in lst are True. I
    think this functionality has been added in python 2.5,
    but I want it now."""

    if not isinstance(lst,list):
        raise ShouldBeList

    for it in lst:
        if not it:
            return False

    return True



############################################################
def all_str(lst):
    """
    Returns True if all the items in LST are strings.
    """

    return all([isinstance(x,str) for x in lst])



############################################################
def all_int(lst):
    """
    Returns True if all the items in LST are ints.
    """

    return all([isinstance(x,int) for x in lst])



############################################################
def list_items_type(lst):
    """
    Returns type(str) if all the items in LST are strings,
    or type(int) if all the items in LST are ints. Failing
    both of those, returns None.
    """

    if not isinstance(lst,list):
        raise ShouldBeList

    strings = all_str(lst)
    ints = all_int(lst)

    if strings:
        return str
    elif ints:
        return int
    else:
        return None



############################################################
def extract_fulltext_patterns(parent_aliases):

    """
    This is called by filter_by_tag_parents. If the
    string-list of PARENT_ALIASES contains any strings that
    start and end in double-quotes, remove them from
    PARENT_ALIASES and return them in a separate FT_PATTERNS
    list for separate grepping.

    Returns: FT_PATTERNS, PARENT_ALIASES
    """

    # i'm sure there's a nice pythonic way of doing this
    # without looping through the lists twice, but i
    # couldn't be bothered to figure it out

    ft_patterns = []

    parent_aliases_no_ft = []

    if not isinstance(parent_aliases, list):
        raise ShouldBeList

    # get rid of any empty parent_aliases that might have
    # crept in (e.g. as a result of splitting //
    # incorrectly)
    parent_aliases = [x for x in parent_aliases if x != '']

    for n,alias in enumerate(parent_aliases):
        
        if (alias[0]=='"') and (alias[-1]=='"'):
            # this is a fulltext pattern
            #
            # so add it to our list of fulltext patterns
            # (without quotes)
            ft_patterns.append(alias[1:-1])

        else:
            parent_aliases_no_ft.append(alias)

    return ft_patterns, parent_aliases_no_ft



############################################################
def fulltext_whittle_nugids(aliases, patterns, case_sensitive=False):

    """
    There has to be a better way of doing this, but if there
    are fulltext patterns, then we need to search within all
    the filenames, and return a list of both filenames and
    nugids that match the fulltext pattern(s).

    Returns: NUGIDS, ALIASES
    """

    if len(aliases)>0:
        # create a quote-delimited aliases string
        aliases_str = string.join( \
            ['"%s"' % x for x in aliases],
            ',')
        sel = """
        SELECT DISTINCT filename FROM nuggets,aliases WHERE
        nuggets.id=aliases.nugid AND
        alias in (%s)
        """ % aliases_str

        # 1/0

        
        filenames = fsqa.db.execute(sel).fetchall()
        
        if len(filenames)>0:
            filenames = [x[0] for x in filenames]

    else:
        filenames = []

    found_files = fulltext(filenames, patterns, case_sensitive)

    nugids = []
    aliases = []
    for f in found_files:
        nugid = get_nugid_from_filename(f)
        nugids.append(nugid)
        aliases.append(get_aliases(nugid))

    aliases = union_lists_no_duplicates(aliases)

    return nugids, aliases



############################################################
def fulltext(filenames, patterns, case_sensitive=False):

    """
    Returns: FOUND_FILES (list of filename strings)
    
    Feed in a list of filenames (complete with extensions),
    which will be fed to agrep for full-text
    searching. Returns a list of files.

    FILENAMES is a list of strings. If its non-empty, then
    these will be fed in to agrep. If it's empty, then we'll
    just feed in a '*.[freex_extension]'. Spaces in
    filenames are escaped with backslashes, but this is the
    only thing we're escaping.

    PATTERNS is a list of strings, which will be ANDed
    together in the agrep regex. Currently, this doesn't
    escape the pattern regex at all, though it does surround it in
    quotes, so the usual agrep rules apply.

    Unless case_sensitive==True, will append a -i flag.

    I think this has been superseded by
    FILTER_BY_TAG_PARENTS, which uses the database to do
    fulltext search.
    """

    # xxx this should check that all the files have extensions

    if not isinstance(filenames,list):
        raise ShouldBeList
    if not isinstance(patterns,list):
        raise ShouldBeList
    
    if case_sensitive:
        case_flag = ''
    else:
        case_flag = '-i'

    # xxx should check that all the items in the pattern
    # list are strings...
    #
    # first strip each of the pattern strings of whitespace,
    # and remove the surrounding quotes - we'll add them
    # back to the whole pattern_str when we create the CMD
    #
    # then AND together multiple patterns with agrep,
    # using semicolons
    for pat in patterns:
        if pat[0]=='"':
            pat = pat[1:]
        if pat[-1]=='"':
            pat = pat[0:-1]
            
    pattern_str = string.join( \
        [x.strip() for x in patterns], \
        ';')

    if len(filenames)>0:

        # escape all the spaces with back-slashes
        filenames = [x.replace(' ', '\ ') for x in filenames]

        # convert to a space-delimited string (with spaces
        # escaped by backslashes), and each file prepended by the
        # database_dir, e.g.
        # /blah/test0.freex /blah/hello\ world.freex
        fnames_str = string.join(
            [fsqa.database_dir + '/' + x for x in filenames],
            ' ')

        # the -l says to just return filenames only (no text
        # context)
        #
        # put the pattern in quotes
        #
        # and then just list the files at the end
        cmd = 'agrep -l %s "%s" %s' % \
              (case_flag, pattern_str, fnames_str)


    else:
        # if we're not restricting the files we're looking
        # through, then there could be too many files to run
        # agrep on directly, so we have to pipe it from a
        # find
        #
        # this is to avoid the '/usr/local/bin/agrep:
        # Argument list too long' error
        cmd = 'find %s -name "*.%s" -print0 | xargs -0 agrep -l %s "%s"' \
              % (fsqa.database_dir, fsqa.file_ext, case_flag, pattern_str)

    # by default, this will raise an exception if there's
    # any kind of problem
    #
    # don't print out the command
    #
    # don't throw an exception, because it looks as though
    # agrep returns RETCODE==1 if it doesn't find any
    # matches
    verbose = False
    retcode, out_str, err_str, subpopen = subproc(cmd, \
                                                  verbose=verbose,
                                                  throw_exception=False)

    if len(out_str)>0:
        # strip away the path to yield just the filename for
        # each of the files in out_str
        found_files = [os.path.basename(x) for x in \
                       string.split(out_str.strip(),'\n')]

    else:
        # if you run the above on an empty string, you get
        # [''], whereas we really want to return an empty
        # list if we didn't find anything
        found_files = []

    # print retcode
    # print out_str
    # print err_str

    return found_files



############################################################
def subproc(cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            verbose=True,
            throw_exception=True):

    """
    Why can't python have a subproc function like this (like
    matlab's 'unix')? Takes in a CMD, returns a RETCODE,
    OUT_STR and ERR_STR strings, and the SUBPOPEN
    subprocess.Popen object.

    RETCODE = 0 (all is well)
    RETCODE = -1 (caught an exception of some kind)
    RETCODE = non-zero (the command ran, but the shell didn't like it)

    If VERBOSE, then it will print CMD before running it,
    and the OUT_STR and ERR_STR after running (if they're
    non-empty).

    If THROW_EXCEPTION, will throw an exception if RETCODE is
    non-zero. If not THROW_EXCEPTION, it'll just return the RETCODE
    and it's up to you to check it.

    Returns: RETCODE, OUT_STR, ERR_STR, SUBPOPEN

    N.B. this is more cautious than matlab's 'unix' command,
    because its default is to throw an exception if there's
    any kind of problem.

    Update: this sucks if you want to use pipes (e.g. 'cat *
    > blah'). In that case, I ended up just using
    os.system().

    Usage e.g.:

        retcode, out_str, err_str, subpopen = subproc('ls')

    """

    if verbose:
        print cmd

    # it's ugly having two different routes for
    # throw_exception, but i don't know how to store and
    # return the exception traceback
    if throw_exception:
        # call subprocess.Popen nakedly, without bothering to catch exceptions
        subpopen = subprocess.Popen(cmd, shell=True,
                                    stdout=stdout,
                                    stderr=stderr)
        retcode = subpopen.wait()
        out_str = subpopen.stdout.read()
        err_str = subpopen.stderr.read()

        if retcode != 0:
            # xxx need to return proper exception classes
            # rather than just strings
            raise 'Non-zero retcode: %i' % retcode

        if len(err_str)>0:
            print out_str
            print err_str
            raise 'Non-empty error string'

    else:
        # wrap any exception, and let the user know with the retcode = -1
        try:
            subpopen = subprocess.Popen(cmd, shell=True,
                                        stdout=stdout,
                                        stderr=stderr)
            retcode = subpopen.wait()
            out_str = subpopen.stdout.read()
            err_str = subpopen.stderr.read()
        except:
            out_str = ''
            err_str = ''
            subpopen = ''
            retcode = -1

    if verbose & len(out_str)>0:
        print out_str
    if verbose & len(err_str)>0:
        print err_str

    return retcode, out_str, err_str, subpopen




############################################################
#
# vectorized versions that take either int nugid or string alias



############################################################
def exist_nugget_multi(nugs):

    """
    This takes in either a:
    
    - single int nugid
    - str alias
    - list of int nugids
    - list of str aliases

    and determines whether they all exist. Returns 
    """

    # this appears to be faster than exist_nugget for many
    # nuggets (244ms rather than 658ms) for 2500 nugids (in_memory=0)
    #
    # s = Stopwatch(); s.start(); exist_nugget_multi(nugids); print s.finish()
    # s = Stopwatch(); s.start(); map(exist_nugget,nugids); print s.finish()

    if nugs==None:
        raise CantBeNone

    # all of the following functions are designed to operate
    # on lists, even lists of one
    if not isinstance(nugs,list):
        nugs = [nugs]

    # get the type of items in the NUGS list
    lit = list_items_type(nugs)

    if lit==int:
        nugids = nugs

        sel = 'select nugid,alias from aliases where '
        # these are the SELECT criteria
        crits = []
        for nugid in nugids:
            crits.append('nugid="%i"' % nugid)
        sel = sel + string.join(crits,' OR ')
        
        results = fsqa.db.execute(sel).fetchall()
        found_nugids = [x[0] for x in results]
        
        found = [False]*len(nugids)
        for a in xrange(0,len(nugids)):
            if nugids[a] in found_nugids:
                found[a] = True
        
    elif lit==str:

        aliases = nugs
        sel = 'select nugid,alias from aliases where '
        # these are the SELECT criteria
        crits = []
        for al in aliases:
            crits.append('alias="%s"' % al)
        sel = sel + string.join(crits,' OR ')
        
        results = fsqa.db.execute(sel).fetchall()
        found_aliases = [x[01] for x in results]

        found = [False]*len(aliases)
        for a in xrange(0,len(aliases)):
            if aliases[a] in found_aliases:
                found[a] = True
            
    else:
        raise IncorrectListItemsType

    return found
        

# ############################################################
# def dupes(lst):

#     """
#     Returns True if there are any duplicates in LST list.
#     """

#     return len(lst)!=len(set(lst))

    

############################################################
# i couldn't figure out a way to get this to behave right if
# there are duplicate aliases, i.e. it just ignored them...
def get_nugid_from_alias_multi(aliases):

    """
    Just like get_nugid_from_alias but allowing a list of
    ALIASES, and returning a list of NUGIDs

    This function should be equal to:

      return [get_nugid_from_alias[x] for x in aliases]

    except that it will raise an exception if any of the
    aliases are duplicates, or if any of them don't exist.
    """

    # comparing speed:
    # aliases = ['nugid1','nugid2','nugid3','nugid4','nugid1000000'] * 500
    # s = Stopwatch(); s.start(); try: map(get_nugid_from_alias,aliases) ; except: pass ; print s.finish()
    # s = Stopwatch(); s.start(); get_nugid_from_alias_multi(aliases); print s.finish()

    if not aliases:
        return None

    if not isinstance(aliases,list):
        aliases = [aliases]

    # aliases must be a list containing strings
    lit = list_items_type(aliases)
    if lit != str:
        raise ShouldBeStr

    # get a boolean list of which aliases exist
    # exists = exist_nugget_multi(aliases)

    # need to fix the second way of doing this. need to
    # change the API so that this doesn't guarantee that the
    # list of nugids it returns is of the same length as the
    # list of aliases you fed in. if it returns both nugids
    # and aliases, then the caller function can figure out
    # which ones don't exist, if need be

    #     sel = 'select nugid,alias from aliases where '
    #     # these are the SELECT criteria
    #     crits = []
    #     for al in aliases:
    #         crits.append('alias="%s"' % al)
    #     sel = sel + string.join(crits,' OR ')
    #     found_nugids = fsqa.db.execute(sel).fetchall()
    #     found_nugids = [x[0] for x in found_nugids]

    #     sel = 'select nugid,alias from aliases where alias in (%s)' \
    #           % string.join(aliases,' ')
    #     found_nugids = fsqa.db.execute(sel).fetchall()
    #     found_nugids = [x[0] for x in found_nugids]

    nugids = [None] * len(aliases)

    # how do i say:
    # nugids(find(exists)) = found_nugids;
    #
    cur = 0
    for a in xrange(0,len(aliases)):
        if exists[a]:
            nugids[a] = found_nugids[cur]
            cur = cur + 1

    return nugids



def uniquify_list(lst):
    """Return a list of the elements in s, but without duplicates, preserving order.

    from comment in http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/52560

      Lightweight and fast ..., Raymond Hettinger, 2002/03/17

    """

    set = {}
    return [set.setdefault(e,e) for e in lst if e not in set]



# update_implicit_link_regexp = update_implicit_link_regexp_original
# get_all_matching_implicit_links = get_all_matching_implicit_links_original

update_implicit_link_regexp = update_implicit_link_regexp_firstn
get_all_matching_implicit_links = get_all_matching_implicit_links_twostage

