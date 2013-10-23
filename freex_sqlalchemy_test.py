#!/usr/bin/env python

"""
Unit tests for freex_sqalchemy.py.

Run this from the command line with 'python
freex_sqalchemy_test.py -v'.

Each test will automatically create a freex_test.db in the current
directory, deleting it if it exists already. Will populate
this with a smattering of test records, and then all the
tests will work on that database.

Copyright 2007, Greg Detre, Per Sederberg.
"""

# questions:
#
# - how do i test create_fsqa, if i'm relying on that to do
# all the testing???
#
#   i suppose i need tests specifically for it that don't
#   run self.setUp()
#
# - likewise, the populate() function should probably just
# copy over an existing, pre-populated .db file from
# somewhere, or run the sqlite statements itself

# if you want to run things interactively in ipython:
#
# from freex_sqlalchemy_test import *
# fst = FsqaTests()
# fst.setUp()


import unittest, os
from freex_sqlalchemy import *

import pdb
import random
import sqlalchemy


############################################################
class BaseTests(unittest.TestCase):
    test_database_dir = '.'
    test_database_file = 'freex_test.db'

    # whether or not to look at the hard disk at all
    # (e.g. for put_contents, which is supposed to write out
    # to files)
    #
    # i'm not sure if i've implemented anything that
    # actually uses this variable
    check_files = True

    # this will contain the object created in
    # freex_sqlalchemy that has the db, session and metadata
    # variables
    fsqa = None

    # each desired nugget consists of int(NUGID) and
    # str(FILENAME) (including .freex)
    fake_nuggets = ( (1, 'nugid1.freex', 'this is the first nug'),
                     (2, 'nugid2.freex', 'this is the second nug'),
                     (3, 'nugid3.freex', 'this is the third nug'),
                     (4, 'nugid4.freex', 'this is the fourth nug'),
                     (5, 'nugid5.freex', 'this is the fifth nug'),
                     )

    # each fake alias consists of int(NUGID) (which must
    # already exist) and str(ALIAS) (without .freex
    # extension)
    fake_aliases = ( (1, 'nugid1a'),
                     (1, 'nugid1b'),
                     (1, 'nugid1c'),
                     (2, 'nugid2a'),
                     (2, 'nugid2b'),
                     (3, 'nugid3a'),
                     (4, 'nugid4a'),
                     (5, 'nugid5a'),
                     )

    fake_nugget_assocs = ( # src_nugid, dest_nugid, assoc_type_id
        ( 2, 1, 1 ), # 2 is a tag-child of 1
        ( 3, 2, 1 ), # 3 is a tag-child of 2
        ( 3, 4, 1 ), # 3 is a tag-child of 4
                           )


########################################
    def runTest(self):
        """Required for this class to work in ipython"""
        
        pass



########################################
    def setUp(self,in_memory=1):
        """
        Deletes 'freex_test.db' if it exists, and creates a
        new Fsqa instance, which will create a new, empty
        'freex_test.db'. Does not populate the database.

        Used to be called reset(), but calling it setUp
        means that it automatically gets run at the
        beginning of every test.
        """

        if in_memory==1:

            # i don't think this will need a filename, but
            # we'll give it one just to be sure
            self.fsqa = create_fsqa('freex_test_in_memory.db',
                                    use_lisp=0,in_memory=1)

            # if you ever do see this file in
            # your directory, it's a bug
            #
            # i should move this to a testSetUp
            if os.path.exists('freex_test_in_memory.db'):
                raise 'Memory db shouldn''t exist as a file'
        
        else:
            # only bother with all the filesystem stuff if
            # we're creating a file-based db
            db_fullfile = os.path.join(
                self.test_database_dir,self.test_database_file)
            
            # if the database file exists already, delete it and
            # create one afresh
            if os.path.exists(db_fullfile):
                os.remove(db_fullfile)

            self.fsqa = create_fsqa(db_fullfile,
                                    use_lisp=0,in_memory=0)


########################################
    def populate(self):
        """
        Populate the database with a bunch of fake records
        """

        for nugid,filename,content in self.fake_nuggets:
            add_nugget(filename,content)

        for nugid,alias in self.fake_aliases:
            add_alias(nugid,alias)

        # tags: child, parent, 1
        for src,dest,assoc_type in self.fake_nugget_assocs:
            # manually insert all the tag relationships
            ins = 'insert into nugget_assocs values (NULL,%d,%d,%d,"%s")' % \
                  (src,dest,assoc_type,datetime.datetime.now())
            self.fsqa.db.execute(ins)



############################################################
class FsqaTests(BaseTests):
    """
    This is the set of test cases for testing freex_sqlalchemy.py
    """

########################################
    def test_populate(self):

        """Test the unit test populate function"""

        # if the unit test db population function doesn't
        # work, then none of the tests will work

        # to begin with, there shouldn't be anything in the
        # db at all
        self.assertEquals([],get_all_ids())
        self.assertEquals([],get_all_aliases())

        # we're not going to check everything about the
        # population function, but at least confirm that
        # it's inserting nuggets and aliases. ignore assocs
        # for now
        self.populate()
        ids = get_all_ids()
        aliases = get_all_aliases()
        self.assertEquals(5, len(ids))
        self.assertEquals(13, len(get_all_aliases()))

        # it should be running the setUp function at the
        # beginning of every test, so let's just confirm
        # that the setUp function does indeed reset
        # everything
        self.setUp()
        self.assertEquals([],get_all_ids())
        self.assertEquals([],get_all_aliases())

        

########################################
    def test_add_tag_child_to_tag_parent_a(self):

        """Adding a tag assoc (child nugid, parent alias)"""

        self.populate()

        # to begin with, nugid4 won't have any tags
        self.assertEquals([],get_tag_parents_for_a('nugid5'))

        # now, we're going to say that nugid5 is a tag-child of nugid4
        add_tag_child_to_tag_parent_a(5,'nugid4')
        self.assertEquals(['nugid4'],get_tag_parents_for_a('nugid5'))
        self.assertEquals(['nugid3','nugid5'],get_tag_children_for_a('nugid4'))

        # there's no way to call this if child doesn't exist

        # if the parent doesn't exist, it should create a new parent
        max_id = max(get_all_ids())
        assocs = add_tag_child_to_tag_parent_a(4,'new parent')
        child = assocs[0]
        parent = assocs[1]
        # the child should have nugid4's nugid
        self.assertEquals(child,4)
        # the parent should have a new nugid
        self.assertEquals(parent,max_id+1)
        self.assertEquals('new parent',get_alias_from_nugid(parent))

        # if you try and add an existing tag assoc, it
        # should do nothing and return None
        assocs = add_tag_child_to_tag_parent_a(4,'new parent')
        self.assertEquals(assocs,None)

        # if child and parent nugids are the same, it'll
        # fail and return None
        self.assertEquals(None,add_tag_child_to_tag_parent_a(1,'nugid1b'))

        # aberrant inputs
        #
        # if you feed it None, it should just quietly fail and return None
        self.assertEquals(None,add_tag_child_to_tag_parent_a(4,None))
        # if you feed it '', it should just quiety fail and return None
        self.assertEquals(None,add_tag_child_to_tag_parent_a(4,''))
        # xxx but because this function is lazy and just
        # calls get_alias_from_nugid, you actually get an
        # exception sometimes
        self.assertRaises(ShouldBeInt,add_tag_child_to_tag_parent_a,None,'nugid4')
        self.assertRaises(ShouldBeInt,add_tag_child_to_tag_parent_a,'','nugid4')
                          



########################################
    def test_add_tag_child_a_to_tag_parent_a(self):

        """Adding a tag assoc (child alias, parent alias)"""

        # this is just like above
        # (test_add_tag_child_to_tag_parent_a, except that
        # here the child is an alias rather than a nugid

        self.populate()

        # to begin with, nugid4 won't have any tags
        self.assertEquals([],get_tag_parents_for_a('nugid5'))

        # now, we're going to say that nugid5 is a tag-child of nugid4
        add_tag_child_a_to_tag_parent_a('nugid5','nugid4')
        self.assertEquals(['nugid4'],get_tag_parents_for_a('nugid5'))
        self.assertEquals(['nugid3','nugid5'],get_tag_children_for_a('nugid4'))

        # if child doesn't exist, it should create a new child
        max_id = max(get_all_ids())
        assocs = add_tag_child_a_to_tag_parent_a('new child','nugid4')
        child = assocs[0]
        parent = assocs[1]
        # the child should have a new nugid
        self.assertEquals(child,max_id+1)
        self.assertEquals('new child',get_alias_from_nugid(child))
        # the parent should have nugid4's nugid
        self.assertEquals(parent,4)

        # if the parent doesn't exist, it should create a new parent
        max_id = max(get_all_ids())
        assocs = add_tag_child_a_to_tag_parent_a('nugid4','new parent')
        child = assocs[0]
        parent = assocs[1]
        # the child should have nugid4's nugid
        self.assertEquals(child,4)
        # the parent should have a new nugid
        self.assertEquals(parent,max_id+1)
        self.assertEquals('new parent',get_alias_from_nugid(parent))

        # if you try and add an existing tag assoc, it
        # should do nothing and return None
        assocs = add_tag_child_a_to_tag_parent_a('nugid4','new parent')
        self.assertEquals(assocs,None)

        # if child and parent nugids are the same, it'll
        # fail and return None
        self.assertEquals(None,add_tag_child_a_to_tag_parent_a('nugid1a','nugid1b'))

        # aberrant inputs
        #
        # if you feed it None, it should just quietly fail and return None
        self.assertEquals(None,add_tag_child_a_to_tag_parent_a(None,'nugid4'))
        self.assertEquals(None,add_tag_child_a_to_tag_parent_a('nugid4',None))
        # if you feed it '', it should just quiety fail and return None
        self.assertEquals(None,add_tag_child_a_to_tag_parent_a('','nugid4'))
        self.assertEquals(None,add_tag_child_a_to_tag_parent_a('nugid4',''))



########################################
    def test_change_filename_from(self):

        """Change filename"""

        self.populate()

        if not 'nugid1.freex' in get_all_filenames():
            raise 'Problem with populating the db'

        # standard filename change
        nugid = change_filename_from('nugid1.freex','new_nugid1.freex')
        self.assertEquals(nugid,1)
        self.assertEquals(get_filename(1),'new_nugid1.freex')
        self.assertEquals(False, 'nugid1.freex' in get_all_filenames())
        self.assertEquals(True, 'new_nugid1' in get_aliases(1))

        # filename with dots in
        nugid = change_filename_from('nugid2.freex','nugid2.with.dots.freex')
        self.assertEquals(nugid,2)
        self.assertEquals(get_filename(2),'nugid2.with.dots.freex')
        self.assertEquals(False, 'nugid2.freex' in get_all_filenames())
        self.assertEquals(True, 'nugid2.with.dots' in get_aliases(2))

        # neither filename can be none or empty
        self.assertRaises(CantBeNone, change_filename_from,'nugid1.freex', None)
        self.assertRaises(CantBeNone, change_filename_from, None, 'newfile.freex')
        self.assertRaises(CantBeEmptyString, change_filename_from,'nugid1.freex', '')
        self.assertRaises(CantBeEmptyString, change_filename_from,'', 'newfile.freex')

        # OLD and NEW can't be identical
        self.assertRaises(
            FilenameAlreadyExists,
            change_filename_from, 'nugid1.freex', 'nugid1.freex')

        # the OLD must exist
        self.assertRaises(NonexistentFilename, change_filename_from, 'newfile.freex', 'nugid2.freex')
        self.assertRaises(NonexistentFilename, change_filename_from, 'newfile.freex', 'newfile2.freex')
        
        # the NEW must not exist
        self.assertRaises(FilenameAlreadyExists, change_filename_from, 'nugid3.freex', 'nugid4.freex')



########################################
    def test_filter_by_tag_parents(self):

        """This is the tab-completion function called by complete-alias"""

        self.populate()

        first_order_cases = (
            ('', get_all_aliases()),
            # ('*', get_all_aliases()),
            
            ('nug', get_all_aliases()),

            # any nuggets that begin with nugid1*, i.e. this
            # is not yet actually looking for tag-children
            ('nugid1', ['nugid1','nugid1a','nugid1b','nugid1c']),
            # this time the stub 'blah*' doesn't match anything
            ('blah', []),
            
            ('nugid1/', ['nugid1/nugid2','nugid1/nugid2a','nugid1/nugid2b']),

            # again, the stub doesn't matter
            ('nugid1/blah', ['nugid1/nugid2','nugid1/nugid2a','nugid1/nugid2b']),

            # note the slash on nugid4 (because it's a
            # suggested tag-parent for nugid3)
            ('nugid2/', ['nugid2/nugid3', 'nugid2/nugid3a',
                         'nugid2/nugid4/', 'nugid2/nugid4a/']),

            ('nugid2/nugid1/',[]),

            # conjunction of two parents
            #
            # for now, it's not, so this is the test
            ('nugid4a/nugid2/',['nugid4a/nugid2/nugid3','nugid4a/nugid2/nugid3a']),

            # xxx - in the future, this function should do a
            # LIKE query on the stub. right now, it doesn't
            # bother because emacs will do this for us, but
            # we should eventually
            #
            # N.B. the only, tiny difference from the above
            # is that nugid2 isn't a parent here, it's a
            # stub
            #
            # ('nugid4a/nugid2',['nugid4a/nugid2a/','nugid4a/nugid2/']),
            )

        for inp,desired_outp in first_order_cases:
            desired_outp.sort()
            actual_outp = filter_by_tag_parents(inp, second_order_tags=False)
            actual_outp.sort()

            self.assertEquals(desired_outp,actual_outp)


        second_order_cases = (
            ('', get_all_aliases()),
            
            ('nug', get_all_aliases()),

            # this should be exactly the same as above
            ('nugid1', ['nugid1','nugid1a','nugid1b','nugid1c']),
            # the same is true if the stub itself doesn't match anything
            ('blah', []),

            # this is tricky to think about, but i think
            # it's right now
            #
            # 2 is a tag-child of 1, and 3 is a tag-child of
            # 2, so all the entries from both 1 and 2 should
            # be included
            ('nugid1/', ['nugid1/nugid2','nugid1/nugid2a','nugid1/nugid2b',
                         'nugid1/nugid3','nugid1/nugid3a',
            # on top of this, we have to suggest potential
            # extra tag-parents by which the 1s can be
            # filtered. again, the 2s are potential tag
            # parents
                         'nugid1/nugid2/','nugid1/nugid2a/','nugid1/nugid2b/',
            # 3s are not potential tag-parents because there
            # is nothing that has both 1 and 3 as a tag
            # parent
            #
            # the confusing part is that 4s *are* potential
            # tag parents, since 3s have both 1 and 4 as
            # tag-parents                        
                         'nugid1/nugid4/','nugid1/nugid4a/',
                         ]),

            # sadly, i've not been as clever about using the
            # stubs to filter things as i'd like, and so in
            # this case, i think we're relying on emacs to
            # throw away everything that doesn't match
            # 'blah*'
            #
            # in other words, this will return the same
            # output as 'nugid1/'
            ('nugid1/blah', ['nugid1/nugid2','nugid1/nugid2a','nugid1/nugid2b',
                             'nugid1/nugid3','nugid1/nugid3a',
                             'nugid1/nugid2/','nugid1/nugid2a/','nugid1/nugid2b/',
                             'nugid1/nugid4/','nugid1/nugid4a/',
                             ]),             

            # note the slash on nugid4 (because it's a
            # suggested tag-parent for nugid3)
            ('nugid2/', ['nugid2/nugid3', 'nugid2/nugid3a',
                         'nugid2/nugid4/', 'nugid2/nugid4a/']),

            ('nugid2/nugid1/', ['nugid2/nugid1/nugid3','nugid2/nugid1/nugid3a',
                                'nugid2/nugid1/nugid4/','nugid2/nugid1/nugid4a/']),

            # conjunction of two parents
            #
            # for now, it's not, so this is the test
            ('nugid4a/nugid2/',['nugid4a/nugid2/nugid3','nugid4a/nugid2/nugid3a']),

            # xxx - in the future, this function should do a
            # LIKE query on the stub. right now, it doesn't
            # bother because emacs will do this for us, but
            # we should eventually
            #
            # N.B. the only, tiny difference from the above
            # is that nugid2 isn't a parent here, it's a
            # stub
            #
            # ('nugid4a/nugid2',['nugid4a/nugid2a/','nugid4a/nugid2/']),
            )

        for inp,desired_outp in second_order_cases:
            desired_outp.sort()
            actual_outp = filter_by_tag_parents(inp, second_order_tags=True)
            actual_outp.sort()

            self.assertEquals(desired_outp,actual_outp)

        
        aberrant_cases = (
            # if you try and give it a tag-parent that
            # doesn't exist, you'll get an exception
            #
            # xxx - it would be better if it just ignored
            # the tag-parent...
            ('blah/', NonexistentAlias),
            ('blah/nug', NonexistentAlias),
            )

        for inp,exc in aberrant_cases:
            self.assertRaises(exc, filter_by_tag_parents, inp)



########################################
    def test_get_tag_parents_delim(self):

        """Get the tag-parents as a semicolon-delimited string"""

        self.populate()

        # get_tag_parents_delim takes in a nugid as input
        nugid_cases = ( # inp, desired_outp
            (1, ''),
            (2, 'nugid1'),
            (3, 'nugid2; nugid4'),
            (4, ''),
            )
        for inp,desired_outp in nugid_cases:
            actual_outp = get_tag_parents_delim(inp)
            self.assertEquals(desired_outp, actual_outp)

        # get_tag_parents_delim_a takes in an alias as input
        alias_cases = ( # inp, desired_outp
            ('nugid1', ''),
            ('nugid2', 'nugid1'),
            ('nugid2a', 'nugid1'),
            ('nugid3', 'nugid2; nugid4'),
            ('nugid4', ''),
            )
        for inp,desired_outp in alias_cases:
            actual_outp = get_tag_parents_delim_a(inp)
            self.assertEquals(desired_outp, actual_outp)

        # we could have a separate ABERRANT_CASES for both
        # get_tag_parents_delim and get_tag_parents_delim_a,
        # but if we just include the function as part of the
        # case, we can do them both at once
        #
        aberrant_cases = ( # exception, function, inp
            (NonexistentNugid, get_tag_parents_delim, 100),
            (NonexistentAlias, get_tag_parents_delim_a, 'blah'),
            (ShouldBeInt, get_tag_parents_delim, 'blah'), 
            (ShouldBeStr, get_tag_parents_delim_a, 100),
            )
        for exc,func,inp in aberrant_cases:
            self.assertRaises(exc, func, inp)




    ########################################
    def test_get_tag_children_delim(self):

        """Get the tag-children as a semicolon-delimited string"""

        self.populate()

        # just to make things a little more interesting
        add_tag_child_a_to_tag_parent_a('nugid5','nugid4')
        
        # get_tag_children_delim takes in a nugid as input
        nugid_cases = ( # inp, desired_outp
            (1, 'nugid2'),
            (2, 'nugid3'),
            (3, ''),
            (4, 'nugid3; nugid5'),
            )
        for inp,desired_outp in nugid_cases:
            actual_outp = get_tag_children_delim(inp)
            self.assertEquals(desired_outp, actual_outp)

        # get_tag_children_delim_a takes in an alias as input
        alias_cases = ( # inp, desired_outp
            ('nugid1',  'nugid2'),
            ('nugid2',  'nugid3'),
            ('nugid2a', 'nugid3'),
            ('nugid3',  ''),
            ('nugid4',  'nugid3; nugid5'),
            )
        for inp,desired_outp in alias_cases:
            actual_outp = get_tag_children_delim_a(inp)
            self.assertEquals(desired_outp, actual_outp)

        # we could have a separate ABERRANT_CASES for both
        # get_tag_children_delim and get_tag_parents_delim_a,
        # but if we just include the function as part of the
        # case, we can do them both at once
        #
        aberrant_cases = ( # exception, function, inp
            (NonexistentNugid, get_tag_children_delim, 100),
            (NonexistentAlias, get_tag_children_delim_a, 'blah'),
            (ShouldBeInt, get_tag_children_delim, 'blah'), 
            (ShouldBeStr, get_tag_children_delim_a, 100),
            )
        for exc,func,inp in aberrant_cases:
            self.assertRaises(exc, func, inp)



    ############################################################
    def test_get_tag_parents_for(self):

        """Get the tag parents for a nugid"""
        
        # this is really the same as above
        # (test_get_tag_parents_delim), but we'd better test
        # it somewhat anyway

        self.populate()
        
        nugid_cases = ( # inp, desired_outp
            (1, []),
            (2, [1]),
            (3, [2,4]),
            (4, []),
            )
        for inp,desired_outp in nugid_cases:
            actual_outp = get_tag_parents_for(inp)
            self.assertEquals(desired_outp, actual_outp)

        alias_cases = ( # inp, desired_outp
            ('nugid1',  []),
            ('nugid2',  ['nugid1']),
            ('nugid2a', ['nugid1']),
            ('nugid3',  ['nugid2', 'nugid4']),
            ('nugid4',  []),
            )
        for inp,desired_outp in alias_cases:
            actual_outp = get_tag_parents_for_a(inp)
            self.assertEquals(desired_outp, actual_outp)

        # xxx - i'm not sure why this returns None but
        # get_tag_parents_a raises NonexistentAlias
        self.assertEquals([],get_tag_parents_for(100)),

        # we could have a separate ABERRANT_CASES for both
        # get_tag_parents_for and get_tag_parents_for_a,
        # but if we just include the function as part of the
        # case, we can do them both at once
        #
        aberrant_cases = ( # exception, function, inp
            (NonexistentAlias, get_tag_parents_for_a, 'blah'),
            (ShouldBeInt, get_tag_parents_for, 'blah'), 
            (ShouldBeStr, get_tag_parents_for_a, 100),
            )
        for exc,func,inp in aberrant_cases:
            self.assertRaises(exc, func, inp)



    ############################################################
    def test_get_tag_children_for(self):

        """Get the tag-children for a nugid"""
        
        self.populate()

        # just to make things a little more interesting
        add_tag_child_a_to_tag_parent_a('nugid5','nugid4')
        
        nugid_cases = ( # inp, desired_outp
            (1, [2]),
            (2, [3]),
            (3, []),
            (4, [3,5]),
            )
        for inp,desired_outp in nugid_cases:
            actual_outp = get_tag_children_for(inp)
            self.assertEquals(desired_outp, actual_outp)

        alias_cases = ( # inp, desired_outp
            ('nugid1',  ['nugid2']),
            ('nugid2',  ['nugid3']),
            ('nugid2a', ['nugid3']),
            ('nugid3',  []),
            ('nugid4',  ['nugid3','nugid5']),
            )
        for inp,desired_outp in alias_cases:
            actual_outp = get_tag_children_for_a(inp)
            self.assertEquals(desired_outp, actual_outp)

        # xxx - i'm not sure why this returns None but
        # get_tag_children_a raises NonexistentAlias
        self.assertEquals([],get_tag_children_for(100)),

        # we could have a separate ABERRANT_CASES for both
        # get_tag_children_for and get_tag_children_for_a,
        # but if we just include the function as part of the
        # case, we can do them both at once
        #
        aberrant_cases = ( # exception, function, inp
            (NonexistentAlias, get_tag_children_for_a, 'blah'),
            (ShouldBeInt, get_tag_children_for, 'blah'), 
            (ShouldBeStr, get_tag_children_for_a, 100),
            )
        for exc,func,inp in aberrant_cases:
            self.assertRaises(exc, func, inp)



            
    ############################################################
    def test_get_or_create_nugid_from_alias(self):

        """Get or create a nugid from alias"""

        self.populate()

        # if we ask for an alias that does exist, it should
        # return its nugid with True
        for nugid,alias in self.fake_aliases:
            # check that basic getting is working
            self.assertEquals((nugid,True),
                             get_or_create_nugid_from_alias(alias))

        # if we ask for an alias that doesn't exist, it
        # should return a new nugid with False
        max_id = max(get_all_ids())
        self.assertEquals((max_id+1,False),
                         get_or_create_nugid_from_alias('nugid100'))

        # aberrant cases
        self.assertRaises(NeedsNoExtension,
                          get_or_create_nugid_from_alias,
                          'nugid1.freex')
        self.assertRaises(ShouldBeStr,
                          get_or_create_nugid_from_alias,
                          1)
        self.assertEquals((None,False),
                         get_or_create_nugid_from_alias(''))
        self.assertEquals((None,False),
                         get_or_create_nugid_from_alias(None))
        


    ############################################################
    def test_intersect_tag_children_a_from_multiple_tag_parents_a(self):

        """Find the tag-children that these tag-parents all have in common"""

        self.populate()
        # 3, 4 + 5 are going to be tag-children of 2
        #
        # 3 is already a tag child of 2
        add_tag_child_a_to_tag_parent_a('nugid4','nugid2')
        add_tag_child_a_to_tag_parent_a('nugid5','nugid2')
        self.assertEquals(
            ['nugid3','nugid4','nugid5'],
            intersect_tag_children_a_from_multiple_tag_parents_a('nugid2'))
        
        # 2 and 5 are going to be tag-children of 1
        #
        add_tag_child_a_to_tag_parent_a('nugid2','nugid1')
        add_tag_child_a_to_tag_parent_a('nugid5','nugid1')
        self.assertEquals(
            ['nugid2','nugid5'],
            intersect_tag_children_a_from_multiple_tag_parents_a('nugid1'))
       
        # so the only nugget that has *both* nugid1 and
        # nugid2 as tag-parents is nugid5
        self.assertEquals(
            ['nugid5'],
            intersect_tag_children_a_from_multiple_tag_parents_a(['nugid1','nugid2']))

        # there's nothing that has nugid1, nugid2 *and* nugid3 as tag-parents
        self.assertEquals(
            [],
            intersect_tag_children_a_from_multiple_tag_parents_a(['nugid1','nugid2','nugid3']))

        empty_air = (None, '', [])
        # feeding in any of these different kinds of empty
        # air is ok
        for empty in empty_air:
            self.assertEquals(
                None,
                intersect_tag_children_a_from_multiple_tag_parents_a(empty))

        self.assertRaises(ShouldBeList,intersect_tag_children_a_from_multiple_tag_parents_a,
                          100)
        self.assertRaises(ShouldBeStr,intersect_tag_children_a_from_multiple_tag_parents_a,
                          [100])
        self.assertRaises(ShouldBeStr,intersect_tag_children_a_from_multiple_tag_parents_a,
                          ['nugid1',100])

    

    ########################################
    def test_get_last_modtime(self):

        """Get the latest modtime"""

        self.populate()
        # initially, none of the nuggets have been modified,
        # so their modtimes should be empty
        self.assertEquals('',get_last_modtime(1))

        # none of these action types should change the
        # modtime, so it should still be empty
        nonmod_actions = [1,3,4,5,6,7,8,9]
        for action_type in nonmod_actions:
            self.assertEquals('',get_last_modtime(1))
            
        # these actions are supposed to modify the modtime,
        # so adding a timestamp with any of these action
        # types should update the modtime
        mod_actions = [2]
        for action_type in mod_actions:
            new_ts = str(self.remove_microseconds(add_timestamp(1,action_type)))
            self.assertEquals(new_ts,get_last_modtime(1))

        aberrants = (
            (ShouldBeInt,'blah'),
            (NonexistentNugid,100),
            )
        for exc,inp in aberrants:
            self.assertRaises(exc,get_last_modtime,inp)



    ########################################
    def test_add_timestamp(self):

        """Add a timestamp"""

        # this code looks a little involved, but i couldn't
        # figure out neater ways of doing things. i had to
        # define my own remove_microseconds function in
        # order to test that datetimes are equal, and my own
        # within_two_seconds function to test that they're
        # close to equal

        self.populate()

        desired_dt = self.remove_microseconds(datetime.datetime.now())
        actual_dt = self.remove_microseconds(add_timestamp(1,2))
        # to pass, the timestamp added has to be within a
        # second of the current time
        self.assertEquals(True,
                         self.within_two_seconds(desired_dt,actual_dt))
        # action id 2 = save, so check that it updated the
        # modtime too
        save_modtime = dt_from_str(get_last_modtime(1))
        # this time it should be exactly the same
        self.assertEquals(True,
                         actual_dt==save_modtime)

        # try a different kind of action, just for kicks
        actual_dt = add_timestamp(1,1)
        # action 1 = load, which shouldn't update the
        # modtime, so check against the old one from before
        self.assertEquals(False,actual_dt==save_modtime)
                         
        aberrants = (
            (CantBeNone, add_timestamp,None,1),
            (CantBeNone, add_timestamp_a,None,1),
            
            # wrong type of first argument
            (ShouldBeInt, add_timestamp, 'nugid1',1),
            (ShouldBeStr, add_timestamp_a, 1,1),

            # wrong type of action
            (ShouldBeInt, add_timestamp, 1,'load'),
            (ShouldBeInt, add_timestamp_a, 1,'load'),

            # wrong action range
            #
            # too low
            (IllegalActionId, add_timestamp, 1, 0),
            (IllegalActionId, add_timestamp_a, 'nugid1', 0),
            # too high
            (IllegalActionId, add_timestamp, 1, 10),
            (IllegalActionId, add_timestamp_a, 'nugid1', 10),

            # nonexistent nugget
            (NonexistentNugid, add_timestamp, 1000, 1),
            (NonexistentAlias, add_timestamp_a, 'nugid1000', 1),
            )



    ########################################
    def test_dt_from_str(self):
        """Datetime from string"""

        desired_dt = datetime.datetime.now()
        actual_dt = dt_from_str( str(desired_dt) )
        self.assertEquals(True,
                         self.within_two_seconds(desired_dt,actual_dt))



    ########################################
    def remove_microseconds(self,dt):

        """
        --- not a test ---
        
        Removes the microseconds field from a datetime
        object. The only way i know how to do this is to
        create a new datetime object without initializing
        the optional microseconds field.
        """

        return datetime.datetime(dt.year,
                                 dt.month,
                                 dt.day,
                                 dt.hour,
                                 dt.minute,
                                 dt.second)
                            

        

    ########################################
    def within_two_seconds(self,dt1,dt2):

        """
        --- not a test ---
        
        This was the best way I could come up with to test
        whether two datetime objects are within a second or
        two. Should work even if one has a microseconds
        field and the other doesn't.

        N.B. do not use for accurate time comparisons. this
        is just intended as an approximate measure that two
        datetime objects are similar. i *think* that it's
        probably actually stricter than two seconds
        """

        # the 'abs' bit here is key, otherwise it might tell
        # you that two datetimes that are a few seconds
        # apart are -1 days and 86000 seconds apart...
        delta = abs(dt1-dt2)
        return delta.seconds<2



    ########################################
    def test_put_aliases_delim(self):

        """Put delimited string of aliases"""

        self.populate()

        cases = (
            # basic test
            #
            # note: the filename without extension can't be
            # removed as an alias
            (1, 'new1a; new1b; new1c', ['new1a','new1b','new1c','nugid1']),
            
            # it should ignore duplicate aliases
            (1, 'new1a; new1b; new1a', ['new1a','new1b','nugid1']),

            # it should ignore empty slots
            (1, 'new1a;;;; new1b; ;', ['new1a','new1b','nugid1']),

            # try it with a really messy input string
            # (including carriage returns)
            (1, ';\nnew1a      ;   ;;; ;new1b;', ['new1a','new1b','nugid1']),
            )

        for inp_nugid, inp_aliases, desired_outp in cases:
            put_aliases_delim(inp_nugid, inp_aliases)
            desired_outp.sort()
            actual_outp = get_aliases(1)
            actual_outp.sort()
            self.assertEquals(desired_outp, actual_outp)

        new_aliases_str = 'new1a; new1b; new1c; nugid1'
        aberrants = (
            # wrong type of first argument
            (ShouldBeInt, put_aliases_delim, 'blah', new_aliases_str),
            (ShouldBeStr, put_aliases_delim_a, 1, new_aliases_str),
            # None for first argument
            (CantBeNone, put_aliases_delim, None, new_aliases_str),
            (CantBeNone, put_aliases_delim_a, None, new_aliases_str),

            # wrong type of second argument
            (ShouldBeStr, put_aliases_delim, 1, 1),
            (ShouldBeStr, put_aliases_delim_a, 'nugid1', 1),
            # None for second argument
            (CantBeNone, put_aliases_delim, 1, None),
            (CantBeNone, put_aliases_delim_a, 'nugid1', None),

            # nonexistent nuggets
            (NonexistentNugid, put_aliases_delim, 100, new_aliases_str),
            (NonexistentAlias, put_aliases_delim_a, 'nugid100', new_aliases_str),

            # it should fail if you try and add an alias that
            # already exists for another nugget
            (DBError, put_aliases_delim, 1, 'nugid1a; nugid3'),
            (DBError, put_aliases_delim_a, 'nugid2', 'nugid1a; nugid3'),

            )
        for exc,func,inp1,inp2 in aberrants:
            self.assertRaises(exc,func,inp1,inp2)



    ########################################
    def test_put_contents(self):

        """Setting the contents"""

        # this used to test that put_contents wrote out to a
        # file, but it's not supposed to do that any more

        self.populate()

        desired_content = 'hello, world'
        put_contents(1,desired_content)
        self.assertEquals(desired_content, get_contents(1))

        # right now, PUT_CONTENTS removes all quotation
        # marks, because it was giving us trouble in
        # sqlite. it would be better if we just figured out
        # how to escape things properly, but for now test
        # that it's removing them correctly
        put_contents(1,'hello, "world')
        self.assertEquals(desired_content, get_contents(1))
        put_contents(1,'hello, world""')
        self.assertEquals(desired_content, get_contents(1))




    ########################################
    def test_put_filename(self):

        """Change the filename field"""

        self.populate()

        desired_filename1 = 'blah1.freex'
        desired_filename2 = 'blah2.freex'
        put_filename(1,desired_filename1)
        put_filename_a('nugid2',desired_filename2)
        # simple test to see if it updated
        self.assertEquals(desired_filename1,get_filename(1))
        self.assertEquals(desired_filename2,get_filename(2))

        # fails, returning None, if the nuggets don't exist
        #
        # shouldn't it raise an exception??? xxx
        self.assertEquals(None, put_filename(100,'blah1000.freex'))
        
        aberrants = (
            # first input is wrong type
            (ShouldBeInt, put_filename, 'nugid1','blah1000.freex'),
            (ShouldBeStr, put_filename_a, 1,'blah1000.freex'),
            # filename is wrong type
            (ShouldBeStr, put_filename, 1,1),
            (ShouldBeStr, put_filename_a, 'nugid1',1),

            # weirdly, this raises an exception whereas
            # put_filename just returns None - xxx
            (NonexistentAlias, put_filename_a, 'blah','blah1000.freex'),

            # try putting a filename that already exists
            # (FilenameAlreadyExists, put_filename, 3,desired_filename1),
            # (FilenameAlreadyExists, put_filename_a, 'nugid4',desired_filename2),

            (NeedsExtension, put_filename, 3, 'new3.froox'),
            )

        for exc,func,inp1,inp2 in aberrants:
            self.assertRaises(exc,func,inp1,inp2)

        ### test a non-default file extension
        # throw away everything
        self.setUp()
        # now try changing the extension
        self.fsqa.file_ext = 'froox'
        # these are fake nuggets with the new extension
        fake_nuggets_froox = ( (1, 'nugid1.froox', 'this is the first nug'),
                               (2, 'nugid2.froox', 'this is the second nug'),
                               )
        for nugid,filename,content in fake_nuggets_froox:
            add_nugget(filename,content)
        self.assertRaises( NeedsExtension, add_nugget, 'should_fail.freex','this has the wrong extension')

        

    ########################################
    def test_remove_alias(self):

        """Remove an alias"""

        self.populate()

        # create a temporary list of all the aliases
        aliases = get_all_aliases()

        nAliases = len(aliases)
        while nAliases:
            # choose a random alias, and remove it
            # (zero-indexed)
            rand = random.randint(0,nAliases-1)
            alias = aliases[rand]
            aliases.remove(alias)
            nugid = get_nugid_from_alias(alias)
            remove_alias(nugid,alias)
            # now confirm that ALIAS is not in the list of all
            # aliases, or the list of aliases for this NUGID
            self.assertEquals(False,
                             alias in get_all_aliases())
            self.assertRaises(NonexistentAlias,
                              get_nugid_from_alias,
                              alias)
            nAliases = len(aliases)

        # check that there are no aliases left in our list
        self.assertEquals(nAliases,0)

        # now check that get_alias_from_nugid for each
        # nugget returns '#1', '#2' etc. and that
        # get_aliases returns []
        #
        # it's dumb that they return different things, but
        # we're not going to change that now
        for id in get_all_ids():
            self.assertEquals('#' + str(id),
                              get_alias_from_nugid(id))
            self.assertEquals([],
                              get_aliases(id))

        # these fail, but they just return None
        #
        # no alias for nugid 1 called 'blah'
        self.assertEquals(None, remove_alias(1,'blah'))
        # 'nugid2' is not an alias for 2 (though it does
        # exist as an alias)
        self.assertEquals(None, remove_alias(1,'nugid2'))

        # reset things before testing the aberrant cases
        self.setUp()
        self.populate()
        aberrants = (
            # the nugid should be a number
            (ShouldBeInt, 'nugid1','nugid2'),
            # the alias should be a string
            (ShouldBeStr, 1,1),
            # neither NUGID nor ALIAS should be None
            (CantBeNone, None,'nugid1'),
            (CantBeNone, 1,None),
            )
        for exc,inp_nugid,inp_alias in aberrants:
            self.assertRaises(exc, remove_alias,
                              inp_nugid, inp_alias)





    ########################################
    def test_remove_aliases(self):

        """Remove all of a nugget's aliases"""

        self.populate()

        # for each nugget, create a list with all its aliases
        ids = get_all_ids()
        aliases = []
        for id in ids:
            aliases.append(get_aliases(id))

            # remove all the aliases for nugget ID and check
            # that get_aliases() for it comes back empty-handed
            remove_aliases(id)
            self.assertEquals([], get_aliases(id))

            # this is overkill, but also check that none of
            # the aliases we just removed are in
            # get_all_aliases() either, just to double-check
            # that get_aliases() isn't lying to us
            for alias in aliases[id-1]:
                self.assertEquals(False, alias in get_all_aliases())

        self.setUp()
        self.populate()

        # returns None if you try and remove a non-existent nugid
        self.assertEquals(None,remove_aliases(100))
            
        aberrants = (
            (CantBeNone, None),
            (ShouldBeInt, 'blah'),
            )
        for exc,inp in aberrants:
            self.assertRaises(exc,remove_aliases,inp)



    ########################################
    def test_remove_tag_children_from(self):

        """Remove tag children from nugid"""

        self.populate()

        # initially, 1 is the tag-parent of 2
        #
        # add 3 as a tag-child too
        add_tag_child_a_to_tag_parent_a('nugid3','nugid1')
        self.assertEqual([2,3], get_tag_children_for(1))
        
        # try a basic test. confirm that removing the
        # tag-children from a nugget works
        remove_tag_children_from(1)
        self.assertEqual([], get_tag_children_for(1))

        # if you try and remove them again (i.e. from a
        # nugget with no tag-children), it doesn't complain
        remove_tag_children_from(1)
        self.assertEqual([], get_tag_children_for(1))

        # it won't fail if you try and feed it a
        # non-existent nugid. it'll just return None
        self.assertEqual(None, remove_tag_children_from(100))

        aberrants = (
            (CantBeNone, None),
            (ShouldBeInt, 'nugid1'),
            )
        for exc,inp in aberrants:
            self.assertRaises(exc,remove_tag_children_from,inp)

    
    ########################################
    def test_remove_tag_parents_from(self):

        """Remove tag parents from nugid"""

        self.populate()

        # try a basic test. confirm that removing the
        # tag-parents from a nugget works
        remove_tag_parents_from(3)
        self.assertEqual([], get_tag_parents_for(3))

        # it won't fail if you try and feed it a
        # non-existent nugid. it'll just return None
        self.assertEqual(None, remove_tag_children_from(100))

        aberrants = (
            (CantBeNone, None),
            (ShouldBeInt, 'nugid1'),
            )
        for exc,inp in aberrants:
            self.assertRaises(exc,remove_tag_children_from,inp)



    ########################################
    def test_union_lists_no_duplicates(self):

        """Union two lists (throw away duplicates)"""

        cases = (
            # basic case
            ( [[1,2],[3,4]], [1,2,3,4] ),
            # nothing to do
            ( [[1,2,3,4]], [1,2,3,4] ),
            # check that's it's just as happy with lists of
            # lists of strings as integers
            ( [['a','b'],['c','d']], ['a','b','c','d'] ),
            # try more than two lists, with different lengths etc.
            ( [[1,2],[3,4,5],[]], [1,2,3,4,5] ),
            # check that it works for empty lists
            ( [], [] ),
            )

        for inp,desired_outp in cases:
            actual_outp = union_lists_no_duplicates(inp)
            actual_outp.sort()
            desired_outp.sort()
            self.assertEquals(desired_outp,actual_outp)

        aberrants = (
            ( ShouldBeList, [1,2,3,4] ),
            ( ShouldBeList, 1 ),
            ( ShouldBeList, 'blah' ),
            ( CantBeNone, None ),
            )

        for exc,inp in aberrants:
            self.assertRaises(exc, union_lists_no_duplicates, inp)

        

    ########################################
    def test_union_tag_parents_a_from_multiple_tag_children_a(self):

        """Find the tag-parents that the tag-children have in common"""

        self.populate()

        # 3, 4 + 5 are going to be tag-children of 2
        #
        # 3 is already a tag child of 2, and of 4
        add_tag_child_a_to_tag_parent_a('nugid4','nugid2')
        add_tag_child_a_to_tag_parent_a('nugid5','nugid2')

        cases = (
            (['nugid3','nugid4','nugid5'], ['nugid2','nugid4']),
            (['nugid3'],['nugid2','nugid4']),
            (['nugid4'],['nugid2']),
            (['nugid1'],[]),
            )
        for inp,desired_outp in cases:
            desired_outp.sort()
            actual_outp = union_tag_parents_a_from_multiple_tag_children_a(inp)
            actual_outp.sort()
            self.assertEquals(desired_outp,actual_outp)

        # these various forms of empty air should all return None
        nones = ('',[],None)
        for inp in nones:
            self.assertEquals(None,
                             union_tag_parents_a_from_multiple_tag_children_a(inp))

        aberrants = (
            (NonexistentAlias,['blah']),
            (NonexistentAlias,'blah'),
            (ShouldBeList,1),
            )
        for exc,inp in aberrants:
            self.assertRaises(exc,
                              union_tag_parents_a_from_multiple_tag_children_a,
                              inp)



    ########################################
    def test_put_tag_parents_delim(self):

        """Add tag parents (in delimited string) form"""

        self.populate()

        # before inserting anything, check that 5 has no tag
        # parents (not really a test of
        # put_tag_parents_delim)
        self.assertEquals([],
                         get_tag_parents_for(5))
        put_tag_parents_delim(5,'nugid1 ; nugid2')
        # now, it should have 1 and 2 as tag parents
        self.assertEquals([1,2],
                         get_tag_parents_for(5))

        # if we try adding the same tag parents again, we
        # should get the same results
        put_tag_parents_delim(5,'nugid1 ; nugid2')
        self.assertEquals([1,2],
                         get_tag_parents_for(5))

        # if we decide we just want nugget 1
        put_tag_parents_delim(5,' ; ; nugid1 ; ')
        self.assertEquals([1],
                         get_tag_parents_for(5))

        # try deleting all the tag-parents
        put_tag_parents_delim(5,' ; ; \n ; ')
        self.assertEquals([],
                         get_tag_parents_for(5))
        
        self.setUp()
        self.populate()
        add_alias(4,'nugid4 with a space')
        # confirm that it doesn't matter if you add
        # semicolons and carriage returns liberally
        put_tag_parents_delim(5,';; nugid1 ;  ;nugid2\n ;\n ;  nugid4 with a space')
        self.assertEquals([1,2,4],
                         get_tag_parents_for(5))

        # if you use an alias that doesn't exist as a tag
        # parent, it should automatically create it for you
        max_id = max(get_all_ids())
        put_tag_parents_delim(5,'nugid1 ; nugid6')
        self.assertEquals(max_id+1,
                         get_nugid_from_alias('nugid6'))
        self.assertEquals([1,max_id+1],
                         get_tag_parents_for(5))

        # if you try and use the same nugget as a tag-parent
        # twice, it should just ignore the second
        put_tag_parents_delim(5,'nugid1 ; nugid2 ; nugid1a')
        self.assertEquals([1,2],
                         get_tag_parents_for(5))
        # and check it again
        put_tag_parents_delim(5,'nugid1 ; nugid2 ; nugid2')
        self.assertEquals([1,2],
                         get_tag_parents_for(5))



    ########################################
    def test_put_tag_children_delim(self):

        """Add tag children (in delimited string) form"""

        # based on test_put_tag_parents_delim
        #
        # except that here, we're using 4 & 5 as tag
        # children of 3

        self.populate()

        # before inserting anything, check that 3 has no tag
        # children (not really a unit test of
        # put_tag_children_delim)
        self.assertEquals([],
                         get_tag_children_for(3))
        put_tag_children_delim(3,'nugid4 ; nugid5')
        # now, it should have 4 and 5 as tag children
        self.assertEquals([4,5],
                         get_tag_children_for(3))

        # if we try adding the same tag children again, we
        # should get the same results
        put_tag_children_delim(3,'nugid4 ; nugid5')
        self.assertEquals([4,5],
                         get_tag_children_for(3))

        # if we decide we just want nugget 4
        put_tag_children_delim(3,' ; ; nugid4 ; ')
        self.assertEquals([4],
                         get_tag_children_for(3))

        # try deleting all the tag-children
        put_tag_children_delim(3,' ; ; \n ; ')
        self.assertEquals([],
                         get_tag_children_for(3))
        
        self.setUp()
        self.populate()
        add_alias(4,'nugid4 with a space')
        # confirm that it doesn't matter if you add
        # semicolons and carriage returns liberally
        #
        # if you put them in a specific order, it should
        # remember the order
        #
        # xxx - what about if there's a carriage return in
        # the middle of an alias???
        put_tag_children_delim(3,';; nugid5 \n ;  ;nugid4 with a space;')
        self.assertEquals([5,4],
                         get_tag_children_for(3))

        # if you use an alias that doesn't exist as a tag
        # parent, it should automatically create it for you
        max_id = max(get_all_ids())
        put_tag_children_delim(3,'nugid4 ; nugid6')
        self.assertEquals(max_id+1,
                         get_nugid_from_alias('nugid6'))
        self.assertEquals([4,max_id+1],
                         get_tag_children_for(3))

        # if you try and use the same nugget as a tag-parent
        # twice, it should just ignore the second
        put_tag_children_delim(3,'nugid4 ; nugid5 ; nugid4a')
        self.assertEquals([4,5],
                         get_tag_children_for(3))
        # and check it again
        #
        # xxx - why bother checking this again???
        put_tag_children_delim(3,'nugid4 ; nugid4 ; nugid5')
        self.assertEquals([4,5],
                         get_tag_children_for(3))
       


        
########################################
    def test_get_nugid_from_alias(self):

        """Get the right nugid back from an alias"""

        self.populate()

        for nugid,alias in self.fake_aliases:
            self.assertEquals(nugid,get_nugid_from_alias(alias))

        # and test a single pre-specified nugid, just for
        # good measure. we know that the nugid for 'nugid1'
        # is 1...
        self.assertEquals(get_nugid_from_alias('nugid1'),1)

        self.assertRaises(NonexistentAlias,
                          get_nugid_from_alias,'nonexistnug')
        # just checking that it's not being too liberal in
        # its matching
        self.assertRaises(NonexistentAlias,
                          get_nugid_from_alias,'nugid1aa')
        self.assertRaises(NonexistentAlias,
                          get_nugid_from_alias,'nugid')
        
        self.assertRaises(NeedsNoExtension,
                          get_nugid_from_alias,'blah.freex')

        self.assertRaises(ShouldBeStr,
                          get_nugid_from_alias,1)

        self.assertRaises(ShouldBeStr,
                          get_nugid_from_alias,['nugid1'])

        # at the moment, it just returns None if you give it
        # empty/None, but perhaps it should raise an
        # exception??? this will affect other
        # functions... xxx
        self.assertEquals(get_nugid_from_alias(None),None)
        self.assertEquals(get_nugid_from_alias(''),None)
        # self.assertRaises(EmptyString,
        #                   get_nugid_from_alias,'')



# ########################################
#     def test_get_nugid_from_alias_multi(self):

#         """Get multiple nugids for multiple aliases"""

#         self.populate()

#         cases = (
#             # basic case
#             (['nugid1','nugid2','nugid1a'],[1,2,1]),
#             # the same nugid and the same aliases should be
#             # able to be included multiple times
#             #
#             # i couldn't get this one to pass
#             (['nugid1','nugid2','nugid1a','nugid2'],[1,2,1,2]),
#             # just for kicks, do it on the whole db
#             (get_all_aliases(), map(get_nugid_from_alias,get_all_aliases())),
#             )

#         for inp,desired_outp in cases:
#             desired_outp.sort()
#             actual_outp = get_nugid_from_alias_multi(inp)
#             actual_outp.sort()

#             self.assertEquals(desired_outp,actual_outp)

#         # what about if one of the aliases doesn't exist??? xxx

#         aberrants = (
#             (ShouldBeList,1),
#             (ShouldBeStr,['nugid1',1])
#             )
#         for exc,inp in aberrants:
#             self.assertRaises(exc,get_nugid_from_alias_multi,inp)



########################################
    def test_get_all_aliases(self):

        """Get all the aliases"""

        # check that you get none back if there are none to
        # return
        self.assertEquals(get_all_aliases(),[])

        self.populate()
        aliases = []
        for nugid,filename,content in self.fake_nuggets:
            alias = remove_ext(filename)
            aliases.append(alias)
        for nugid,alias in self.fake_aliases:
            aliases.append(alias)
        # test it returns all the aliases that we populated
        # the database with, including the aliases constructed
        # by removing the extension from the filename
        self.assertEqual(get_all_aliases(),aliases)
        self.assertEqual(get_all_aliases(nugids_to_exclude=None),
                          aliases)
        self.assertEqual(get_all_aliases(nugids_to_exclude=[]),
                          aliases)
        self.assertEqual(get_all_aliases(nugids_to_exclude=[None]),
                          aliases)

        aliases_no_1_3 = []
        for nugid,filename,content in self.fake_nuggets:
            if (nugid != 1) and (nugid != 3):
                alias = remove_ext(filename)
                aliases_no_1_3.append(alias)
        for nugid,alias in self.fake_aliases:
            if (nugid != 1) and (nugid != 3):
                aliases_no_1_3.append(alias)
        # test that the nugids_to_exclude works with a list of
        # nugids
        self.assertEqual(get_all_aliases(nugids_to_exclude=['1','3']),
                          aliases_no_1_3)
        self.assertEqual(get_all_aliases(nugids_to_exclude=['1',3]),
                          aliases_no_1_3)
        self.assertEquals(get_all_aliases(nugids_to_exclude=[1,3]),
                          aliases_no_1_3)
        # if any of the nugids_to_exclude are None, ignore
        # them
        self.assertEquals(get_all_aliases(nugids_to_exclude=[1,3,None]),
                          aliases_no_1_3)

        # nugids_to_exclude are nonexistent nugids
        self.assertEquals(get_all_aliases(nugids_to_exclude=[1,3,100,101]),
                          aliases_no_1_3)


        # nugids_to_exclude must be a list (or None)
        self.assertRaises(ShouldBeList,get_all_aliases,nugids_to_exclude='1')
        self.assertRaises(ShouldBeList,get_all_aliases,nugids_to_exclude=1)



############################################################
    def test_get_all_filenames(self):

        """Get all filenames"""
        
        self.assertEquals([], get_all_filenames())
        
        self.populate()
        all_filenames = []
        for nugid,filename,content in self.fake_nuggets:
            all_filenames.append(filename)

        # we don't require that the filenames be retrieved
        # in any particular order, so we have to test with
        # sorted lists
        all_filenames.sort()
        got_all_filenames = get_all_filenames()
        got_all_filenames.sort()
        self.assertEquals(all_filenames,got_all_filenames)



########################################
    def test_get_all_ids(self):

        """Get all the ids"""

        # try it with no nuggets in the database
        self.assertEqual(get_all_ids(),[])

        self.populate()
        ids = []
        for desired in self.fake_nuggets:
            id = desired[0]
            ids.append(id)
        # test it returns all the ids that we populated the
        # database with
        self.assertEqual(get_all_ids(),ids)



########################################
    def test_get_contents(self):
        
        """Get the right content for each nugget"""

        self.populate()

        for nugid,filename,content in self.fake_nuggets:
            alias = remove_ext(filename)

            # check that it returns the correct contents
            self.assertEquals(get_contents(nugid),content)
            self.assertEquals(get_contents_a(alias),content)

        self.assertRaises(ShouldBeInt,get_contents,'blah')
        self.assertRaises(ShouldBeStr,get_contents_a,100)

        self.assertRaises(NeedsNoExtension,get_contents_a,'blah.freex')
        self.assertRaises(NonexistentAlias,get_contents_a,'blah')
        self.assertRaises(NonexistentNugid,get_contents,100)
        


########################################
    def test_get_filename(self):

        """Get the filename"""

        self.populate()

        for nugid,filename,content in self.fake_nuggets:
            self.assertEquals(filename, get_filename(nugid))
            self.assertEquals(filename, get_filename_a(remove_ext(filename)))
            
        for nugid,alias in self.fake_aliases:
            self.assertEquals(get_filename(nugid),
                             get_filename_a(alias))

        # xxx - didn't want to mess with the delicate
        # balance of the universe
        #
        # xxx maybe all the nugid functions *should* allow
        # strings, and they'll just cast them automatically???
        self.assertRaises(ShouldBeInt, get_filename, 'blah')
        # xxx
        # self.assertRaises(ShouldBeStr, get_filename_a, 1)
        
        self.assertRaises(CantBeNone, get_filename, None)
        self.assertRaises(CantBeNone, get_filename_a, None)

        # self.assertRaises(NonexistentNugid, get_filename, 100)
        self.assertEquals(None,get_filename(100))
        # self.assertRaises(NonexistentAlias, get_filename_a, 'blah')
        self.assertEquals(None,get_filename_a('blah'))
                


########################################
    def test_get_nugid_from_filename(self):

        """Get the right nugid back from a filename"""

        self.populate()

        for desired in self.fake_nuggets:
            nugid = desired[0]
            filename = desired[1]
            self.assertEquals(nugid,get_nugid_from_filename(filename))

        self.assertEquals(get_nugid_from_filename('nugid1.freex'),1)

        self.assertRaises(NonexistentFilename,
                          get_nugid_from_filename,'nonexistnug.freex')
        self.assertRaises(NeedsExtension,
                          get_nugid_from_filename,'blah')
        # i think we've tested this already as part of the
        # fake_nuggets, but do it again by hand, just in
        # case
        self.assertRaises(NeedsExtension,
                          get_nugid_from_filename,'nugid1')
        self.assertRaises(ShouldBeStr,
                          get_nugid_from_filename,1)
        self.assertRaises(CantBeNone,
                          get_nugid_from_filename,None)



########################################
    def test_roundtrip_from_nugid(self):

        """Roundtrip from nugid to alias back to nugid"""
        
        self.populate()
        nugids = get_all_ids()
        for nugid in nugids:
            result = get_nugid_from_alias(
                get_alias_from_nugid(nugid) )
            self.assertEquals(result,nugid)


        
########################################
    def test_roundtrip_from_filename(self):

        """Roundtrip from filename to nugid back to filename"""

        self.populate()
        filenames = get_all_filenames()
        for filename in filenames:
            result = get_filename(
                get_nugid_from_filename(filename) )
            self.assertEquals(result,filename)


########################################
    def test_exist_nugget(self):

        """Whether a nugget exists"""

        self.populate()

        for nugid,filename,content in self.fake_nuggets:
            alias = remove_ext(filename)
            
            # it's supposed to return the nugid if the nugget exists
            self.assertEquals(True, exist_nugget(nugid))
            # it's supposed to return the alias if the nugget exists
            #
            # xxx - should just return True like exist_nugget
            self.assertEquals(alias, exist_nugget_a(alias))

        for nugid,alias in self.fake_aliases:
            
            self.assertEquals(True, exist_nugget(nugid))
            self.assertEquals(alias, exist_nugget_a(alias))

        # xxx - these exceptions need to be tidied up

        # try a few nugids that we know don't exist
        self.assertEquals(False, exist_nugget(0))
        self.assertEquals(False, exist_nugget(-3))
        self.assertEquals(False, exist_nugget(10))
        self.assertRaises(CantBeNone, exist_nugget, None)
        self.assertRaises(ShouldBeInt, exist_nugget,'blah')

        self.assertEquals(None, exist_nugget_a('blah'))
        # xxx - this is totally wrong. feeding in an empty
        # string should give you something like
        # None/False/exception, but instead you just get the
        # emptry string back
        self.assertEquals(None, exist_nugget_a(''))
        # xxx - both these next two should be exceptions
        self.assertRaises(ShouldBeStr, exist_nugget_a, 1)
        self.assertRaises(ShouldBeStr, exist_nugget_a, None)



########################################
    def test_get_alias_from_nugid(self):

        """Getting an alias from a nugid"""

        self.populate()

        for nugid,filename,content in self.fake_nuggets:
            alias = remove_ext(filename)
            self.assertEquals(alias, get_alias_from_nugid(nugid))

            # if we get all the aliases, and then get the
            # first one, that should be right
            self.assertEquals(alias, get_aliases(nugid)[0])

        for nugid,alias in self.fake_aliases:
            # xxx - rename get_alias_from_nugid ->
            # get_primary_alias???
            # 
            # none of the aliases in FAKE_ALIASES are the
            # *primary* alias, so none of them should get
            # returned by get_alias_from_nugid
            self.assertNotEqual(alias, get_alias_from_nugid(nugid))

            # if we get th e

        # N.B. TEST_ROUNDTRIP_FROM_NUGID is checking that a
        # roundtrip from nugid to alias back to nugid works,
        # so we don't need to do that here

        self.assertRaises(ShouldBeInt, get_alias_from_nugid, None)
        # xxx - this should return None or something if it
        # can't find a nugget
        self.assertRaises(NonexistentNugid, get_alias_from_nugid, 10)
        self.assertRaises(NonexistentNugid, get_alias_from_nugid, -110)
        self.assertRaises(NonexistentNugid, get_alias_from_nugid, 0)
        self.assertRaises(ShouldBeInt, get_alias_from_nugid, 'blah')
        self.assertRaises(ShouldBeInt, get_alias_from_nugid, '')
        self.assertRaises(ShouldBeInt, get_alias_from_nugid, [])
        


########################################
    def test_get_aliases(self):

        """Get all the aliases for this nugid"""

        self.populate()

        nugid1_aliases = ['nugid1','nugid1a','nugid1b','nugid1c']
        self.assertEquals(nugid1_aliases, get_aliases(1))

        nugid1_aliases_delim = 'nugid1; nugid1a; nugid1b; nugid1c'
        self.assertEquals(nugid1_aliases_delim, get_aliases_delim(1))
        self.assertEquals(nugid1_aliases_delim, get_aliases_delim_a('nugid1'))
        self.assertEquals(nugid1_aliases_delim, get_aliases_delim_a('nugid1a'))

        self.assertRaises(NonexistentNugid, get_aliases, 100)
        self.assertRaises(NonexistentNugid, get_aliases_delim, 100)
        self.assertRaises(NonexistentAlias, get_aliases_delim_a, 'blah')

        self.assertRaises(ShouldBeInt, get_aliases, 'blah')
        self.assertRaises(ShouldBeInt, get_aliases_delim, 'blah')

        self.assertRaises(ShouldBeStr, get_aliases_delim_a, 1)

        self.assertRaises(CantBeNone, get_aliases, None)
        self.assertRaises(CantBeNone, get_aliases_delim, None)
        self.assertRaises(CantBeNone, get_aliases_delim_a, None)


    

########################################
    def test_get_all_aliases(self):

        """Get all the aliases in the database"""

        # try it with no nuggets in the database
        self.assertEquals([],get_all_aliases())

        self.populate()
        
        # show that all the aliases in the existing database
        # are returned
        all_aliases = []
        for nugid,filename,content in self.fake_nuggets:
            all_aliases.append(remove_ext(filename))
        for nugid,alias in self.fake_aliases:
            all_aliases.append(alias)
        self.assertEquals(all_aliases, get_all_aliases())

        # add_nugget and add_alias check that if you add new
        # nuggets or aliases that they show up too in the
        # list

        # remove_nugget checks that deleting a nugget
        # removes its aliases



########################################
    def test_remove_nugget(self):

        """Remove a nugget"""

        # xxx - for some reason, the docstring isn't showing
        # up in the list of tests

        # delete a nugget using its nugid
        self.populate()
        all_aliases = get_all_aliases()
        remove_nugget(1)
        # check the nugid doesn't exist
        self.assertEquals(False, exist_nugget(1))
        # check that it removes its aliases too
        #
        # xxx should be False
        self.assertEquals(None, exist_nugget_a('nugid1a'))
        all_aliases.remove('nugid1')
        all_aliases.remove('nugid1a')
        all_aliases.remove('nugid1b')
        all_aliases.remove('nugid1c')
        self.assertEquals(all_aliases,get_all_aliases())

        # delete a nugget using its alias
        self.setUp()
        self.populate()
        all_aliases = get_all_aliases()
        remove_nugget_a('nugid1a')
        # check the nugid doesn't exist
        self.assertEquals(False, exist_nugget(1))
        # check that it removes its aliases too
        self.assertEquals(None, exist_nugget_a('nugid1a'))
        all_aliases.remove('nugid1')
        all_aliases.remove('nugid1a')
        all_aliases.remove('nugid1b')
        all_aliases.remove('nugid1c')
        self.assertEquals(all_aliases,get_all_aliases())

        # deal with nonexistent nuggets
        #
        # xxx - should these be exceptions???
        #
        # xxx - at the very least, remove_nugget and
        # remove_nugget_a should fail in consistent ways
        # (both Nones, or both exceptions)
        self.assertEquals(None, remove_nugget(100))
        self.assertRaises(NonexistentAlias, remove_nugget_a, 'madeup_alias')

        # deal with incorrect type inputs
        # xxx
        self.assertRaises(ShouldBeInt, remove_nugget, 'nugid1')
        self.assertRaises(ShouldBeInt, remove_nugget, 'blah')
        self.assertRaises(CantBeNone, remove_nugget, None)
        self.assertRaises(ShouldBeStr, remove_nugget_a, 1)
        self.assertRaises(ShouldBeStr, remove_nugget_a, 100)
        self.assertRaises(ShouldBeStr, remove_nugget_a, None)
        


########################################
    def test_remove_all_nuggets(self):

        """Remove all the nuggets in the database"""

        # try it with no nuggets in the database
        self.assertEquals([],get_all_ids())
        self.assertEquals([],get_all_aliases())
        self.assertEquals([],get_all_filenames())

        self.setUp()
        self.populate()
        remove_all_nuggets()
        self.assertEquals([],get_all_ids())
        self.assertEquals([],get_all_aliases())
        self.assertEquals([],get_all_filenames())
        


########################################
    def test_add_nugget(self):

        """Add a new nugget"""

        self.populate()

        # we'll need this for figuring out what the new id should be
        max_id = max(get_all_ids())
        
        # this is the list of aliases before we add the nugget
        all_aliases = get_all_aliases()

        new_nugid = add_nugget('a new nugget.freex','this is the content')

        # at the moment, we're requiring that the new
        # nugget's ID be one greater than the largest nugid
        # in existence. perhaps we'll relax this in the
        # future if we're generating them randomly or with a
        # has
        self.assertEquals(new_nugid,max_id+1)

        # at the moment, the only alias that this nugget
        # will have is its primary alias (based on the
        # filename), though it will still be part of  list
        aliases = ['a new nugget']
        self.assertEquals(aliases,get_aliases(new_nugid))
        
        # adding a nugget should cause its alias to be added
        # to the list of all aliases in the database
        all_aliases.append('a new nugget')
        self.assertEquals(all_aliases,get_all_aliases())

        self.assertEquals('this is the content', get_contents(new_nugid))

        # check that the alias is created correctly if the
        # filename has dots in it
        new_nugid = add_nugget('a.new.nugget.freex','this is the content')
        self.assertEquals('a.new.nugget',get_alias_from_nugid(new_nugid))
        self.assertEquals(new_nugid,get_nugid_from_filename('a.new.nugget.freex'))

        # the filename has to have a '.freex' extension
        self.assertRaises(NeedsExtension, add_nugget, 'blah', 'new content')
        self.assertRaises(NeedsExtension, add_nugget, 'blah.muse', 'new content')
        self.assertRaises(NeedsExtension, add_nugget, 'blah.free', 'new content')
        self.assertRaises(NeedsExtension, add_nugget, 'blah.freex2', 'new content')
        self.assertRaises(NeedsExtension, add_nugget, 'blahfreex', 'new content')

        self.assertRaises(CantBeNone, add_nugget, None, 'new content')
        self.assertRaises(CantBeNone, add_nugget, '.freex', 'new content')

        # when you violate the uniqueness constrain on the
        # aliases column, you get a
        # SQLError/IntegrityError. i don't know how to test
        # that it's specifically a uniqueness exception, so
        # i'm just going to assertRaises for that
        self.assertRaises(sqlalchemy.exceptions.IntegrityError, add_nugget, 'nugid1.freex', 'new content')



########################################
    def test_add_alias(self):

        """Adding an alias"""

        self.populate()

        # we're going to add an alias to nugid 1, and check
        # that the list of all the aliases in the database,
        # and the list of aliases for this nugget, have both
        # been updated accordingly
        
        nugid = 1
        aliases = get_aliases(nugid)
        all_aliases = get_all_aliases()
        
        add_alias(nugid,'a new alias')
        aliases.append('a new alias')
        all_aliases.append('a new alias')
        
        self.assertEquals(aliases,get_aliases(nugid))
        self.assertEquals(all_aliases,get_all_aliases())

        # the nugid must be an int
        self.assertRaises(ShouldBeInt, add_alias, 'blah', 'another new alias')
        self.assertRaises(ShouldBeInt, add_alias, 'nugid1', 'another new alias')
        self.assertRaises(CantBeNone, add_alias, None, 'another new alias')

        # the alias must be a string
        self.assertRaises(ShouldBeStr, add_alias, 1, 100)
        self.assertRaises(CantBeNone, add_alias, 1, None)

        # xxx - should there be an add_alias_a???

        # shouldn't be able to add an alias to a nugget that
        # already has that alias
        #
        # xxx - should this raise an AliasAlreadyExists???
        # hmmm.  you don't want things to be too brittle for
        # the user...
        #
        # self.assertRaises(AliasAlreadyExists, add_alias, 1, 'nugid1')
        self.assertEquals(None, add_alias(1,'nugid1'))

        # shouldn't be able to add an alias to a nugget if
        # that alias already exists for another nugget
        #
        # xxx - currently, it deals with things differently
        # if the alias exists for a different nugid. does
        # this make sense???
        # don't know how to catch the db errors
        # self.assertRaises(AliasAlreadyExists, add_alias, 3, 'nugid1')
        self.assertRaises(DBError, add_alias, 3, 'nugid1')
        # xxx
        # self.assertRaises(AliasAlreadyExists, add_alias, 3, 'Nugid1')
        


########################################
    def test_has_ext(self):

        """Checking for .freex extension"""

        cases = ( ('blah.freex', True),
                  ('.freex',True),
                  ('a.new.nugget.freex',True),
                  ('.freex.freex',True),
                  ('blah',False),
                  ('blah.muse',False),
                  ('blah.freex2',False),
                  ('blahfreex',False),
                  ('something.freex.blah',False),
                  ('',False),
                  )

        for filename,ans in cases:
            self.assertEquals(ans, has_ext(filename))

        self.assertRaises(CantBeNone, has_ext, None)

        self.assertRaises(ShouldBeStr,has_ext,100)

        

########################################
    def test_add_ext(self):

        """Adding .freex extension"""

        self.assertEquals(add_ext('blah'),'blah.freex')
        self.assertEquals(add_ext(''),'.freex')
        self.assertEquals(add_ext('blah.free'),'blah.free.freex')

        self.assertRaises(ShouldBeStr,add_ext,100)

        self.assertRaises(NeedsNoExtension,add_ext,'blah.freex')



########################################
    def test_remove_ext(self):

        """Removing .freex extension"""

        self.assertEquals(remove_ext('blah.freex'),'blah')
        self.assertEquals(remove_ext('freex.freex'),'freex')
        self.assertEquals(remove_ext('.freex.freex'),'.freex')
        self.assertEquals(remove_ext('blah.freex.freex'),'blah.freex')
        self.assertEquals(remove_ext('.freex'),'')

        self.assertRaises(ShouldBeStr,remove_ext,100)

        self.assertRaises(NeedsExtension,remove_ext,'blah')
        self.assertRaises(NeedsExtension,remove_ext,'blahfreex')


########################################
    def test_intersect_lists(self):
        """Intersecting lists"""
        # all of them match
        a = [1,2,3,4,5]
        b = [1,2,3,4,5]
        self.assertEquals(intersect_lists([a,b]),
                         [1,2,3,4,5])

        # some of them match. the sublists can be of different lengths
        a = [1,2,3,4,5]
        b = [3,4,5,6]
        self.assertEquals(intersect_lists([a,b]),
                         [3,4,5])

        # the order the items get returned is not
        # defined. the sublists don't have to be sorted in
        # any way
        a = [1,2,3,4,5]
        b = [5,4,3,2,1]
        # sort sorts in place, and returns None, so we need
        # an intermediate variable to call it on
        result = intersect_lists([a,b])
        result.sort()
        self.assertEquals(result,
                         [1,2,3,4,5])
        a = [1,4,5,2,3]
        b = [5,2,1,4]
        result = intersect_lists([a,b])
        result.sort()
        self.assertEquals(result,
                         [1,2,4,5])

        # none of them match
        a = [1,2,3,4,5]
        b = [6,7,8,9,10]
        self.assertEquals(intersect_lists([a,b]),
                         [])

        # all of the items returned will be unique
        a = [1,2,3,4,1]
        b = [2,3,4,1,2]
        result = intersect_lists([a,b])
        result.sort()
        self.assertEquals(result,
                         [1,2,3,4])
        

        # allows for multiple sublists
        a = [1,2,3,4,5]
        b = [2,3,4,5,6,7]
        c = [3,4,5]
        d = [4]
        self.assertEquals(intersect_lists([a,b,c,d]),
                         [4])

        # allows for a single sublist
        a = [1,2,3]
        self.assertEquals(intersect_lists([a]),
                         a)
        self.assertEquals(intersect_lists([[]]),
                                         [])
        
        # allows for no sublists
        self.assertEquals(intersect_lists([]),
                                         [])

        # doesn't allow any of the items in the sublists to be a list
        a = [1,2,[3,4],5]
        b = [3,4,5,6,7]
        self.assertRaises(CantBeList,intersect_lists,[a,b])

        # some of them match, strings
        a = ['aa','bb','cc','dd','ee']
        b = ['cc','dd','ee','ff','gg']
        result = intersect_lists([a,b])
        result.sort()
        self.assertEquals(result,
                         ['cc','dd','ee'])

        # check it deals with None as an item
        a = [None]
        b = [None]
        self.assertEquals(intersect_lists([a,b]),
                         [None])
        a = [1,2,None]
        b = [None,2,None]
        self.assertEquals(intersect_lists([a,b]),
                         [None,2])

        # its input must be a list
        self.assertRaises(ShouldBeList,intersect_lists,1)

        # all of the sublists must be lists
        a = None
        b = [1,2,3]
        self.assertRaises(ShouldBeList,intersect_lists,[a,b])
        a = 1
        b = [1,2,3]
        self.assertRaises(ShouldBeList,intersect_lists,[a,b])
        a = [1,2,3]
        b = 'hello'
        self.assertRaises(ShouldBeList,intersect_lists,[a,b])
        
        # the sublists can be empty
        a = [1,2,3]
        b = []
        self.assertEquals(intersect_lists([a,b]),
                         [])
        a = []
        b = [None]
        self.assertEquals(intersect_lists([a,b]),
                         [])



############################################################
# main
#
# run 'python freex_sqalchemy_test.py -v' at the commandline to test

if __name__ == "__main__":

    forbidden_dirs = ['/home/greg/docs/freex']
    
    # this is an unnecessary precaution, but just in
    # case. i've been pretty careful to make sure it creates
    # a .db file with a special test name, but just to be on
    # the safe side, tell it not to run in your docs
    # directory
    if os.getcwd() in forbidden_dirs:
        raise "Don't run the tests in your main freex docs directory, just in case"

    unittest.main()

