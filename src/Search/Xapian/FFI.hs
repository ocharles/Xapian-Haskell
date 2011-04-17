{-
Copyright (C) 2011 by Oliver Charles <oliver.g.charles@googlemail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN

-}

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Search.Xapian.FFI where

import Data.ByteString.Char8
import Foreign
import Foreign.C.String
import Foreign.C.Types

data XapianDocument
data XapianDatabase
data XapianEnquire
data XapianQuery
data XapianMSetIterator
data XapianStem

newtype CreateDBOption = CreateDBOption { unCreateDBOption :: Int }
                         deriving (Show, Eq)

foreign import ccall "cxapian.h xapian_writable_db_new"
  c_xapian_writable_db_new :: CString ->
                              CreateDBOption ->
                              Ptr CString ->
                              IO (Ptr XapianDatabase)

foreign import ccall "cxapian.h xapian_writable_db_add_document"
  c_xapian_database_add_document :: Ptr XapianDatabase ->
                                    Ptr XapianDocument ->
                                    IO ()

foreign import ccall "cxapion.h xapian_database_new"
  c_xapian_database_new :: CString ->
                           Ptr CString ->
                           IO (Ptr XapianDatabase)

foreign import ccall "cxapion.h &xapian_database_delete"
  c_xapian_database_delete :: FunPtr (Ptr XapianDatabase -> IO ())

foreign import ccall "cxapian.h xapian_document_new"
  c_xapian_document_new :: IO (Ptr XapianDocument)

foreign import ccall "cxapian.h xapian_get_document"
  c_xapian_get_document :: Ptr XapianDatabase -> Int -> IO (Ptr XapianDocument)

foreign import ccall "cxapion.h &xapian_document_delete"
  c_xapian_document_delete :: FunPtr (Ptr XapianDocument -> IO ())

foreign import ccall "cxapian.h xapian_document_set_data"
  c_xapian_document_set_data :: Ptr XapianDocument -> CString -> IO ()

foreign import ccall "cxapian.h xapian_document_get_data"
  c_xapian_document_get_data :: Ptr XapianDocument -> IO (CString)

foreign import ccall "cxapian.h xapian_document_add_posting"
  c_xapian_document_add_posting :: Ptr XapianDocument ->
                                   CString ->
                                   Int ->
                                   IO ()

foreign import ccall "cxapian.h xapian_enquire_new"
  c_xapian_enquire_new :: Ptr XapianDatabase ->
                          IO (Ptr XapianEnquire)

foreign import ccall "cxapion.h &xapian_enquire_delete"
  c_xapian_enquire_delete :: FunPtr (Ptr XapianEnquire -> IO ())

foreign import ccall "cxapian.h xapian_query_new"
  c_xapian_query_new :: CString -> IO (Ptr XapianQuery)

foreign import ccall "cxapian.h xapian_query_combine"
  c_xapian_query_combine :: Int ->
                            Ptr XapianQuery ->
                            Ptr XapianQuery ->
                            IO (Ptr XapianQuery)

foreign import ccall "cxapian.h xapian_query_describe"
  c_xapian_query_describe :: Ptr XapianQuery ->
                             CString

foreign import ccall "cxapion.h &xapian_query_delete"
  c_xapian_query_delete :: FunPtr (Ptr XapianQuery -> IO ())

foreign import ccall "cxapian.h xapian_enquire_query"
  c_xapian_enquire_query :: Ptr XapianEnquire -> Ptr XapianQuery ->
                            Int -> Int ->
                            Ptr XapianMSetIterator

foreign import ccall "cxapian.h xapian_msets_valid"
  c_xapian_msets_valid :: Ptr XapianMSetIterator -> Bool

foreign import ccall "cxapian.h xapian_msets_get"
  c_xapian_msets_get :: Ptr XapianMSetIterator -> IO (Int)

foreign import ccall "cxapian.h xapian_msets_next"
  c_xapian_msets_next :: Ptr XapianMSetIterator -> IO ()

foreign import ccall "cxapian.h xapian_stem_new"
  c_xapian_stem_new :: CString -> IO (Ptr XapianStem)

foreign import ccall "cxapian.h &xapian_stem_delete"
  c_xapian_stem_delete :: FunPtr (Ptr XapianStem -> IO())

foreign import ccall "cxapian.h xapian_stem_string"
  c_xapian_stem_string :: Ptr XapianStem ->
                          Ptr XapianDocument ->
                          CString ->
                          IO ()

foreign import ccall "cxapian.h xapian_parse_query"
  c_xapian_parse_query :: CString ->
                          Ptr XapianStem ->
                          IO (Ptr XapianQuery)
