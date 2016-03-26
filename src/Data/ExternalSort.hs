{- | = External Sort

When you don't have enough memory to sort a large number of values an
<https://en.wikipedia.org/wiki/External_sorting external sort>
can be used.

This library implements an /external sort/ algorithm using Gabriel Gonzales'
<https://hackage.haskell.org/package/pipes pipes> library and Ben Gamari's
<https://hackage.haskell.org/package/pipes-interleave pipes-interleave> extension.

== Usage

The library exports two functions, 'externalSortFile' and 'externalSortHandle'. The former is
simply a wrapper around the latter. The first argument to both functions is a configuration
value with the polymorphic type @'ExternalSortCfg' a@. The configuration value contains:

* 'readVal': a function to read a value of type @a@ from a 'Handle'
* 'writeVal': a function to write a value of type @a@ to a 'Handle'
* 'chunkSize': the number of values that will be read into memory at any one time
* 'sorter': a sort algorithm to use. (Rather than choose one for you, you are free to use your own)
* 'comparer': a function to compare two values

It is up to the user to work out how much memory @chunkSize@ values will use at it will depend
on the type being read/written.

== Algorithm design

The algorithm proceeds in two stages. In the first phase @chunkSize@ values are repeatedly read
into memory, sorted, and then written to temporary intermediate files. The number of intermediate
files should be @ceiling (n div chunkSize)@ where @n@ is the number of values in the file.

In the second phase all of the temporary intermediate files are opened for reading and a k-way
merge is performed on the values. The results are streamed to the output file/handle (depending
on which algorithm you use). It is important that your operating system allow this many open files
otherwise you will receive a @openFile: resource exhausted (Too many open files)@ exception.

-}
module Data.ExternalSort (
  -- | = Types
    ExternalSortCfg(..)
  -- | = Functions

  -- $externalSortFile
  , externalSortFile

  -- $externalSortHandle
  , externalSortHandle
  ) where

import Data.ExternalSort.Internal

-- $externalSortFile
-- Here is some documentation for 'externalSortFile'
--

-- $externalSortHandle
-- Here is some documentation for 'externalSortHandle'
--