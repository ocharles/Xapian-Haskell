> import Distribution.Simple
> import Distribution.Simple.Setup
> import Distribution.Simple.LocalBuildInfo
> import Distribution.PackageDescription
> import Distribution.Verbosity as Verbosity

> import Control.Arrow ((***))
> import Control.Monad (forM_, when)
> import Data.Maybe (fromMaybe)
> import qualified Data.List as List
> import System.FilePath (joinPath, replaceExtension, takeBaseName)
> import System.Directory (createDirectoryIfMissing)
> import System (system)

For Xapian-Haskell we need to compile the C bindings to the Xapian C++ library
with a C++ compiler. This is done via a procedure that will is hooked into the
building sequence as 'buildHook' hook. We use simpleUserHooks as basis.

> main :: IO ()
> main = defaultMainWithHooks simpleUserHooks{ buildHook = cplusplusHook }

Now we go on defining the cplusplusHook.

> cplusplusHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
> cplusplusHook desc bInfo hooks bFlags =

First extract the list of C++ sources, compiler and compiler options from the
package description 'desc'

>  do let ( cc_sources
>           , cc_compiler
>           , cc_options) = fromMaybe ([], "/usr/bin/c++", "-Wall -c") $
>          do lib <- library desc
>             let fields = customFieldsBI $ libBuildInfo lib
>             src <- fmap words $ List.lookup "x-cc-sources" fields
>             cmp <- List.lookup "x-cc-compiler" fields
>             opt <- List.lookup "x-cc-options" fields
>             return (src, cmp, opt)

Get the build directory or create it

>     let builddir = buildDir bInfo
>     createDirectoryIfMissing True builddir

Keep the user informed if he wants to.

>     let be_verbose = case flagToMaybe $ buildVerbosity bFlags of
>                           Just v  -> v > Verbosity.normal
>                           Nothing -> False

Compile each source and put the resulting object files into the build dir

>     let objectOf source = joinPath [builddir, replaceExtension (takeBaseName source) "o"]
>
>     putStrLn "Compiling C++ sources"
>
>     forM_ cc_sources $ \source ->
>      do let command = unwords $ [ cc_compiler, cc_options, source, "-o"
>                                 , objectOf source]
>         when be_verbose $ putStrLn command
>         system command

Pass the list of object files to the linker.

>     let objects = map objectOf cc_sources
>     let bFlags' = bFlags{ buildProgramArgs =
>             [("ld",objects)] }

Since we now prepared all the object files, we can proceed with the normal
building.

>     buildHook simpleUserHooks desc bInfo hooks bFlags'
>     return ()
