# sunlight

sunlight helps you test your Cabalized package against multiple
dependency versions.

## Why?

I wrote sunlight because of minor quibbles I have with the Haskell
[Package Versioning
Policy](http://www.haskell.org/haskellwiki/Package_versioning_policy).
The PVP encourages you to record top bounds on each of the
dependencies in your package.  This often keeps packages from
building when dependencies are updated, even if the dependencies
have changed in harmless ways.

I'm not the only one with quibbles here: [Haskell Cafe
discussion](http://www.haskell.org/pipermail/haskell-cafe/2012-August/102885.html).
My only quibble is with upper bounds; specifying the release of a
package by using the PVP is an excellent practice.

I figure it would be much more useful for the Cabal file to serve as
documentation of software versions that the package will build with.
The problem is that there was no easy way to make sure that a
package builds with the minimum versions specified in the Cabal
file.  For example, cabal install does not have an option to fetch
the lowest version of each dependency; instead, it eagerly fetches
the most recent dependencies--which is what you want most of the
time, but this makes it hard to ensure that the package builds with
the lowest possible dependencies.

sunlight automatically builds the package using multiple GHC
versions, so you can verify that the package builds even with
different dependencies or compiler versions.  This also makes it
easier to maintain a package that builds with older GHC versions
that you may not use regularly.

## Usage

To use sunlight, you need to specify a minimum bound for each
dependency.  That minimum bound must be resolvable to an actual
version of the package.  It's OK to specify an exact bound, or an
optional maximum bound, but there must be a minimum.

For example, these are all acceptable:

    bytestring ##0.9.2.1
    bytestring ># 0.9.2.1 && < 0.10
    bytestring ># 0.9.2.1

This is not acceptable because there is no bound at all:

    bytestring

This is not acceptable because there is no lower bound:

    bytestring <# 0.10

Then you build a test executable.  An example is in the
`sunlight-test.hs` file in the sunlight git tree.  This program
tests sunlight itself.

The executable will output a number of reports in a `sunlight`
directory, and it will also output a `current-versions.txt` report
to show you the most recent versions of dependencies that work with
your package, as well as a `minimum-versions.txt` file showing you
that your package builds and tests with the minimum package
versions.

Also, see the Haddocks on the Test.Sunlight module.

## Disclaimer

sunlight works, but I'm not sure it is a good solution to the
problem of managing bounds and dependencies in a Cabal file.  It
works on simple Cabal files but I have not tested it on any
exotic files.

## See also

cabal-bounds - program to automatically manage the bounds and
versions in a Cabal file.
[Github](https://github.com/dan-t/cabal-bounds)
[Hackage](http://hackage.haskell.org/package/cabal-bounds)

## License

sunlight is licensed under the BSD license.

## Why the name?

The word "cabal" has shadowy connotations.  This package helps
ensure your cabal is doing what you say it is doing...so it helps
shine sunlight on your cabal which is, as they say, the best
disinfectant.
