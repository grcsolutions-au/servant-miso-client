{-# LANGUAGE CPP #-}

{-| MisoString without the 'miso' dependency

Whilst this package attempt to provide a completely compatible interface
that allows one to write @servant@ style interfaces that work with @miso@,
in some cases you'll find you'll need to use @MisoString@ directly in
your interface definitions. This will work fine in native compiles,
because @MisoString@ in @Miso.String@ is defined as follows:

@
#if defined(VANILLA)
type MisoString = Text
#else
type MisoString = JSString
#endif
@

So in native code you're just using @Text@ which is good.

But, you've then still got a dependency on @miso@.

To avoid that dependency, you can just use the 'MisoString' in this module.

That way the @miso@ dependency is avoided in native code, as this package
does not depend on @miso@ when building in native code.
-}
module Miso.String.Compat (MisoString) where

#ifndef VANILLA
import Miso.String (MisoString)
#else
import Data.Text (Text)
#endif

#ifdef VANILLA
type MisoString = Text
#endif