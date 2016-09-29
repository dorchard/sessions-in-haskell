This document is a tutorial with various examples for using the
'Session types as an effect system' encoding, translated into Haskell,
provided by our Control.Effect.Session library.

The internals of the library can be explored in:
    Control/Effect/Sessions.hs
    Control/Effect/Sessions/Process.lhs
    Control/Effect/Sessions/Operations.lhs
Or via the haddock documentation at:
   http://www.doc.ic.ac.uk/~dorchard/sessions/doc/html/effect-sessions/Control-Effect-Sessions.html

Note that all the examples here have session types (embedded in effect types)
that are inferred by GHC (given only some type signatures to name channels). This
inference reduces programmer effort and eases development and verification.

****** Setup

First, some language extensions and modules are required:

> {-# LANGUAGE RebindableSyntax, DataKinds, TypeOperators, GADTs, ScopedTypeVariables,
>              FlexibleInstances, FlexibleContexts #-}

> import Prelude hiding (Monad(..), print, putStrLn) -- hide the usual Monad
> import Control.Effect.Sessions                     -- our library

> import GHC.Exts (Constraint)

The 'Msg' data type will be used throughout.

> data Msg = Ping | Pong deriving Show

All the examples from this file can be run in one go from here
to get their outputs.

> main = run $
>         do putStrLn "Running all examples in this file...\n"
>            putStrLn "Simple send-receive example:"
>            simple
>            putStrLn "\nSend-receive with multiple channels:"
>            incProc
>            putStrLn "\nDelegation example: "
>            simpleDelg
>            putStrLn "\nExample 4 from the paper: "
>            process
>            putStrLn "\nConditionals (alternation): "
>            condProc
>            putStrLn "\nRecursion example, modelling replication in the session calculus: "
>            repProc
>            putStrLn "\nDone!"

****** Simple sending and receiving

The first example gives two processes: 'simpleServer' which receives a message
from 'simpleClient'.

> simpleServer c = do x <- recv c
>                     putStrLn $ "Received: " ++ x
>
> simpleClient c = do send c "Hello"
>
> simple = new $ \(c, c') -> simpleClient c `par` simpleServer c'

This program can be run using ghci as follows:

$ ghci examples.lhs

 Main*> run simple
 "Received: Hello"
 Main*>

Depending on your system, you might get an output that overlaps
the output message with the prompt message, e.g.

 Main*> run simple
 "ReMain*>cieved: Hello"

This is normal: it is a result of the process 'simple' running in parallel
with GHCi and interleaving output to the console.

This example combines three elements: sending/receiving, 'new'
channels, and 'parallel composition' via 'par. The session types of
each component can be queried:

 *Main> :t simpleServer
 simpleServer :: Chan c -> Process '[c ':-> ([Char] ':? 'End)] ()

 *Main> :t simpleClient
 simpleClient :: Chan c -> Process '[c ':-> ([Char] ':! 'End)] ()

 *Main> :t simple
 simple :: Process '[] ()

This shows the inferred session types for the server, client, and
composed whole. (Note '[Char]' is often aliased by Haskell to
'String') Since there are no free channels in 'simple', then it can be
run via 'run :: Process '[] a -> IO a'. We see that 'simpleServer'
receives a string then stops, and 'simpleClient' send a string.

The 'new' combinator models $\nu$ from the session calculus, taking a
function mapping from a pair of two endpoints for a channel to a
process.  We'll examine its type along with channel naming next.

****** Naming and endpoints

Once we start using multiple channels we will need to explicitly
name these with additional type signatures. Here's an example.

> incServer (c :: Chan (Ch "c")) (d :: (Chan (Ch "d"))) =
>                 do x <- recv c
>                    send d (x + 1)

> incClient (c :: Chan (Op "c")) (d :: (Chan (Op "d"))) =
>                 do send c 42
>                    x <- recv d
>                    putStrLn $ "Got " ++ show x

Names comprise either 'Ch' or 'Op' of a type-level symbol. In the
above, the server 'incServer' has channels named 'Ch "c"' and 'Ch "d"'
whereas 'incClient' has 'Op "c"' and 'Op "d"'-- the names of the opposite
endpoints.  These explicit names must be unique. This is a necessary
part of the encoding due to Haskell limitations [aside: existentials could
be used for (fresh) names, but this does not interact well with the type-level
encoding of finite maps].

These two processes are then composed via:

> incProc = new $ \(c, c') -> new $ \(d, d') -> incServer c d `par` incClient c' d'

Again we can run this:

$ ghci examples.lhs  [if you haven't already loaded it]

  *Main> run incProc
  "Got 43"

And we can query the inferred session types, e.g.

  Main*> :t incServer

  incServer
   :: Num t =>
      Chan ('Ch "c")
      -> Chan ('Ch "d")
      -> Process
           '['Ch "c" ':-> (t ':? 'End), 'Ch "d" ':-> (t ':! 'End)] ()

Here we see the session types of the two channels "c" and "d", which receive
and send a value of type 't' which is a member of the 'Num' class.

Looking at the type of 'new' we can see that it generates two dual
endpoints of a channel and passes them to a process function, and
checking duality of their usage in the parameter process:

  *Main> :t new
  new :: DualP (env :@ 'Ch c) (env :@ 'Op c) =>
     ((Chan ('Ch c), Chan ('Op c)) -> Process env a)
     -> Process ((env :\ 'Op c) :\ 'Ch c) a

That is, check that "Ch c" and "Op c" are dual in "env" (lookup is via ':@') and then
in the returned process, remove them from the environment (via ':\'). 'DualP' is a
binary predicate (relation) for duality of session types (defined in
Control.Effect.Sessions.Process).

****** Delegation

Here is an example using delegation, which is essentially 'Example 4'
in the paper (p.11).

Consider the following process 'serverD'
which receives a channel 'd' on 'c', and then sends a 'Ping' on it:

> serverD (c :: (Chan (Ch "c"))) = do chRecvSeq c (\(d :: Chan (Ch "x")) -> send d Ping)

Note, the variable name 'd' and the channel name 'Ch "x"' do not have to match.
[chRecvSeq is a simplified version of chRecv]

The type of 'serverD' is inferred as:

 serverD :: Chan ('Ch "c")
       -> Process '['Ch "c" ':-> ('Delg (Msg ':! 'End) ':? 'End)] ()

This explains that along the channel we receive a delegated session channel
on which a 'Msg' can be sent.

We then define a client to interact with this that binds d (and its dual d'),
then sends d over c and waits to receive a ping on d'.

> clientD (c :: Chan (Op "c")) =
>        new (\(d :: (Chan (Ch "d")), d') ->
>                              do  chSend c d
>                                  Ping <- recv d'
>                                  putStrLn "Client got a ping")

> simpleDelg = new $ \(c, c') -> serverD c `par` clientD c'

Let's examine the type of |clientD|:

 *Main> :t clientD
  clientD
    :: (DualP s (Msg ':? 'End), Dual s ~ (Msg ':? 'End)) =>
       Chan ('Op "c") -> Process '['Op "c" ':-> ('Delg s ':! 'End)] ()

Looking at the body of the type, and not the constraint (before =>),
we see that the 'Op "c"' channel endpoint has a channel sent over it
with session type 's'.  The constraint explains that the dual of 's'
must be 'Msg :? 'End'. This constraint later gets satisfied when
'serverD' and 'clientD' are composed in parallel and restricted over
by 'new'.

Here is Example 4 exactly as it appears in the paper (which is similar to
the above example):

> client (c :: (Chan (Ch "c")))
>    = new (\(d :: (Chan (Ch "d")), d') ->
>                   do  chSend c d
>                       Ping <- recv d'
>                       print "Client: got a ping")
>
> server c = do { k <- chRecv c; k (\x -> send x Ping) }
> process = new (\(c, c') -> (client c) `par` (server c'))
>

 Main*> run process
 "Client got a ping"

Here's a slight variation, combining value and delegation communication.

> clientV (c :: (Chan (Ch "c")))
>    = new (\(d :: (Chan (Ch "d")), d') ->
>                   do  chSend c d
>                       send c Ping
>                       Ping <- recv d'
>                       print "Client: got a ping")
>
> serverV c = do k <- chRecv c
>                k (\(x :: Chan (Ch "x")) -> do Ping <- recv c
>                                               send x Ping)
>
> processV = new (\(c, c') -> (client c) `par` (server c'))

****** Alternation

The usual 'case' and 'if' constructs of Haskell can be used, but since
Haskell does not have subtyping, any subeffecting that would occur in
fPCF must be inserted explicitly via the 'sub' combinator. There are a
 number of specialised variants provided by the library for common cases
of subeffecting:

  subL :: Process '[c :-> s] a -> Process '[c :-> s :+ t] a
  subR :: Process '[c :-> t] a -> Process '[c :-> s :+ t] a
  subEnd :: Chan c -> Process '[] a -> Process '[c :-> End] a

where 'subL' and 'subR' are used to introduce the upper bound :+
effect type and 'subEnd' essentially provides a kind of weakening.

The following shows an example:

> condServer (c :: (Chan (Ch "x"))) =
>                  do (l :: Int) <- recv c
>                     case l of 0 -> subL $ send c True
>                               n -> do (x :: Bool) <- subR $ recv c
>                                       return ()

The server receives an Int. If its 0 then it sends back 'True'
else it waits to receive a 'Bool' before stopping. The inferred type
of the server explains this protocol:

 *Main> :t condServer
 condServer
   :: Chan ('Ch "x")
      -> Process
           '['Ch "x" ':-> (Int ':? ((Bool ':! 'End) ':+ (Bool ':? 'End)))] ()

This is then composed in parallel with a dual client to make 'condProc'

> condClient c' = do send c' (0 :: Int)
>                    case 0 of 0 -> do { (x :: Bool) <- subL $ recv c'; print x }
>                              n -> subR $ send c' False

> condProc = new (\(c :: (Chan (Ch "x")), c') ->
>                 (condServer c) `par` (condClient c'))

 *Main> run condProc
 True

Note that 'condClient' does not happen to need an explicit channel
name for c' since it is the only channel used in the
computation. However, examining the type (via :t condClient in GHCi)
reveals a large, unwieldy type, that exposes a lot of the internals of
managing type-level finite sets. This is due to GHC expanding all the
type functions as far as it can go, which is not the ideal behaviour
for users. The clutter can be reduced by giving an explicit name to
c', although we choose to leave it polymorphic in the channel name
here for illustration.

****** Fixed-points

Since Haskell does not have equi-recursive types, the full encoding of
Section 6 cannot be directly embedded. Instead, we have opted for a
mid-point, a restricted form of recursive processes is allowed: those
whose fixed-point effect equation is *affine*. That is given an effect
type that can be simplified (via some type-level normalisation) into x
|-> a x + b then the effect type x = a* b is computed.

The following gives the Haskell rendering of equation (19), p. 9,
which for some channel 'c' and (encoded) process 'p', embeds
'*c?(d).p', the replicated input of a channel 'd' along 'c', which is
then bound in the scope of 'p'.

> -- the type signature is not strictly necessary here, but is included for clarity
> repInp :: (Chan (Ch "c")) -> (Chan (Ch "d") -> Process '[Ch "d" :-> s] ())
>                           -> (Process '[Ch "c" :-> Fix (Int :? ((Delg s) :? Star)) (Int :? End)] ())
> repInp c p = affineFix (\f -> \() ->
>                        do (x :: Int) <- recv c
>                           case x of 0 -> subL $ subEnd c $ return ()
>                                     n -> subR $ do { k <- chRecv c; (k p) `par` (f ()) }) ()

As described in Section 6, replicated input proceeds by repeatedly
receiving first an integer 'x' which signifies whether to stop
receiving if x = 0 (the 'garbage collect' action) or to receive a
channel and recurse if x > 0.  Note again the use of explicit
subeffecting which is needed in Haskell, whereas FPCF assumes the
ability to implicitly insert subeffecting casts.

For example, we might have a server that repeatedly receives and
prints a message using the recursive 'repInp' process above.

> serverA (c :: (Chan (Ch "c"))) =
>     repInp c (\(d :: (Chan (Ch "d"))) -> do (x :: Msg) <- recv d
>                                             print x)

A number of clients can then have some finite number of repeated
interactions with the server.

> clientA (c :: (Chan (Op "c"))) = new (\(d :: Chan (Ch "d"), d') ->
>                                    do -- Send a ping
>                                       send c (1 :: Int)
>                                       rsend c d
>                                       send d' Ping)

> clientB (c :: (Chan (Op "c"))) = new (\(d :: (Chan (Ch "d")), d') ->
>                                    do -- Send a pong
>                                       send c (1 :: Int)
>                                       rsend c d
>                                       send d' Pong)

> repProc = new $ \(c, c') -> do (serverA c) `par` (do (clientA c') `par` (clientB c')
>                                                      send c' (0 :: Int)) -- end the server


[Note, with GHC 7.10.1 on Mac OS X, we noticed that 'run repProc'
occasionally causes a runtime SegFault. This bug appears to be with
the runtime for 7.10.1 and does not occur in GHC 7.8.* or GHC 7.10.2.]

----------------------

******* Ill-typed erroneous processes

The following are some examples of processes which fail to type check, either as is,
or when composed with 'run'. This is a good thing, because these processes
are erroneous! The types are doing their job.
Uncomment them one at a time to see how they fail.

* Non-dual behaviours

Consider an erroneous permutation of the first example, 'simple'>

> simpleServerE c = do x <- recv c
>                      putStrLn $ "Received: " ++ x
>                      y <- recv c
>                      putStrLn $ "Received: " ++ y
>
> simpleClientE c = do send c "Hello"

'simpleServerE' receives two values, but only one is sent by the client.
Uncomment the following line to fine the type error

> -- Uncomment below to see the expected type error
> -- simpleE = new $ \(c, c') -> simpleClientE c `par` simpleServerE c'

GHC reports:

  Could not deduce (DualP 'End ([Char] ':? 'End))

Thus, we see that 'End (from the client) is not dual to the additional
[Char] :? End operation of the server.

* Unbalanced delegation behaviour.

This example shows further how the balancing predicate inside the composition works.
If we send a channel and then use it, this is 'unbalanced' behaviour:

> unbalanced (c :: (Chan (Ch "c"))) (d :: Chan (Ch "d")) = do chSend c d; send d "Hello"

If you look at the type of 'unbalanced', you will see that it has the constraint
that 'NotBal (Bal s)', which can never be satisfied: 'NotBal' is a predicate on all
session types apart from 'Bal' session types. [Furthermore, 'NotBal' is a 'closed class'
and so cannot be maliciously extended with a 'Bal' instance in another file].

* Non-deterministic behaviour

Session types rule out non-deterministic behaviours, for example, they reject
the pi-calculus term:  (c?(x).P | c?(x).Q | c!<0>) which non-deterministically
reduces to either P or Q. This is ruled out by balancing, and thus by the
'balanced' predicate in 'par that checks all the right-hand session types
are used in balanced way to the left.
This rejects the encoding of the non-deterministic pi-calculus term above:

> --badServer (c :: (Chan (Ch "c"))) = do { recv c;  return () } `par` do { recv c;  return () }
> --badProc = new (\(c, c') -> badServer c `par` (send c' True))

If just 'badServer' is uncommented, we see in its type the unsatisfiable constraint
of NotBal (Bal (a :? End)). When both are uncommented this error arises directly.
