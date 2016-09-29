module Control.Effect.Sessions
                (Process(..), Chan(..), Name(..), Symbol, Session(..), Delegate(..), 
                 Dual, DualP, SessionSeq, Balanced, BalancedPar, NotBal, 
                 Effect(..), Control.Effect.fail, run, 
                 Map(..), (:@), Union, (:\), 
                 send, recv, new, par, rsend, chSend, chRecv, chRecvSeq, 
                 sub, subL, subR, subEnd, affineFix, caseUnion, 
                 print, putStrLn, liftIO, ifThenElse) where

import GHC.TypeLits

import Prelude hiding (print, putStrLn, Monad(..))

import Control.Effect
import Control.Effect.Sessions.Process
import Control.Effect.Sessions.Operations
import Data.Type.FiniteMap

chRecvSeq c k = (chRecv c) >>= (\kf -> kf k)
