{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Mallard.Graph
    ( HasMigrationNodeTable (..)
    , HasMigrationGraph (..)
    , generateMigrationGraph
    , getUnappliedMigrations
    ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.Graph.Inductive.Basic        as G
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.DFS    as G
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as Set
import           Database.Mallard.Types

class HasMigrationNodeTable a where
    migrationNodeTable :: Lens' a (HashMap MigrationId G.Node)

class HasMigrationGraph a where
    migrationGraph :: Lens' a (G.Gr MigrationId ())

generateMigrationGraph
    :: ( MonadIO m, MonadState s m, MonadThrow m
        , HasMigrationTable s, HasMigrationNodeTable s, HasMigrationGraph s) => m ()
generateMigrationGraph = do
    migrations <- fmap (\s -> Map.elems (s ^. migrationTable)) get

    let nodeAssignment = zip [1..] (fmap (^. migrationName) migrations)
        nodeLookupMap = Map.fromList $ fmap (\(a, b) -> (b, a)) nodeAssignment
        lookupNode mName =
            case Map.lookup mName nodeLookupMap of
                Nothing -> error "This migration requires a migration that doesn't exist. (Non recoverable, contact andrewrademacher@icloud.com)"
                Just n -> n
        replaceRequires m = fmap lookupNode (m ^. migrationRequires)

    let graph = G.grev
                $ G.insEdges (concatMap (\m' -> zip3 (repeat (lookupNode (m' ^. migrationName))) (replaceRequires m') (repeat ())) migrations)
                $ G.insNodes nodeAssignment G.empty

    modify $ \s -> s
        & migrationNodeTable .~ nodeLookupMap
        & migrationGraph .~ graph

getUnappliedMigrations
    :: ( MonadState s m
        , HasMigrationNodeTable s, HasMigrationGraph s )
    => [MigrationId] -> m [MigrationId]
getUnappliedMigrations applied = do
    mNodeTable <- fmap (^. migrationNodeTable) get
    mGraph <- fmap (^. migrationGraph) get

    let appliedMigrationIds = Set.fromList applied
        unappliedGraph = flip G.delNodes mGraph
            $ fmap (\(_, v) -> v) $ Map.toList
            $ Map.filterWithKey (\k _ -> Set.member k appliedMigrationIds)
            $ mNodeTable
    return $ G.topsort' unappliedGraph
