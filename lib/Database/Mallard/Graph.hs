{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Mallard.Graph
    ( MigrationGraph
    , mkMigrationGraph
    , getUnappliedMigrations
    , emptyMigrationGraph
    ) where

import           Control.Lens
import qualified Data.Graph.Inductive.Basic        as G
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive.Query.DFS    as G
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as Set
import           Data.Monoid
import           Database.Mallard.Types

data MigrationGraph
    = MigrationGraph (HashMap MigrationId G.Node) (G.Gr MigrationId ())

emptyMigrationGraph :: MigrationGraph
emptyMigrationGraph = MigrationGraph Map.empty G.empty

-- | Graph will only build if there are no circular references.
mkMigrationGraph :: MigrationTable -> Maybe MigrationGraph
mkMigrationGraph mTable =
    if hasCircle graph
        then Nothing
        else Just $ MigrationGraph nodeLookupMap graph
    where
        migrations = Map.elems mTable
        nodeAssignment = zip [1..] (fmap (^. migrationName) migrations)
        nodeLookupMap = Map.fromList $ fmap (\(a, b) -> (b, a)) nodeAssignment
        lookupNode mName =
            case Map.lookup mName nodeLookupMap of
                Nothing -> error $ "This migration requires \"" <> show mName <> "\" and it does not exist."
                Just n -> n
        replaceRequires m = fmap lookupNode (m ^. migrationRequires)
        graph = G.grev
                $ G.insEdges (concatMap (\m' -> zip3 (repeat (lookupNode (m' ^. migrationName))) (replaceRequires m') (repeat ())) migrations)
                $ G.insNodes nodeAssignment G.empty

hasCircle :: G.Gr a b -> Bool
hasCircle g = or $ fmap (\g' -> length g' /= 1) $ G.scc g

getUnappliedMigrations :: MigrationGraph -> [MigrationId] -> [MigrationId]
getUnappliedMigrations (MigrationGraph mNodeTable mGraph) applied = G.topsort' unappliedGraph
    where
        appliedMigrationIds = Set.fromList applied
        unappliedGraph = flip G.delNodes mGraph
            $ fmap (\(_, v) -> v) $ Map.toList
            $ Map.filterWithKey (\k _ -> Set.member k appliedMigrationIds)
            $ mNodeTable
