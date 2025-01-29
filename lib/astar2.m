|| astar.m -- implementation of the A-Star search algorithm
||
|| Note on heuristic admissiblity: for the A-Star algorithm to return the minimum-cost solution, all of the estimates provided by the
|| estimate fn must be strictly less than or equal to the actual cost


%export astar

%import <heap>
%import <map>
%import <maybe>
%import <set>


|| return min cost, path, and final expansion state if a solution is found, or
|| a list of the reachable nodes and final expansion state, if no solution is found
astar :: ordI *                        ->    || ord instance for graph nodes
         *                             ->    || start node
         **                            ->    || initial expand state
         (* -> bool)                   ->    || goal function
         (* -> ** -> ([(*, int)], **)) ->    || expand function (expands to list of nodes and costs and updated expand state)
         (* -> int)                    ->    || heuristic estimate function (estimate remaining cost to move from node to goal)
         (maybe int, [*], **)                || final expand state, along with either a final cost and a path,
                                             ||  if there is a solution, or the list of visited nodes, otherwise
astar cmp start es gf xf ef
    = solve (h_singleton (ef start, 0, start)) es s_empty (m_singleton start 0) m_empty
      where
        solve q es seen minCosts paths
            = case h_viewMin cmpest q of
                Nothing  -> (Nothing, m_keys paths, es)         || solution not found; return reachable nodes
                Just elt -> check elt
              where
                cmpest (e1, _, _) (e2, _, _) = cmpint e1 e2     || compare function for ordering priority queue

                check ((_, cost, node), q')
                    = (Just cost, findPath paths node, es), if gf node                  || reached goal
                    = solve q' es seen minCosts paths,      if s_member cmp node seen   || node already seen
                    = expand q' node cost,                  otherwise                   || expand out from node

                expand q node cost
                    = case s_insert cmp node seen of seen' ->                   || add node to seen set
                        case foldl insq q ns of q' ->                           || add neighbors to queue
                          case foldl insCost minCosts ns of minCosts' ->        || add neighbors to minCosts map
                            case foldl insPath paths ns of paths' ->            || add neighbors to paths
                              solve q' es' seen' minCosts' paths'
                      where
                        (rs, es') = xf node es
                        ns        = [(ef n + cost, c + cost, n) | (n, c) <- rs; ~s_member cmp n seen & best n (c + cost)]
                        best n c  = fromMaybef True (> c) $ m_lookup cmp n minCosts     || is cost to move here less than any previous?

                        insq    q v         = h_insert cmpest v q
                        insCost m (_, c, n) = m_insert cmp n c m
                        insPath m (_, _, n) = m_insert cmp n node m

        findPath m n
            = case m_lookup cmp n m of
                Nothing -> [n]
                Just n' -> findPath m n' ++ [n]
