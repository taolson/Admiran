|| astar.m -- implementation of the A-Star search algorithm
||
|| Note on heuristic admissiblity: for the A-Star algorithm to return the minimum-cost solution, all of the estimates provided by the
|| estimate fn must be strictly less than or equal to the actual cost


%export aStarSolve aStarReachable

%import <map>
%import <maybe>
%import <mirandaExtensions>
%import <set>


aStarState * **                  || * = move type, ** = expansion state type
    ::= AStarState
        (ordI *)                 || ord instance for move type
        (ordI (int, *))          || ord instance for (int, *)
        (* -> bool)              || goal test
        ((*, **) -> ([*], **))   || expansion fn (with expansion state, e.g. cache)
        ** !                     || expansion state
        (* -> * -> int)          || cost fn
        (* -> int)               || estimate fn
        (s_set (int, *))!        || open set
        (s_set *)!               || closed set
        (m_map * int)!           || gScore
        (m_map * int)!           || fScore
        (m_map * *)!             || path map

aStarInit :: ordI * -> * -> (* -> bool) -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> aStarState * **
aStarInit cmp start gf xf xs cf ef
    = AStarState
      cmp
      cmpo
      gf
      xf
      xs
      cf
      ef
      (s_singleton (initEst, start))
      s_empty
      (m_singleton start 0)
      (m_singleton start initEst)
      m_empty
      where
        initEst = ef start
        cmpo    = cmppair cmpint cmp

backtrace :: aStarState * ** -> * -> ([*], **)
backtrace (AStarState cmp cmpo gf xf xs cf ef os cs gs fs pm) move
    = (path, xs)
      where
        path   = go [move] move    
        go p m = p,              if isNothing mm
               = go (m' : p) m', otherwise
                 where
                   mm = m_lookup cmp m pm
                   m' = fromJust mm

addMove :: * -> aStarState * ** -> * -> aStarState * **
addMove current st move
    = st,                                                     if s_member cmp move cs
    = AStarState cmp cmpo gf xf xs cf ef os'' cs gs' fs' pm', if update
    = AStarState cmp cmpo gf xf xs cf ef os'' cs gs  fs  pm,  otherwise
      where
        AStarState cmp cmpo gf xf xs cf ef os cs gs fs pm = st
        score    = m_findWithDefault cmp maxBound current gs + cf current move
        moveFs   = score + ef move
        update   = score < m_findWithDefault cmp maxBound move gs
        pm'      = m_insert cmp move current pm
        prevFs   = m_findWithDefault cmp maxBound move fs
        os'      = s_delete cmpo (prevFs, move) os, if update & prevFs < maxBound
                 = os, otherwise
        gs'      = m_insert cmp move score gs
        fs'      = m_insert cmp move moveFs fs
        os''     = s_insert cmpo (moveFs, move) os'
        maxBound = 0xffffffffffff

solve :: aStarState * ** -> ([*], **)
solve (AStarState cmp cmpo gf xf xs cf ef os cs gs fs pm)
    = ([], xs),                                                                if s_null os
    = backtrace (AStarState cmp cmpo gf xf xs cf ef os' cs  gs fs pm) current, if gf current
    = ns $seq solve ns,                                                        otherwise
      where
        ns                  = foldl (addMove current) (AStarState cmp cmpo gf xf xs' cf ef os' cs' gs fs pm) adj
        ((_, current), os') = fromJust $ s_viewMin os 
        cs'                 = s_insert cmp current cs
        (adj, xs')          = xf (current, xs)

reachable :: aStarState * ** -> m_map * *
reachable (AStarState cmp cmpo gf xf xs cf ef os cs gs fs pm)
    = pm,                                                  if s_null os
    = adj $seq reachable (foldl (addMove current) ns adj), otherwise
      where
        ns                  = AStarState cmp cmpo gf xf xs' cf ef os' cs' gs fs pm
        ((_, current), os') = fromJust $ s_viewMin os 
        cs'                 = s_insert cmp current cs
        (adj, xs')          = xf (current, xs)

aStarSolve :: ordI * -> * -> (* -> bool) -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> ([*], **)
aStarSolve cmp start gf xf xs cf ef = solve (aStarInit cmp start gf xf xs cf ef)

aStarReachable :: ordI * -> * -> ((*, **) -> ([*], **)) -> ** -> (* -> * -> int) -> (* -> int) -> m_map * *
aStarReachable cmp start xf xs cf ef = reachable (aStarInit cmp start (const False) xf xs cf ef)
