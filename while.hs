type State = Int

bool condition iftrue iffalse = if condition then iftrue else iffalse

while :: State -> (State -> Bool) -> (State -> State) -> State
while initialState condition body =
  if condition initialState then
    initialState
  else
    while (body initialState) condition body

helper condition initialState = bool (condition initialState) initialState

while' :: State -> (State -> Bool) -> (State -> State) -> State
while' initialState condition = \x ->
  helper condition initialState
  (while (x initialState) condition x)

while1' initialState condition =
  ap
  (\x -> \c -> helper condition initialState c)
  (\x -> while (x initialState) condition x)

while2' initialState condition =
  ap
  (const $ helper condition initialState)
  (\x -> while (x initialState) condition x)

while3' initialState condition =
  ap
  (const $ helper condition initialState)
  (ap
    (\x -> \c -> while (x initialState) condition c)
    id 
  )

while4' initialState condition =
  ap
  (const $ helper condition initialState)
  (ap
    (ap
      (\x -> while (x initialState))
      (const condition)
    )
    id 
  )

while5' initialState condition =
  ap
  (const $ helper condition initialState)
  (ap
    (ap
      (ap (const while) (ap id (const initialState)))
      (const condition)
    )
    id 
  )

-- 20 means 2.0
while20 initialState =
  ap
  (ap (const ap) (ap (const const) (flip helper initialState)))
  (\x -> ap
    (ap
      (ap (const while) (ap id (const initialState)))
      (const x)
    )
    id 
  )

while21 initialState =
  ap
  (ap (const ap) (ap (const const) (flip helper initialState)))
  (ap (ap (const ap)
    (ap (const $ ap (ap (const while) (ap id (const initialState))))
      const
    ))
    (const id)
  )

while30 =
  ap
  (ap (const ap) 
    (ap (const $ ap (const ap))
      (ap (const $ ap (const const)) (flip helper))
    )
  )
  (\x -> ap (ap (const ap)
    (ap (const $ ap (ap (const while) (ap id (const x))))
      const
    ))
    (const id)
  )

while31 =
  ap
  (ap (const ap) 
    (ap (const $ ap (const ap))
      (ap (const $ ap (const const)) (flip helper))
    )
  )
  (ap (ap (const ap) (ap (const $ ap (const ap)) (ap (ap (const ap) (ap (const const) (ap (const ap)

  (\x -> ap (const while) (ap id (const x)))

  ))) (const const)))) (const $ const id))

-- Final version, eta reduced by hand following the SKI rules
-- from the stack overflow answer at https://stackoverflow.com/a/33456089
while32 =
  ap
  (ap (const ap) 
    (ap (const $ ap (const ap))
      (ap (const $ ap (const const)) (flip helper))
    )
  )
  (ap (ap (const ap) (ap (const $ ap (const ap)) (ap (ap (const ap) (ap (const const) (ap (const ap)

  (ap (const $ ap (const while32)) (ap (const $ ap id) const))

  ))) (const const)))) (const $ const id))

ap f g = \x -> f x (g x)
-- The computer generated version by pointfree.io
pfwhile' =
  ap (ap . ((.) .) . flip helper) (flip flip id . (ap .) . flip . (while .) . flip id)
