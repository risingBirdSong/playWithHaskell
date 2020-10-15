
-- see if list of strings contain given chars
  filtering xs c = filter (==c) xs -- "oo"

  contains y xs = any (==y) xs
  
  listContains xxs y = map (contains y) xxs
  
  -- add list of list numbers
  add' xs = sum (map (sum) xs)
