sum :: k:Int -> {v:Int | v>=0 && v>=k}
Q = { 0 <= v, * <= v, v <= *, v < len * }

# GOOD

let rec sum k =
  if k <= 0 then 0
  else let s = sum (k-1) in s+k

k:k1, k<=0 |- {v=0} <: k_r
k:k1, ~(k<=0) |- k_e <: k_r
k:k1, ~(k<=0), s:[k-1|k]k_r |- {v=s+k} <: k_e
k:k1, ~(k<=0) |- {v=k-1} <: k_1

iteration 1:
k1 = 0 <= v && k <= v && v <= k
k_e = 0 <= v && k <= v && v <= k
k_r = 0 <= v && k <= v && v <= k

iteration 2 (constraint 1)
k1 = empty (int)
k_r= 0 <= v && v <= k
k_e = 0 <= v && k <= v && v <= k

iteration 3 (constraint 3):
k1 = empty (int)
k_r= 0 <= v && k <= v
k_e = 0 <= v && k <= v


# BAD

let rec sum k =
  if k <= 0 then 0
  else let s = sum (k-1) in s-k

k:k1, k<=0 |- {v=0} <: k_r
k:k1, ~(k<=0) |- k_e <: k_r
k:k1, ~(k<=0), s:[k-1|k]k_r |- {v=s-k} <: k_e
k:k1, ~(k<=0) |- {v=k-1} <: k_1

iteration 1:
k1 = 0 <= v && k <= v && v <= k
k_r = 0 <= v && k <= v && v <= k
k_e = 0 <= v && k <= v && v <= k

iteration 2 (constraint 1):
k1 = empty (int)
k_r= 0 <= v && k <= v
k_e = 0 <= v && k <= v && v <= k

iteration 3 (constraint 3)
k1 = empty (int)
k_r = 0 <= v && k <= v
k_e = empty (int)

iteration 4 (constraint 2)
k1 = empty (int)
k_r = empty (int)
k_e = empty (int)

## fault localization: throw away constraints

k:k1, k<=0 |- {v=0} <: k_r
k:k1, ~(k<=0) |- k_e <: k_r
k:k1, ~(k<=0), s:[k-1|k]k_r |- {v=s-k} <: k_e (ERASE)
k:k1, ~(k<=0) |- {v=k-1} <: k_1

iteration 1:
k1 = 0 <= v && k <= v && v <= k
k_e = 0 <= v && k <= v && v <= k
k_r = 0 <= v && k <= v && v <= k

iteration 2 (constraint 1)
k1 = empty (int)
k_r = 0 <= v && k <= v
k_e = 0 <= v && k <= v && v <= k

iteration 3 (constraint 2)
k1 = empty (int)
k_r = 0 <= v && k <= v
k_e = 0 <= v && k <= v

REFINEMENT PASSED
WEIGHT OF REMOVED CONSTRAINTS: 10

## fault localization: throw away constraints

k:k1, k<=0 |- {v=0} <: k_r
k:k1, ~(k<=0) |- k_e <: k_r
k:k1, ~(k<=0), s:[k-1|k]k_r |- {v=s-k} <: k_e
k:k1, ~(k<=0) |- {v=k-1} <: k_1 (ERASE)

iteration 1:
k1 = 0 <= v && k <= v && v <= k
k_e = 0 <= v && k <= v && v <= k
k_r = 0 <= v && k <= v && v <= k

iteration 2 (constraint 1):
k1 = empty (int)
k_r = 0 <= v && k <= v
k_e = 0 <= v && k <= v && v <= k

iteration 3 (constraint 3):
k1 = empty (int)
k_r = 0 <= v && k <= v
k_e = empty (int)

iteration 4 (constraint 2):
k1 = empty (int)
k_r = empty (int)
k_e = empty (int)


