
local func(a=1, b) = a - b;

assert func(1, 2) == -1;
assert func(b = 5) == -4;
assert func(7, b = 3) == 4;

true
