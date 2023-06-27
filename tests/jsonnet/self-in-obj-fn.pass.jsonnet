
local x = {
    a: "test",
    b: function(c) self.a + c
};

assert x.b(" 2") == "test 2";

true
