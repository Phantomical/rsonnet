
local x = {
    a: "outer",
    b: {
        a: "1",

        [self.a]: self.a
    }
};

assert "outer" in x.b;
assert x.b.outer == "1";
true
