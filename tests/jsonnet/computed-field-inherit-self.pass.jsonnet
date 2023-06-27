
local x = {
    a: "v1",
    b: {
        a: "v2",

        [self.a]+: self.a
    }
};

assert "v1" in x.b;
assert x.b.v1 == "v2";
x
