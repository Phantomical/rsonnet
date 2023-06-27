
local x = {
    a: "v1",
    b: {
        a: "v2",

        [
            local c = {
                a: "v3",
                b: self.a
            };
            c.a
        ]: self.a
    }
};

assert "v3" in x.b;
assert x.b.v3 == "v2";
true
