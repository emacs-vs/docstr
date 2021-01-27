# Lua

You can customize variable `docstr-lua-style` for the following value.

* `luadoc` - Kepler's specification (default)
* `doxygen` - doxygen/Javadoc-like style
* `scriptum` - Lua based document generator to Markdown
* `nil` - Respect to user's customization

### Spliter

If you are using style that requres spliiter; you can change the splitter by
customize the variable `docstr-lua-splitter`.

The `doxygen` style look like this,

```lua
-- taken from cgilua/src/cgilua/session.lua
-------------------------------------
-- Deletes a session.
-- @param id Session identification.
-------------------------------------
function delete (id)
    assert (check_id (id))
    remove (filename (id))
end
```

## References

* [Lua Wiki](http://lua-users.org/wiki/DocumentingLuaCode)
* [LuaDoc](https://keplerproject.github.io/luadoc/manual.html)
* [lua-scriptum](https://github.com/charlesmallah/lua-scriptum)
