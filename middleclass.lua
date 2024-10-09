local middleclass = {
  _VERSION     = 'middleclass v4.1.1 +EXTRA',
  _DESCRIPTION = 'Object Orientation for Lua',
  _URL         = 'https://github.com/kikito/middleclass',
  _LICENSE     = [[
    MIT LICENSE

    Copyright (c) 2011 Enrique García Cota

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

local assert, getmetatable, pairs, rawget, select, setmetatable, tostring, type =
    assert, getmetatable, pairs, rawget, select, setmetatable, tostring, type
local DefaultMixin, weakKeys, _createIndexWrapper, _propagateInstanceMethod, _declareInstanceMethod, _tostring, _call, _createClass, _includeMixin, _is

weakKeys = { __mode = 'k' }

function _is(v, x) return type(v) == x end

function _createIndexWrapper(aClass, f)
  if f == nil then
    return aClass.__instanceDict
  elseif _is(f, 'function') then
    return function(self, name)
      local value = aClass.__instanceDict[name]

      if value ~= nil then
        return value
      else
        return (f(self, name))
      end
    end
  else -- if  type(f) == "table" then
    return function(self, name)
      local value = aClass.__instanceDict[name]

      if value ~= nil then
        return value
      else
        return f[name]
      end
    end
  end
end

function _propagateInstanceMethod(aClass, name, f)
  f = name == '__index' and _createIndexWrapper(aClass, f) or f
  aClass.__instanceDict[name] = f

  for subclass in pairs(aClass.subclasses) do
    if rawget(subclass.__declaredMethods, name) == nil then
      _propagateInstanceMethod(subclass, name, f)
    end
  end
end

function _declareInstanceMethod(aClass, name, f)
  aClass.__declaredMethods[name] = f

  if f == nil and aClass.super then
    f = aClass.super.__instanceDict[name]
  end

  _propagateInstanceMethod(aClass, name, f)
end

function _tostring(self) return "class " .. self.name end

function _call(self, ...) return self:new(...) end

function _createClass(name, super)
  local dict = {}
  dict.__index = dict

  local aClass = {
    name = name,
    super = super,
    static = {},
    subclasses = setmetatable({}, weakKeys),
    __instanceDict = dict,
    __declaredMethods = {},
  }

  dict.__class = aClass

  if super then
    setmetatable(aClass.static, {
      __index = function(_, k)
        local result = rawget(dict, k)
        if result == nil then
          return super.static[k]
        end
        return result
      end
    })
  else
    setmetatable(aClass.static, { __index = function(_, k) return rawget(dict, k) end })
  end

  setmetatable(aClass, {
    __index = aClass.static,
    __tostring = _tostring,
    __call = _call,
    __newindex = _declareInstanceMethod
  })

  return aClass
end

function _includeMixin(aClass, mixin)
  assert(_is(mixin, 'table'), "mixin must be a table")

  for name, method in pairs(mixin) do
    if name ~= "included" and name ~= "static" then aClass[name] = method end
  end

  for name, method in pairs(mixin.static or {}) do
    aClass.static[name] = method
  end

  if _is(mixin.included, 'function') then mixin:included(aClass) end
  return aClass
end

DefaultMixin = {
  __tostring = function(self) return "instance of " .. tostring(self:class()) end,

  initialize = function(self, ...) end,

  class = function(self)
    local instanceMt = getmetatable(self)
    return instanceMt and instanceMt.__class
  end,

  isInstanceOf = function(self, aClass)
    return _is(aClass, 'table')
        and _is(self, 'table')
        and (self:class() == aClass
          or _is(self:class(), 'table')
          and _is(self:class().isSubclassOf, 'function')
          and self:class():isSubclassOf(aClass))
  end,

  static = {
    allocate = function(self)
      assert(_is(self, 'table'), "Make sure that you are using 'Class:allocate' instead of 'Class.allocate'")
      return setmetatable({}, self.__instanceDict)
    end,

    new = function(self, ...)
      assert(_is(self, 'table'), "Make sure that you are using 'Class:new' instead of 'Class.new'")
      local instance = self:allocate()
      instance:initialize(...)
      return instance
    end,

    subclass = function(self, name, ...)
      assert(_is(self, 'table'), "Make sure that you are using 'Class:subclass' instead of 'Class.subclass'")
      assert(_is(name, 'string'), "You must provide a name(string) for your class")

      local subclass = _createClass(name, self)

      for methodName, f in pairs(self.__instanceDict) do
        if not (methodName == '__index' and _is(f, 'table')) then
          _propagateInstanceMethod(subclass, methodName, f)
        end
      end
      subclass.initialize = function(instance, ...) return self.initialize(instance, ...) end

      self.subclasses[subclass] = true
      self:subclassed(subclass, ...)

      return subclass
    end,

    subclassed = function(self, other, ...) end,

    isSubclassOf = function(self, other)
      return _is(other, 'table') and
          _is(self.super, 'table') and
          (self.super == other or self.super:isSubclassOf(other))
    end,

    include = function(self, ...)
      assert(_is(self, 'table'), "Make sure you that you are using 'Class:include' instead of 'Class.include'")
      local argc = select('#', ...)
      for i = 1, argc do _includeMixin(self, select(i, ...)) end
      return self
    end
  }
}

function middleclass.class(name, super, ...)
  assert(_is(name, 'string'), "A name (string) is needed for the new class")
  return super and super:subclass(name, ...) or _includeMixin(_createClass(name), DefaultMixin)
end

setmetatable(middleclass, { __call = function(_, ...) return middleclass.class(...) end })

return middleclass
