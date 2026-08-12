local _type, _assert, _setmetatable, _rawget, _pairs, _ipairs, _tostring_global = type, assert, setmetatable, rawget, pairs, ipairs, tostring

local middleclass = {
  _VERSION     = 'middleclass v4.1.1',
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

local function _createIndexWrapper(aClass, f)
  local instanceDict = aClass.__instanceDict

  if f == nil then
    return instanceDict
  elseif _type(f) == "function" then
    return function(self, name)
      local value = instanceDict[name]

      if value ~= nil then
        return value
      else
        return (f(self, name))
      end
    end
  else -- if  type(f) == "table" then
    return function(self, name)
      local value = instanceDict[name]

      if value ~= nil then
        return value
      else
        return f[name]
      end
    end
  end
end

local function _propagateInstanceMethod(aClass, name, f)
  f = name == "__index" and _createIndexWrapper(aClass, f) or f
  aClass.__instanceDict[name] = f

  for subclass in _pairs(aClass.subclasses) do
    if _rawget(subclass.__declaredMethods, name) == nil then
      _propagateInstanceMethod(subclass, name, f)
    end
  end
end

local function _declareInstanceMethod(aClass, name, f)
  aClass.__declaredMethods[name] = f

  if f == nil and aClass.super then
    f = aClass.super.__instanceDict[name]
  end

  _propagateInstanceMethod(aClass, name, f)
end

local function _tostring(self) return "class " .. self.name end
local function _call(self, ...) return self:new(...) end

local function _createClass(name, super)
  local dict = {}
  dict.__index = dict

  local aClass = { name = name, super = super, static = {},
                   __instanceDict = dict, __declaredMethods = {},
                   subclasses = _setmetatable({}, {__mode='k'})  }

  -- put the class reference on the instance dictionary so instances
  -- get `class` through __index without storing it per-instance
  dict.class = aClass

  if super then
    _setmetatable(aClass.static, {
      __index = function(_,k)
        local result = _rawget(dict,k)
        if result == nil then
          return super.static[k]
        end
        return result
      end
    })
  else
    _setmetatable(aClass.static, { __index = function(_,k) return _rawget(dict,k) end })
  end

  _setmetatable(aClass, { __index = aClass.static, __tostring = _tostring,
                         __call = _call, __newindex = _declareInstanceMethod })

  return aClass
end

local function _includeMixin(aClass, mixin)
  _assert(_type(mixin) == 'table', "mixin must be a table")

  for name,method in _pairs(mixin) do
    if name ~= "included" and name ~= "static" then aClass[name] = method end
  end

  for name,method in _pairs(mixin.static or {}) do
    aClass.static[name] = method
  end

  if _type(mixin.included)=="function" then mixin:included(aClass) end
  return aClass
end

local DefaultMixin = {
  __tostring   = function(self) return "instance of " .. _tostring_global(self.class) end,

  initialize   = function(self, ...) end,

  isInstanceOf = function(self, aClass)
    return _type(aClass) == 'table'
       and _type(self) == 'table'
       and (self.class == aClass
            or _type(self.class) == 'table'
            and _type(self.class.isSubclassOf) == 'function'
            and self.class:isSubclassOf(aClass))
  end,

  static = {
    allocate = function(self)
      _assert(_type(self) == 'table', "Make sure that you are using 'Class:allocate' instead of 'Class.allocate'")
      -- return an instance WITHOUT attaching `class` as a per-instance key;
      -- the `class` value is provided via __index on the class's __instanceDict
      return _setmetatable({}, self.__instanceDict)
    end,

    new = function(self, ...)
      _assert(_type(self) == 'table', "Make sure that you are using 'Class:new' instead of 'Class.new'")
      local instance = self:allocate()
      instance:initialize(...)
      return instance
    end,

    subclass = function(self, name)
      _assert(_type(self) == 'table', "Make sure that you are using 'Class:subclass' instead of 'Class.subclass'")
      _assert(_type(name) == "string", "You must provide a name(string) for your class")

      local subclass = _createClass(name, self)

      local srcInstanceDict = self.__instanceDict
      for methodName, f in _pairs(srcInstanceDict) do
        if not (methodName == "__index" and _type(f) == "table") then
          _propagateInstanceMethod(subclass, methodName, f)
        end
      end
      subclass.initialize = function(instance, ...) return self.initialize(instance, ...) end

      self.subclasses[subclass] = true
      self:subclassed(subclass)

      return subclass
    end,

    subclassed = function(self, other) end,

    isSubclassOf = function(self, other)
      return _type(other)      == 'table' and
             _type(self.super) == 'table' and
             ( self.super == other or self.super:isSubclassOf(other) )
    end,

    include = function(self, ...)
      _assert(_type(self) == 'table', "Make sure you that you are using 'Class:include' instead of 'Class.include'")
      for _,mixin in _ipairs({...}) do _includeMixin(self, mixin) end
      return self
    end
  }
}

function middleclass.class(name, super)
  _assert(_type(name) == 'string', "A name (string) is needed for the new class")
  return super and super:subclass(name) or _includeMixin(_createClass(name), DefaultMixin)
end

_setmetatable(middleclass, { __call = function(_, ...) return middleclass.class(...) end })

return middleclass
