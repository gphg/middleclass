--- middleclass: Object Orientation for Lua
--
-- Based on https://github.com/kikito/middleclass by Enrique García Cota
-- This version includes user modifications.
-- Modified for memory-efficient access to instance class via metatable lookup ('instance.class').
-- 'rawget(instance, "class")' returns nil.
--
local middleclass = {
  _VERSION     = 'middleclass v4.1.1 MODIFIED', -- User-modified version string
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

--- Creates a wrapper function or table for the instance's __index metavalue.
-- This handles method lookup, first checking the instance dictionary,
-- then falling back to a function or a table provided.
-- @param aClass table The class table.
-- @param f function|table|nil The function or table to fall back to for lookup.
-- @return function|table The created index wrapper.
local function _createIndexWrapper(aClass, f)
  -- If no fallback, instance dictionary is the only place to look (beyond instance itself)
  if f == nil then
    return aClass.__instanceDict
  end

  -- If fallback is a function, return a function wrapper.
  if type(f) == "function" then
    return function(self, name)
      local value = rawget(aClass.__instanceDict, name) -- Use rawget to avoid infinite loops
      if value ~= nil then
        return value
      end
      -- Call the provided function for the lookup
      return (f(self, name))
    end
  end

  -- If fallback is a table, look up the method in that table (implicit type(f) == "table")
  return function(self, name)
    local value = rawget(aClass.__instanceDict, name) -- Use rawget
    if value ~= nil then
      return value
    end
    -- Look up in the provided table
    return rawget(f, name) -- Use rawget here too for safety
  end
end

--- Propagates an instance method down the class hierarchy.
-- This ensures subclasses inherit the method unless they have declared their own version.
-- Updates the __instanceDict of the class and its subclasses recursively.
-- Handles the special case of the __index metavalue.
-- @param aClass table The class table to propagate the method from.
-- @param name string The name of the method.
-- @param f function|any The method function or value.
local function _propagateInstanceMethod(aClass, name, f)
  -- Special handling for __index: create the wrapper function/table
  f = (name == "__index") and _createIndexWrapper(aClass, f) or f

  -- Set the method in the instance dictionary
  aClass.__instanceDict[name] = f

  -- Propagate to subclasses
  for subclass in pairs(aClass.subclasses) do
    -- Only propagate if the subclass hasn't already declared its own version
    if rawget(subclass.__declaredMethods, name) == nil then
      _propagateInstanceMethod(subclass, name, f)
    end
  end
end

--- Declares an instance method for a class.
-- This marks the method as belonging to this class specifically (not inherited).
-- Also triggers propagation of the method down the hierarchy.
-- If f is nil, it effectively removes the method or reverts to the superclass method.
-- @param aClass table The class table.
-- @param name string The name of the method.
-- @param f function|any|nil The method function or value.
local function _declareInstanceMethod(aClass, name, f)
  -- Record that this method was declared by this class
  aClass.__declaredMethods[name] = f

  -- If f is nil, the method might be inherited from the superclass
  if f == nil and aClass.super then
    f = rawget(aClass.super.__instanceDict, name) -- Use rawget to get the super method
  end

  -- Propagate the method (either the new one or the inherited one if f was nil)
  _propagateInstanceMethod(aClass, name, f)
end

--- Metavalue for the class table: handles tostring conversion.
-- @param self table The class table.
-- @return string A string representation of the class.
local function _tostring(self) return "class " .. tostring(self.name) end

--- Metavalue for the class table: allows calling the class directly to create new instances.
-- @param self table The class table.
-- @param ... any Arguments passed to the new method.
-- @return table A new instance of the class.
local function _call(self, ...) return self:new(...) end

--- Internal function to create a new class table structure.
-- Sets up metatables and core properties like name, super, static, instanceDict, etc.
-- @param name string The name of the class.
-- @param super table|nil The superclass table, if any.
-- @return table The newly created class table.
local function _createClass(name, super)
  -- The dictionary holding instance methods and properties
  local instanceDict = {}

  local aClass = {
    name = name,                     -- The name of the class (string)
    super = super,                   -- The superclass table (or nil)
    static = {},                     -- Table for static methods and properties
    __instanceDict = instanceDict,   -- Dictionary for instance methods/properties
    __declaredMethods = {},          -- Tracks methods declared directly by this class
    -- Weak table to keep track of subclasses without preventing garbage collection
    subclasses = setmetatable({}, {__mode='k'})
  }

  -- Set the instance dictionary's __index to itself for method lookup
  instanceDict.__index = instanceDict
  -- Set the 'class' field on the instance dictionary
  -- This is accessed via metatable lookup from instances
  instanceDict.class = aClass -- Moved this line after aClass is defined

  -- Set up the metatable for static members
  if super then
    -- If there's a superclass, static members inherit from the super's static members
    setmetatable(aClass.static, {
      __index = function(_, k)
        -- Check the class's instance dictionary first (original logic)
        local result = rawget(instanceDict, k)
        if result == nil then
          -- Then check super's static members
          return rawget(super.static, k)
        end
        return result
      end
    })
  else
    -- If no superclass, static members only look in the instanceDict (original logic)
    setmetatable(aClass.static, { __index = function(_, k) return rawget(instanceDict, k) end })
  end

  -- Set up the metatable for the class table itself
  return setmetatable(aClass, {
    __index = aClass.static,           -- Class methods/properties look in the static table
    __tostring = _tostring,          -- Allows tostring(class)
    __call = _call,                  -- Allows Class(...) for Class:new(...)
    __newindex = _declareInstanceMethod -- Handles `Class.method = function(...) ... end` syntax
  })
end

--- Internal function to include a mixin into a class.
-- Copies instance and static methods/properties from the mixin to the class.
-- Calls the 'included' function on the mixin if it exists.
-- @param aClass table The class table to include the mixin into.
-- @param mixin table The mixin table.
-- @return table The modified class table.
local function _includeMixin(aClass, mixin)
  assert(type(mixin) == 'table', "Mixin must be a table")

  -- Copy instance methods/properties from mixin
  for name, method in pairs(mixin) do
    -- Avoid copying special keys 'included' and 'static'
    if name ~= "included" and name ~= "static" then
        -- Use _declareInstanceMethod to correctly add and propagate methods
        _declareInstanceMethod(aClass, name, method)
    end
  end

  -- Copy static methods/properties from mixin.static
  if mixin.static then
    for name, method in pairs(mixin.static) do
      -- Directly set static members
      aClass.static[name] = method
    end
  end

  -- Call the 'included' hook on the mixin if it exists
  if type(mixin.included)=="function" then
      -- Pass the class being included into to the mixin's included function
      mixin:included(aClass)
  end
  return aClass
end

--- The default mixin included in all base classes created with `middleclass.class`.
-- Provides fundamental instance and static methods.
middleclass.DefaultMixin = {
  --- Metavalue for instances: handles tostring conversion.
  -- @param self table The instance table.
  -- @return string A string representation of the instance.
  __tostring   = function(self) return "instance of " .. tostring(self.class) end,

  --- Default initializer method for new instances.
  -- Can be overridden by subclasses.
  -- @param self table The instance table.
  -- @param ... any Arguments passed during instance creation.
  initialize   = function(self, ...) end,

  --- Checks if an instance is an instance of a given class or its subclasses.
  -- @param self table The instance table.
  -- @param aClass table The class table to check against.
  -- @return boolean True if the instance is of the given class or a subclass, false otherwise.
  isInstanceOf = function(self, aClass)
    -- Check if both self and aClass are tables and self has a 'class' field which is a table
    return type(aClass) == 'table'
       and type(self) == 'table'
       and type(self.class) == 'table'
       and (self.class == aClass -- Check if it's the same class
            -- Check if the instance's class is a subclass of the given class
            or (type(self.class.isSubclassOf) == 'function' and self.class:isSubclassOf(aClass)))
  end,

  static = {
    --- Allocates a new instance table without calling initialize.
    -- Useful for custom construction patterns.
    -- Sets the instance's metatable to the class's instance dictionary.
    -- The instance dictionary handles the '__index' lookup for methods and the 'class' property.
    -- @param self table The class table.
    -- @return table A new instance table with the correct metatable.
    allocate = function(self)
      assert(type(self) == 'table', "Make sure that you are using 'Class:allocate' with ':' instead of 'Class.allocate'")
      local instance = {} -- Create an empty table
      -- Set its metatable to the class's instance dictionary.
      -- The instance dictionary's __index and 'class' field handle lookups.
      return setmetatable(instance, self.__instanceDict)
    end,

    --- Creates a new instance of the class and calls its initialize method.
    -- This is the standard way to create new objects.
    -- @param self table The class table.
    -- @param ... any Arguments passed to the initialize method.
    -- @return table A new initialized instance.
    new = function(self, ...)
      assert(type(self) == 'table', "Make sure that you are using 'Class:new' with ':' instead of 'Class.new'")
      local instance = self:allocate() -- Allocate the instance
      instance:initialize(...)         -- Call the instance's initialize method
      return instance
    end,

    --- Creates a new subclass of the current class.
    -- Inherits methods and properties from the superclass.
    -- @param self table The superclass table.
    -- @param name string The name of the new subclass.
    -- @return table The newly created subclass table.
    subclass = function(self, name)
      assert(type(self) == 'table', "Make sure that you are using 'Class:subclass' with ':' instead of 'Class.subclass'")
      assert(type(name) == "string", "You must provide a name (string) for your class")

      -- Create the basic subclass structure
      local subclass = _createClass(name, self)

      -- Propagate all instance methods from the superclass to the subclass
      for methodName, f in pairs(self.__instanceDict) do
        -- Avoid propagating the superclass's __index table if it was a table fallback
        -- Instead, _createClass will set up the correct __index for the subclass
        if not (methodName == "__index" and type(f) == "table") then
          _propagateInstanceMethod(subclass, methodName, f)
        end
      end
      -- Ensure the subclass's initialize calls the superclass's initialize initially
      -- This can be overridden by declaring initialize in the subclass
      _declareInstanceMethod(subclass, "initialize", self.initialize)

      -- Record the new subclass in the superclass's subclasses list
      self.subclasses[subclass] = true
      -- Call the subclassed hook on the superclass
      self:subclassed(subclass)

      return subclass
    end,

    --- Hook method called on the superclass whenever a new subclass is created.
    -- Can be overridden by subclasses.
    -- @param self table The superclass table.
    -- @param other table The newly created subclass table.
    subclassed = function(self, other) end, -- Default implementation does nothing

    --- Checks if this class is a subclass of another given class.
    -- @param self table The class table.
    -- @param other table The class table to check against.
    -- @return boolean True if this class is a subclass of the other class, false otherwise.
    isSubclassOf = function(self, other)
      -- Ensure 'other' is a table and this class has a superclass
      return type(other)      == 'table' and
             type(self.super) == 'table' and
             ( self.super == other -- Check if the direct super is the 'other' class
               -- Recursively check if the superclass is a subclass of 'other'
            or (type(self.super.isSubclassOf) == 'function' and self.super:isSubclassOf(other)) )
    end,

    --- Includes one or more mixins into the class.
    -- Copies methods and properties from the mixins to the class.
    -- @param self table The class table.
    -- @param ... table One or more mixin tables.
    -- @return table The modified class table (self).
    include = function(self, ...)
      assert(type(self) == 'table', "Make sure you that you are using 'Class:include' with ':' instead of 'Class.include'")
      for _, mixin in ipairs({...}) do
          _includeMixin(self, mixin) -- Include each provided mixin
      end
      return self -- Return the class to allow chaining calls
    end
  }
}

--- Creates a new class.
-- If a superclass is provided, creates a subclass. Otherwise, creates a base class
-- and includes the DefaultMixin.
-- @param name string The name of the new class.
-- @param super table|nil The optional superclass.
-- @return table The newly created class table.
function middleclass.class(name, super)
  assert(type(name) == 'string', "A name (string) is needed for the new class")

  -- If a superclass is provided, create a subclass and return early.
  if super then
    return super:subclass(name)
  end

  -- If no superclass, create a base class and include the DefaultMixin.
  return _includeMixin(_createClass(name), middleclass.DefaultMixin)
end

-- Make the module callable like `local Class = require('middleclass')('MyClass')`
return setmetatable(middleclass, {
    __call = function(_, ...)
        -- Delegate the call to the middleclass.class function
        return middleclass.class(...)
    end
})
