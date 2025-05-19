-- Based on https://github.com/kikito/middleclass by Enrique García Cota
-- This version includes user modifications, with following changes:
-- * Instance class via metatable lookup ('instance.class'), 'rawget(instance, "class")' returns nil.
--   This is achieved by setting instance.__metatable = class.__instanceDict,
--   and class.__instanceDict.class = class. Accessing instance.class triggers __index on __instanceDict,
--   finding the 'class' field. rawget(instance, "class") bypasses the metatable.
-- * Modified include method to process mixins in argument order.
--   The original version processed them in an arbitrary order (pairs).
--
local middleclass = {
  _VERSION     = 'middleclass v4.1.1 MODIFIED-1',
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
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

-- Micro-optimization: Localize frequently used global functions
local type = type
local assert = assert
local rawget = rawget
local pairs = pairs
local select = select
local setmetatable = setmetatable
local getmetatable = getmetatable -- Added getmetatable localization

---@class instance # Default instance type definition, to be applied to a class
---@field class class # Instance's class (accessed via metatable lookup)
---@field initialize fun(self, ...) # Instance initializer method
---@field isInstanceOf fun(self, class: class|table): b: boolean # Checks if the instance is of a class

---Creates a wrapper function or table for the instance's __index metavalue.
---
---This handles method lookup, first checking the instance dictionary,
---then falling back to a function or a table provided (if any).
---@param aClass class The class table.
---@param f function|table? The function or table to fall back to for lookup.
---@return function|table __index The created index wrapper.
local function _createIndexWrapper(aClass, f)
  -- If no fallback function/table is provided, the instance dictionary itself
  -- acts as the __index, providing direct lookup of methods/properties
  -- defined or propagated to this class.
  if f == nil then
    return aClass.__instanceDict
  end

  -- If a fallback function is provided, the wrapper function will first check
  -- the instance dictionary, and if the name is not found, call the fallback function.
  if type(f) == "function" then
    return function(self, name)
      -- First, check the instance dictionary for the method/property
      local value = rawget(aClass.__instanceDict, name)
      if value ~= nil then
        return value
      end
      -- If not found in instance dictionary, call the provided fallback function
      return (f(self, name))
    end
  end

  -- If a fallback table is provided, the wrapper function will first check
  -- the instance dictionary, and if the name is not found, look up in the provided fallback table.
  -- (Implicit type(f) == "table")
  return function(self, name)
    -- First, check the instance dictionary for the method/property
    local value = rawget(aClass.__instanceDict, name)
    if value ~= nil then
      return value
    end
    -- If not found in instance dictionary, look up in the provided fallback table
    return rawget(f, name) -- Use rawget here to avoid triggering metatables on the fallback table
  end
end

---Propagates an instance method down the class hierarchy.
---
---This ensures subclasses inherit the method unless they have declared their own version.
---Updates the __instanceDict of the class and its subclasses recursively.
---Handles the special case of the __index metavalue by creating the appropriate wrapper.
---@param aClass class The class table to propagate the method from.
---@param name string The name of the method.
---@param f function|any The method function or value.
local function _propagateInstanceMethod(aClass, name, f)
  -- Special handling for __index: create the wrapper function/table based on the provided 'f'
  -- This 'f' would typically be the superclass's __instanceDict or a custom __index function/table.
  local propagated_f = (name == "__index") and _createIndexWrapper(aClass, f) or f

  -- Set the method in the instance dictionary for this class
  aClass.__instanceDict[name] = propagated_f

  -- Propagate to subclasses
  for subclass in pairs(aClass.subclasses) do
    -- Only propagate if the subclass hasn't already declared its own version
    -- Checking against __declaredMethods ensures we don't overwrite methods
    -- explicitly defined by the subclass itself.
    if rawget(subclass.__declaredMethods, name) == nil then
      _propagateInstanceMethod(subclass, name, propagated_f)
    end
  end
end

---Declares an instance method for a class.
---
---This marks the method as belonging to this class specifically (not inherited).
---Also triggers propagation of the method down the hierarchy.
---If f is nil, it effectively removes the method or reverts to the superclass method.
---@param aClass class The class table.
---@param name string The name of the method.
---@param f function|any|nil The method function or value.
local function _declareInstanceMethod(aClass, name, f)
  -- Record that this method was declared directly by this class.
  -- This is used later during propagation to avoid overwriting subclass methods.
  aClass.__declaredMethods[name] = f

  -- Determine the function/value to propagate.
  -- If 'f' is nil, we need to find the method from the superclass if one exists.
  local func_to_propagate = f
  if func_to_propagate == nil and rawget(aClass, "super") then
    -- Get the method from the superclass's instance dictionary.
    -- This dictionary already includes methods propagated from higher up the hierarchy.
    func_to_propagate = rawget(rawget(aClass, "super").__instanceDict, name)
  end

  -- Propagate the determined function/value (either the new one or the inherited one if f was nil)
  _propagateInstanceMethod(aClass, name, func_to_propagate)
end

---Metavalue for the class table: handles tostring conversion.
---
---@param self class The class table.
---@return string str A string representation of the class.
local function _tostring(self) return "class " .. tostring(rawget(self, "name")) end -- Use rawget for safety

---Metavalue for the class table: allows calling the class directly to create new instances.
--
---@param self class The class table.
---@param ... any Arguments passed to the new method.
---@return instance obj A new instance of the class.
local function _call(self, ...) return self:new(...) end

---@class class # Type definition for a class created by middleclass.
---@field name string # The name of the class.
---@field super class? # The superclass table, if any.
---@field static table # Table namespace for static methods and properties.
---@field __instanceDict table # The metatable dictionary assigned to instances. Contains instance methods and properties.
---@field __declaredMethods table<any, any> # Tracks methods declared directly by this class.
---@field subclasses table<class, true?> # Weak table to keep track of subclasses.
---@field allocate fun(self: class): instance # Allocates a new instance table.
---@field new fun(self: class, ...: unknown): instance # Creates and initializes a new instance.
---@field subclass fun(self: class, name: string): class # Creates a new subclass.
---@field subclassed fun(self: class, other: class) # Hook called when a subclass is created.
---@field isSubclassOf fun(self: class, other: class|table): boolean # Checks if this class is a subclass of another.
---@field include fun(self: class, ...: mixin|table): class # Includes mixins into the class.
---@overload fun(...: unknown): instance # Allows calling the class table directly to create instances (Class(...)).

---Internal function to create a new class table structure.
---
---Sets up metatables and core properties like name, super, static, instanceDict, etc.
---@param name string The name of the class.
---@param super class? The superclass table, if any.
---@return class aClass The newly created class table.
local function _createClass(name, super)
  -- The dictionary holding instance methods and properties.
  -- This table will serve as the __index metatable for instances.
  local instanceDict = {}

  local aClass = {
    name = name,
    super = super,
    static = {},                   -- Table for static methods and properties
    __instanceDict = instanceDict, -- The metatable for instances
    __declaredMethods = {},        -- Tracks methods declared directly by this class
    -- Weak table to keep track of subclasses without preventing garbage collection.
    subclasses = setmetatable({}, { __mode = 'k' })
  }

  -- Set the instance dictionary's __index to itself.
  -- This allows `instance.method` lookup to find methods within instanceDict.
  instanceDict.__index = instanceDict
  -- Set the 'class' field on the instance dictionary.
  -- This is accessed via metatable lookup from instances (`instance.class`).
  instanceDict.class = aClass

  -- Set up the metatable for the class table's static members.
  -- Accessing `Class.static_method` or `Class.static_property` will trigger this __index.
  if super then
    -- If there's a superclass, static members inherit from the super's static members.
    setmetatable(aClass.static, {
      __index = rawget(super, "static") -- Directly use the super's static table for lookup
    })
  else
    -- If no superclass, static members are only found directly on the 'static' table.
    -- The __index is nil, meaning lookups only happen on the 'static' table itself.
    -- setmetatable(aClass.static, { __index = nil }) -- This is the default, can be omitted
  end

  -- Set up the metatable for the class table itself.
  -- This handles how the class table behaves when accessed or called.
  setmetatable(aClass, {
    __index = rawget(aClass, "static"), -- Class methods/properties look in the static table
    __tostring = _tostring,             -- Allows tostring(class)
    __call = _call,                     -- Allows Class(...) for Class:new(...)
    -- __newindex is triggered when a new key is set on the class table (e.g., `MyClass.myMethod = function(...) end`).
    -- This is used to declare instance methods.
    __newindex = _declareInstanceMethod
  })

  return aClass
end

---Internal function to include a mixin into a class.
--
-- Copies instance and static methods/properties from the mixin to the class.
-- Calls the 'included' function on the mixin if it exists.
---@generic T: class
---@param aClass T The class table to include the mixin into.
---@param mixin mixin|table The mixin table.
---@return T aClass The modified class table.
local function _includeMixin(aClass, mixin)
  assert(type(mixin) == 'table', "Mixin must be a table")

  -- Copy instance methods/properties from mixin
  for name, method in pairs(mixin) do
    -- Avoid copying special keys 'included' and 'static' from the mixin's instance part
    if name ~= "included" and name ~= "static" then
      -- Use _declareInstanceMethod to correctly add the method to the class
      -- and propagate it down the hierarchy.
      _declareInstanceMethod(aClass, name, method)
    end
  end

  -- Copy static methods/properties from mixin.static if it exists
  local mixin_static = rawget(mixin, "static")
  if mixin_static and type(mixin_static) == "table" then
    local class_static = rawget(aClass, "static")
    for name, method in pairs(mixin_static) do
      -- Directly set static members on the class's static table.
      -- This will overwrite existing static members with the same name from the mixin.
      class_static[name] = method
    end
  end

  -- Call the 'included' hook on the mixin if it exists.
  -- This allows the mixin to perform setup or modifications on the class it's included into.
  local included_func = rawget(mixin, "included")
  if type(included_func) == "function" then
    -- Pass the mixin table as self, and the class being included into as the argument
    included_func(mixin, aClass)
  end
  return aClass
end

---The default mixin included in all base classes created with `middleclass.class`.
-- Provides fundamental instance and static methods.
---@class mixin # Type definition for a mixin table. Mixins provide methods/properties to be included in classes.
middleclass.DefaultMixin = {
  ---Metavalue for instances: handles tostring conversion.
  ---@param self instance The instance table.
  ---@return string A string representation of the instance.
  __tostring = function(self)
    -- Access the class via the metatable lookup (instance.class)
    local instance_metatable = getmetatable(self)
    local instance_class = instance_metatable and rawget(instance_metatable, "class")
    -- Provide a fallback string if class cannot be determined
    return "instance of " .. tostring(instance_class or "unknown class")
  end,

  ---Default initializer method for new instances.
  -- Can be overridden by subclasses. Called automatically by Class:new().
  ---@param self instance The instance table.
  ---@param ... any Arguments passed during instance creation.
  initialize = function(self, ...) end, -- Default implementation does nothing

  ---Checks if an instance is an instance of a given class or its subclasses.
  ---@param self instance The instance table.
  ---@param aClass class|table The class table to check against.
  ---@return boolean b True if the instance is of the given class or a subclass, false otherwise.
  isInstanceOf = function(self, aClass)
    -- Ensure 'aClass' is a table and the instance 'self' is a table
    if type(aClass) ~= 'table' or type(self) ~= 'table' then
      return false
    end

    -- Get the instance's metatable
    local instance_metatable = getmetatable(self)
    -- Get the instance's class via metatable lookup
    local instance_class = instance_metatable and rawget(instance_metatable, "class")

    -- Check if the instance's metatable has a 'class' field and it's a table (i.e., it's a middleclass instance)
    if type(instance_class) ~= 'table' then
      return false
    end

    -- Check if it's the same class or if the instance's class is a subclass of the given class
    -- Ensure isSubclassOf exists before calling
    return instance_class == aClass
        or (type(rawget(instance_class, "isSubclassOf")) == 'function' and instance_class:isSubclassOf(aClass))
  end,

  ---@class mixin.static # Table of methods/properties to be applied to the class itself (static members).
  static = {
    ---Allocates a new instance table without calling initialize.
    ---Useful for custom construction patterns or pooling.
    ---Sets the instance's metatable to the class's instance dictionary (__instanceDict).
    ---The instance dictionary handles the '__index' lookup for methods and the 'class' property.
    ---@param self class The class table.
    ---@return instance obj A new instance table with the correct metatable.
    allocate = function(self)
      assert(type(self) == 'table', "Make sure that you are using 'Class:allocate' with ':' instead of 'Class.allocate'")
      local instance = {} -- Create an empty table for the instance's own properties
      -- Set its metatable to the class's instance dictionary.
      -- This is how method calls and 'instance.class' are resolved.
      return setmetatable(instance, rawget(self, "__instanceDict"))
    end,

    ---Creates a new instance of the class and calls its initialize method.
    -- This is the standard way to create new objects.
    ---@param self class The class table.
    ---@param ... unknown Arguments passed to the initialize method.
    ---@return instance obj A new initialized instance.
    new = function(self, ...)
      assert(type(self) == 'table', "Make sure that you are using 'Class:new' with ':' instead of 'Class.new'")
      local instance = self:allocate() -- Allocate the instance table
      instance:initialize(...)         -- Call the instance's initialize method via metatable lookup
      return instance
    end,

    ---Creates a new subclass of the current class.
    -- Inherits methods and properties from the superclass.
    ---@param self class The superclass table.
    ---@param name string The name of the new subclass.
    ---@return class aSubClass The newly created subclass table.
    subclass = function(self, name)
      assert(type(self) == 'table', "Make sure that you are using 'Class:subclass' with ':' instead of 'Class.subclass'")
      assert(type(name) == "string", "You must provide a name (string) for your class")

      -- Create the basic subclass structure
      local subclass = _createClass(name, self)

      -- Propagate all instance methods from the superclass to the subclass.
      -- This copies methods from the super's instance dictionary to the subclass's instance dictionary,
      -- respecting methods already declared by the subclass.
      local super_instanceDict = rawget(self, "__instanceDict")
      for methodName, f in pairs(super_instanceDict) do
        -- Avoid propagating the superclass's __index table if it was a table fallback.
        -- The subclass will get its own __index wrapper created by _createIndexWrapper in _createClass.
        -- Propagate the actual function/value 'f'.
        if not (methodName == "__index" and type(f) == "table") then
          _propagateInstanceMethod(subclass, methodName, f)
        end
      end
      -- Ensure the subclass's initialize method initially calls the superclass's initialize.
      -- This can be overridden by declaring an 'initialize' method in the subclass definition.
      local super_initialize = rawget(super_instanceDict, "initialize")
      if type(super_initialize) == "function" then
        _declareInstanceMethod(subclass, "initialize", super_initialize)
      end


      -- Record the new subclass in the superclass's subclasses list (weak table).
      rawget(self, "subclasses")[subclass] = true
      -- Call the subclassed hook on the superclass, if it exists.
      local subclassed_hook = rawget(self, "subclassed")
      if type(subclassed_hook) == "function" then
        subclassed_hook(self, subclass)  -- Call with superclass as self, pass subclass as argument
      end

      return subclass
    end,

    ---Hook method called on the superclass whenever a new subclass is created.
    -- Can be overridden by subclasses.
    ---@param self class The superclass table.
    ---@param other class The newly created subclass table.
    subclassed = function(self, other) end, -- Default implementation does nothing

    ---Checks if this class is a subclass of another given class.
    ---@param self class The class table.
    ---@param other class|table The class table to check against.
    ---@return boolean b True if this class is a subclass of the other class, false otherwise.
    isSubclassOf = function(self, other)
      -- Ensure 'other' is a table and this class has a superclass
      local super_class = rawget(self, "super")
      if type(other) ~= 'table' or type(super_class) ~= 'table' then
        return false
      end

      -- Check if the direct superclass is the 'other' class
      if super_class == other then
        return true
      end

      -- Recursively check if the superclass is a subclass of 'other'
      local super_isSubclassOf = rawget(super_class, "isSubclassOf")
      if type(super_isSubclassOf) == "function" then
        return super_isSubclassOf(super_class, other) -- Call with super_class as self
      end

      return false -- No superclass or superclass doesn't have isSubclassOf (shouldn't happen with middleclass)
    end,

    ---Includes one or more mixins into the class.
    -- Copies methods and properties from the mixins to the class.
    -- Mixins are processed in the order they are provided as arguments.
    ---@param self class The class table.
    ---@param ... mixin|table One or more mixin tables.
    ---@return class self The modified class table (self).
    include = function(self, ...)
      assert(type(self) == 'table',
        "Make sure you that you are using 'Class:include' with ':' instead of 'Class.include'")
      local argc = select("#", ...)
      for i = 1, argc do
        local mixin = select(i, ...)
        -- Only include if the mixin is a table (and not nil)
        if type(mixin) == 'table' then
          _includeMixin(self, mixin) -- Include each provided mixin in order
        end
      end
      return self -- Return the class to allow chaining calls
    end
  }
}

---Holds defined classes to be looked up later.
---This is used by middleclass.isClass.
---@type table<class, true?>
middleclass.__classes = setmetatable({}, { __mode = "k" }) -- Use weak keys

---Checks if a given table is a middleclass class table.
---@param t table|any The value to check.
---@return boolean b True if the value is a middleclass class table, false otherwise.
function middleclass.isClass(t)
  -- A class must be a table and registered in our __classes table.
  return type(t) == "table" and rawget(middleclass.__classes, t) ~= nil
end

---Creates a new class.
---
---If a superclass is provided, the new class will inherit from it.
---Otherwise, it will be a base class including the DefaultMixin.
---@param name string The name of the class.
---@param super class? The optional superclass.
---@return class aClass The newly created class table.
function middleclass.class(name, super)
  assert(type(name) == 'string', "A name (string) is needed for the new class")
  assert(super == nil or middleclass.isClass(super), "Super must be a middleclass class or nil")

  local aClass
  if super then
    -- If a superclass is provided, create a subclass
    aClass = super:subclass(name)
  else
    -- If no superclass, create a base class and include the DefaultMixin
    aClass = _createClass(name, nil)
    _includeMixin(aClass, middleclass.DefaultMixin)
  end

  -- Register the newly created class in the internal list.
  rawget(middleclass, "__classes")[aClass] = true

  return aClass
end

-- Make the module callable like `local Class = require('middleclass')('MyClass')`
-- Calling the loaded module will delegate the call to the middleclass.class function.
return setmetatable(middleclass, {
  __call = function(_, ...)
    -- Delegate the call to the middleclass.class function
    return middleclass.class(...)
  end
})
