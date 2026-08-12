---@meta
---
--- Type annotations for middleclass.lua to improve editor (LuaLS) completions.
--- This file uses EmmyLua annotations (---@) and is intended to be placed
--- alongside `middleclass.lua`. It does not modify runtime behavior.
---
--- Usage: place this file in your project so LuaLS (sumneko/EmmyLua) can
--- pick up types when you `require 'middleclass'`.

---@class Class
---@field name string
---@field super Class|nil
---@field static table
---@field __instanceDict table
---@field __declaredMethods table
---@field subclasses table
local Class = {}

---@class Instance
---@field class Class  -- the class of the instance (available via __index)
local Instance = {}

--- Main module table returned by `require 'middleclass'`.
---@class middleclass
local middleclass = {}

--- Create a new class.
---
---@param name string The name for the class (required)
---@param super Class|nil Optional superclass to inherit from
---@return Class class The newly created class
function middleclass.class(name, super) end

--- Shortcut so `local A = require 'middleclass'('A')` works.
---@param ... any
---@return Class
function middleclass(...) end

-- Methods available on Class (static table)
--- Allocate an instance (does NOT call initialize).
---@return Instance
function Class:allocate() end

--- Create a new instance and call initialize on it.
---@param ... any
---@return Instance
function Class:new(...) end

--- Create a subclass of this class.
---@param name string
---@return Class
function Class:subclass(name) end

--- Hook called after subclassing; default implementation is a no-op.
---@param other Class
function Class:subclassed(other) end

--- Check whether this class is a subclass of `other`.
---@param other Class
---@return boolean
function Class:isSubclassOf(other) end

--- Include one or more mixins into this class.
---@param ... table
---@return Class
function Class:include(...) end

--- Instance methods
--- Override this to initialize instance state.
---@param self Instance
---@vararg any
function Instance:initialize(...) end

--- Check whether an object is an instance of a class (or its subclasses).
---@param aClass Class
---@return boolean
function Instance:isInstanceOf(aClass) end

return middleclass

--[[
Examples (for editor hints). Uncomment and run these in a Lua environment
that can `require 'middleclass'` if you want to try them out.

local class = require 'middleclass'

-- Define a simple class
---@type Class
local Person = class('Person')

function Person:initialize(name)
  self.name = name
end

function Person:greet()
  return 'Hello, ' .. (self.name or '<unknown>')
end

-- Create an instance
---@type Instance
local p = Person:new('Alice')
-- Editor should know p.class and Person methods
assert(p.class == Person)
print(p:greet()) -- Hello, Alice

-- Subclass example
---@type Class
local Employee = Person:subclass('Employee')
function Employee:initialize(name, id)
  Person.initialize(self, name) -- call super initialize
  self.id = id
end

local e = Employee:new('Bob', 123)
assert(e.class == Employee)
print(e:greet(), e.id)
]]
