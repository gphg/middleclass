---@meta
---
--- Type annotations for middleclass.lua to improve editor (LuaLS) completions.
--- This file uses EmmyLua annotations (---@) and is intended to be placed
--- alongside `middleclass.lua`. It does not modify runtime behavior.
---
--- Added generic type parameters so editors can infer instance types.
---
--- Usage: place this file in your project so LuaLS (sumneko/EmmyLua) can
--- pick up types when you `require 'middleclass'`.

---@class Class<T>
---@field name string
---@field super Class<any>|nil
---@field static table
---@field __instanceDict table
---@field __declaredMethods table
---@field subclasses table
local Class = {}

---@class Instance<T>
---@field class Class<T>  -- the class of the instance (available via __index)
local Instance = {}

--- Main module table returned by `require 'middleclass'`.
---@class middleclass
local middleclass = {}

--- Create a new class.
---
---@generic T
---@param name string The name for the class (required)
---@param super Class<any>|nil Optional superclass to inherit from
---@return Class<T> class The newly created class
function middleclass.class(name, super) end

--- Shortcut so `local A = require 'middleclass'('A')` works.
---@generic T
---@param ... any
---@return Class<T>
function middleclass(...) end

-- Methods available on Class (static table)
--- Allocate an instance (does NOT call initialize).
---@generic T
---@return Instance<T>
function Class:allocate() end

--- Create a new instance and call initialize on it.
---@generic T
---@param ... any
---@return Instance<T>
function Class:new(...) end

--- Create a subclass of this class.
---@generic T
---@param name string
---@return Class<T>
function Class:subclass(name) end

--- Hook called after subclassing; default implementation is a no-op.
---@param other Class<any>
function Class:subclassed(other) end

--- Check whether this class is a subclass of `other`.
---@generic T
---@param other Class<any>
---@return boolean
function Class:isSubclassOf(other) end

--- Include one or more mixins into this class.
---@generic T
---@param ... table
---@return Class<T>
function Class:include(...) end

--- Instance methods
--- Override this to initialize instance state.
---@generic T
---@param self Instance<T>
---@vararg any
function Instance:initialize(...) end

--- Check whether an object is an instance of a class (or its subclasses).
---@generic T
---@param self Instance<T>
---@param aClass Class<any>
---@return boolean
function Instance:isInstanceOf(aClass) end

--[[
Examples (for editor hints). Uncomment and run these in a Lua environment
that can `require 'middleclass'` if you want to try them out.

local class = require 'middleclass'

-- Define a typed instance shape for editors
---@class PersonInstance
---@field name string
---@field greet fun(self:PersonInstance):string

---@type Class<PersonInstance>
local Person = class('Person')

function Person:initialize(name)
  self.name = name
end

function Person:greet()
  return 'Hello, ' .. (self.name or '<unknown>')
end

---@type PersonInstance
local p = Person:new('Alice')
assert(p.class == Person)
print(p:greet()) -- Hello, Alice

-- Subclass example with extended fields
---@class EmployeeInstance:PersonInstance
---@field id number

---@type Class<EmployeeInstance>
local Employee = Person:subclass('Employee')
function Employee:initialize(name, id)
  Person.initialize(self, name) -- call super initialize
  self.id = id
end

---@type EmployeeInstance
local e = Employee:new('Bob', 123)
assert(e.class == Employee)
print(e:greet(), e.id)
]]

return middleclass
