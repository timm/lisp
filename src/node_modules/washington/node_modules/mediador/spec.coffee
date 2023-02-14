spec       = require "washington"
assert     = require "assert"
Mediador   = require "./mediador"



spec "Calls listeners subscribed to it. Chainable", ->
  # given
  venue         = {}
  venue.on      = Mediador::on
  venue.emit    = Mediador::emit

  # given
  spy          = (arg)->
    spy.called = true
    spy.arg    = arg

  # when
  result = venue.on "fire", spy

  # then chainable
  assert result is venue

  # when 
  result = venue.emit "fire", ["text"]

  # then emitted
  assert spy.called
  assert spy.arg == "text"



spec "Removes listeners", ->
  # given
  venue         = {}
  venue.on      = Mediador::on
  venue.off     = Mediador::off
  venue.emit    = Mediador::emit

  # given
  spy          = (arg)->
    spy.called = true
    spy.arg    = arg

  # when 
  venue.on "fire", spy
  result = venue.off "fire", spy

  # then chainable
  assert result is venue

  # when
  venue.emit "fire", ["text"]

  # then unhooked
  assert not spy.called



spec "Subscribes a listener set", ->
  # given
  set = 
    action: (arg)->
      set.action.called   = true
      set.action.arg      = arg
    reaction: (arg)->
      set.reaction.called = true
      set.reaction.arg    = arg

  # given
  venue         = {}
  venue.on      = Mediador::on
  venue.off     = Mediador::off
  venue.emit    = Mediador::emit

  # when
  venue.on set
  venue.emit 'action', ["act"]
  venue.emit 'reaction', ["react"]

  # then
  assert set.action.called
  assert set.action.arg == "act"
  assert set.reaction.called
  assert set.reaction.arg == "react"



spec "Unsubscribes a listener set", ->
  # given
  set = 
    action: ->
      set.action.called   = true
    reaction: ->
      set.reaction.called = true

  # given
  venue         = {}
  venue.on      = Mediador::on
  venue.off     = Mediador::off
  venue.emit    = Mediador::emit

  # when
  venue.on set
  venue.off set
  venue.emit 'action'
  venue.emit 'reaction'

  # then
  assert not set.action.called
  assert not set.reaction.called



spec "Called listeners receive the venue as the last argument", ->
  # given
  spy = ->
    spy.last = spy.last or []
    spy.last.push arguments[arguments.length - 1]

  # given
  venue         = {}
  venue.on      = Mediador::on
  venue.off     = Mediador::off
  venue.emit    = Mediador::emit

  # when
  venue.on 'event', spy
  venue.emit "event", ["lala"]
  venue.emit "event", []
  venue.emit "event", [2, 32, true]
  venue.emit "event"

  # then
  assert arg is venue for arg in spy.last



spec "Works even when no comprehensions are available", ->
  # given
  hijacked = {}
  hijacked.forEach = Array::forEach
  hijacked.filter  = Array::filter
  hijacked.map     = Array::map   
  Array::forEach   = null
  Array::filter    = null
  Array::map       = null

  # given
  venue           = {}
  venue.on        = Mediador::on
  venue.off       = Mediador::off
  venue.emit      = Mediador::emit

  # then
  listener = ->
  venue.on "event", listener
  venue.emit "event", ["argument"]
  venue.off "event", listener

  # then
  hash = event: ->
  venue.on hash
  venue.emit "event", ["argument"]
  venue.off hash

  # restore
  Array::forEach  = hijacked.forEach
  Array::filter   = hijacked.filter
  Array::map      = hijacked.map



spec "Allows setting 'this' with an argument", ->
  # given
  scope    = {}
  mediador = new Mediador
  mediador.on "event", ->
      assert @ is scope
      scope.callback = true 
    , scope

  # when
  mediador.emit "event"

  # then
  assert scope.callback



spec "Respects the original 'this' in listener sets", ->
  # given
  hash            = 
    event: ->
      hash.event.called = true
      assert @ is hash

  # given
  venue           = new Mediador
  venue.on hash

  # when
  venue.emit "event"

  # then
  assert hash.event.called



spec "Doesn't hangs if the listener does not exist"

spec "Supports using objets with 'name' property as events"

spec "Emits synchronously by default"

spec "Emits asynchronously if instructed to"


spec.go()
