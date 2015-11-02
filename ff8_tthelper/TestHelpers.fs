module TestHelpers

open DomainTypes

let card powers owner powerModifier element =
    Some { powers = List.toArray powers; powerModifier = powerModifier
           element = element; owner = owner}
let hc powers owner = card powers owner 0
let pce powers owner powerModifier elem = Full (card powers owner powerModifier elem).Value
let pc powers owner powerModifier = pce powers owner powerModifier (Some UnknownElement)
let emptySlot = PlayGridSlot.Empty None
let emptySlotElem elem = PlayGridSlot.Empty elem

let n = None
let e = Some Earth
let f = Some Fire
let h = Some Holy
let i = Some Ice
let p = Some Poison
let t = Some Thunder
let w = Some Wind
let a = Some Water
let u = Some UnknownElement