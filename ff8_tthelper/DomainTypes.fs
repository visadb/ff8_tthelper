module DomainTypes

type Element = Earth | Fire | Holy | Ice | Thunder | Water | Wind | None
type Card = { powers: int list ; powerModifier: int ; element: Element}

