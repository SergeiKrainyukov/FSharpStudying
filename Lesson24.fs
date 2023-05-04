type TimeOfDay = { hours: int; minutes: int; f: string }

let more = function
  | x, y when (x.f = "PM") && (y.f = "AM") -> true
  | x, y when (x.f = "AM") && (y.f = "PM") -> false
  | x, y when (x.f = y.f) && (x.hours>y.hours) -> true
  | x, y when (x.f = y.f) && (x.hours<y.hours) -> false
  | x, y when (x.f = y.f) && (x.hours=y.hours) && (x.minutes>y.minutes)-> true
  | x, y when (x.f = y.f) && (x.hours=y.hours) && (x.minutes<y.minutes)-> false
  |_ -> false

let (.>.) x y = more (x, y)