def  sortConsideringEqualValues [ T ] ( map : Map [ T , Int ]) :
List [( Set [ T ], Int )] = {
  map.groupBy (_._2) .map {
    case (count, s) => s.keys.toSet -> count
  } .toList.sortBy {
    case (_, count) => count}
}


val v = Map(
  "tomatoes" -> 17,
  "peppers" -> 234,
  "olives" -> 17,
  "cucumbers" -> 323)



