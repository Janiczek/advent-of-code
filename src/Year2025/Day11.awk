BEGIN { global conn = [] }
{
  from = $1 |> sub(/:/,"")
  conn[from] = []
  for (i=2;i<=NF;i++) conn[from][$i]
}
END {
  print("p1",p1("you","out",[]))
}
function p1(from,to,stop) {
  todos = [from]
  while (length(todos) > 0) {
    for (todo_i in todos) break
    todo = todos[todo_i]
    delete todos[todo_i]
    if (todo == to) { c++; continue }
    if (todo in stop) continue
    if (!(todo in conn)) continue
    new_todos = keys(conn[todo])
    for (t in new_todos)
      todos[] = new_todos[t]
  }
  return c
}
