BEGIN {
  global seen_states, max_x, max_y, max_seen_steps
}
NR >= 3 {
  [xx,yy] = $1 |> match(/-x([0-9]+)-y([0-9]+)$/)
  [x,y] = [xx,yy] |> map(int)

  if (x > max_x) { max_x = x }
  if (y > max_y) { max_y = y }

  ++n
  coor[n]["x"] = x
  coor[n]["y"] = y
  used[x][y] = int($3)
  avai[x][y] = int($4)
}
END {
  #part1(coor, n, used, avai)
  part2(used, avai)
}

function part1(coor,n,used,avai) {
  # Part 1: count "viable nodes"
  # any two nodes (A,B) regardless of whether they are directly connected, such that
  # 1. A.Used != 0 
  # 2. A != B
  # 3. A.Used <= B.Avail

  for (i = 1; i <= n; i++) {
    x1 = coor[i]["x"]
    y1 = coor[i]["y"]
    used1 = used[x1][y1]
    if (used1 == 0) { continue } # 1
    for (j = 1; j <= n; j++) {
      if (i == j) { continue } # 2
      x2 = coor[j]["x"]
      y2 = coor[j]["y"]
      avai2 = avai[x2][y2]
      if (used1 <= avai2) { count++ } # 3
    }
  }
  print "Part 1 answer: ", count
}

function is_viable(used,avai,from_x,from_y,to_x,to_y) {
  used_from = used[from_x][from_y]
  avai_to   = avai[to_x][to_y]
  #print([from_x,from_y],[to_x,to_y],used_from, avai_to, used_from != 0, [from_x,from_y] != [to_x,to_y], used_from <= avai_to)
  return used_from != 0
      && [from_x,from_y] != [to_x,to_y]
      && used_from <= avai_to
}

function part2(used,avai) {
  # Part 2: find shortest number of steps to get data from (0,maxY) to (0,0)
  # Step = move all data from one node to another (using the same condition as in part 1)
  # Plan: Make a graph of all possible moves, BFS

  go(used,avai)
}

function go(used,avai) {
  target_data_at = [max_x, 0]
  first_todos = find_valid_steps(used,avai,target_data_at,0)
  go_aux(first_todos)
}

function go_aux(todos) {
  left = length(todos)
  if (left == 0) {
    print("We should have found the min amount of steps before now")
    exit 1
  }
  for (i in todos) break
  todo = todos[i]
  delete todos[i]

  if (todo["steps"] > max_seen_steps) {
    max_seen_steps = todo["steps"]
    print("New max: " max_seen_steps ", TODOs left: " length(todos))
  }

  [used2,avai2] = move(todo)

  h = hash([used2,avai2,todo["old_target_data_at"]])

  if (h in seen_states) {
    printf("|")
    fflush("/dev/stdout")
    return go_aux(todos)
  }

  seen_states[h] = 1

  if (todo["new_target_data_at"] == [0,0]) {
    print("Part 2:", todo["steps"])
    exit 0
  }

  new_todos = find_valid_steps(used2,avai2,todo["new_target_data_at"],todo["steps"])

  printf("%d",length(new_todos))
  fflush("/dev/stdout")

  for (i in new_todos) {
    todos[] = new_todos[i]
  }
  return go_aux(todos)
}

function move(todo) {
  from_x = todo["from_x"]
  from_y = todo["from_y"]
  to_x   = todo["to_x"]
  to_y   = todo["to_y"]

  used_from = todo["used"][from_x][from_y]
  avai_from = todo["avai"][from_x][from_y]
  used_to = todo["used"][to_x][to_y]
  avai_to = todo["avai"][to_x][to_y]

  new_used = todo["used"]
  new_avai = todo["avai"]

  new_used[from_x][from_y] = 0
  new_avai[from_x][from_y] = avai_from + used_from
  new_used[to_x][to_y] = used_to + used_from
  new_avai[to_x][to_y] = avai_to - used_from

  return [new_used, new_avai]
}

function find_neighbours(x,y) {
  ns = []
  if (x > 0)     ns[] = [x-1,y]
  if (x < max_x) ns[] = [x+1,y]
  if (y > 0)     ns[] = [x,y-1]
  if (y < max_y) ns[] = [x,y+1]
  return ns
}

# goals:
#   move item (x1,y1) to (x2,y2)
#     - prereq: clearing enough of item (x2,y2)
#   make sure item (G.x-1, G.y) is free
#     - prereq: move 

# (.) _ G

# (.) G _
#

function find_valid_steps(used,avai,target_data_at,steps) {
  valid = []
  for (from_x in used) {
    for (from_y in used[from_x]) {
      neighbours = find_neighbours(from_x,from_y)
      for (i in neighbours) {
        [to_x,to_y] = neighbours[i]
        if (is_viable(used,avai,from_x,from_y,to_x,to_y)) {
          step = [
            "used" => used,
            "avai" => avai,
            "from_x" => from_x,
            "from_y" => from_y,
            "to_x" => to_x,
            "to_y" => to_y,
            "steps" => steps + 1,
            "old_target_data_at" => target_data_at,
            "new_target_data_at" =>
              [from_x, from_y] == target_data_at
                ? [to_x, to_y]
                : target_data_at,
          ]
          valid[] = step
        }
      }
    }
  }
  return valid
}

function transpose(a2d) {
  r = []
  for (x in a2d) {
    for (y in a2d[x]) {
      r[y][x] = a2d[x][y]
    }
  }
  return r
}

function pp(used,avai,target_data_at) {
  used = transpose(used)
  avai = transpose(avai)
  for (x in used) {
    s = "     "
    for (y in used[x]) {
      used_ = used[x][y]
      avai_ = avai[x][y]
      c = [y,x] == target_data_at ? "G" : # we've transposed
          used_ == 0 ? "_" : 
          "."
      s = s c
    }
    print(s)
  }
}

function todo_str(todo) {
  return "#" todo["steps"] ":" [todo["from_x"],todo["from_y"]]  "=>"  [todo["to_x"],todo["to_y"]]
}
