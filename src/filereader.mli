
(**
[read dir] is the contents of the file with path [dir]
raises: InvalidDir if d is not a valid directory
[dir] is interpreted as a directory relative to the root directory of the project
*)
val read: string -> string

exception InvalidDir