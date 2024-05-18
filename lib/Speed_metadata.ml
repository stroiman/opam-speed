type t = ..

module List = struct
  (* This function mostly exists for the purpose of the ppx rewriter, making
     it generate code that depends only on this library *)
  let find_map = Base.List.find_map
end
