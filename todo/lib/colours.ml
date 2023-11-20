module Colourscheme = struct
  type colour =
    { r : int
    ; g : int
    ; b : int
    }
  [@@deriving yojson]

  type scheme =
    { todo : colour
    ; completed : colour
    ; selected : colour
    ; comp_selected : colour
    ; folder : colour
    ; prompt : colour
    ; border : colour
    ; text : colour
    }
  [@@deriving yojson]

  let json_of_scheme scheme = scheme_to_yojson scheme
  let scheme_of_json json = scheme_of_yojson json

  let default =
    { todo = { r = 0; g = 0; b = 255 }
    ; completed = { r = 0; g = 255; b = 0 }
    ; folder = { r = 255; g = 0; b = 0 }
    ; selected = { r = 255; g = 255; b = 0 }
    ; prompt = { r = 255; g = 255; b = 255 }
    ; border = { r = 255; g = 0; b = 0 }
    ; comp_selected = { r = 0; g = 255; b = 255 }
    ; text = { r = 255; g = 255; b = 255 }
    }
  ;;

  let store_scheme scheme =
    let json_file = Data.get_json_file_path "colourscheme.json" in
    let json = json_of_scheme scheme in
    let out_channel = open_out json_file in
    let _ = Yojson.Safe.pretty_to_channel out_channel json in
    close_out out_channel
  ;;

  let get_scheme () =
    let json_file = Data.get_json_file_path "colourscheme.json" in
    let in_channel = open_in json_file in
    try
      let json = Yojson.Safe.from_channel in_channel in
      let todos = scheme_of_yojson json in
      close_in in_channel;
      match todos with
      | Ok scheme -> scheme
      | Error _ ->
        store_scheme default;
        default
    with
    | Yojson.Json_error _ ->
      close_in in_channel;
      store_scheme default;
      default
    | _ ->
      close_in in_channel;
      store_scheme default;
      failwith "Unable to read colourscheme.json"
  ;;

  (* TODO: read from file *)
end
