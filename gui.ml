(* Based on example pousse.ml from lablgtk2 *)

open StdLabels
open GMain
open Gtk
open State

let initstate = match State.init_from_file "map.txt" with
  | Some state -> state
  | None -> State.init_state 25

let _ = GtkMain.Main.init ()

type building = [`none|`house|`house2]

let b1 = ref true
let b2 = ref false
let bulldozePressed = ref false

module type GridSpec = sig
  type t
  val size : int
  val get : t -> x:int -> y:int -> building
  val set : t -> x:int -> y:int -> building:building -> unit
end


module Grid (Spec : GridSpec) = struct
  open Spec
  let size = size

  (* [on_grid x y] is [true] if (x,y) is on the game grid, [false otherwise] *)
  let on_grid x y =
    x >= 0 && x < size && y >= 0 && y < size

  (* [action board x y building] returns [false] if there is already a building
   * at (x,y), or sets (x,y) to have the pixmap associated with [building] and
   * returns [true] *)
  let action board ~x ~y ~building =
    if get board ~x ~y <> `none  && !bulldozePressed = false then false else begin
      if !b1 && !b2 = false && !bulldozePressed = false then
        set board ~x ~y ~building:(`house :> building)
      else if !b2 then
        set board ~x ~y ~building:(`house2 :> building)
      else
        set board ~x ~y ~building:(`none :> building); true
    end
end


(* Makes new window with title "Not Sim City" *)
let window = GWindow.window ~title:"Not Sim City" ()


(* Create pixmaps of buildings *)
let pixnone =
  (* empty *)
  GDraw.pixmap ~window ~width:20 ~height:20 ~mask:true ()
let pixhouse =
  GDraw.pixmap_from_xpm ~file:"smslice.xpm" ()
let pixhouse2 =
  GDraw.pixmap_from_xpm ~file:"house2.xpm" ()

<<<<<<< HEAD
=======
let pixwater =
  GDraw.pixmap_from_xpm ~file:"water.xpm" ()
let pixclear =
  GDraw.pixmap_from_xpm ~file:"clear.xpm" ()
let pixforest =
  GDraw.pixmap_from_xpm ~file:"forest.xpm" ()


>>>>>>> e4bcd1294daf684b494ce505ee2594f549ed7c59
(* Create a new hbox with an image packed into it
 * and pack the box *)
let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in

  (* make pixmap from file, put pixmap in box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ();
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()

  (* cell: a button with a pixmap on it *)
<<<<<<< HEAD
class cell ?packing ?show () =

  let button = GButton.button ?packing ?show ~relief:`NONE () in
=======
class cell ~build ~terrain ?packing ?show () =
  let button = GButton.button ?packing ?show ~relief:`NONE () in
  (* let bldimg =
    match build with
    | `none -> begin match terrain with
      | Clear -> "clear.xpm"
      | Forest -> "forest.xpm"
      | Water -> "water.xpm" end
        | `house -> "house.xpm"
        | `house2 -> "house1.xpm" in
  let _ = xpm_label_box ~file:bldimg ~text:"" ~packing:button#add () in *)

  let c = 3 in
  (* let bldng =
    match build with
    | `none -> `none
    | `house -> `house
     | `house2 -> `house2  in *)
     let bldimg = match build with
     | Empty -> begin match terrain with
       | Clear -> pixclear
       | Forest -> pixforest
       | Water -> pixwater end
     | Dorm -> pixhouse
     | _ -> pixhouse2 in
>>>>>>> e4bcd1294daf684b494ce505ee2594f549ed7c59

  object (self)
    inherit GObj.widget button#as_widget
    method connect = button#connect
    val mutable building : building = `none
    val pm = GMisc.pixmap bldimg ~packing:button#add ()
    method mbuilding = building
    method set_bld bld =
      if bld <> building then begin
        building <- bld;
        pm#set_pixmap
          (match bld with
           | `none -> pixclear
           | `house -> pixhouse
           | `house2 -> pixhouse2)
      end
  end


module GameGrid = Grid (
  struct
    type t = cell array array
    let size = 25
    let get (grid : t) ~x ~y = grid.(x).(y)#mbuilding
    let set (grid : t) ~x ~y ~building = grid.(x).(y)#set_bld building
  end
  )


(* Conducting a game *)
open GameGrid

class game ~(frame : #GContainer.container) ~(label : #GMisc.label)
    ~(statusbar : #GMisc.statusbar) =

  let table = GPack.table ~columns:size ~rows:size ~packing:(frame#add) () in

  object (self)
    val mutable cells =
      Array.init size
        ~f:(fun i -> Array.init size
               ~f:(fun j ->
                   let t = (Array.get (Array.get initstate.grid i) j).terrain in
                   let b = (Array.get (Array.get initstate.grid i) j).btype in
                    new cell ~build:b ~terrain:t ~packing:(table#attach ~top:i ~left:j) ()))

    (* for the text displayed in bottom right *)
    val label = label

    (* message that is usually displayed in statusbar *)
    val turn = statusbar#new_context ~name:"turn"

    (* message that will flash in statusbar *)
    val messages = statusbar#new_context ~name:"messages"

    method grid = cells
    method table = table
    val mutable current_building = `house
    val mutable turnnum = 1
    val mutable state = initstate

    (* end of game *)
    method finish () =
      turn#pop ();
      let w, b = 0, 400 in
      turn#push
        (if w <= 0 then "Population 0" else
         if b < 0 then "Out of Funds" else
           "You Lost."); ()

    method update_label () =
      let w, b = 2, 2 in
      label#set_text (Printf.sprintf "Population: %d \nFunds: $%d " w b)

    method update_turn () =
      turnnum <- turnnum +  1

    method updatestate x y : bool =
        try (state <- match current_building with
        | `none -> turn#pop ();
          turn#push "Current Date: Dec 2017";
          self#update_turn (); State.do' (Delete (x,y)) state
        | `house -> turn#pop ();
          turn#push "Current Date: May 2020";
          self#update_turn (); State.do' (Build (x,y,Dorm)) state
        | `house2 -> turn#pop ();
          turn#push "Current Date: May 1860";
          self#update_turn (); State.do' (Build (x,y,Lecture)) state); true
        with
        | _ -> false

    method play x y =
      (* if action cells ~x ~y ~building:current_building then *)
      if self#updatestate x y then
        cells <- Array.init size
          ~f:(fun i -> Array.init size
                 ~f:(fun j ->
                     let t = (Array.get (Array.get state.grid i) j).terrain in
                     let b = (Array.get (Array.get state.grid i) j).btype in
                     new cell ~build:b ~terrain:t ~packing:(table#attach ~top:i ~left:j) ()))
      (* else
        messages#flash "You cannot build there" ; *)

    initializer
      for i = 0 to size-1 do
        for j = 0 to size-1 do
          let cell = cells.(i).(j) in
          cell#connect#enter ~callback:cell#misc#grab_focus; (* when hovering *)
          cell#connect#clicked ~callback:(fun () -> self#play i j) (* when clicked, execute [play i j]*)
        done done;

      self#update_label ();
      turn#push "Current Date: April 1865";
      ()
  end


(* Graphical stuff *)

let vbox = GPack.vbox ~packing:window#add ()

(* top menu bar *)
let ui_info = "<ui>\
               <menubar name='MenuBar'>\
               <menu action='FileMenu'>\
               <menuitem action='New'/>\
               <menuitem action='Open'/>\
               <menuitem action='Save'/>\
               <menuitem action='SaveAs'/>\
               <separator/>\
               <menuitem action='Quit'/>\
               </menu>\
               <menu action='PreferencesMenu'>\
               <menu action='DisasterMenu'>\
               <menuitem action='Fire'/>\
               <menuitem action='Blizzard'/>\
               <menuitem action='Prelim'/>\
               </menu>\
               <menuitem action='Pause'/>\
               </menu>\
               <menu action='HelpMenu'>\
               <menuitem action='About'/>\
               </menu>\
               </menubar>\
               </ui>"

(* [activ_action ac] is the result of clicking [ac]
 * Currently, it simply prints the action *)
let activ_action ac =
  Printf.printf "Action '%s' activated\n" ac#name ;
  flush stdout;
  match ac#name with
  | "Quit" -> window#destroy ()
  | _ -> ()

let setup_ui window =
  let a = GAction.add_action in
  let ta = GAction.add_toggle_action in
  let radio = GAction.group_radio_actions in
  let ra = GAction.add_radio_action in

  let actions = GAction.action_group ~name:"Actions" () in
  GAction.add_actions actions
    [ a "FileMenu" ~label:"_File" ;
      a "PreferencesMenu" ~label:"_Preferences" ;
      a "DisasterMenu" ~label:"_Disaster" ;
      a "HelpMenu" ~label:"_Help" ;

      (* - first string is the action name, which is the same as in ui_info
       * - next is the GtkStock.id, which is prebuilt common menu/toolbar items
       *   and corresponding icons
       * - tooltip should display when hovering, but doesn't right now for
       *   unknown reasons
       * - callback is what happens when clicked
       * - accel is the keyboard shortcut *)
      a "New" ~stock:`NEW ~tooltip:"Create a new file"
        ~callback:activ_action ;
      a "Open" ~stock:`OPEN ~tooltip:"Open a file"
        ~callback:activ_action ;
      a "Save" ~stock:`SAVE ~tooltip:"Save current file"
        ~callback:activ_action ;
      a "SaveAs" ~stock:`SAVE_AS
        ~tooltip:"Save to a file" ~callback:activ_action ;
      a "Quit" ~stock:`QUIT ~tooltip:"Quit"
        ~callback:activ_action;
      a "About" ~label:"_About" ~accel:"<control>A" ~tooltip:"About"
        ~callback:activ_action ;

      ta "Pause" ~label:"_Pause"
        ~accel:"<control>P"
        ~callback:activ_action ~active:true ;

      radio ~init_value:0 ~callback:(fun n -> Printf.printf "radio action %d\n%!" n)
        [ ra "Fire" 0 ~label:"_Fire"
            ~accel:"<control>F" ;
          ra "Blizzard" 1 ~label:"_Blizzard"
            ~accel:"<control>B" ;
          ra "Prelim" 2 ~label:"_Prelim"
            ~accel:"<control>R" ;
        ] ;
    ] ;

  (* UI manager constructs the user interface from the ui_info and actions *)
  let ui_m = GAction.ui_manager () in
  ui_m#insert_action_group actions 0 ;
  window#add_accel_group ui_m#get_accel_group ;
  ui_m#add_ui_from_string ui_info ;

  (* box1 contains the top menu bar thing, and is contained in vbox *)
  let box1 = GPack.vbox ~packing:vbox#add () in
  box1#pack (ui_m#get_widget "/MenuBar") ;

  (* h_box1 will contain the house buttons, is in box1 *)
  let h_box1 = GPack.hbox ~packing:box1#add () in

  (* create button and put it in h_box1
   * - [~relief: `NONE] removes shadows around edge of button  *)
  let dorm_button = GButton.button ~packing:h_box1#add () in
  (* connects the click to callback *)
  dorm_button#connect#clicked ~callback:
    (fun () -> b1 := true; b2 := false; bulldozePressed := false;
      print_endline "house button was pressed") ;
  (* create box with xpm image and put into button *)
  xpm_label_box ~file:"smslice.xpm" ~text:"Dorm" ~packing:dorm_button#add ();

  let button2 = GButton.button ~packing:h_box1#add () in
  button2#connect#clicked ~callback:
    (fun () -> b1 := false; b2 := true; bulldozePressed := false;
      print_endline "house2 button was pressed");
  xpm_label_box ~file:"house2.xpm" ~text:"Dining Hall" ~packing:button2#add ();

  let bulldoze = GButton.button ~packing:h_box1#add () in
  bulldoze#connect#clicked ~callback:
    (fun () -> b1 := false; b2 := false; bulldozePressed := true;
      print_endline "bulldoze button was pressed");
  xpm_label_box ~file:"bulldozer.xpm" ~text:"Bulldozer" ~packing:bulldoze#add ();

  (* horizontal line *)
  GMisc.separator `HORIZONTAL ~packing:box1#pack () ;

  (* frame for game *)
  let frame = GBin.frame ~shadow_type:`IN ~packing:box1#add () in

  (* box at bottom *)
  let hbox = GPack.hbox ~packing:vbox#pack () in

  (* status bar, displaying turn and messages *)
  let bar = GMisc.statusbar ~packing:hbox#add () in
  let frame2 = GBin.frame ~shadow_type:`IN ~packing:hbox#pack () in
  (* label displaying population and money *)
  let label = GMisc.label ~justify:`LEFT ~xpad:5 ~xalign:0.0 ~packing:frame2#add () in

  new game ~frame ~label ~statusbar:bar ;

  (* 'close' button, not really necessary *)
  let b = GButton.button ~stock:`CLOSE ~packing:hbox#pack () in
  (* destroys window when close button is clicked *)
  b#connect#clicked window#destroy



(* Start *)
let _ =
  window#connect#destroy ~callback:Main.quit;
  setup_ui window ;
  window#show ();
  Main.main ()
