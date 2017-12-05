(* Based on example pousse.ml from lablgtk2 *)

open StdLabels
open GMain
open Gtk
open GToolbox
open State

let initstate = ref (State.init_state 25)

let _ = GtkMain.Main.init ()

type building = [`none|`dorm|`dining|`lecture|`power|`park|`road|`pline]

let dorm_pressed = ref true
let dining_pressed = ref false
let lecture_pressed = ref false
let power_pressed = ref false
let park_pressed = ref false
let road_pressed = ref false
let pline_pressed = ref false
let bulldoze_pressed = ref false

let welcome_mess = "Welcome to NOT SIM CITY, an open-ended University Simulator
based on real-life experience at Cornell University!"
let about_message = "About"

module type GridSpec = sig
  type t
  val get : t -> x:int -> y:int -> State.building_type
  val set : t -> x:int -> y:int -> building:State.building_type -> unit
end


module Grid (Spec : GridSpec) = struct
  open Spec

  (* [action board x y building] returns [false] if there is already a building
   * at (x,y), or sets (x,y) to have the pixmap associated with [building] and
   * returns [true] *)
  let action board ~x ~y ~building =
    if get board ~x ~y <> Empty  && !bulldoze_pressed = false then false
    else begin
      set board ~x ~y ~building ; true
    end
end


(* Makes new window with title "Not Sim City" *)
let window = GWindow.window ~title:"Not Sim City" ()


(* Create pixmaps of buildings *)
let pixnone =
  GDraw.pixmap ~window ~width:20 ~height:20 ~mask:true ()
let pixwater =
  GDraw.pixmap_from_xpm ~file:"water.xpm" ()
let pixclear =
  GDraw.pixmap_from_xpm ~file:"clear.xpm" ()
let pixforest =
  GDraw.pixmap_from_xpm ~file:"forest.xpm" ()

let pixdorm =
  GDraw.pixmap_from_xpm ~file:"dorm.xpm" ()
let pixdining =
  GDraw.pixmap_from_xpm ~file:"dining.xpm" ()
let pixlecture =
  GDraw.pixmap_from_xpm ~file: "lecture.xpm" ()
let pixpower =
  GDraw.pixmap_from_xpm ~file: "power.xpm" ()
let pixpark =
  GDraw.pixmap_from_xpm ~file: "park.xpm" ()
let pixroad =
  GDraw.pixmap_from_xpm ~file: "road.xpm" ()
let pixpline =
  GDraw.pixmap_from_xpm ~file: "power.xpm" ()

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
class cell ~build ~terrain ?packing ?show () =
  let button = GButton.button ?packing ?show ~relief:`NONE () in

   let bldimg = match build with
   | Empty -> begin match terrain with
     | Clear -> pixclear
     | Forest -> pixforest
     | Water -> pixwater end
   | Dorm -> pixdorm
   | Dining -> pixdining
   | Lecture -> pixlecture
   | Power -> pixpower
   | Park -> pixpark
   | Road -> pixroad
   | Pline -> pixpline
   | _ -> pixdining in

  object (self)
    inherit GObj.widget button#as_widget
    method connect = button#connect
    val mutable building : State.building_type = Empty
    val pm = GMisc.pixmap bldimg ~packing:button#add ()
    method building = building
    method set_bld bld =
      if bld <> building then begin
        building <- bld;
        pm#set_pixmap
          (match bld with
           | Dorm -> pixdorm
           | Dining -> pixdining
           | Lecture -> pixlecture
           | Power -> pixpower
           | Park -> pixpark
           | Road -> pixroad
           | Pline -> pixpline
           | Empty -> begin match terrain with
               | Clear -> pixclear
               | Forest -> pixforest
               | Water -> pixwater end
           | _ -> pixdining)
      end
  end


module GameGrid = Grid (
  struct
    type t = cell array array
    let get (grid : t) ~x ~y = grid.(x).(y)#building
    let set (grid : t) ~x ~y ~building = grid.(x).(y)#set_bld building
  end
  )


(* Conducting a game *)
open GameGrid

class game ~(frame : #GContainer.container) ~(label : #GMisc.label)
    ~(statusbar : #GMisc.statusbar) =

  let size = Array.length (!initstate.grid) in

  let table = GPack.table ~columns:size ~rows:size ~packing:(frame#add) () in

  object (self)
    val mutable cells =
      Array.init size
        ~f:(fun i -> Array.init size
               ~f:(fun j ->
                   let t = (Array.get (Array.get !initstate.grid i) j).terrain in
                   let b = (Array.get (Array.get !initstate.grid i) j).btype in
                    new cell ~build:b ~terrain:t ~packing:(table#attach ~top:i ~left:j) ()))

    (* for the text displayed in bottom right *)
    val label = label

    (* message that is usually displayed in statusbar *)
    val turn = statusbar#new_context ~name:"turn"

    (* message that will flash in statusbar *)
    val messages = statusbar#new_context ~name:"messages"

    method grid = cells
    method table = table
    val mutable current_building = Dorm
    val mutable turnnum = 1
    val mutable state = !initstate

    (* end of game *)
    method finish () =
      turn#pop ();
      let w, b = 0, 400 in
      turn#push
        (if w <= 0 then "Population 0" else
         if b < 0 then "Out of Funds" else
           "You Lost."); ()

    method make_message =
      match state.message with
      | Some m -> GToolbox.message_box ~title:"Message" m
      | None -> ()

    method update_label () =
      let p, f = 2, state.money in
      label#set_text (Printf.sprintf "Population: %d \nFunds: $%d " p f)

    method update_turn () =
      turnnum <- turnnum +  1

    method update_build () =
      current_building <- if !dorm_pressed then Dorm
        else if !lecture_pressed then Lecture
        else if !dining_pressed then Dining
        else if !power_pressed then Power
        else if !park_pressed then Park
        else if !road_pressed then Road
        else if !pline_pressed then Pline
        else if !bulldoze_pressed then Empty
        else Empty

    method updatestate x y : bool =
        try (self#update_build (); state <- match current_building with
        | Empty -> turn#pop ();
          turn#push "Current Date: Dec 2017";
          self#update_turn (); State.do' (Delete (x,y)) state
        | Dorm -> turn#pop ();
          turn#push "Current Date: May 2020";
          self#update_turn (); State.do' (Build (x,y,Dorm)) state
        | Dining -> turn#pop ();
          turn#push "Current Date: May 1860";
          self#update_turn (); State.do' (Build (x,y,Dining)) state
        | Lecture -> turn#pop ();
          turn#push "Current Date: May 1870";
          self#update_turn (); State.do' (Build (x,y,Lecture)) state
        | Power -> turn#pop ();
          turn#push "Current Date: May 1880";
          self#update_turn (); State.do' (Build (x,y,Power)) state
        | Park -> turn#pop ();
          turn#push "Current Date: May 1890";
          self#update_turn (); State.do' (Build (x,y,Park)) state
        | Road -> turn#pop ();
          turn#push "Current Date: May 1900";
          self#update_turn (); State.do' (Build (x,y,Road)) state
        | Pline -> turn#pop ();
          turn#push "Current Date: May 1910";
          self#update_turn (); State.do' (Build (x,y,Pline)) state
        | _ -> state); true
        with
        | _ -> false

        method btostring btype =
          match btype with
              | Empty -> "empty"
              | Dorm -> "dorm"
              | Lecture -> "lecture"
              | Power -> "power"
              | Dining -> "dining"
              | Park -> "park"
              | Road -> "road"
              | Pline -> "pline"
              | Section (x,y) -> "section"

    method play x y =
      (* if action cells ~x ~y ~building:current_building then *)
      if self#updatestate x y then
        (self#update_label (); self#make_message;
        turn#pop (); turn#push ("Current Date: "^(string_of_int state.time_passed));
        for i = max (x-2) 0 to min (x+2) (size-1) do
          for j = max (y-2) 0 to min (y+2) (size-1) do
            (* for i = 0 to (size-1) do
               for j = 0 to (size-1) do *)
            let t = (Array.get (Array.get state.grid i) j).terrain in
            let b = (Array.get (Array.get state.grid i) j).btype in
            let bld = match b with
              | Section (x,y) -> (Array.get (Array.get state.grid x) y).btype
              | _ -> b in
            print_endline ((string_of_int i)^(self#btostring bld));
            action cells i j bld
          done done)
      (* else
        messages#flash "You cannot build there" ; *)

    initializer
      for i = 0 to size-1 do
        for j = 0 to size-1 do
          let cell = cells.(i).(j) in
          cell#connect#enter ~callback:cell#misc#grab_focus; (* when hovering *)
          cell#connect#clicked ~callback:(fun () -> self#play i j) (* when clicked, execute [play i j]*)
        done done;

      (*self#update_label ();
      turn#push "Current Date: April 1865";
        ()*)
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
  | "About" -> GToolbox.message_box ~title:"About" about_message
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

  let h_box2 = GPack.hbox ~packing:box1#add () in

  (* create button and put it in h_box1
   * - [~relief: `NONE] removes shadows around edge of button  *)
  let dorm_button = GButton.button ~packing:h_box1#add () in
  (* connects the click to callback *)
  dorm_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := true; dining_pressed := false;
      lecture_pressed := false; power_pressed := false;
      park_pressed := false; road_pressed := false;
      pline_pressed := false; bulldoze_pressed := false;
      print_endline "Dorm button was pressed") ;
  (* create box with xpm image and put into button *)
  xpm_label_box ~file:"dorm.xpm" ~text:"Dorm" ~packing:dorm_button#add ();

  let dining_button = GButton.button ~packing:h_box1#add () in
  dining_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := true;
      lecture_pressed := false; power_pressed := false;
      park_pressed := false; road_pressed := false;
      pline_pressed := false; bulldoze_pressed := false;
      print_endline "Dining button was pressed");
  xpm_label_box ~file:"dining.xpm" ~text:"Dining Hall" ~packing:dining_button#add ();

  let lecture_button = GButton.button ~packing:h_box1#add () in
  lecture_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := false;
      lecture_pressed := true; power_pressed := false;
      park_pressed := false; road_pressed := false;
      pline_pressed := false; bulldoze_pressed := false;
      print_endline "Lecture button was pressed");
  xpm_label_box ~file:"lecture.xpm" ~text:"Lecture Hall" ~packing:lecture_button#add ();

  let power_button = GButton.button ~packing:h_box1#add () in
  power_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := false;
      lecture_pressed := false; power_pressed := true;
      park_pressed := false; road_pressed := false;
      pline_pressed := false; bulldoze_pressed := false;
      print_endline "Power button was pressed");
  xpm_label_box ~file:"power.xpm" ~text:"Power Source" ~packing:power_button#add ();

  let park_button = GButton.button ~packing:h_box2#add () in
  park_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := false;
      lecture_pressed := false; power_pressed := false;
      park_pressed := true; road_pressed := false;
      pline_pressed := false; bulldoze_pressed := false;
      print_endline "Park button was pressed");
  xpm_label_box ~file:"park.xpm" ~text:"Park" ~packing:park_button#add ();

  let road_button = GButton.button ~packing:h_box2#add () in
  road_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := false;
      lecture_pressed := false; power_pressed := false;
      park_pressed := false; road_pressed := true;
      pline_pressed := false; bulldoze_pressed := false;
      print_endline "Road button was pressed");
  xpm_label_box ~file:"road.xpm" ~text:"Road" ~packing:road_button#add ();

  let pline_button = GButton.button ~packing:h_box2#add () in
  pline_button#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := false;
      lecture_pressed := false; power_pressed := false;
      park_pressed := false; road_pressed := false;
      pline_pressed := true; bulldoze_pressed := false;
      print_endline "Power line button was pressed");
  xpm_label_box ~file:"power.xpm" ~text:"Power Line" ~packing:pline_button#add ();

  let bulldoze = GButton.button ~packing:h_box2#add () in
  bulldoze#connect#clicked ~callback:
    (fun () -> dorm_pressed := false; dining_pressed := false;
      lecture_pressed := false; power_pressed := false;
      park_pressed := false; road_pressed := false;
      pline_pressed := false; bulldoze_pressed := true;
      print_endline "Bulldoze button was pressed");
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

  let filebutton = GButton.button () in
  filebutton#connect#clicked ~callback:
    (fun () -> (*GToolbox.select_file ~title:"Select"*) ());

  let buttonslist = ["About";"Map from file";"Map from size"] in
  let sizelist = ["20x20";"30x30";"40x40"] in

  let beginbox = GToolbox.question_box ~title:"Welcome!" ~buttons:buttonslist welcome_mess in

  let nextbutton = match beginbox with
    | 1 -> GToolbox.message_box ~title:"About" about_message
    | 2 -> (let t = GToolbox.select_file ~title:"File Name" () in
      match t with
      | Some m -> (initstate := match (State.init_from_file m) with
          | Some st -> st
          | None -> GToolbox.message_box ~title:"File Name"
            "Cannot load map from file - using default map"; !initstate)
      | None ->  GToolbox.message_box ~title:"About" "No file selected - using default map")
    | 3 -> (let numbox = GToolbox.question_box ~title:"Choose Size" ~buttons:sizelist "Choose your map size" in
            match numbox with
            | 1 -> initstate := State.init_state 20
            | 2 -> initstate := State.init_state 30
            | 3 -> initstate := State.init_state 40)
  in

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
