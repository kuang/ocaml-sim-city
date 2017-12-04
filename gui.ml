(* Based on example pousse.ml from lablgtk2 *)

open StdLabels
open GMain
open Gtk

type building = [`none|`house|`house2]

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

  let action board ~x ~y ~building =
    if get board ~x ~y <> `none then false else begin
      set board ~x ~y ~building:(building :> building); true
    end
end

(* Makes new window with title "Not Sim City" *)
let window = GWindow.window ~title:"Not Sim City" ()

(* Create pixmaps of buildings *)
let pixnone =
  GDraw.pixmap ~window ~width:20 ~height:20 ~mask:true ()
let pixhouse =
  GDraw.pixmap_from_xpm ~file:"smslice.xpm" ()
let pixhouse2 =
  GDraw.pixmap_from_xpm ~file:"house2.xpm" ()

(* Create a new hbox with an image and a label packed into it
 * and pack the box *)
let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~border_width:2 ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ();

  (* Create a label for the button and pack into box *)
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()

(* cell: a button with a pixmap on it *)

class cell ?packing ?show () =
  let button = GButton.button ?packing ?show  ~relief:`NONE () in

  object (self)
    inherit GObj.widget button#as_widget
    method connect = button#connect
    val mutable building : building = `none
    val pm = GMisc.pixmap pixnone ~packing:button#add ()
    method building = building
    method set_bld bld =
      if bld <> building then begin
        building <- bld;
        pm#set_pixmap
          (match bld with
          | `none -> pixnone
          | `house -> pixhouse
          | `house2 -> pixhouse2)
      end
  end

module GameGrid = Grid (
  struct
    type t = cell array array
    let size = 20
    let get (grid : t) ~x ~y = grid.(x).(y)#building
    let set (grid : t) ~x ~y ~building = grid.(x).(y)#set_bld building
  end
  )

(* Conducting a game *)
open GameGrid

class game ~(frame : #GContainer.container) ~(label : #GMisc.label)
    ~(statusbar : #GMisc.statusbar) =

  let table = GPack.table ~columns:size ~rows:size ~packing:frame#add () in

  object (self)
    (* adds all cells in game grid into table *)
    val cells =
      Array.init size
        ~f:(fun i -> Array.init size
               ~f:(fun j -> new cell ~packing:(table#attach ~top:i ~left:j) ()))

    val label = label
    val turn = statusbar#new_context ~name:"turn"
    val messages = statusbar#new_context ~name:"messages"

    method grid = cells
    method table = table
    val mutable current_building = `house
    val mutable turnnum = 1

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

    method play x y =
      if action cells ~x ~y ~building:current_building then begin
        current_building <-
          match current_building with
          | `house ->
            turn#pop (); turn#push "Current Date: May 2020"; self#update_turn (); `house2
          | `house2 -> turn#pop (); turn#push "Current Date: May 2020"; self#update_turn (); `house (*self#finish ();*)
         end
      else
        messages#flash "You cannot build there" ;

    initializer
      for i = 0 to size-1 do
        for j = 0 to size-1 do
          let cell = cells.(i).(j) in
          cell#connect#enter ~callback:cell#misc#grab_focus; (* when hovering? *)
          cell#connect#clicked ~callback:(fun () -> self#play i j) (* when clicked, execute [play i j]*)
        done done;

      self#update_label ();
      turn#push "Current Date: April 1865";
      ()
  end

(* Graphical stuff *)

let vbox = GPack.vbox ~packing:window#add ()

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

let activ_action ac =
  Printf.printf "Action '%s' activated\n" ac#name ;
  flush stdout

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

      a "New" ~stock:`NEW ~tooltip:"Create a new file"
        ~callback:activ_action ;
      a "Open" ~stock:`OPEN ~tooltip:"Open a file"
        ~callback:activ_action ;
      a "Save" ~stock:`SAVE ~tooltip:"Save current file"
        ~callback:activ_action ;
      a "SaveAs" ~stock:`SAVE_AS
        ~tooltip:"Save to a file" ~callback:activ_action ;
      a "Quit" ~stock:`QUIT ~tooltip:"Quit"
        ~callback:activ_action ;
      a "About" ~label:"_About" ~accel:"<control>A" ~tooltip:"About"
        ~callback:activ_action ;
      a "Logo" ~stock:(`STOCK "demo-gtk-logo") ~tooltip:"GTK+"
        ~callback:activ_action ;

      ta "Pause" ~stock:`BOLD ~label:"_Pause"
        ~accel:"<control>P" ~tooltip:"Bold"
        ~callback:activ_action ~active:true ;

      radio ~init_value:0 ~callback:(fun n -> Printf.printf "radio action %d\n%!" n)
        [ ra "Fire" 0 ~label:"_Fire"
            ~tooltip:"Blood" ~accel:"<control>F" ;
          ra "Blizzard" 1 ~label:"_Blizzard"
            ~tooltip:"Grass" ~accel:"<control>B" ;
          ra "Prelim" 2 ~label:"_Prelim"
            ~tooltip:"Sky" ~accel:"<control>R" ;
        ] ;
    ] ;

  let ui_m = GAction.ui_manager () in
  ui_m#insert_action_group actions 0 ;
  window#add_accel_group ui_m#get_accel_group ;
  ui_m#add_ui_from_string ui_info ;

  let box1 = GPack.vbox ~packing:vbox#add () in
  box1#pack (ui_m#get_widget "/MenuBar") ;

  let h_box1 = GPack.hbox ~packing:box1#add () in

  let button = GButton.button ~packing:h_box1#add ~relief:`NONE () in

  (* Connect the "clicked" signal of the button to callback *)
  button#connect#clicked ~callback:
    (fun () -> print_endline "house button was pressed");
  (* Create box with xpm and label and pack into button *)
  xpm_label_box ~file:"smslice.xpm" ~text:"" ~packing:button#add ();

  let button2 = GButton.button ~packing:h_box1#add () in
  button2#connect#clicked ~callback:
    (fun () -> print_endline "house2 button was pressed");
  xpm_label_box ~file:"house2.xpm" ~text:"" ~packing:button2#add ();

  let bulldoze = GButton.button ~packing:h_box1#add () in
  bulldoze#connect#clicked ~callback:
    (fun () -> print_endline "bulldoze button was pressed");
  xpm_label_box ~file:"bulldozer.xpm" ~text:"" ~packing:bulldoze#add ();


  GMisc.separator `HORIZONTAL ~packing:box1#pack () ;
  let frame = GBin.frame ~shadow_type:`IN ~packing:box1#add () in
  let hbox = GPack.hbox ~packing:vbox#pack () in


  let bar = GMisc.statusbar ~packing:hbox#add () in
  let frame2 = GBin.frame ~shadow_type:`IN ~packing:hbox#pack () in
  let label = GMisc.label ~justify:`LEFT ~xpad:5 ~xalign:0.0 ~packing:frame2#add () in

  new game ~frame ~label ~statusbar:bar ;

  let b = GButton.button ~stock:`CLOSE ~packing:box1#pack () in
  b#connect#clicked window#destroy ;
  b#misc#set_can_default true ;
  b#misc#grab_default ()



(* Start *)
let _ =
  window#connect#destroy ~callback:Main.quit;
  setup_ui window ;
  window#show ();
  Main.main ()
