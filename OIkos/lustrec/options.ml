(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2009-2011, ONERA, Toulouse, FRANCE - LIFL, Lille, FRANCE
 *
 * This file is part of Prelude
 *
 * Prelude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation ; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY ; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program ; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

let version = Version.number
let main_node = ref ""
let static_mem = ref true
let print_types = ref true
let print_clocks = ref true
let delay_calculus = ref true
let track_exceptions = ref true
let ansi = ref false
let check = ref false
let spec = ref "acsl"
let output = ref "C"
let dest_dir = ref "."
let verbose_level = ref 1
let global_inline = ref false
let witnesses = ref false
let optimization = ref 2
let horntraces = ref false
let horn_cex = ref false


let options =
  [ "-d", Arg.Set_string dest_dir,
    "produces code in the specified directory (default: .)";
    "-node", Arg.Set_string main_node, "specifies the main node";
    "-init", Arg.Set delay_calculus, "performs an initialisation analysis for Lustre nodes";
    "-dynamic", Arg.Clear static_mem, "specifies a dynamic allocation scheme for main Lustre node (default: static)";
    "-ansi", Arg.Set ansi, "specifies that generated C code is ansi C90 compliant (default: C99)";
    "-check-access", Arg.Set check, "checks at runtime that array accesses always lie within bounds (default: no check)";

    "-no-spec", Arg.Unit (fun () -> spec := "no"), "do not generate any specification";
    "-acsl-spec", Arg.Unit (fun () -> spec := "acsl"), "generates an ACSL encoding of the specification. Only meaningful for the C backend (default)";
    "-c-spec", Arg.Unit (fun () -> spec := "c"), "generates a C encoding of the specification instead of ACSL contracts and annotations. Only meaningful for the C backend";
    "-java", Arg.Unit (fun () -> output := "java"), "generates Java output instead of C";
    "-horn", Arg.Unit (fun () -> output := "horn"), "generates Horn clauses encoding output instead of C";
    "-horn-traces", Arg.Unit (fun () -> output := "horn"; horntraces:=true), "produce traceability file for Horn backend. Enable the horn backend.";
    "-horn-cex", Arg.Set horn_cex, "generate cex enumeration. Enable the horn backend (work in progress)";
    "-lustre", Arg.Unit (fun () -> output := "lustre"), "generates Lustre output, performing all active optimizations";
    "-inline", Arg.Set global_inline, "inline all node calls (require a main node)";
    "-ikos", Arg.Unit (fun () -> output := "ikos"; global_inline := true), "check properties with IKOS backend";
    "-witnesses", Arg.Set witnesses, "enable production of witnesses during compilation";
    "-print_types", Arg.Set print_types, "prints node types";
    "-print_clocks", Arg.Set print_clocks, "prints node clocks";
    "-O", Arg.Set_int optimization, " changes optimization level <default: 2>";
    "-verbose", Arg.Set_int verbose_level, " changes verbose level <default: 1>";
    "-version", Arg.Unit (fun () -> print_endline version), " displays the version";]

let get_witness_dir filename =
  (* Make sure the directory exists *)
  let dir = !dest_dir ^ "/" ^ (Filename.basename filename) ^ "_witnesses" in
  let _ = try
	    if not (Sys.is_directory dir) then (
	      Format.eprintf "File of name %s exists. It should be a directory.@." dir;
	      exit 1
	    )
    with Sys_error _ -> Unix.mkdir dir 0o750
  in
  dir

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
