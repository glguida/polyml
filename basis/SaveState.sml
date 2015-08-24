(*
    Title:  Saving and loading state
    Author: David C.J. Matthews 2008, 2015

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature SAVESTATE =
sig
    val loadState : string -> unit
    val saveState : string -> unit
    val renameParent : {child: string, newParent: string} -> unit
    val saveChild : string * int -> unit
    val showHierarchy : unit -> string list
    val showParent : string -> string option
end;

local
    open RuntimeCalls;
in
    structure SaveState: SAVESTATE = 
    struct
        fun saveChild(f: string, depth: int): unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (20, (f, depth))
        fun saveState f = saveChild (f, 0);
        fun showHierarchy(): string list =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (22, ())
        fun renameParent{ child: string, newParent: string }: unit =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (23, (child, newParent))
        fun showParent(child: string): string option =
            RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (24, child)

        fun loadState (f: string): unit = RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (21, f)
    end
end;
