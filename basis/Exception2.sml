(*
    Title:  Extend the Exception structure.
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

(* Extend the exception structure adding in exception_trace.
   We want the basic Exception structure with reraise in Thread and
   we want Thread in TextIO. *)
structure Exception =
struct
    open Exception

    local
        fun printTrace(trace, exn) =
        let
            fun pr s = TextIO.output(TextIO.stdOut, s)
        in
            (* Exception *)
            pr "Exception trace for exception - ";
            pr (General.exnName exn);
            (* Location if available *)
            case exceptionLocation exn of
                SOME { file, startLine=line, ... } =>
                    (
                        if file = "" then () else (pr " raised in "; pr file);
                        if line = 0 then () else (pr " line "; pr(Int.toString line))
                    )
            |   NONE => ();
            pr "\n\n";
            (* Function list *)
            List.app(fn s => (pr s; pr "\n")) trace;
            pr "End of trace\n\n";
        
            TextIO.flushOut TextIO.stdOut;
            (* Reraise the exception. *)
            LibrarySupport.reraise exn
        end
    in
        fun exception_trace f = traceException(f, printTrace)
    end

end;
