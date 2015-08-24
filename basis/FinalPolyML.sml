(*
    Title:      Nearly final version of the PolyML structure
    Author:     David Matthews
    Copyright   David Matthews 2008-9, 2014, 2015

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

(*
Based on:

    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


(*
This is the version of the PolyML structure that can be compiled after we
have the rest of the basis library.  In particular it binds in TextIO.stdIn
and TextIO.stdOut.

This contains the top-level read-eval-print loop as well as "use" and
Poly/ML's "make".

The rootFunction has now been pulled out into a separate file and is added on
after this. 
*)
local
    (* A hash table with a mutex that protects against multiple threads
       rehashing the table by entering values at the same time. *)
    structure ProtectedTable :>
    sig
        type 'a ptable
        val create: unit -> 'a ptable
        val lookup: 'a ptable -> string -> 'a option
        val enter: 'a ptable -> string * 'a -> unit
        val all: 'a ptable -> unit -> (string * 'a) list
        val delete: 'a ptable -> string -> unit
    end
    =
    struct
        open HashArray Thread.Mutex LibraryIOSupport
        type 'a ptable = 'a hash * mutex

        fun create () = (hash 10, mutex())
        and lookup(tab, mutx) = protect mutx (fn s => sub(tab, s))
        and enter(tab, mutx) = protect mutx (fn (s, v) => update(tab, s, v))
        and all(tab, mutx) = protect mutx (fn () => fold (fn (s, v, l) => ((s, v) :: l)) [] tab)
        and delete(tab, mutx) = protect mutx (fn s => HashArray.delete (tab, s))
    end

    open PolyML.NameSpace
 
    local
        open ProtectedTable
        val fixTable = create() and sigTable = create() and valTable = create()
        and typTable = create() and fncTable = create() and strTable = create()
    in
        val globalNameSpace: PolyML.NameSpace.nameSpace =
        {
            lookupFix    = lookup fixTable,
            lookupSig    = lookup sigTable,
            lookupVal    = lookup valTable,
            lookupType   = lookup typTable,
            lookupFunct  = lookup fncTable,
            lookupStruct = lookup strTable,
            enterFix     = enter fixTable,
            enterSig     = enter sigTable,
            enterVal     = enter valTable,
            enterType    = enter typTable,
            enterFunct   = enter fncTable,
            enterStruct  = enter strTable,
            allFix       = all fixTable,
            allSig       = all sigTable,
            allVal       = all valTable,
            allType      = all typTable,
            allFunct     = all fncTable,
            allStruct    = all strTable
        }

        val forgetFix    = delete fixTable
        and forgetSig    = delete sigTable
        and forgetVal    = delete valTable
        and forgetType   = delete typTable
        and forgetFunct  = delete fncTable
        and forgetStruct = delete strTable
    end

    (* PolyML.compiler takes a list of these parameter values.  They all
       default so it's possible to pass only those that are actually
       needed. *)
    datatype compilerParameters =
        CPOutStream of string->unit
        (* Output stream for debugging and other output from the compiler.
           Provides a default stream for other output.  Default: TextIO.print *)
    |   CPNameSpace of PolyML.NameSpace.nameSpace
        (* Name space to look up and enter results.  Default: globalNameSpace *)
    |   CPErrorMessageProc of
            { message: PolyML.pretty, hard: bool, location: PolyML.location, context: PolyML.pretty option } -> unit
        (* Called by the compiler to generate error messages.
           Arguments (message, isHard, lineNo, context).  message is the message.
           isHard is true if this is an error, false if a warning. 
           location is the file-name, line number and position.  context is an
           optional extra piece of information showing the part of the parse tree
           where the error was detected.
           Default: print this to CPOutStream value using CPLineNo and CPFileName. *)
    |   CPLineNo of unit -> int
        (* Called by the compiler to get the current "line number".  This is passed
           to CPErrorMessageProc and the debugger.  It may actually be a more general
           location than a source line.  Default: fn () => 0 i.e. no line numbering. *)
    |   CPLineOffset of unit -> int
        (* Called by the compiler to get the current "offset".  This is passed
           to CPErrorMessageProc and the debugger.  This may either be an offset on
           the current file, a byte offset or simply zero.
           Default: fn () => 0 i.e. no line offset. *)
    |   CPFileName of string
        (* The current file being compiled.  This is used by the default CPErrorMessageProc
           and the debugger.  Default: "" i.e. interactive stream. *)
    |   CPPrintInAlphabeticalOrder of bool
        (* Whether to sort the results by alphabetical order before printing them.  Applies
           only to the default CPResultFun.  Default value of printInAlphabeticalOrder. *)
    |   CPResultFun of {
            fixes: (string * fixityVal) list, values: (string * valueVal) list,
            structures: (string * structureVal) list, signatures: (string * signatureVal) list,
            functors: (string * functorVal) list, types: (string * typeVal) list} -> unit
        (* Function to apply to the result of compiling and running the code.
           Default: print and enter the values into CPNameSpace. *)
    |   CPCompilerResultFun of
            PolyML.parseTree option *
            ( unit -> {
                fixes: (string * fixityVal) list, values: (string * valueVal) list,
                structures: (string * structureVal) list, signatures: (string * signatureVal) list,
                functors: (string * functorVal) list, types: (string * typeVal) list}) option -> unit -> unit
        (* Function to process the result of compilation.  This can be used to capture the
           parse tree even if type-checking fails.
           Default: Execute the code and call the result function if the compilation
           succeeds.  Raise an exception if the compilation failed. *)
    |   CPProfiling of int
        (* Control profiling.  0 is no profiling, 1 is time etc.  Default is value of PolyML.profiling. *)
    |   CPTiming of bool
        (* Control whether the compiler should time various phases of the
           compilation and also the run time. Default: value of PolyML.timing. *)
    |   CPDebug of bool
        (* Control whether calls to the debugger should be inserted into the compiled
           code.  This allows breakpoints to be set, values to be examined and printed
           and functions to be traced at the cost of a very large run-time overhead.
           Default: value of PolyML.Compiler.debug *)
    |   CPPrintDepth of unit->int
        (* This controls the depth of printing if the default CPResultFun is used.  It
           is also bound into any use of PolyML.print in the compiled code and will
           be called to get the print depth whenever that code is executed.
           Default: Get the current value of PolyML.print_depth. *)
    |   CPPrintStream of string->unit
        (* This is bound into any occurrence of PolyML.print and is used to produce
           the outut.  Default: CPOutStream. *)
    |   CPErrorDepth of int
        (* Controls the depth of context to produce in error messages.
           Default : value of PolyML.error_depth. *)
    |   CPLineLength of int
        (* Bound into any occurrences of PolyML.print.  This is the length of a line
           used in the pretty printer.  Default: value of PolyML.line_length. *)
    |   CPRootTree of
        {
            parent: (unit -> PolyML.parseTree) option,
            next: (unit -> PolyML.parseTree) option,
            previous: (unit -> PolyML.parseTree) option
        }
        (* This can be used to provide a parent for parse trees created by the
           compiler.  This appears as a PTparent property in the tree.
           The default is NONE which does not to provide a parent.  *)
    |   CPAllocationProfiling of int
        (* Controls whether to add profiling information to each allocation.  Currently
           zero means no profiling and one means add the allocating function. *)

    |   CPDebuggerFunction of int * valueVal * int * string * string * nameSpace -> unit
        (* This is no longer used and is just left for backwards compatibility. *)

    (* References for control and debugging. *)
    val profiling = ref 0
    and timing = ref false
    and printDepth = ref 0
    and errorDepth = ref 6
    and lineLength = ref 77
    and allocationProfiling = ref 0
    
    val assemblyCode = ref false
    and codetree = ref false
    and codetreeAfterOpt = ref false
    and pstackTrace = ref false
    and parsetree = ref false
    and reportUnreferencedIds = ref false
    and reportExhaustiveHandlers = ref false
    and narrowOverloadFlexRecord = ref false
    and createPrintFunctions = ref true
    val lowlevelOptimise = ref true
    
    val debug = ref false
    val inlineFunctors = ref true
    val maxInlineSize = ref 80
    val printInAlphabeticalOrder = ref true
    val traceCompiler = ref false
    
    fun prettyPrintWithIDEMarkup(stream : string -> unit, lineWidth : int): PolyML.pretty -> unit =
    let
        open PolyML
        val openDeclaration = "\u001bD"
        val closeDeclaration = "\u001bd"
        val separator = "\u001b,"
        val finalSeparator = "\u001b;"
        
        fun beginMarkup context =
            case List.find (fn ContextLocation _ => true | _ => false) context of
                SOME (ContextLocation{file,startLine,startPosition,endPosition, ...}) =>
                let
                    (* In the unlikely event there's an escape character in the
                       file name convert it to ESC-ESC. *)
                    fun escapeEscapes #"\u001b" = "\u001b\u001b"
                    |   escapeEscapes c = str c
                in
                    stream openDeclaration;
                    stream(String.translate escapeEscapes file);
                    stream separator;
                    stream(Int.toString startLine);
                    stream separator;
                    stream(Int.toString startPosition);
                    stream separator;
                    stream(Int.toString endPosition);
                    stream finalSeparator
                end
            |   _ => ()
            
        fun endMarkup context =
            List.app (fn ContextLocation _ => stream closeDeclaration | _ => ()) context
    in
        prettyMarkup (beginMarkup, endMarkup) (stream, lineWidth)
    end;

    (* useMarkupInOutput is set according to the setting of *)
    val useMarkupInOutput = ref false
    fun prettyPrintWithOptionalMarkup(stream, lineWidth) =
        if ! useMarkupInOutput then prettyPrintWithIDEMarkup(stream, lineWidth)
        else PolyML.prettyPrint(stream, lineWidth)

    (* Top-level prompts. *)
    val prompt1 = ref "> " and prompt2 = ref "# ";

    fun printOut s =
        TextIO.print s
        (* If we get an exception while writing to stdOut we've got
           a big problem and can't continue.  It could happen if
           we have closed stdOut.  Try reporting the error through
           stdErr and exit. *)
        handle Thread.Thread.Interrupt => raise Thread.Thread.Interrupt
        |     exn =>
            (
                (
                    TextIO.output(TextIO.stdErr,
                        concat["Exception ", exnName exn,
                               " raised while writing to stdOut.\n"]);
                    TextIO.flushOut TextIO.stdErr (* probably unnecessary. *)
                ) handle _ => ();
                (* Get out without trying to do anything else. *)
                OS.Process.terminate OS.Process.failure
            )

    local
        open Bootstrap Bootstrap.Universal
        (* To allow for the possibility of changing the representation we don't make Universal
           be the same as Bootstrap.Universal. *)

        (* Default error message function. *)
        fun defaultErrorProc printString
            {message: PolyML.pretty, hard: bool,
             location={startLine, startPosition, endPosition, file, ...}: PolyML.location,
             context: PolyML.pretty option} =
        let
            open PolyML
            val fullMessage =
                case context of
                    NONE => message
                |   SOME ctxt =>
                        PrettyBlock(0, true, [],
                            [ message, PrettyBreak(1, 0),
                                PrettyBlock(2, false, [], [PrettyString "Found near", PrettyBreak(1, 0), ctxt])
                            ])
        in
            if ! useMarkupInOutput
            then (* IDE mark-up of error messages.  This is actually the same as within the IDE. *)
            let
                val openError = "\u001bE"
                val closeError = "\u001be"
                val separator = "\u001b,"
                val finalSeparator = "\u001b;"
            in
                printString(
                    concat
                        [
                            openError,
                            if hard then "E" else "W", separator,
                            file, (* TODO double any escapes. *) separator,
                            Int.toString startLine, separator,
                            Int.toString startPosition, separator,
                            Int.toString endPosition, finalSeparator
                         ]
                    );
                prettyPrintWithIDEMarkup(printString, !lineLength) fullMessage;
                printString closeError
            end
            else (* Plain text form. *)
            (
                printString(concat
                   ( (if file = "" then ["poly: "] else [file, ":"]) @
                     (if startLine = 0 then [] else [Int.toString startLine]) @
                     (if startPosition = 0 then [": "] else [".", Int.toString startPosition, "-", Int.toString endPosition, ": "]) @
                     (if hard then ["error: "] else ["warning: "]) ));
(*                   ( (if hard then ["Error-"] else ["Warning-"]) @
                     (if file = "" then [] else [" in '", file, "',"]) @
                     (if startLine = 0 then [] else [" line ", Int.toString startLine]) @
                     (if startLine = 0 andalso file = "" then [] else [".\n"]))); *)
                PolyML.prettyPrint(printString, !lineLength) fullMessage
            )
        end

        (* Default function to print and enter a value. *)
        fun printAndEnter (inOrder: bool, space: PolyML.NameSpace.nameSpace,
                           stream: string->unit, depth: int)
            { fixes: (string * fixityVal) list, values: (string * valueVal) list,
              structures: (string * structureVal) list, signatures: (string * signatureVal) list,
              functors: (string * functorVal) list, types: (string * typeVal) list}: unit =
        let
            (* We need to merge the lists to sort them alphabetically. *)
            datatype decKind =
                FixStatusKind of fixityVal
            |   TypeConstrKind of typeVal
            |   SignatureKind of signatureVal
            |   StructureKind of structureVal
            |   FunctorKind of functorVal
            |   ValueKind of valueVal

            val decList =
                map (fn (s, f) => (s, FixStatusKind f)) fixes @
                map (fn (s, f) => (s, TypeConstrKind f)) types @
                map (fn (s, f) => (s, SignatureKind f)) signatures @
                map (fn (s, f) => (s, StructureKind f)) structures @
                map (fn (s, f) => (s, FunctorKind f)) functors @
                map (fn (s, f) => (s, ValueKind f)) values

            fun kindToInt(FixStatusKind _) = 0
            |   kindToInt(TypeConstrKind _) = 1
            |   kindToInt(SignatureKind _) = 2
            |   kindToInt(StructureKind _) = 3
            |   kindToInt(FunctorKind _) = 4
            |   kindToInt(ValueKind _) = 5

            fun order (s1: string, k1) (s2, k2) =
                    if s1 = s2 then kindToInt k1 <= kindToInt k2
                    else s1 <= s2

            fun quickSort _                      ([]:'a list)      = []
            |   quickSort _                      ([h]:'a list)     = [h]
            |   quickSort (leq:'a -> 'a -> bool) ((h::t) :'a list) =
            let
                val (after, befor) = List.partition (leq h) t
            in
                quickSort leq befor @ (h :: quickSort leq after)
            end;

            (* Don't sort the declarations if we want them in declaration order. *)
            val sortedDecs =
                if inOrder then quickSort order decList else decList

            fun enterDec(n, FixStatusKind f) = #enterFix space (n,f)
            |   enterDec(n, TypeConstrKind t) = #enterType space (n,t)
            |   enterDec(n, SignatureKind s) = #enterSig space (n,s)
            |   enterDec(n, StructureKind s) = #enterStruct space (n,s)
            |   enterDec(n, FunctorKind f) = #enterFunct space (n,f)
            |   enterDec(n, ValueKind v) = #enterVal space (n,v)

            fun printDec(n, FixStatusKind f) =
                    prettyPrintWithOptionalMarkup (stream, !lineLength) (displayFix(n,f))

            |   printDec(_, TypeConstrKind t) =
                    prettyPrintWithOptionalMarkup (stream, !lineLength) (displayType(t, depth, space))

            |   printDec(_, SignatureKind s) =
                    prettyPrintWithOptionalMarkup (stream, !lineLength) (displaySig(s, depth, space))

            |   printDec(_, StructureKind s) =
                    prettyPrintWithOptionalMarkup (stream, !lineLength) (displayStruct(s, depth, space))

            |   printDec(_, FunctorKind f) =
                    prettyPrintWithOptionalMarkup (stream, !lineLength) (displayFunct(f, depth, space))

            |   printDec(_, ValueKind v) =
                    prettyPrintWithOptionalMarkup (stream, !lineLength) (displayVal(v, depth, space))

        in
            (* First add the declarations to the name space and then print them.  Doing it this way
               improves the printing of types since these require look-ups in the name space.  For
               instance the constructors of a datatype from an opened structure should not include
               the structure name but that will only work once the datatype itself is in the global
               name-space. *)
            List.app enterDec sortedDecs;
            if depth > 0 then List.app printDec sortedDecs else ()
        end
    in
        fun polyCompiler (getChar: unit->char option, parameters: compilerParameters list) =
        let
            (* Find the first item that matches or return the default. *)
            fun find _ def [] = def
              | find f def (hd::tl) =
                  case f hd of
                      SOME s => s
                  |   NONE => find f def tl
        
            val outstream = find (fn CPOutStream s => SOME s | _ => NONE) TextIO.print parameters
            val nameSpace = find (fn CPNameSpace n => SOME n | _ => NONE) globalNameSpace parameters
            val lineNo = find (fn CPLineNo l => SOME l | _ => NONE) (fn () => 0) parameters
            val lineOffset = find (fn CPLineOffset l => SOME l | _ => NONE) (fn () => 0) parameters
            val fileName = find (fn CPFileName s => SOME s | _ => NONE) "" parameters
            val printInOrder = find (fn CPPrintInAlphabeticalOrder t => SOME t | _ => NONE)
                                (! printInAlphabeticalOrder) parameters
            val profiling = find (fn CPProfiling i => SOME i | _ => NONE) (!profiling) parameters
            val timing = find  (fn CPTiming b => SOME b | _ => NONE) (!timing) parameters
            val printDepth = find (fn CPPrintDepth f => SOME f | _ => NONE) (fn () => !printDepth) parameters
            val resultFun = find (fn CPResultFun f => SOME f | _ => NONE)
               (printAndEnter(printInOrder, nameSpace, outstream, printDepth())) parameters
            val printString = find (fn CPPrintStream s => SOME s | _ => NONE) outstream parameters
            val errorProc =  find (fn CPErrorMessageProc f => SOME f | _ => NONE) (defaultErrorProc printString) parameters
            val debugging = find (fn CPDebug t => SOME t | _ => NONE) (! debug) parameters
            val allocProfiling = find(fn CPAllocationProfiling l  => SOME l | _ => NONE) (!allocationProfiling) parameters
            local
                (* Default is to filter the parse tree argument. *)
                fun defaultCompilerResultFun (_, NONE) = raise Fail "Static Errors"
                |   defaultCompilerResultFun (_, SOME code) = fn () => resultFun(code()) 
            in
                val compilerResultFun = find (fn CPCompilerResultFun f => SOME f | _ => NONE)
                    defaultCompilerResultFun parameters
            end

            (* TODO: Make this available as a parameter. *)
            val prettyOut = prettyPrintWithOptionalMarkup(printString, !lineLength)
            
            val compilerOut = prettyPrintWithOptionalMarkup(outstream, !lineLength)

            (* Parent tree defaults to empty. *)
            val parentTree =
                find (fn CPRootTree f => SOME f | _ => NONE)
                    { parent = NONE, next = NONE, previous = NONE } parameters

            (* Pass all the settings.  Some of these aren't included in the parameters datatype (yet?). *)
            val treeAndCode =
                PolyML.compiler(nameSpace, getChar,
                    [
                    tagInject errorMessageProcTag errorProc,
                    tagInject compilerOutputTag compilerOut,
                    tagInject lineNumberTag lineNo,
                    tagInject offsetTag lineOffset,
                    tagInject fileNameTag fileName,
                    tagInject inlineFunctorsTag (! inlineFunctors),
                    tagInject maxInlineSizeTag (! maxInlineSize),
                    tagInject parsetreeTag (! parsetree),
                    tagInject codetreeTag (! codetree),
                    tagInject pstackTraceTag (! pstackTrace),
                    tagInject lowlevelOptimiseTag (! lowlevelOptimise),
                    tagInject assemblyCodeTag (! assemblyCode),
                    tagInject codetreeAfterOptTag (! codetreeAfterOpt),
                    tagInject timingTag timing,
                    tagInject profilingTag profiling,
                    tagInject profileAllocationTag allocProfiling,
                    tagInject errorDepthTag (! errorDepth),
                    tagInject printDepthFunTag printDepth,
                    tagInject lineLengthTag (! lineLength),
                    tagInject traceCompilerTag (! traceCompiler),
                    tagInject debugTag debugging,
                    tagInject printOutputTag prettyOut,
                    tagInject rootTreeTag parentTree,
                    tagInject reportUnreferencedIdsTag (! reportUnreferencedIds),
                    tagInject reportExhaustiveHandlersTag (! reportExhaustiveHandlers),
                    tagInject narrowOverloadFlexRecordTag (! narrowOverloadFlexRecord),
                    tagInject createPrintFunctionsTag (! createPrintFunctions)
                    ])
        in
            compilerResultFun treeAndCode
        end
 
        (* Top-level read-eval-print loop.  This is the normal top-level loop and is
           also used for the debugger. *)
        fun topLevel {isDebug, nameSpace, exitLoop, exitOnError, isInteractive } =
        let
            (* This is used as the main read-eval-print loop.  It is also invoked
               by running code that has been compiled with the debug option on
               when it stops at a breakpoint.  In that case debugEnv contains an
               environment formed from the local variables.  This is placed in front
               of the normal top-level environment. *)
           
            (* Don't use the end_of_stream because it may have been set by typing
               EOT to the command we were running. *)
            val endOfFile    = ref false;
            val realDataRead = ref false;
            val lastWasEol   = ref true;
    
            (* Each character typed is fed into the compiler but leading
               blank lines result in the prompt remaining as firstPrompt until
               significant characters are typed. *)
            fun readin () : char option =
            let
                val () =
                    if isInteractive andalso !lastWasEol (* Start of line *)
                    then if !realDataRead
                    then printOut (if isDebug then "debug " ^ !prompt2 else !prompt2)
                    else printOut (if isDebug then "debug " ^ !prompt1 else !prompt1)
                    else ();
             in
                case TextIO.input1 TextIO.stdIn of
                    NONE => (endOfFile := true; NONE)
                |   SOME #"\n" => ( lastWasEol := true; SOME #"\n" )
                |   SOME ch =>
                       (
                           lastWasEol := false;
                           if ch <> #" "
                           then realDataRead := true
                           else ();
                           SOME ch
                       )
            end; (* readin *)
    
            (* Remove all buffered but unread input. *)
            fun flushInput () =
                case TextIO.canInput(TextIO.stdIn, 1) of
                    SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
                |   _ => (* No input waiting or we're at EOF. *) ()
    
            fun readEvalPrint () : unit =
            let
                (* If we have executed a deeply recursive function the stack will have
                   extended to be very large.  It's better to reduce the stack if we
                   can.  This is RISKY.  Each function checks on entry that the stack has
                   sufficient space for everything it will allocate and assumes the stack
                   will not shrink.  It's unlikely that any of the functions here will
                   have asked for very much but as a precaution we allow for an extra 8k words. *)
                fun shrink_stack (newsize : int) : unit = 
                    RunCall.run_call1 RuntimeCalls.POLY_SYS_shrink_stack newsize
                val () = if isDebug then () else shrink_stack 8000;
            in
                realDataRead := false;
                (* Compile and then run the code. *)
                let
                    val code =
                        polyCompiler(readin, [CPNameSpace nameSpace, CPOutStream printOut])
                        handle Fail s => 
                        (
                            printOut(s ^ "\n");
                            flushInput();
                            lastWasEol := true;
                            raise Fail s
                        )
                in
                    code ()
                    (* Report exceptions in running code. *)
                        handle exn =>
                        let
                            open PolyML
                            open Exception
                            val exLoc =
                                case exceptionLocation exn of
                                    NONE => []
                                |   SOME loc => [ContextLocation loc]
                        in
                            prettyPrintWithOptionalMarkup(TextIO.print, ! lineLength)
                                (PrettyBlock(0, false, [],
                                    [
                                        PrettyBlock(0, false, exLoc, [PrettyString "Exception-"]),
                                        PrettyBreak(1, 3),
                                        prettyRepresentation(exn, ! printDepth),
                                        PrettyBreak(1, 3),
                                        PrettyString "raised"
                                    ]));
                            LibrarySupport.reraise exn
                        end
                end
            end; (* readEvalPrint *)
            
            fun handledLoop () : unit =
            (
                (* Process a single top-level command. *)
                readEvalPrint() handle _ =>
                                      if exitOnError
                                      then OS.Process.exit OS.Process.failure
                                      else ();
                (* Exit if we've seen end-of-file or we're in the debugger
                   and we've run "continue". *)
                if !endOfFile orelse exitLoop() then ()
                else handledLoop ()
            )
        in
            handledLoop ()  
        end

        (* Normal, non-debugging top-level loop. *)
        fun shell () =
        let
            val argList = CommandLine.arguments()
            fun switchOption option = List.exists(fn s => s = option) argList
            (* Generate mark-up in IDE code when printing if the option has been given
               on the command line. *)
            val () = useMarkupInOutput := switchOption "--with-markup"
            val exitOnError = switchOption"--error-exit"
            val interactive =
                switchOption "-i" orelse
                let
                    open TextIO OS
                    open StreamIO TextPrimIO IO
                    val s = getInstream stdIn
                    val (r, v) = getReader s
                    val RD { ioDesc, ...} = r
                in
                    setInstream(stdIn, mkInstream(r,v));
                    case ioDesc of
                        SOME io => (kind io = Kind.tty handle SysErr _ => false)
                    |   _  => false
                end
        in
            topLevel
                { isDebug = false, nameSpace = globalNameSpace, exitLoop = fn _ => false,
                  isInteractive = interactive, exitOnError = exitOnError }
        end
    end

    val suffixes = ref ["", ".ML", ".sml"];


    (*****************************************************************************)
    (*                  "use": compile from a file.                              *)
    (*****************************************************************************)

    fun use (originalName: string): unit =
    let
        (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
        (* We use the functional layer and a reference here rather than TextIO.input1 because
           that requires locking round every read to make it thread-safe.  We know there's
           only one thread accessing the stream so we don't need it here. *)
        fun trySuffixes [] =
            (* Not found - attempt to open the original and pass back the
               exception. *)
            (TextIO.getInstream(TextIO.openIn originalName), originalName)
         |  trySuffixes (s::l) =
            (TextIO.getInstream(TextIO.openIn (originalName ^ s)), originalName ^ s)
                handle IO.Io _ => trySuffixes l
        (* First in list is the name with no suffix. *)
        val (inStream, fileName) = trySuffixes("" :: ! suffixes)
        val stream = ref inStream

        val lineNo   = ref 1;
        fun getChar () : char option =
            case TextIO.StreamIO.input1 (! stream) of
                NONE => NONE
            |   SOME (eoln as #"\n", strm) =>
                (
                    lineNo := !lineNo + 1;
                    stream := strm;
                    SOME eoln
                )
            |   SOME(c, strm) => (stream := strm; SOME c)
    in
        while not (TextIO.StreamIO.endOfStream(!stream)) do
        let
            val code = polyCompiler(getChar, [CPFileName fileName, CPLineNo(fn () => !lineNo)])
                handle exn =>
                    ( TextIO.StreamIO.closeIn(!stream); LibrarySupport.reraise exn )
        in
            code() handle exn =>
            (
                (* Report exceptions in running code. *)
                TextIO.print ("Exception- " ^ exnMessage exn ^ " raised\n");
                TextIO.StreamIO.closeIn (! stream);
                LibrarySupport.reraise exn
            )
        end;
        (* Normal termination: close the stream. *)
        TextIO.StreamIO.closeIn (! stream)

    end (* use *)
 
    local
        open Time
    in
        fun maxTime (x : time, y : time): time = 
            if x < y then y else x
    end

    exception ObjNotFile;
    
    type 'a tag = 'a Universal.tag;
  
    fun splitFilename (name: string) : string * string =
    let
         val {dir, file } = OS.Path.splitDirFile name
    in
         (dir, file)
    end

    (* Make *)
    (* There are three possible states - The object may have been checked,
     it may be currently being compiled, or it may not have been
     processed yet. *)
    datatype compileState = NotProcessed | Searching | Checked;

    fun longName (directory, file) = OS.Path.joinDirFile{dir=directory, file = file}
    
    fun fileReadable (fileTuple as (directory, object)) =
        (* Use OS.FileSys.isDir just to test if the file/directory exists. *)
        if (OS.FileSys.isDir (longName fileTuple); false) handle OS.SysErr _ => true
        then false
        else
        let
            (* Check that the object is present in the directory with the name
             given and not a case-insensitive version of it.  This avoids
             problems with "make" attempting to recursively make Array etc
             because they contain signatures ARRAY. *)
            open OS.FileSys
            val d = openDir (if directory = "" then "." else directory)
            fun searchDir () =
              case readDir d of
                 NONE => false
              |  SOME f => f = object orelse searchDir ()
            val present = searchDir()
        in
            closeDir d;
            present
        end
    
    fun findFileTuple _                   [] = NONE
    |   findFileTuple (directory, object) (suffix :: suffixes) =
    let
        val fileName  = object ^ suffix
        val fileTuple = (directory, fileName)
    in
        if fileReadable fileTuple
        then SOME fileTuple
        else findFileTuple (directory, object) suffixes
    end;
    
    fun filePresent (directory : string, object : string) =
    let
        (* Construct suffixes with the architecture and version number in so
           we can compile architecture- and version-specific code. *)
        val archSuffix = "." ^ String.map Char.toLower (PolyML.architecture())
        val versionSuffix = "." ^ Int.toString Bootstrap.compilerVersionNumber
        val extraSuffixes = [archSuffix, versionSuffix, "" ]
        val addedSuffixes =
            List.foldr(fn (i, l) => (List.map (fn s => s ^ i) extraSuffixes) @ l) [] (!suffixes)
    in
        (* For each of the suffixes in the list try it. *)
        findFileTuple (directory, object) addedSuffixes
    end
    
    (* See if the corresponding file is there and if it is a directory. *)
    fun testForDirectory (name: string) : bool =
        OS.FileSys.isDir name handle OS.SysErr _ => false (* No such file. *)

    (* Time stamps. *)
    type timeStamp = Time.time;
    val firstTimeStamp : timeStamp = Time.zeroTime;
    (* Get the current time. *)
    val newTimeStamp : unit -> timeStamp = Time.now
    (* Get the date of a file. *)
    val fileTimeStamp : string -> timeStamp = OS.FileSys.modTime
    
    local
        open ProtectedTable
        (* Global tables to hold information about entities that have been made using "make". *)
        val timeStampTable: timeStamp ptable = create()
        and dependencyTable: string list ptable = create()
    in
        (* When was the entity last built?  Returns zeroTime if it hasn't. *)
        fun lastMade (objectName : string) : timeStamp =
            getOpt(lookup timeStampTable objectName, firstTimeStamp)

        (* Get the dependencies as an option type. *)
        val getMakeDependencies = lookup dependencyTable

        (* Set the time stamp and dependencies. *)
        fun updateMakeData(objectName, times, depends) =
        (
            enter timeStampTable (objectName, times);
            enter dependencyTable (objectName, depends)
        )
    end

    (* Main make function *)
    fun make (targetName: string) : unit =
    let
        (* This serves two purposes. It provides a list of objects which have been
           re-made to prevent them being made more than once, and it also prevents
           circular dependencies from causing infinite loops (e.g. let x = f(x)) *)
            local
                open HashArray;
                val htab : compileState hash = hash 10;
            in
                fun lookupStatus (name: string) : compileState =
                    getOpt(sub (htab, name), NotProcessed);
                  
                fun setStatus (name: string, cs: compileState) : unit =
                    update (htab, name, cs)
            end;

        (* Remove leading directory names to get the name of the object itself.
           e.g. "../compiler/parsetree/gencode" yields simply "gencode". *)
        val (dirName,objectName) = splitFilename targetName;
 
        (* Looks to see if the file is in the current directory. If it is and
           the file is newer than the corresponding object then it must be
           remade. If it is a directory then we attempt to remake the
           directory by compiling the "bind" file. This will only actually be
           executed if it involves some identifier which is newer than the
           result object. *)
        fun remakeObj (objName: string) (findDirectory: string -> string) =
        let
        (* Find a directory that contains this object. An exception will be
             raised if it is not there. *)
            val directory = findDirectory objName;
            val fullName  =
                if directory = "" (* Work around for bug. *)
                then objName
                else OS.Path.joinDirFile{dir=directory, file=objName};

            val objIsDir  = testForDirectory fullName;
            val here      = fullName;
      
            (* Look to see if the file exists, possibly with an extension,
               and get the extended version. *)
            val fileTuple =
                let
                    (* If the object is a directory the source is in the bind file. *)
                    val (dir : string, file : string) =
                        if objIsDir
                        then (here,"ml_bind")
                        else (directory, objName);
                in
                    case filePresent (dir, file) of
                        SOME res' => res'
                    |   NONE      => raise Fail ("No such file or directory ("^file^","^dir^")")
                end ;
            
            val fileName = longName fileTuple;

            val newFindDirectory : string -> string =
                if objIsDir
                then
                let
                    (* Look in this directory then in the ones above. *)
                    fun findDirectoryHere (name: string) : string =
                        case filePresent (here, name) of
                          NONE => findDirectory name (* not in this directory *)
                        | _    => here;
                in
                    findDirectoryHere
                end
                else findDirectory;
    
            (* Compiles a file. *)
            fun remakeCurrentObj () =
            let
                val () = print ("Making " ^ objName ^ "\n");
                local
                    (* Keep a list of the dependencies. *)
                    val deps : bool HashArray.hash = HashArray.hash 10;
                    
                    fun addDep name =
                        if getOpt(HashArray.sub (deps, name), true)
                        then HashArray.update(deps, name, true)
                        else ();
                    
                    (* Called by the compiler to look-up a global identifier. *)
                    fun lookupMakeEnv globalLook (name: string) : 'a option =
                    let
                        (* Have we re-declared it ? *)
                        val res = lookupStatus name;
                    in
                        case res of
                            NotProcessed  =>
                            (
                                (* Compile the dependency. *)
                                remakeObj name newFindDirectory;
                                (* Add this to the dependencies. *)
                                addDep name
                            )

                        |  Searching => (* In the process of making it *)
                           print("Circular dependency: " ^ name ^  " depends on itself\n")

                        | Checked => addDep name; (* Add this to the dependencies. *)

                        (* There was previously a comment about returning NONE here if
                           we had a problem remaking a dependency. *)
                        globalLook name
                    end; (* lookupMakeEnv *)

                     (* Enter the declared value in the table. Usually this will be the
                        target we are making. Also set the state to "Checked". The
                        state is set to checked when we finish making the object but
                        setting it now suppresses messages about circular dependencies
                        if we use the identifier within the file. *)
                    fun enterMakeEnv (kind : string, enterGlobal) (name: string, v: 'a) : unit =
                    (
                        (* Put in the value. *)
                        enterGlobal (name, v);
                        print ("Created " ^ kind ^ " " ^ name ^ "\n");
                        
                        (* The name we're declaring may appear to be a dependency
                           but isn't, so don't include it in the list. *)
                        HashArray.update (deps, name, false);
                        
                        if name = objName
                        then
                        let
                            (* Put in the dependencies i.e. those names set to true in the table. *)
                            val depends =
                                HashArray.fold (fn (s, v, l) => if v then s :: l else l) [] deps;
                            
                            (* Put in a time stamp for the new object.  We need to make
                               sure that it is no older than the newest object it depends
                               on.  In theory that should not be a problem but clocks on
                               different machines can get out of step leading to objects
                               made later having earlier time stamps. *)
                            val newest =
                                List.foldl (fn (s: string, t: timeStamp) =>
                                    maxTime (lastMade s, t)) (fileTimeStamp fileName) depends;
                            
                            val timeStamp = maxTime(newest, newTimeStamp());
                        in         
                            setStatus (name, Checked);
                            updateMakeData(name, timeStamp, depends)
                        end
                        else ()
                    ) (* enterMakeEnv *);
     
                in
                    val makeEnv =
                        { 
                            lookupFix    = #lookupFix globalNameSpace,
                            lookupVal    = #lookupVal globalNameSpace,
                            lookupType   = #lookupType globalNameSpace,
                            lookupSig    = lookupMakeEnv (#lookupSig globalNameSpace),
                            lookupStruct = lookupMakeEnv (#lookupStruct globalNameSpace),
                            lookupFunct  = lookupMakeEnv (#lookupFunct globalNameSpace),
                            enterFix     = #enterFix globalNameSpace,
                            enterVal     = #enterVal globalNameSpace,
                            enterType    = #enterType globalNameSpace,
                            enterStruct  = enterMakeEnv ("structure", #enterStruct globalNameSpace),
                            enterSig     = enterMakeEnv ("signature", #enterSig globalNameSpace),
                            enterFunct   = enterMakeEnv ("functor", #enterFunct globalNameSpace),
                            allFix       = #allFix globalNameSpace,
                            allVal       = #allVal globalNameSpace,
                            allType      = #allType globalNameSpace,
                            allSig       = #allSig globalNameSpace,
                            allStruct    = #allStruct globalNameSpace,
                            allFunct     = #allFunct globalNameSpace
                        };
                end; (* local for makeEnv *)

                val inputFile = OS.Path.joinDirFile{dir= #1 fileTuple, file= #2 fileTuple}
    
                val inStream = TextIO.openIn inputFile;
    
                val () =
                let (* scope of exception handler to close inStream *)
                    val endOfStream = ref false;
                    val lineNo     = ref 1;
        
                    fun getChar () : char option =
                        case TextIO.input1 inStream of
                            NONE => (endOfStream := true; NONE) (* End of file *)
                        |   eoln as SOME #"\n" => (lineNo := !lineNo + 1; eoln)
                        |   c => c
                 in
                    while not (!endOfStream) do
                    let
                        val code = polyCompiler(getChar,
                            [CPNameSpace makeEnv, CPFileName fileName, CPLineNo(fn () => !lineNo)])
                    in
                        code ()
                            handle exn as Fail _ => LibrarySupport.reraise exn
                            |  exn =>
                            (
                                print ("Exception- " ^ exnMessage exn ^ " raised\n");
                                LibrarySupport.reraise exn
                            )
                    end
                end (* body of scope of inStream *)
                    handle exn => (* close inStream if an error occurs *)
                    (
                        TextIO.closeIn inStream;
                        LibrarySupport.reraise exn
                    )
            in (* remake normal termination *)
                TextIO.closeIn inStream 
            end (* remakeCurrentObj *)
            
        in (* body of remakeObj *)
            setStatus (objName, Searching);
         
             (* If the file is newer than the object then we definitely must remake it.
               Otherwise we look at the dependency list and check those. If the result
               of that check is that one of the dependencies is newer than the object
               (probably because it has just been recompiled) we have to recompile
               the file. Compiling a file also checks the dependencies and recompiles
               them, generating a new dependency list. That is why we don't check the
               dependency list if the object is out of date with the file. Also if the
               file has been changed it may no longer depend on the things it used to
               depend on. *)
 
            let
                val objDate = lastMade objName
       
                fun maybeRemake (s:string) : unit =
                case lookupStatus s of
                    NotProcessed => (* see if it's a file. *)
                        (* Compile the dependency. *)
                        remakeObj s newFindDirectory
                    
                    | Searching => (* In the process of making it *)
                        print ("Circular dependency: " ^ s ^ " depends on itself\n")
                    
                    |  Checked => () (* do nothing *)

                open Time
                (* Process each entry and return true if
                   any is newer than the target. *)
                val processChildren =
                    List.foldl
                    (fn (child:string, parentNeedsMake:bool) =>
                        (
                            maybeRemake child;
                            (* Find its date and see if it is newer. *)
                            parentNeedsMake orelse lastMade child > objDate
                        )
                    )
                    false;
            in
                if objDate < fileTimeStamp fileName orelse
                    (
                        (* Get the dependency list. There may not be one if
                           this object has not been compiled with "make". *) 
                        case getMakeDependencies objName of
                            SOME d => processChildren d
                        |   NONE => true (* No dependency list - must use "make" on it. *)
                    )       
                then remakeCurrentObj ()
                else ()
            end;

            (* Mark it as having been checked. *)
            setStatus (objName, Checked)
        end (* body of remakeObj *)
  
        (* If the object is not a file mark it is checked. It may be a
           pervasive or it may be missing. In either case mark it as checked
           to save searching for it again. *)
        handle
                ObjNotFile => setStatus (objName, Checked)
            
            |   exn => (* Compilation (or execution) error. *)
                (
                    (* Mark as checked to prevent spurious messages. *)
                    setStatus (objName, Checked);
                    raise exn
                )
    in (*  body of make *)
        (* Check that the target exists. *)
        case filePresent (dirName, objectName) of
            NONE =>
            let
                val dir =
                    if dirName = "" then ""
                    else " (directory "^dirName^")";
                val s = "File "^objectName^" not found" ^ dir
            in
                print (s ^ "\n");
                raise Fail s
            end
        
        | _ =>
        let
            val targetIsDir = testForDirectory targetName;
            
            (* If the target we are making is a directory all the objects
               must be in the directory. If it is a file we allow references
               to other objects in the same directory. Objects not found must
               be pervasive. *)
            fun findDirectory (s: string) : string =
                if (not targetIsDir orelse s = objectName) andalso
                    isSome(filePresent(dirName,  s))
                then dirName
                else raise ObjNotFile;
        in
            remakeObj objectName findDirectory
                handle exn  => 
                (
                    print (targetName ^ " was not declared\n");
                    LibrarySupport.reraise exn
                )
        end
    end (* make *)

in
    structure PolyML =
    struct
        open PolyML
        (* We must not have a signature on the result otherwise print and makestring
           will be given polymorphic types and will only produce "?" *)

        val globalNameSpace = globalNameSpace

        val use = use and make = make and shell = shell
        val suffixes = suffixes
        val compiler = polyCompiler

        val prettyPrintWithIDEMarkup = prettyPrintWithIDEMarkup

        structure Compiler =
        struct
            datatype compilerParameters = datatype compilerParameters

            val compilerVersion = Bootstrap.compilerVersion
            val compilerVersionNumber = Bootstrap.compilerVersionNumber

            val forgetSignature: string -> unit = forgetSig
            and forgetStructure: string -> unit = forgetStruct
            and forgetFunctor: string -> unit = forgetFunct
            and forgetValue: string -> unit = forgetVal
            and forgetType: string -> unit = forgetType
            and forgetFixity: string -> unit = forgetFix

            fun signatureNames (): string list = #1(ListPair.unzip (#allSig globalNameSpace ()))
            and structureNames (): string list = #1(ListPair.unzip (#allStruct globalNameSpace ()))
            and functorNames (): string list = #1(ListPair.unzip (#allFunct globalNameSpace ()))
            and valueNames (): string list = #1(ListPair.unzip (#allVal globalNameSpace ()))
            and typeNames (): string list = #1(ListPair.unzip (#allType globalNameSpace ()))
            and fixityNames (): string list = #1(ListPair.unzip (#allFix globalNameSpace ()))

            val prompt1 = prompt1 and prompt2 = prompt2 and profiling = profiling
            and timing = timing and printDepth = printDepth
            and errorDepth = errorDepth and lineLength = lineLength
            and allocationProfiling = allocationProfiling
            
            val assemblyCode = assemblyCode and codetree = codetree
            and codetreeAfterOpt = codetreeAfterOpt and pstackTrace = pstackTrace
            and parsetree = parsetree and reportUnreferencedIds = reportUnreferencedIds
            and lowlevelOptimise = lowlevelOptimise and reportExhaustiveHandlers = reportExhaustiveHandlers
            and narrowOverloadFlexRecord = narrowOverloadFlexRecord
            and createPrintFunctions = createPrintFunctions
            
            val debug = debug
            val inlineFunctors = inlineFunctors
            val maxInlineSize = maxInlineSize
            val printInAlphabeticalOrder = printInAlphabeticalOrder
            val traceCompiler = traceCompiler
        end
        
        (* Debugger control.  Extend DebuggerInterface set up by INITIALISE.  Replaces the original DebuggerInterface. *)
        structure DebuggerInterface =
        struct
            open PolyML.DebuggerInterface
 
            fun debugState(t: Thread.Thread.thread): debugState list =
            let
                val stack = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word(t, 0w5)
                and static = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word(t, 0w6)
                and dynamic = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word(t, 0w7)
                and locationInfo = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word(t, 0w8)

                (* Turn the chain of saved entries along with the current top entry
                   into a list.  The bottom entry will generally be the state from
                   non-debugging code and needs to be filtered out. *)
                fun toList r =
                    if RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short r
                    then []
                    else
                    let
                        val s = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word_immut(r, 0w0)
                        and d = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word_immut(r, 0w1)
                        and l = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word_immut(r, 0w2)
                        and n = RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word_immut(r, 0w3)
                    in
                        if RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short s orelse
                           RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short l
                        then toList n
                        else RunCall.unsafeCast (s, d, l) :: toList n
                    end
            in
                if RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short static orelse
                   RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short locationInfo
                then toList stack
                else RunCall.unsafeCast (static, dynamic, locationInfo) :: toList stack
            end

        end

        structure Debug =
        struct
            local
                open DebuggerInterface

                fun debugLocation(d: debugState): string * PolyML.location =
                    (debugFunction d, DebuggerInterface.debugLocation d)

                fun getStack() = debugState(Thread.Thread.self())
                (* These are only relevant when we are stopped at the debugger but
                   we need to use globals here so that the debug functions such
                   as "variables" and "continue" will work. *)
                val inDebugger = ref false
                (* Current stack and debug level. *)
                val currentStack = ref []
                fun getCurrentStack() =
                    if !inDebugger then !currentStack else raise Fail "Not stopped in debugger"
                val debugLevel = ref 0
                (* Set to true to exit the debug loop.  Set by commands such as "continue". *)
                val exitLoop = ref false
                (* Exception packet sent if this was continueWithEx. *)
                val debugExPacket: exn option ref = ref NONE

                (* Call tracing. *)
                val tracing = ref false
                val breakNext = ref false
                (* Single stepping. *)
                val stepDebug = ref false
                val stepDepth = ref ~1 (* Only break at a stack size less than this. *)
                (* Break points.  We have three breakpoint lists: a list of file-line
                   pairs, a list of function names and a list of exceptions. *)
                val lineBreakPoints = ref []
                and fnBreakPoints = ref []
                and exBreakPoints = ref []

                fun checkLineBreak (file, line) =
                    let
                        fun findBreak [] = false
                         |  findBreak ((f, l) :: rest) =
                              (l = line andalso f = file) orelse findBreak rest
                    in
                        findBreak (! lineBreakPoints)
                    end

                fun checkFnBreak exact name =
                let
                    (* When matching a function name we allow match if the name
                       we're looking for matches the last component of the name
                       we have.  e.g. if we set a break for "f" we match F().S.f . *)
                    fun matchName n =
                        if name = n then true
                        else if exact then false
                        else
                        let
                            val nameLen = size name
                            and nLen = size n
                            fun isSeparator #"-" = true
                             |  isSeparator #")" = true
                             |  isSeparator #"." = true
                             |  isSeparator _    = false
                        in
                            nameLen > nLen andalso String.substring(name, nameLen - nLen, nLen) = n
                            andalso isSeparator(String.sub(name, nameLen - nLen - 1))
                        end
                in
                    List.exists matchName (! fnBreakPoints)
                end

                (* Get the exception id from an exception packet.  The id is
                   the first word in the packet.  It's a mutable so treat it
                   as an int ref here. *)
                fun getExnId(ex: exn): int ref =
                    RunCall.run_call2 RuntimeCalls.POLY_SYS_load_word (ex, 0)
    
                fun checkExnBreak(ex: exn) =
                    let val exnId = getExnId ex in List.exists (fn n => n = exnId) (! exBreakPoints) end

                fun getArgResult stack get =
                    case stack of
                        hd :: _ => Bootstrap.printValue(get hd, !printDepth, globalNameSpace)
                    |   _ => PrettyString "?"

                fun printTrace (funName, location, stack, argsAndResult) =
                let
                    (* This prints a block with the argument and, if we're exiting the result.
                       The function name is decorated with the location.
                       TODO: This works fine so long as the recursion depth is not too deep
                       but once it gets too wide the pretty-printer starts breaking the lines. *)
                    val block =
                        PrettyBlock(0, false, [],
                            [
                                PrettyBreak(length stack, 0),
                                PrettyBlock(0, false, [],
                                [
                                    PrettyBlock(0, false, [ContextLocation location], [PrettyString funName]),
                                    PrettyBreak(1, 3)
                                ] @ argsAndResult)
                            ])
                in
                    prettyPrintWithOptionalMarkup (TextIO.print, !lineLength) block
                end

                (* Try to print the appropriate line from the file.*)
                fun printSourceLine(fileName: string, line: int, funName: string, justLocation) =
                let
                    open TextIO
                    open PolyML
                    (* Use the pretty printer here because that allows us to provide a link to the
                       function in the markup so the IDE can go straight to it. *)
                    val prettyOut = prettyPrintWithOptionalMarkup (printOut, !lineLength)
                    val lineInfo =
                        concat(
                            (if fileName = "" then [] else [fileName, " "]) @
                            (if line = 0 then [] else [" line:", Int.toString line, " "]) @
                            ["function:", funName])
                in
                    (* First just print where we are. *)
                    prettyOut(
                        PrettyBlock(0, true,
                            [ContextLocation{file=fileName,startLine=line, endLine=line,startPosition=0,endPosition=0}],
                            [PrettyString lineInfo]));
                    (* Try to print it.  This may fail if the file name was not a full path
                       name and we're not in the correct directory. *)
                    if justLocation orelse fileName = "" then ()
                    else
                    let
                        val fd = openIn fileName
                        fun pLine n =
                            case inputLine fd of
                                NONE => ()
                            |   SOME s => if n = 1 then printOut s else pLine(n-1)
                    in
                        pLine line;
                        closeIn fd
                    end handle IO.Io _ => () (* If it failed simply ignore the error. *)
                end

                (* These functions are installed as global callbacks if necessary. *)
                fun onEntry (funName, location as {file, startLine, ...}: PolyML.location) =
                (
                    if ! tracing
                    then
                    let
                        val stack = getStack()
                        val arg = getArgResult stack debugFunctionArg
                    in
                        printTrace(funName, location, stack, [arg])
                    end
                    else ();
                    (* We don't actually break here because at this stage we don't
                       have any variables declared. *)
                    (* TODO: If for whatever reason we fail to find the breakpoint we need to cancel
                       the pending break in the exit code.  Otherwise we could try and break
                       in some other code. *)
                    if checkLineBreak (file, startLine) orelse checkFnBreak false funName
                    then (breakNext := true; setOnBreakPoint(SOME onBreakPoint))
                    else ()
                )
                
                and onExit (funName, location) =
                (
                    if ! tracing
                    then
                    let
                        val stack = getStack()
                        val arg = getArgResult stack debugFunctionArg
                        val res = getArgResult stack debugFunctionResult
                    in
                        printTrace(funName, location, stack,
                            [arg, PrettyBreak(1, 3), PrettyString "=", PrettyBreak(1, 3), res])
                    end
                    else ()
                )

                and onExitException(funName, location) exn =
                (
                    if ! tracing
                    then
                    let
                        val stack = getStack()
                        val arg = getArgResult stack debugFunctionArg
                    in
                        printTrace(funName, location, stack,
                            [arg, PrettyBreak(1, 3), PrettyString "=", PrettyBreak(1, 3),
                             PrettyString "raised", PrettyBreak(1, 3), PrettyString(exnName exn)])
                    end
                    else ();
                    if checkExnBreak exn
                    then enterDebugger ()
                    else ()
                )
                
                and onBreakPoint({file, startLine, ...}: PolyML.location, _) =
                (
                    if (!stepDebug andalso (!stepDepth < 0 orelse List.length(getStack()) <= !stepDepth)) orelse
                       checkLineBreak (file, startLine) orelse ! breakNext
                    then enterDebugger ()
                    else () 
                )
                
                (* Set the callbacks except if we're stopped in the debugger. *)
                and setCallBacks () =
                if ! inDebugger
                then ()
                else
                (
                    setOnEntry(if !tracing orelse not(null(! fnBreakPoints)) then SOME onEntry else NONE);
                    setOnExit(if !tracing then SOME onExit else NONE);
                    setOnExitException(if !tracing orelse not(null(! exBreakPoints)) then SOME onExitException else NONE);
                    setOnBreakPoint(if !tracing orelse ! stepDebug orelse not(null(! lineBreakPoints)) then SOME onBreakPoint else NONE)
                )

                and enterDebugger () =
                let
                    (* Clear the onXXX functions to prevent any recursion. *)
                    val () = setOnEntry NONE and () = setOnExit NONE
                    and () = setOnExitException NONE and () = setOnBreakPoint NONE
                    val () = inDebugger := true
                    (* Remove any type-ahead. *)
                    fun flushInput () =
                        case TextIO.canInput(TextIO.stdIn, 1) of
                            SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
                        |   _ => ()
                    val () = flushInput ()

                    val () = exitLoop := false
                    val () = debugLevel := 0
                    val () = breakNext := false
                    (* Save the stack on entry.  If we execute any code with
                       debugging enabled while we're in the debugger we could
                       change this. *)
                    val () = currentStack := getStack()
 
                    val () =
                        case !currentStack of
                            hd :: _ =>
                                let
                                    val (funName, {file, startLine, ...}) = debugLocation hd
                                in
                                    printSourceLine(file, startLine, funName, false)
                                end
                        |   [] => () (* Shouldn't happen. *)

                    val compositeNameSpace =
                    (* Compose any debugEnv with the global environment.  Create a new temporary environment
                       to contain any bindings made within the shell.  They are discarded when we continue
                       from the break-point.  Previously, bindings were made in the global environment but
                       that is problematic.  It is possible to capture local types in the bindings which
                       could actually be different at the next breakpoint. *)
                    let
                        val fixTab = ProtectedTable.create() and sigTab = ProtectedTable.create()
                        and valTab = ProtectedTable.create() and typTab = ProtectedTable.create()
                        and fncTab = ProtectedTable.create() and strTab = ProtectedTable.create()
                        (* The debugging environment depends on the currently selected stack frame. *)
                        fun debugEnv() = debugNameSpace (List.nth(!currentStack, !debugLevel))
                        fun dolookup f t s =
                            case ProtectedTable.lookup t s of NONE => (case f (debugEnv()) s of NONE => f globalNameSpace s | v => v) | v => v
                        fun getAll f t () = ProtectedTable.all t () @ f (debugEnv()) () @ f globalNameSpace ()
                    in
                        {
                        lookupFix    = dolookup #lookupFix fixTab,
                        lookupSig    = dolookup #lookupSig sigTab,
                        lookupVal    = dolookup #lookupVal valTab,
                        lookupType   = dolookup #lookupType typTab,
                        lookupFunct  = dolookup #lookupFunct fncTab,
                        lookupStruct = dolookup #lookupStruct strTab,
                        enterFix     = ProtectedTable.enter fixTab,
                        enterSig     = ProtectedTable.enter sigTab,
                        enterVal     = ProtectedTable.enter valTab,
                        enterType    = ProtectedTable.enter typTab,
                        enterFunct   = ProtectedTable.enter fncTab,
                        enterStruct  = ProtectedTable.enter strTab,
                        allFix       = getAll #allFix fixTab,
                        allSig       = getAll #allSig sigTab,
                        allVal       = getAll #allVal valTab,
                        allType      = getAll #allType typTab,
                        allFunct     = getAll #allFunct fncTab,
                        allStruct    = getAll #allStruct strTab
                        }
                    end
                in
                    topLevel
                        { isDebug = true, nameSpace = compositeNameSpace, exitLoop = fn _ => ! exitLoop,
                          exitOnError = false, isInteractive = true };

                    inDebugger := false;
                    setCallBacks();

                    (* If this was continueWithEx raise the exception. *)
                    case ! debugExPacket of
                        NONE => ()
                    |   SOME exn => (debugExPacket := NONE; raise exn)
                end
            in
                (* Functions that are only relevant when called from the debugger.  These
                   check the debugging state using getCurrentStack which raises an
                   exception if we're not in the debugger. *)
                (* "step" causes the debugger to be entered on the next call.
                   "stepOver" enters the debugger on the next call when the stack is no larger
                   than it is at present.
                   "stepOut" enters the debugger on the next call when the stack is smaller
                   than it is at present. *)
                fun step () = 
                let
                    val _ = getCurrentStack()
                in
                    stepDebug := true; stepDepth := ~1; exitLoop := true
                end

                and stepOver() =
                let
                    val stack = getCurrentStack()
                in
                    stepDebug := true; stepDepth := List.length stack; exitLoop := true
                end

                and stepOut() =
                let
                    val stack = getCurrentStack()
                in
                    stepDebug := true; stepDepth := List.length stack - 1; exitLoop := true
                end

                and continue () =
                let
                    val _ = getCurrentStack()
                in
                    stepDebug := false; stepDepth := ~1; exitLoop := true
                end

                and continueWithEx exn =
                let
                    val _ = getCurrentStack()
                in
                    stepDebug := false; stepDepth := ~1; exitLoop := true; debugExPacket := SOME exn
                end

                (* Stack traversal. *)
                fun up () =
                let
                    val stack = getCurrentStack()
                in
                    if !debugLevel < List.length stack -1
                    then
                    let
                        val _ = debugLevel := !debugLevel + 1;
                        val (funName, {startLine, file, ...}) =
                            debugLocation(List.nth(stack, !debugLevel))
                    in
                        printSourceLine(file, startLine, funName, false)
                    end
                    else TextIO.print "Top of stack.\n"
                end
        
                and down () =
                let
                    val stack = getCurrentStack()
                in
                    if !debugLevel = 0
                    then TextIO.print "Bottom of stack.\n"
                    else
                    let
                        val () = debugLevel := !debugLevel - 1;
                        val (funName, {startLine, file, ...}) =
                            debugLocation(List.nth(stack, !debugLevel))
                    in
                        printSourceLine(file, startLine, funName, false)
                    end
                end

                (* Just print the functions without any other context. *)
                fun stack () : unit =
                let
                    fun printTrace d =
                    let
                        val (funName, {file, startLine, ...}) = debugLocation d
                    in
                        printSourceLine(file, startLine, funName, true)
                    end
                in
                    List.app printTrace (getCurrentStack())
                end

                local
                    fun printVal v =
                        prettyPrintWithOptionalMarkup(TextIO.print, !lineLength)
                            (NameSpace.displayVal(v, !printDepth, globalNameSpace))
                    fun printStack (stack: debugState) =
                        List.app (fn (_,v) => printVal v) (#allVal (debugNameSpace stack) ())
                in
                    (* Print all variables at the current level. *)
                    fun variables() = printStack (List.nth(getCurrentStack(), !debugLevel))
                    (* Print all the levels. *)
                    and dump() =
                    let
                        fun printLevel stack =
                        let
                            val (funName, _) = debugLocation stack
                        in
                            TextIO.print(concat["Function ", funName, ":"]);
                            printStack stack;
                            TextIO.print "\n"
                        end
                    in
                        List.app printLevel (getCurrentStack())
                    end
                end

                (* Functions to adjust tracing and breakpointing.  May be called
                   either within or outside the debugger. *)
                fun trace b = (tracing := b; setCallBacks ())

                fun breakAt (file, line) =
                    if checkLineBreak(file, line) then () (* Already there. *)
                    else (lineBreakPoints := (file, line) :: ! lineBreakPoints; setCallBacks ())
        
                fun clearAt (file, line) =
                let
                    fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                     |  findBreak ((f, l) :: rest) =
                          if l = line andalso f = file
                          then rest else (f, l) :: findBreak rest
                in
                    lineBreakPoints := findBreak (! lineBreakPoints);
                    setCallBacks ()
                end
         
                fun breakIn name =
                    if checkFnBreak true name then () (* Already there. *)
                    else (fnBreakPoints := name :: ! fnBreakPoints; setCallBacks ())
        
                fun clearIn name =
                let
                    fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                     |  findBreak (n :: rest) =
                          if name = n then rest else n :: findBreak rest
                in
                    fnBreakPoints := findBreak (! fnBreakPoints);
                    setCallBacks ()
                end

                fun breakEx exn =
                    if checkExnBreak exn then  () (* Already there. *)
                    else (exBreakPoints := getExnId exn :: ! exBreakPoints; setCallBacks ())

                fun clearEx exn =
                let
                    val exnId = getExnId exn
                    fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                     |  findBreak (n :: rest) =
                          if exnId = n then rest else n :: findBreak rest
                in
                    exBreakPoints := findBreak (! exBreakPoints);
                    setCallBacks ()
                end

            end
        end
        
        structure CodeTree =
        struct
            open PolyML.CodeTree
            (* Add options to the code-generation phase. *)
            val genCode =
                fn (code, numLocals) =>
                let
                    open Bootstrap Bootstrap.Universal
                    val compilerOut = prettyPrintWithOptionalMarkup(TextIO.print, !lineLength)
                in
                    genCode(code,
                        [
                            tagInject compilerOutputTag compilerOut,
                            tagInject maxInlineSizeTag (! maxInlineSize),
                            tagInject codetreeTag (! codetree),
                            tagInject pstackTraceTag (! pstackTrace),
                            tagInject lowlevelOptimiseTag (! lowlevelOptimise),
                            tagInject assemblyCodeTag (! assemblyCode),
                            tagInject codetreeAfterOptTag (! codetreeAfterOpt)
                        ], numLocals)
                end

            (* For simplicity these are given types with string rather than
               Word8Vector.vector in INITIALISE. *)
            val decodeBinary = decodeBinary o Byte.bytesToString
            and encodeBinary = Byte.stringToBytes o encodeBinary
        end

        (* Original print_depth etc functions. *)
        fun profiling   i = Compiler.profiling := i
        and timing      b = Compiler.timing := b
        and print_depth i = Compiler.printDepth := i
        and error_depth i = Compiler.errorDepth := i
        and line_length i = Compiler.lineLength := i
    end
end (* PolyML. *);
