exception ParseExn of string
  datatype jsonvalue = 
    Objectoj of (string * jsonvalue) list
  | Arrayoj of jsonvalue list
  | Numberoj of Real64.real
  | Stringoj of string
  | Booloj of bool
  | Nulloj
  val currentPos = ref 0

  fun eatExpect(s,ins,posr) = (let
    val ls = String.size(s);
    val ext = String.extract(ins,!posr,SOME ls);
  in
    if String.compare(s,ext)=EQUAL then 
      posr := !posr + ls
    else
      raise ParseExn ("Expecting : \""^s^"\",but got : "^ext)
  end) 
    handle Subscript => raise ParseExn ("Expecting : \""^s^"\",but met with eof")
  
  fun eatSpaces(ins,posr) = let
    val c = String.sub (ins,!posr)
  in
    case c of
      (#" " | #"\n" | #"\t" | #"\r") => (posr := !posr + 1;eatSpaces(ins,posr))
    | _ => ()
  end
  handle Subscript => raise ParseExn ("Eating spaces,EOF reached")

  fun parseNull (ins,posr) =
    (eatSpaces(ins,posr);
     eatExpect("null",ins,posr);
     Nulloj)

  fun parseBool (ins,posr) =
    (eatSpaces(ins,posr); 
      case String.sub (ins,!posr) of
        #"t" => (eatExpect("true",ins,posr);
          Booloj true)
      | #"f" => (eatExpect("false",ins,posr);
          Booloj false)
      | _ => raise ParseExn("Expecting bool,but get prefix : "^
         String.str(String.sub (ins,!posr))))

  fun parseNum (ins,posr) = let
    val n = ref (!posr);
    fun s i = String.sub(ins,i);
    fun go (n) = (
      if s(!n) = #"-" then n := !n + 1
      else ()
      ;
      if s(!n) = #"0" then n := !n + 1
      else while (Char.isDigit (s(!n))) do
            (n := !n + 1)
      ;
      if s(!n) = #"." then (
        n := !n + 1;
        while (Char.isDigit (s(!n))) do
            (n := !n + 1))
      else ()
      ;
      if (s(!n) = #"e" orelse s(!n) = #"E") then (
        n := !n + 1;
        if (s(!n) = #"+" orelse s(!n) = #"-") then
          n := !n + 1
        else ();
        while (Char.isDigit (s(!n))) do
            (n := !n + 1)
      )
      else ()
      )
  in(
    (eatSpaces(ins,posr);go(n);raise Subscript) handle Subscript =>
    case Real64.fromString(
      String.extract(ins,!posr,SOME(!n - !posr))) of
      SOME r => (posr:= !n;Numberoj r)
    | NONE => raise ParseExn ("Parsing Illegal number")
  )
  end

  fun parseStr (ins,posr) = let
    val n = ref(!posr)
    val l = ref []
    fun s i = String.sub(ins,i)    
  in
    (eatSpaces(ins,n);
    if (s(!n)  = #"\"") then(
      n := !n + 1;
      while ((not (s(!n) = #"\"")) andalso
             (not (Char.isCntrl(s(!n) )))) do(
        if(not (s(!n)  = #"\\")) then (l := (s(!n) ::(!l));n := !n + 1)
        else(
          n := !n + 1;
          case s(!n) of
            ((#"\"") | (#"/") | (#"\\")) =>
              (l := (s(!n)::(!l)))    
          | #"b" => (l := (#"\b"::(!l)))
          | #"f" => (l := (#"\f"::(!l)))
          | #"n" => (l := (#"\n"::(!l)))
          | #"r" => (l := (#"\r"::(!l)))
          | #"t" => (l := (#"\t"::(!l)))
          | #"u" => (l := (#"u" :: #"\\" ::(!l)))
          | _ => raise ParseExn("Unknown char after \\");
          n := !n + 1               
        )
      );
      if Char.isCntrl(s(!n)) then 
        raise ParseExn ("Cntrl char found when parsing Str")
      else(
        posr := !n + 1;
        Stringoj (String.implode(List.rev (!l)))
      )
    )
    else raise ParseExn ("Parsing Str.Str not found"))
  end
  handle Subscript => raise ParseExn ("Parsing Str,but met with eof")
    
  fun parseArr(ins,posr) = let 
    val n = ref(!posr)
    val l = ref []
    fun s i = String.sub(ins,i)     
  in(
    eatSpaces(ins,n);
    if(s(!n) = #"[") then(
      n := !n + 1;
      eatSpaces(ins,n);    
      while(not (s(!n) = #"]")) do(
        eatSpaces(ins,n);
        l := ((parsejson(ins,n))::(!l));
        eatSpaces(ins,n);
        if(s(!n) = #",") then n := !n + 1 
        else if (not (s(!n) = #"]")) then
              raise ParseExn ("Parsing Arr,expecting ,")
             else ()
      );      
      posr := !n + 1; (*step over Arr *)
      Arrayoj (List.rev(!l))
    )
    else raise ParseExn ("Parsing Arr.Arr not found")
  )
  end
  handle Subscript => raise ParseExn ("Parsing Arr,but met with eof")
and
  parseObj(ins,posr) = let 
    val n = ref(!posr)
    val l = ref []
    fun s i = String.sub(ins,i)
    val str = ref (Stringoj "")
    val relstr = ref ""
    val obj = ref Nulloj     
  in(
    eatSpaces(ins,n);
    if(s(!n) = #"{") then(
      n := !n + 1;
      eatSpaces(ins,n);
      while(not (s(!n) = #"}")) do(
        eatSpaces(ins,n);
        str := parsejson(ins,n);
        case (!str) of
          Stringoj s => relstr := s
        | _ => (raise ParseExn ("Parsing Obj,expecting string,but got unexpected json element");());
        eatSpaces(ins,n);
        if(s(!n) = #":") then n := !n + 1
        else raise ParseExn ("Parsing Obj,expecting :,but got : "^String.str(s(!n)));
        eatSpaces(ins,n);
        obj := parsejson(ins,n);
        l := ((!relstr,!obj)::(!l));
        eatSpaces(ins,n);
        if(s(!n) = #",") then n := !n + 1 else ()
      );      
      posr := !n + 1; (*step over Arr *)
      Objectoj (!l)
    )
    else raise ParseExn ("Parsing Arr.Arr not found")
  )
  end
  handle Subscript => raise ParseExn ("Parsing Obj,but met with eof")
and
  parsejson(ins,posr) = let
    val len = String.size(ins)  
  in
    if (ins = "" orelse (!posr) = len) then raise ParseExn ("Parsing json,EOF reached")
    else (        
        eatSpaces(ins,posr);
        case (String.sub(ins,(!posr))) of 
          #"n" => parseNull(ins,posr)
        | (#"t" | #"f") => parseBool(ins,posr)
        | #"\"" => parseStr(ins,posr)
        | #"[" => parseArr(ins,posr)
        | #"{" => parseObj(ins,posr)
        | ( #"-" | #"0" | #"1" | #"2" | #"3" | #"4" 
          | #"5" | #"6" | #"7" | #"8" | #"9" ) => parseNum(ins,posr)
    )    
  end
  handle ParseExn s => (print (s^"\n");Nulloj)
  
  fun test(s) = let
    val file = TextIO.openIn(s);
    val res = TextIO.inputAll(file);
    val jsres = parsejson(res,ref 0)
  in
    jsres
  end
