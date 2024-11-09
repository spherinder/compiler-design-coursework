%{
open Ast

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

/* Declare your tokens here. */
%token EOF
%token <int64>  INT
%token NULL
%token <string> STRING
%token <string> IDENT

%token TINT     /* int */
%token TVOID    /* void */
%token TSTRING  /* string */
%token TBOOL    /* bool */
%token IF       /* if */
%token ELSE     /* else */
%token WHILE    /* while */
%token RETURN   /* return */
%token VAR      /* var */
%token SEMI     /* ; */
%token COMMA    /* , */
%token LBRACE   /* { */
%token RBRACE   /* } */
%token PLUS     /* + */
%token DASH     /* - */
%token STAR     /* * */
%token EQEQ     /* == */
%token EQ       /* = */
%token LPAREN   /* ( */
%token RPAREN   /* ) */
%token LBRACKET /* [ */
%token RBRACKET /* ] */
%token TILDE    /* ~ */
%token BANG     /* ! */
%token GLOBAL   /* global */
%token FOR      /* for */
%token TRUE     /* true */
%token FALSE    /* false */
%token LT       /* < */
%token GT       /* > */
%token LTE      /* <= */
%token GTE      /* >= */
%token NEQ      /* != */
%token SHL      /* << */
%token SHR      /* >> */
%token SAR      /* >>> */
%token AND      /* & */
%token OR       /* | */
%token IAND     /* [&] */
%token IOR      /* [|] */
%token NEW      /* new */
%token ARR_BR   /* [] */

/* order matters! */
%left IOR
%left IAND
%left OR
%left AND
%left NEQ EQEQ
%left LT LTE GT GTE
%left SHL SHR SAR
%left PLUS DASH
%left STAR

%nonassoc BANG
%nonassoc TILDE
%nonassoc LBRACKET
%nonassoc LPAREN

/* ---------------------------------------------------------------------- */

%start prog
%start exp_top
%start stmt_top
%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.prog> prog
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp EOF { e }

stmt_top:
  | s=stmt EOF { s }

prog:
  | p=list(decl) EOF  { p }

decl:
  | GLOBAL name=IDENT EQ init=gexp SEMI
    { Gvdecl (loc $startpos $endpos { name; init }) }
  | frtyp=ret_ty fname=IDENT LPAREN args=arglist RPAREN body=block
    { Gfdecl (loc $startpos $endpos { frtyp; fname; args; body }) }

arglist:
  | l=separated_list(COMMA, pair(ty,IDENT)) { l }
    
ty:
  | TINT   { TInt }
  | TBOOL  { TBool }
  | r=rtyp { TRef r } 

%inline ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

%inline rtyp:
  | TSTRING { RString }
  | t=ty ARR_BR { RArray t }

%inline bop:
  | STAR   { Mul }
  | PLUS   { Add }
  | DASH   { Sub }
  | SHL    { Shl }
  | SHR    { Shr }
  | SAR    { Sar }
  | LT     { Lt }
  | LTE    { Lte }
  | GT     { Gt }
  | GTE    { Gte }
  | EQEQ   { Eq }
  | NEQ    { Neq }
  | AND    { And }
  | OR     { Or }
  | IAND   { IAnd }
  | IOR    { IOr }

%inline uop:
  | DASH  { Neg }
  | BANG  { Lognot }
  | TILDE { Bitnot }

gexp:
  | t=rtyp NULL  { loc $startpos $endpos @@ CNull t }
  | i=INT        { loc $startpos $endpos @@ CInt i }
  | TRUE         { loc $startpos $endpos @@ CBool true }
  | FALSE        { loc $startpos $endpos @@ CBool false }
  | s=STRING     { loc $startpos $endpos @@ CStr s }
  | NEW t=ty ARR_BR LBRACE es=separated_list(COMMA, gexp) RBRACE
                 { loc $startpos $endpos @@ CArr (t,es) }
lhs:  
  | id=IDENT           { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }

exp:
  | i=INT               { loc $startpos $endpos @@ CInt i }
  | t=rtyp NULL           { loc $startpos $endpos @@ CNull t }
  | e1=exp b=bop e2=exp { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=uop e=exp         { loc $startpos $endpos @@ Uop (u, e) }
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN
                        { loc $startpos $endpos @@ Call (e,es) }
  | LPAREN e=exp RPAREN { e }
  | TRUE                { loc $startpos $endpos @@ CBool true }
  | FALSE               { loc $startpos $endpos @@ CBool false }
  | s=STRING            { loc $startpos $endpos @@ CStr s }
  | NEW t=ty ARR_BR LBRACE es=separated_list(COMMA, exp) RBRACE
                        { loc $startpos $endpos @@ CArr (t,es) }
  | NEW TINT LBRACKET e=exp RBRACKET
                        { loc $startpos $endpos @@ NewArr (TInt,e) }
  | NEW TBOOL LBRACKET e=exp RBRACKET
                        { loc $startpos $endpos @@ NewArr (TBool,e) }

vdecl: 
  | VAR id=IDENT EQ init=exp { (id, init) }

stmt: 
  | d=vdecl SEMI        { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp SEMI { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN SEMI
                        { loc $startpos $endpos @@ SCall (e, es) }
  | ifs=if_stmt         { ifs }
  | RETURN SEMI         { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp SEMI   { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp RPAREN b=block  
                        { loc $startpos $endpos @@ While(e, b) }
  | FOR LPAREN vs=separated_list(COMMA, vdecl) SEMI e=exp? SEMI s=stmt? RPAREN b=block
                        { loc $startpos $endpos @@ For(vs, e, s, b) }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp RPAREN b1=block b2=else_stmt
    { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }

/* %% */
/* let print_token = _menhir_print_token */