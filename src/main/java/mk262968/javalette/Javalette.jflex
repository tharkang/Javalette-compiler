package mk262968.javalette.ast;
import java_cup.runtime.Symbol;

%%

%public
%class JavaletteLexer
%cup
%type java_cup.runtime.Symbol
%eofval{
  return new java_cup.runtime.Symbol(EOF);
%eofval}
%eofclose
%implements JavaletteConstants
%extends JavaletteSym
%line
%column

%{
  StringBuffer string = new StringBuffer();

  private Symbol symbol(int sym) {
    return new Symbol(sym, yyline+1, yycolumn+1);
  }
  
  private Symbol symbol(int sym, Object val) {
    return new Symbol(sym, yyline+1, yycolumn+1, val);
  }
  
  private void error(String message) {
    System.out.println("Error at line "+(yyline+1)+", column "+(yycolumn+1)+" : "+message);
  }
%} 


LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}

TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment     = "//" {InputCharacter}* {LineTerminator}

Identifier = [:jletter:] [:jletterdigit:]*

DecIntegerLiteral = 0 | [1-9][0-9]*
/*DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?*/
DoubleLiteral = ({FLit1}|{FLit2}) {Exponent}?

FLit1    = [0-9]+ \. [0-9]* 
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 
Exponent = [eE] [+-]? [0-9]+
%state STRING

%%
 /* keywords */
<YYINITIAL> "while"              { return symbol(WHILE); }
<YYINITIAL> "for"                { return symbol(FOR); }
<YYINITIAL> "if"                 { return symbol(IF); }
<YYINITIAL> "else"               { return symbol(ELSE); }
<YYINITIAL> "return"             { return symbol(RETURN); }
<YYINITIAL> "true"               { return symbol(TRUE); }
<YYINITIAL> "false"              { return symbol(FALSE); }
<YYINITIAL> {
  /* identifiers */ 
  {Identifier}                   { return symbol(IDENT, yytext()); }
 
  /* literals */
  {DoubleLiteral}            { return symbol(DOUBLE_LITERAL, new Double(Double.parseDouble(yytext()))); }
  {DecIntegerLiteral}            { return symbol(INTEGER_LITERAL, new Integer(Integer.parseInt(yytext()))); }
  \"                             { string.setLength(0); yybegin(STRING); }

  /* operators */
  "||"                           { return symbol(LOGOR); }
  "&&"                           { return symbol(LOGAND); }
  "=="                           { return symbol(COMPOP, "=="); }
  "<="                           { return symbol(COMPOP, "<="); }
  ">="                           { return symbol(COMPOP, ">="); }
  "!="                           { return symbol(COMPOP, "!="); }
  "++"                           { return symbol(INCR); }
  "--"                           { return symbol(DECR); }
  "="                            { return symbol(EQ); }
  "+"                            { return symbol(ADDOP, "+"); }
  "-"                            { return symbol(ADDOP, "-"); }
  "*"                            { return symbol(MULOP, "*"); }
  "/"                            { return symbol(MULOP, "/"); }
  "%"                            { return symbol(MULOP, "%"); }
  "<"                            { return symbol(COMPOP, "<"); }
  ">"                            { return symbol(COMPOP, ">"); }
  "("                            { return symbol(LPAR); }
  ")"                            { return symbol(RPAR); }
  "{"                            { return symbol(BEGIN); }
  "}"                            { return symbol(END); }
  "!"                            { return symbol(LOGNOT); }
  ";"                            { return symbol(SEMICOLON); }
  ","                            { return symbol(COMMA); }

  /* comments */
  {Comment}                      { /* ignore */ }
 
  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}
<STRING> {
\"                             { yybegin(YYINITIAL); 
                                   return symbol(STRING_LITERAL, 
                                   string.toString()); }
  [^\n\r\"\\]+                   { string.append( yytext() ); }
  \\t                            { string.append('\t'); }
  \\n                            { string.append('\n'); }

  \\r                            { string.append('\r'); }
  \\\"                           { string.append('\"'); }
  \\                             { string.append('\\'); }
}

/* error fallback */
.|\n                             { throw new LexerError("Illegal character <"+ yytext()+">", yyline+1, yycolumn+1); }
