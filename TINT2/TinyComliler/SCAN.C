/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the TINY compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"

/* states in scanner DFA */
/*typedef enum
   { START,INASSIGN,INCOMMENT,INNUM,INID,DONE }
   StateType;*/

/* lexeme of identifier or reserved word */
typedef enum
{
	START,
	INEQUAL,
	INGT,
	INLT,
	INEXCLA,
	INCOMMENTORDIV,
	OUTCOMMENT,
	INCOMMENT1,
	INCOMMENT2,
	INNUM,
	INID,
	INADD,
	INSUB,
	INMUL,
	INDIV,
	DONE
} StateType;
char tokenString[MAXTOKENLEN+1];

/* BUFLEN = length of the input buffer for
   source code lines */
#define BUFLEN 256

static char lineBuf[BUFLEN]; /* holds the current line */
static int linepos = 0; /* current position in LineBuf */
static int bufsize = 0; /* current size of buffer string */
static int EOF_flag = FALSE; /* corrects ungetNextChar behavior on EOF */

/* getNextChar fetches the next non-blank character
   from lineBuf, reading in a new line if lineBuf is
   exhausted */
static int getNextChar(void)
{ if (!(linepos < bufsize))
  { lineno++;
    if (fgets(lineBuf,BUFLEN-1,source))
    { if (EchoSource) fprintf(listing,"%4d: %s",lineno,lineBuf);
      bufsize = strlen(lineBuf);
      linepos = 0;
      return lineBuf[linepos++];
    }
    else
    { EOF_flag = TRUE;
      return EOF;
    }
  }
  else return lineBuf[linepos++];
}

/* ungetNextChar backtracks one character
   in lineBuf */
static void ungetNextChar(void)
{ if (!EOF_flag) linepos-- ;}

/* lookup table of reserved words */
/*static struct
    { char* str;
      TokenType tok;
    } reservedWords[MAXRESERVED]
   = {{"if",IF},{"then",THEN},{"else",ELSE},{"end",END},
      {"repeat",REPEAT},{"until",UNTIL},{"read",READ},
      {"write",WRITE}};*/
static struct
{
	char *str;
	TokenType tok;
} reservedWords[MAXRESERVED] = { { "if", IF },{ "else", ELSE },{ "int", INT },{"float",FLOAT}, { "void", VOID },{ "return", RETURN },{ "while", WHILE },{"for",FOR} };


/* lookup an identifier to see if it is a reserved word */
/* uses linear search */
static TokenType reservedLookup (char * s)
{ int i;
  for (i=0;i<MAXRESERVED;i++)
    if (!strcmp(s,reservedWords[i].str))
      return reservedWords[i].tok;
  return ID;
}

/****************************************/
/* the primary function of the scanner  */
/****************************************/
/* function getToken returns the 
 * next token in source file
 */
TokenType getToken(void)
{  /* index for storing into tokenString */
   int tokenStringIndex = 0;
   /* holds current token to be returned */
   TokenType currentToken;
   /* current state - always begins at START */
   StateType state = START;
   /* flag to indicate save to tokenString */
   int save;
   while (state != DONE)
   {
	   int c = getNextChar();
	   save = TRUE;
	   switch (state)
	   {
	   case START:
		   if (isdigit(c))
			   state = INNUM;
		   else if (isalpha(c))
			   state = INID;
		   else if (c == '=')
			   state = INEQUAL;
		   else if ((c == ' ') || (c == '\t') || (c == '\n'))
			   save = FALSE;
		   else if (c == '/')
		   {
			   save = FALSE;
			   state = INCOMMENTORDIV;
		   }
		   else if (c == '>')
		   {
			   state = INGT;//大于
		   }
		   else if (c == '<')
		   {
			   state = INLT;
		   }
		   else if (c == '!')
		   {
			   state = INEXCLA;
		   }

		   //后面是加的目的是实现+= *= -=
		   else if (c == '+')
		   {
			   state = INADD;
		   }
		   else if (c == '-')
		   {
			   state = INSUB;
		   }
		   else if (c == '*')
		   {
			   state = INMUL;
		   }
		   else
		   {
			   state = DONE;
			   switch (c)
			   {
			   case EOF:
				   save = FALSE;
				   currentToken = ENDFILE;
				   break;
			   /*case '+':
				   currentToken = IPLUS;
				   break;
			   case '-':
				   currentToken = SUB;
				   break;
			   case '*':
				   currentToken = MUL;
				   break;*/
			   case '(':
				   currentToken = LPAREN;
				   break;
			   case ')':
				   currentToken = RPAREN;
				   break;
			   case ';':
				   currentToken = SEMI;
				   break;
			   case '[':
				   currentToken = LBRACKET;
				   break;
			   case ']':
				   currentToken = RBRACKET;
				   break;
			   case '{':
				   currentToken = LBRACE;
				   break;
			   case '}':
				   currentToken = RBRACE;
				   break;
			   case ',':
				   currentToken = COMMA;
				   break;
			   default:
				   currentToken = ERROR;
				   break;
			   }
		   }
		   break;
	   case INCOMMENTORDIV: // 注释
		   save = FALSE;
		   if (c == '*')
		   {
			   state = INCOMMENT1;
		   }
		   else if (c == '/')
		   {
			   state = INCOMMENT2;
		   }
		   else
		   {
			   ungetNextChar();
			   tokenString[tokenStringIndex++] = (char)'/';
			   //currentToken = DIV;BY JJ 6.7
			   state = INDIV;
		   }
		   break;
	   case INCOMMENT1:
		   save = FALSE;
		   if (c == '*')
		   {
			   state = OUTCOMMENT;
		   }
		   else if (c == EOF)
		   {
			   state = DONE;
			   currentToken = ERRORENDFILE;
		   }
		   break;
	   case INCOMMENT2:
		   save = FALSE;
		   if (c == '/n')
		   {
			   state = START;
		   }
		   else if (c == EOF)
		   {
			   state = DONE;
			   currentToken = ERRORENDFILE;
		   }
		   break;
	   case OUTCOMMENT:
		   save = FALSE;
		   if (c == '/')
		   {
			   state = START;
		   }
		   else if (c == EOF)
		   {
			   state = DONE;
			   currentToken = ERRORENDFILE;
		   }
		   else
		   {
			   state = INCOMMENT1;
		   }
		   break;
	   case INEQUAL: // 赋值或相等
		   state = DONE;
		   if (c == '=')
			   currentToken = EQ;
		   else
		   { /* backup in the input */
			   save = FALSE;
			   ungetNextChar();
			   currentToken = ASSIGN;
		   }
		   break;
	   case INNUM: // 数字
		   if (!isdigit(c))
		   { /* backup in the input */
			   ungetNextChar();
			   save = FALSE;
			   state = DONE;
			   currentToken = NUM;
		   }
		   break;
	   case INID: // 标识符
		   if (!isalpha(c))
		   { /* backup in the input */
			   ungetNextChar();
			   save = FALSE;
			   state = DONE;
			   currentToken = ID;
		   }
		   break;
	   case INGT:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = GE;
		   }
		   else
		   {
			   ungetNextChar();
			   currentToken = GT;
		   }
		   break;
	   case INLT:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = LE;
		   }
		   else
		   {
			   ungetNextChar();
			   currentToken = LT;
		   }
		   break;
	   case INEXCLA:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = NE;
		   }
		   else
		   {
			   ungetNextChar();
			   save = TRUE;
			   currentToken = ERROR;
		   }
		   break;
	   case INADD:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = SELFADD;
		   }
		   else
		   {
			   ungetNextChar();
			   save = TRUE;
			   currentToken = ADD;
		   }
		   break;
	   case INSUB:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = SELFSUB;
		   }
		   else
		   {
			   ungetNextChar();
			   save = TRUE;
			   currentToken = SUB;
		   }
		   break;
	   case INMUL:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = SELFMUL;
		   }
		   else
		   {
			   ungetNextChar();
			   save = TRUE;
			   currentToken = MUL;
		   }
		   break;
	   case INDIV:
		   state = DONE;
		   if (c == '=')
		   {
			   save = TRUE;
			   currentToken = SELFDIV;
		   }
		   else
		   {
			   ungetNextChar();
			   save = TRUE;
			   currentToken = DIV;
		   }
		   break;
	   case DONE:
	   default: /* should never happen */
		   fprintf(listing, "Scanner Bug: state= %d\n", state);
		   state = DONE;
		   currentToken = ERROR;
		   break;
	   }

	   if ((save) && (tokenStringIndex <= MAXTOKENLEN))
		   tokenString[tokenStringIndex++] = (char)c;
	   if (state == DONE)
	   {
		   tokenString[tokenStringIndex] = '\0';
		   if (currentToken == ID)
			   // 查看是否为保留字
			   currentToken = reservedLookup(tokenString);
	   }
   }
   if (TraceScan)
   {
	   fprintf(listing, "\t%d: ", lineno);
	   printToken(currentToken, tokenString);
   }
   return currentToken;
} /* end getToken */

