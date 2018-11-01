/* fichier: "petit-comp.c" */

/* Un petit compilateur et machine virtuelle pour un sous-ensemble de C.  */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <setjmp.h>

/*---------------------------------------------------------------------------*/

/* Analyseur lexical. */

jmp_buf env;

enum { DO_SYM, ELSE_SYM, IF_SYM, WHILE_SYM, PRINT_SYM, GOTO_SYM, CONTINUE, BREAK_SYM, LBRA, RBRA, LPAR, 
       RPAR, PLUS, MINUS, MULTIPLY, DIVIDE, MODULO, LESS, LESS_EQ, SEMI, COLON, 
       EQUAL, INT, ID, NOT_EQ, EOI, DOUBLE_EQUAL, MORE, MORE_EQUAL};

char *words[] = { "do", "else", "if", "while","print", "goto", "continue", "break",  NULL };

int ch = ' ';
int sym;
int int_val;
char id_name[100];
char erreur = 0;
char label[26];
void next_ch() { ch = getchar(); }

void next_sym()
{
  while (ch == ' ' || ch == '\n') next_ch();
  switch (ch)
    { case '{': sym = LBRA;  next_ch(); break;
      case '}': sym = RBRA;  next_ch(); break;
      case '(': sym = LPAR;  next_ch(); break;
      case ')': sym = RPAR;  next_ch(); break;
      case '+': sym = PLUS;  next_ch(); break;
      case '-': sym = MINUS; next_ch(); break;
      case '*': sym = MULTIPLY; next_ch(); break;
      case '/': sym = DIVIDE; next_ch(); break;
      case '%': sym = MODULO; next_ch(); break;
      case ':': sym = COLON; next_ch(); break;
      case ';': sym = SEMI;  next_ch(); break;
      case EOF: sym = EOI;   next_ch(); break;
      default:
        if (ch >= '0' && ch <= '9')
          {
            int_val = 0; /* overflow? */
      
            while (ch >= '0' && ch <= '9')
              {
                int_val = int_val*10 + (ch - '0');
                next_ch();
                if (int_val > 127) {
                  printf("Too big Integer.\n");
                  erreur = 1;
                  break;
                }
              }
      
            sym = INT;
          }
        else if (ch == '='){
	        sym = EQUAL;
	        next_ch();
	        if (ch == '='){
	          sym = DOUBLE_EQUAL;
	          next_ch();
	        }
	      }
	      else if (ch == '>') {
            sym = MORE;
            next_ch();
            if (ch == '=') {
                sym = MORE_EQUAL;
                next_ch();
            }
        }
        else if (ch == '<') {
            sym = LESS;
            next_ch();
            if (ch == '=') {
                sym = LESS_EQ;
                next_ch();
            }
        }
        else if (ch == '!') {
	        next_ch();

	        if (ch == '='){
	          sym = NOT_EQ;
	          next_ch();

	        }
	        else {
	          printf("Invalid symbole.\n");
	          sym = -1;
            erreur = 1;
	        }
	      }
        else if (ch >= 'a' && ch <= 'z')
          {
            int i = 0; /* overflow? */
      
            while ((ch >= 'a' && ch <= 'z') || ch == '_')
              {
                if (i < 0) {
                  printf("Too Long String.\n");
                  erreur = 1;
                  break;
                }
                id_name[i++] = ch;
                next_ch();
              }
      
            id_name[i] = '\0';
            sym = 0;
      
            while (words[sym]!=NULL && strcmp(words[sym], id_name)!=0)
              sym++;
      
            if (words[sym] == NULL)
              {
                if (id_name[1] == '\0') sym = ID; 
                else {
                  printf("Invalid ID name.\n");
                  sym = -1;
                  erreur = 1;
                }
              }
          }
        else {
          printf("Invalid Character : \"%c.\"\n", ch);
          sym = -1;
          erreur = 1;
        }
    }
}

/*---------------------------------------------------------------------------*/

/* Analyseur syntaxique. */

enum { VAR, CST, ADD, SUB, MULT, DIV, MOD, LT, LEQ, ASSIGN, PRINT,
       GOTOID, IF1, IF2, WHILE, ETQ, DO, EMPTY, SEQ, EXPR, PROG,
       NEQ, DOUBLE_EQ, GREATER, GEQ, CONTINUE_ID, CONTINUE_NOID, BREAK_ID, BREAK_NOID };

struct node
  {
    int kind;
    struct node *o1;
    struct node *o2;
    struct node *o3;
    int val;
  };

typedef struct node node;

void closeASA(node *x) {
  if (x != NULL) {
    if(x->o1 != NULL) {
      closeASA(x->o1);
    } 
    if(x->o2 != NULL) {
      closeASA(x->o2);
    } 
    if(x->o3 != NULL) {
      closeASA(x->o3);
    }
    free(x);
  }
}

node *new_node(int k)
{ 
  node *x = malloc(sizeof(node));
  if (x != NULL) {
    x->kind = k;
  } 
  else {
    printf("Not enough space.\n"); 
    erreur = 1;
  }
  return x;
}

node *paren_expr(); /* forward declaration */

node *term() /* <term> ::= <id> | <int> | <paren_expr> */
{
  node *x = NULL;

  if (sym == ID)           /* <term> ::= <id> */
    {
      x = new_node(VAR);
      if (x != NULL) {
        
        x->val = id_name[0]-'a';
        next_sym();
        
      }
    }
  else if (sym == INT)     /* <term> ::= <int> */
    {
      x = new_node(CST);
      if (x != NULL) {
      
        x->val = int_val;
        next_sym();
        
      }
    }
  else if (sym == -1) {
    return NULL;
  }
  else                     /* <term> ::= <paren_expr> */
    if (sym == LPAR) x = paren_expr();

  return x;
}

node *mult() /* <mult> ::= <term>|<mult>"*"<term> */
{
  node *x = term();
  if (x != NULL) {
    while (sym == MULTIPLY || sym == DIVIDE || sym == MODULO) 
    {
      node *t = x;
      if (sym == MULTIPLY) 
      {
        x = new_node(MULT);
      } 
      else if (sym == DIVIDE)
      {
        x = new_node(DIV);
      }
      else if (sym == MODULO)
      {
        x = new_node(MOD);
      }
      if (x != NULL) {
        
        next_sym();
        x->o1 = t;
        x->o2 = term();
      }
      else {
        erreur = 1;
        x = t;
        break;
      }
    }
  }
  return x;
}
      
    
    
node *sum() /* <sum> ::= <mult>|<sum>"+"<mult>|<sum>"-"<mult> */
{
  node *x = mult();
  if (x != NULL) {
    while (sym == PLUS || sym == MINUS)
    {
      node *t = x;
      x = new_node(sym==PLUS ? ADD : SUB);
      if (x != NULL) {
      
        x->o1 = t;
        next_sym();
        x->o2 = mult();
      
      }
      else {
        erreur = 1;
        x = t;
        break;
      }
      
    }
  }

  return x;
}

node *test() /* <test> ::= <sum> | <sum> "<" <sum> */
{
  node *x = sum();
  if (x != NULL) {
    if (sym == LESS)
    {
      node *t = x;
      x = new_node(LT);
      if (x != NULL) {
        
        x->o1 = t;
        next_sym();
        x->o2 = sum();
        
      }
      else {
        erreur = 1;
        x = t;
      }
      
    }
    else if (sym == LESS_EQ) {
      node *t = x;
      x = new_node(LEQ);
      if (x != NULL) {
        
        x->o1 = t;
        next_sym();
        x->o2 = sum();
        
      }
      else {
        erreur = 1;
        x = t;
      }
      
    }
    else if(sym == NOT_EQ){
      node *t = x;
      x = new_node(NEQ);
      if (x != NULL) {
        
        x->o1 = t;
        next_sym();
        x->o2 = sum();
        
      }
      else {
        erreur = 1;
        x = t;
      }

    }
    else if(sym == DOUBLE_EQUAL){

      node *t = x;
      x = new_node(DOUBLE_EQ);
      if (x != NULL) {
        
        x->o1 = t;
        next_sym();
        x->o2 = sum();
        
      }
      else {
        erreur = 1;
        x = t;
      }

    }
    else if (sym == MORE){
      
      node *t = x;
      x = new_node(GREATER);
      if (x != NULL) {
        
        x->o1 = t;
        next_sym();
        x->o2 = sum();
        
      }
      else {
        erreur = 1;
        x = t;
      }

    }
    else if (sym == MORE_EQUAL){
      
      node *t = x;
      x = new_node(GEQ);
      if (x != NULL) {
        
        x->o1 = t;
        next_sym();
        x->o2 = sum();
      
      }
      else {
        erreur = 1;
        x = t;
      }
      
    }
  }

  return x;
}

node *expr() /* <expr> ::= <test> | <id> "=" <expr> */
{
  node *x = NULL;

  if (sym != ID) return test();
  x = test();
  if (x != NULL) {
    if (sym == EQUAL)
    {
      node *t = x;
      x = new_node(ASSIGN);
      if (x != NULL) {
        x->o1 = t;
        next_sym();
        x->o2 = expr();
      }
      else {
        erreur = 1;
        x = t;
      }
    }
  }

  return x;
}

node *paren_expr() /* <paren_expr> ::= "(" <expr> ")" */
{
  node *x = NULL;

  if (sym == LPAR){
    next_sym(); 
  }
  else {
    printf("Missing opening parenthese in parenthese expression.\n");
    erreur = 1;
    return NULL;
  }

  x = expr();
  
  if (x != NULL){
    
    if (sym == RPAR){
      next_sym(); 
    }
    else {
      printf("Missing closing parenthese in parenthese expression.\n");
      erreur = 1;
    }
    
  }
  else {
    erreur = 1;
  }
  return x;
}

node *statement()
{
  node *x = NULL;

  if (sym == IF_SYM)       /* "if" <paren_expr> <stat> */
    {
      x = new_node(IF1);
      if (x != NULL) {
        
        next_sym();
        x->o1 = paren_expr();
        x->o2 = statement();
        
        if (sym == ELSE_SYM) /* ... "else" <stat> */
        { 
          x->kind = IF2;
          next_sym();
          x->o3 = statement();
        }
      }
      else {
        erreur = 1;
      }
    }
  else if (sym == WHILE_SYM) /* "while" <paren_expr> <stat> */
    {
      x = new_node(WHILE);
      if (x != NULL) {
        
        next_sym();
        x->o1 = paren_expr();
        x->o2 = statement();
        
      }
      else {
        erreur = 1;
      }
    }
  else if (sym == DO_SYM)  /* "do" <stat> "while" <paren_expr> ";" */
    {
      x = new_node(DO);
      if (x != NULL) {
        
        next_sym();
        x->o1 = statement();
        
        if (sym == WHILE_SYM) {
          next_sym();
          
          x->o2 = paren_expr();
          if (sym == SEMI) {
            next_sym();
          }
          else {
            printf("Missing semi-colon in do while.\n");
            erreur = 1;
          }
        }
        else {
          printf("Missing while in do while.\n");
          erreur = 1;
        }
      }
      else {
        erreur = 1;
      }
    }
  else if (sym == PRINT_SYM) /* print <paren_expr> ";" */
    {
      x = new_node(PRINT);
      if (x != NULL) {
        next_sym();
        x->o1 = paren_expr();
        if (sym == SEMI) {
          next_sym(); 
        }
        else {
          printf("Missing semi-colon in print.\n");
          erreur = 1;
        }
      }
      else {
        erreur = 1;
      }
    } 
  else if (sym == GOTO_SYM) /* goto ID ";" */
    {
      x = new_node(GOTOID);
      if (x != NULL) {
        next_sym();
        if (sym == ID)           /* <term> ::= <id> */
        {
          x->o1 = new_node(VAR);
          if (x->o1 != NULL) {
            
            x->o1->val = id_name[0]-'a';
            next_sym();
             
          }
        } 
        else {
          printf("Invalid label in goto <Label>;.\n");
          erreur = 1;
        }

        if (sym == SEMI) {
          next_sym();
        } 
        else {
          printf("Missing semi-colon after goto.\n");
          erreur = 1;
        }
      }
      else {
        erreur = 1;
      }
    } 
  else if (sym == SEMI)    /* ";" */
    {
      x = new_node(EMPTY);
      if (x != NULL) {
        next_sym();
      }
      else {
        erreur = 1;
      }
    }
  else if (sym == LBRA)    /* "{" { <stat> } "}" */
    {
      x = new_node(EMPTY);
      if (x != NULL) {
        next_sym();
        
        while (sym != RBRA && sym != EOI && !erreur)
          {
            node *t = x;
            x = new_node(SEQ);
            
            if (x != NULL) {
              x->o1 = t;
              x->o2 = statement();
            }
            else {
              erreur = 1;
              x = t;
            }
        }
        if (sym == EOI) {
          printf("Missing closing braket.\n");
          erreur = 1;
        }
        else {
          next_sym();
        }
      }
    }
  else if (sym == CONTINUE) /* continue [ <id> ] ; */
    {
      x = new_node(CONTINUE_NOID);
      if (x != NULL) {
        next_sym();
        if (sym == ID){
          x->kind = CONTINUE_ID;
          
          x->o1 = new_node(VAR);
          if (x->o1 != NULL) {
            
            x->o1->val = id_name[0]-'a';
            next_sym();
             
          }
        }
	      if (sym == SEMI) next_sym(); 
        else {
          printf("Missing semi-colon after continue or invalid label.\n");
          erreur = 1;
        }
      }
      else {
        erreur = 1;
      }
    }
   else if (sym == BREAK_SYM) /* break [ <id> ] ; */
    {
      x = new_node(BREAK_NOID);
      if (x != NULL) {
        next_sym();

        if (sym == ID){
          x->kind = BREAK_ID;
          
          x->o1 = new_node(VAR);
          if (x->o1 != NULL) {
            
            x->o1->val = id_name[0]-'a';
            next_sym();
             
          }

        }
	      if (sym == SEMI) next_sym(); 
        else {
          printf("Missing semi-colon after break or invalid label.\n");
          erreur = 1;
        }
      }
      else {
        erreur = 1;
      }
    }
  else                     /* <expr> ";" */
    {
      if (sym == ID && ch == ':') {
        x = new_node(ETQ);
        if (x != NULL) {
          x->o1 = term();
          if (x->o1 == NULL || x->o1->kind != VAR) {
            erreur = 1;
          }
          else if (label[x->o1->val]) {
            erreur = 1;
            printf("Label already used.\n");
          }
          else {
            label[x->o1->val] = 1;
          }
          next_sym();
          
          x->o2 = statement();
        }
        else {
          erreur = 1;
        }
      }
      else {
        x = new_node(EXPR);
        if (x != NULL) {
          x->o1 = expr();
          if (sym == SEMI) {
            next_sym(); 
          }
          
          else {
            printf("Missing semi-colon after experssion.\n");
            erreur = 1;
          }
        }
        else {
          erreur = 1;
        }
      }
    }

  return x;
}

node *program()  /* <program> ::= <stat> */
{
  node *x = new_node(PROG);
  next_sym();
  x->o1 = statement();
  if (x == NULL || erreur || x->o1 == NULL)  {
    closeASA(x);
    exit(1);
  }
  if (sym != EOI) {
    printf("Too many statement without brakets.\n");
    closeASA(x);
    exit(1);
  }
  return x;
}

/*---------------------------------------------------------------------------*/

/* Generateur de code. */

enum { ILOAD, ISTORE, BIPUSH, DUP, POP, IADD, ISUB, IMULT, IDIV,
       IMOD, GOTO, IFEQ, IFNE, IFLT, IFLE, IPRINT, RETURN };

typedef signed char code;

code object[1000], *here = object;

code *continueno[1000], **topcontinueno = continueno;
code *breakno[1000], **topbreakno = breakno;

code *gotoPosition[1000], **topGotoPosition = gotoPosition;
int gotoLabel[1000], *topGotoLabel = gotoLabel;

code *contPosition[1000], **topContPosition = contPosition;
int contLabel[1000], *topContLabel = contLabel;

code *breakPosition[1000], **topBreakPosition = breakPosition;
int breakLabel[1000], *topBreakLabel = breakLabel;

int labelsCourant[26];
code *labelPos[26];
int labelPending = -1;
code *labelContinuePos[26];
code *labelBreakPos[26];

void gen(code c) { 
  *here++ = c;
  if (here - object > 1000) {
    printf("Programme recieved is too long.\n");
    longjmp(env, 1);
  }
}

#ifdef SHOW_CODE
#define g(c) do { printf(" %d",c); gen(c); } while (0)
#define gi(c) do { printf("\n%s", #c); gen(c); } while (0)
#else
#define g(c) gen(c)
#define gi(c) gen(c)
#endif
void fix(code *src, code *dst) { 
  if (dst-src < -128 || dst-src > 127) {
    printf("Invalid jump in code : the jump is too long.\n");
    longjmp(env, 1);
  }
  *src = dst-src;
}

/* fix les jump de continue entre topcontinueno et start vers to*/
void fixContinueNoId(code **start, code *to) {
  while(start - (code**)&continueno <
        topcontinueno - (code**)&continueno) {
      fix(*--topcontinueno, to);
  }
}

void validateGoto(int l) {
  if (l >= 26 || l < 0 || !label[l]) {
    printf("Invalid goto in code : the label is not set.\n");
    longjmp(env, 1);
  }
}

void validateCB(int l) {
  if (l >= 26 || l < 0 || !labelsCourant[l]) {
    printf("Invalid continue or break in code : the label is not set or is not an outside loop.\n");
    longjmp(env, 1);
  }
}
    
/* fix les jump de continue entre topcontinueno et start vers to*/
void fixBreakNoId(code **start, code *to) {
  while(start - (code**)&breakno <
        topbreakno - (code**)&breakno) {
      fix(*--topbreakno, to);
  }
}

void c(node *x)
{
  
  switch (x->kind)
    { 
    case VAR   : gi(ILOAD); g(x->val); break;

      
    case CST   : gi(BIPUSH); g(x->val); break;

    case ADD   : c(x->o1); c(x->o2); gi(IADD); break;

    case SUB   : c(x->o1); c(x->o2); gi(ISUB); break;
    
    case MULT  : c(x->o1); c(x->o2); gi(IMULT); break;
    
    case DIV  : c(x->o1); c(x->o2); gi(IDIV); break;
    
    case MOD  : c(x->o1); c(x->o2); gi(IMOD); break;

    case LT    : gi(BIPUSH); g(1);
                c(x->o1);
                c(x->o2);
                gi(ISUB);
                gi(IFLT); g(4);
                gi(POP);
                gi(BIPUSH); g(0); break;
      
    case LEQ   : gi(BIPUSH); g(1);
                c(x->o1);
                c(x->o2);
                gi(ISUB);
                gi(IFLE); g(4);
                gi(POP);
                gi(BIPUSH); g(0); break;
                
    case NEQ : gi(BIPUSH); g(0);
      c(x->o1);
      c(x->o2);
      gi(ISUB);
      gi(IFEQ); g(4);
      gi(POP);
      gi(BIPUSH); g(1); break;
    case DOUBLE_EQ: gi(BIPUSH); g(1);
      c(x->o1);
      c(x->o2);
      gi(ISUB);
      gi(IFEQ); g(4);
      gi(POP);
      gi(BIPUSH); g(0); break;
      
    case GREATER : gi(BIPUSH); g(0);
      c(x->o1);
      c(x->o2);
      gi(ISUB);
      gi(IFLE); g(4);
      gi(POP);
      gi(BIPUSH); g(1); break;
      
    case GEQ :  gi(BIPUSH); g(0);
      c(x->o1);
      c(x->o2);
      gi(ISUB);
      gi(IFLT); g(4);
      gi(POP);
      gi(BIPUSH); g(1); break;
      
    case PRINT : c(x->o1);
                gi(IPRINT); break;
    
    case GOTOID  : validateGoto(x->o1->val); 
                   gi(GOTO); *topGotoPosition++ = here++;
                   *topGotoLabel++ = x->o1->val; break;
    
    case CONTINUE_NOID  : gi(GOTO); 
                          *topcontinueno++ = here++; break;
    
    case BREAK_NOID  : gi(GOTO); 
                          *topbreakno++ = here++; break;
    
    case CONTINUE_ID  : validateCB(x->o1->val); 
                        gi(GOTO); *topContPosition++ = here++;
                        *topContLabel++ = x->o1->val; break;
    case BREAK_ID  : validateCB(x->o1->val); 
                     gi(GOTO); *topBreakPosition++ = here++;
                     *topBreakLabel++ = x->o1->val; break;

      case ASSIGN: c(x->o2);
                   gi(DUP);
                   gi(ISTORE); g(x->o1->val); break;

      case IF1   : { code *p1; labelPending = -1;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2); fix(p1,here); break;
                   }

      case IF2   : { code *p1, *p2; labelPending = -1;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2);
                     gi(GOTO); p2 = here++; fix(p1,here);
                     c(x->o3); fix(p2,here); break;
                   }

      case ETQ   : labelPos[x->o1->val] = here;
                   labelPending = x->o1->val;
            		   c(x->o2);
            		   labelPending = -1; break;
                   
      case WHILE : { int nom = -1;
                     if (labelPending > -1) {
                        nom = labelPending;
                        labelsCourant[nom] = 1;
                        labelPending = -1;
                     }
                     
                     code **begin = topcontinueno;
                     code **beginBreak = topbreakno;
                     code *p1 = here, *p2;
                     c(x->o1);
                     gi(IFEQ); p2 = here++;
                     c(x->o2);
                     fixContinueNoId(begin, p1);
                     gi(GOTO); fix(here++,p1); fix(p2,here);
                     fixBreakNoId(beginBreak, here);
                     
                     if (nom > -1) {
                        labelContinuePos[nom] = p1;
                        labelBreakPos[nom] = here;
                        labelsCourant[nom] = 0;
                     }
                     
                     break;
                   }

      case DO    : { int nom = -1;
                     if (labelPending > -1) {
                        nom = labelPending;
                        labelsCourant[nom] = 1;
                        labelPending = -1;
                     }
                     
                     code **begin = topcontinueno;
                     code **beginBreak = topbreakno; 
                     code *p1 = here; c(x->o1);
                     fixContinueNoId(begin, here);
                     c(x->o2);
                     gi(IFNE); fix(here++,p1); 
                     fixBreakNoId(beginBreak, here); 
                     
                     if (nom > -1) {
                        labelContinuePos[nom] = p1;
                        labelBreakPos[nom] = here;
                        labelsCourant[nom] = 0;
                     }
                     
                     break;
                   }

      case EMPTY : break;

      case SEQ   : labelPending = -1; c(x->o1);
                   c(x->o2); break;

      case EXPR  : c(x->o1);
                   gi(POP); break;

      case PROG  : c(x->o1);
                   gi(RETURN); break;
    }
    
}

/*---------------------------------------------------------------------------*/

/* Machine virtuelle. */

int globals[26];

char validateADD(int g, int d) {
  if ((g > 0 && d > 0 && g+d<0) || (g < 0 && d < 0 && g+d>0)) {
    return 0;
  }
  return 1;
}

char validateSUB(int g, int d) {
  if ((g < 0 && d > 0 && g-d>0) || (g > 0 && d < 0 && g-d<0)) {
    return 0;
  }
  return 1;
}

char validateMULT(int g, int d) {
  if ((((g > 0 && d > 0) || (g < 0 && d < 0)) && g*d<0) 
  || (((g < 0 && d > 0) || (g > 0 && d < 0)) && g*d>0)) {
    return 0;
  }
  return 1;
}

void run()
{
  int stack[1000], *sp = stack; /* overflow? */
  code *pc = object;
  for (;;) {
    switch (*pc++)
      {
        case ILOAD : *sp++ = globals[*pc++];             break;
        case ISTORE: globals[*pc++] = *--sp;             break;
        case BIPUSH: *sp++ = *pc++;                      break;
        case DUP   : sp++; sp[-1] = sp[-2];              break;
        case POP   : --sp;                               break;
        
        case IADD  : if(!validateADD(sp[-2], sp[-1])){
	                      printf("Overflow in addition.\n"); exit(1);
	                   }
         	           sp[-2] = sp[-2] + sp[-1]; --sp;     break;
		   
      case ISUB  : if(!validateSUB(sp[-2], sp[-1])){
	                    printf("Overflow in substraction.\n"); exit(1);
	                 }
         	         sp[-2] = sp[-2] - sp[-1]; --sp;       break;
         	            
        case IMULT : if(!validateMULT(sp[-2], sp[-1])){
	                     printf("Overflow in multiplication.\n"); exit(1);
	                   }
         	           sp[-2] = sp[-2] * sp[-1]; --sp;     break;
         	           
        case IDIV : { if (sp[-1] == 0) { printf("Invalid divsion by zero.\n"); exit(1);}
                      sp[-2] = sp[-2] / sp[-1]; --sp;    break;}
        
        case IMOD : { if (sp[-1] == 0) { printf("Invalid modulo by zero.\n"); exit(1);}
                      sp[-2] = sp[-2] % sp[-1]; --sp;    break;}
        
        case GOTO  : pc += *pc;                          break;
        case IFEQ  : if (*--sp==0) pc += *pc; else pc++; break;
        case IFNE  : if (*--sp!=0) pc += *pc; else pc++; break;
        case IFLT  : if (*--sp< 0) pc += *pc; else pc++; break;
        case IFLE  : if (*--sp<= 0) pc += *pc; else pc++;break;
        case IPRINT: printf("%d\n",*--sp);               break;
        case RETURN: return;
    }
    if (sp - stack > 1000) {
      printf("Too many variables on the stack\n");
      exit(1);
    }
  }
}

/*---------------------------------------------------------------------------*/

/* Programme principal. */
void fixPos(code *src, code *dst) { 
  if (dst-src < -128 || dst-src > 127) {
    printf("Invalid jump in code : the jump is too long.\n");
    exit(1);
  }
  *src = dst-src;
}

int main()
{
  int i;
  
  for (i=0; i<26; i++) {
    label[i] = 0;
    labelsCourant[i] = 0;
  }

  node *prog = program();
  
  int r = setjmp(env);
  if (r != 0) {
    closeASA(prog);
    exit(1);
  }
  
  c(prog);
  closeASA(prog);
  
  while(topGotoPosition - (code**)&gotoPosition > 0) {
    fixPos(*--topGotoPosition, labelPos[*--topGotoLabel]);
  }
  
  while(topContPosition - (code**)&contPosition > 0) {
    fixPos(*--topContPosition, labelContinuePos[*--topContLabel]);
  }
  
  while(topBreakPosition - (code**)&breakPosition > 0) {
    fixPos(*--topBreakPosition, labelBreakPos[*--topBreakLabel]);
  }
  
  if (topcontinueno - (code**)&continueno > 0) {
    printf("Invalid continue not in a loop.\n");
    exit(1);
  }
  if (topbreakno - (code**)&breakno > 0) {
    printf("Invalid break not in a loop.\n");
    exit(1);
  }

#ifdef SHOW_CODE
  printf("\n");
#endif

  for (i=0; i<26; i++)
    globals[i] = 0;

  run();

  return 0;
}
/*---------------------------------------------------------------------------*/
