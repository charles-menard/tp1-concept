/* fichier: "petit-comp.c" */

/* Un petit compilateur et machine virtuelle pour un sous-ensemble de C.  */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <setjmp.h>

#define SHOW_CODE 1
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
      case '=': sym = EQUAL; next_ch(); break;
      case EOF: sym = EOI;   next_ch(); break;
      default:
        if (ch >= '0' && ch <= '9')
          {
            int_val = 0; /* overflow? */
      
            while (ch >= '0' && ch <= '9')
              {
                int_val = int_val*10 + (ch - '0');
                next_ch();
                if (int_val < 0) {
                  printf("Too big Integer.\n");
                  longjmp(env, 1);
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
            longjmp(env, 1);
	        }
	      }
        else if (ch >= 'a' && ch <= 'z')
          {
            int i = 0; /* overflow? */
      
            while ((ch >= 'a' && ch <= 'z') || ch == '_')
              {
                if (i < 0) {
                  printf("Too Long String.\n");
                  longjmp(env, 1);
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
                  longjmp(env, 1);
                }
              }
          }
        else {
          printf("Invalid Character.\n");
          longjmp(env, 1);
        }
    }
}

/*---------------------------------------------------------------------------*/

/* Analyseur syntaxique. */

enum { VAR, CST, ADD, SUB, MULT, DIV, MOD, LT, LEQ, ASSIGN, PRINT, GOTOID,
       IF1, IF2, WHILE, ETQ, DO, EMPTY, SEQ, EXPR, PROG, NEQ, DOUBLE_EQ, GREATER,
       GEQ, CONTINUE_ID, CONTINUE_NOID, BREAK_ID, BREAK_NOID };
char etqFlag[26]; /* met a true si la lettre est utilisee comme etq */

  
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
  return x;
}

node *paren_expr(); /* forward declaration */

node *term() /* <term> ::= <id> | <int> | <paren_expr> */
{
  node *x;

  if (sym == ID)           /* <term> ::= <id> */
    {
      x = new_node(VAR);
      if (x != NULL) {
        x->val = id_name[0]-'a';
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
      }
    }
  else if (sym == INT)     /* <term> ::= <int> */
    {
      x = new_node(CST);
      if (x != NULL) {
        x->val = int_val;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
      }
    }
  else                     /* <term> ::= <paren_expr> */
    x = paren_expr();

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
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o1 = t;
        
        x->o2 = term();
        if(x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
      }
      else {
        closeASA(t);
        return NULL;
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

        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = mult();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
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
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = sum();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
      }
      
    }
    else if (sym == LESS_EQ) {
      node *t = x;
      x = new_node(LEQ);
      if (x != NULL) {
        x->o1 = t;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = sum();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
      }
      
    }
    else if(sym == NOT_EQ){
      node *t = x;
      x = new_node(NEQ);
      if (x != NULL) {
        x->o1 = t;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = sum();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
      }

    }
    else if(sym == DOUBLE_EQUAL){

      node *t = x;
      x = new_node(DOUBLE_EQ);
      if (x != NULL) {
        x->o1 = t;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = sum();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
      }

    }
    else if (sym == MORE){
      
      node *t = x;
      x = new_node(GREATER);
      if (x != NULL) {
        x->o1 = t;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = sum();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
      }

    }
    else if (sym == MORE_EQUAL){
      
      node *t = x;
      x = new_node(GEQ);
      if (x != NULL) {
        x->o1 = t;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = sum();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
      }
      else {
        closeASA(t);
        return NULL;
      }
      
    }
  }

  return x;
}

node *expr() /* <expr> ::= <test> | <id> "=" <expr> */
{
  node *x;

  if (sym != ID) return test();
  x = test();
  if (x != NULL) {
    if (sym == EQUAL)
    {
      node *t = x;
      x = new_node(ASSIGN);
      if (x != NULL) {
        x->o1 = t;
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o2 = expr();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
      }
      else {
        closeASA(t);
        return NULL;
      }
    }
  }

  return x;
}

node *paren_expr() /* <paren_expr> ::= "(" <expr> ")" */
{
  node *x;

  if (sym == LPAR){
    int r = setjmp(env);
    if (r != 0) {
      return NULL;
    }
    next_sym(); 
  }
  else {
    printf("Missing opening parenthese in parenthese expression.\n");
    return NULL;
  }

  x = expr();

  if (x != NULL){
    if (sym == RPAR){
      int r = setjmp(env);
      if (r != 0) {
        closeASA(x);
        return NULL;
      }
      next_sym(); 
    }
    else {
      printf("Missing closing parenthese in parenthese expression.\n");
      closeASA(x);
      return NULL;
    }
  }
  return x;
}

node *statement()
{
  node *x;

  if (sym == IF_SYM)       /* "if" <paren_expr> <stat> */
    {
      x = new_node(IF1);
      if (x != NULL) {
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o1 = paren_expr();
        if (x->o1 == NULL) {
          closeASA(x);
          return NULL;
        }
        
        x->o2 = statement();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
        
        if (sym == ELSE_SYM) /* ... "else" <stat> */
        { 
          x->kind = IF2;
          
          int r = setjmp(env);
          if (r != 0) {
            closeASA(x);
            return NULL;
          }
          next_sym();

          x->o3 = statement();
          if (x->o3 == NULL) {
            closeASA(x);
            return NULL;
          }
        }
      }
    }
  else if (sym == WHILE_SYM) /* "while" <paren_expr> <stat> */
    {
      x = new_node(WHILE);
      if (x != NULL) {
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o1 = paren_expr();
        if (x->o1 == NULL) {
          closeASA(x);
          return NULL;
        }
        
        x->o2 = statement();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }
      }
    }
  else if (sym == DO_SYM)  /* "do" <stat> "while" <paren_expr> ";" */
    {
      x = new_node(DO);
      if (x != NULL) {
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o1 = statement();
        if (x->o1 == NULL) {
          closeASA(x);
          return NULL;
        }
        
        if (sym == WHILE_SYM) {
          int r = setjmp(env);
          if (r != 0) {
            closeASA(x);
            return NULL;
          }
          next_sym();
        }
        else {
          printf("Missing while in do while.\n");
          closeASA(x);
          return NULL;
        }
        
        x->o2 = paren_expr();
        if (x->o2 == NULL) {
          closeASA(x);
          return NULL;
        }

        if (sym == SEMI) {
          int r = setjmp(env);
          if (r != 0) {
            closeASA(x);
            return NULL;
          }
          next_sym();
        }
        else {
          printf("Missing semi-colon in do while.\n");
          closeASA(x);
          return NULL;
        }
      }
    }
  else if (sym == PRINT_SYM) /* print <paren_expr> ";" */
    {
      x = new_node(PRINT);
      if (x != NULL) {
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o1 = paren_expr();
        if (x->o1 == NULL) {
          closeASA(x);
          return NULL;
        }
        if (sym == SEMI) {
          int r = setjmp(env);
          if (r != 0) {
            closeASA(x);
            return NULL;
          }
          next_sym(); 
        }
        else {
          printf("Missing semi-colon in print.\n");
          closeASA(x);
          return NULL;
        }
      }
    } 
  else if (sym == GOTO_SYM) /* goto ID ";" */
    {
      x = new_node(GOTOID);
      if (x != NULL) {
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        x->o1 = term();
        if (x->o1 == NULL) {
          closeASA(x);
          return NULL;
        }
        if (x->o1->kind != VAR) {
          printf("Invalid label in gtoto <Label>;.\n");
          closeASA(x);
          return NULL;
        }
        
        if (sym == SEMI) {
          int r = setjmp(env);
          if (r != 0) {
            closeASA(x);
            return NULL;
          }
          next_sym();
        } 
        else {
          printf("Missing semi-colon after goto.\n");
          closeASA(x);
          return NULL;
        }
      }
    } 
  else if (sym == SEMI)    /* ";" */
    {
      x = new_node(EMPTY);
      if (x != NULL) {
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
      }
    }
  else if (sym == LBRA)    /* "{" { <stat> } "}" */
    {
      x = new_node(EMPTY);
      if (x != NULL) {
        
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
        
        while (sym != RBRA)
          {
            node *t = x;
            x = new_node(SEQ);
            
            if (x != NULL) {
              x->o1 = t;
              
              x->o2 = statement();
              if (x->o2 == NULL) {
                closeASA(x);
                return NULL;
              }
            }
            else {
              closeASA(t);
              return NULL;
            }
        }
        
        int n = setjmp(env);
        if (n != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();
      }
    }
  else if (sym == CONTINUE) /* continue [ <id> ] ; */
    {
      x = new_node(CONTINUE_NOID);
      if (x != NULL) {
      
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();

        if (sym == ID){
          x->kind = CONTINUE_ID;
          
          x->o1 = term();
          if (x->o2 == NULL) {
            closeASA(x);
            return NULL;
          }
          
          if (x->o1->kind != VAR) {
            printf("Invalid label in continue <Label>;.\n");
            closeASA(x);
            return NULL;
          }

        }
	      if (sym == SEMI) next_sym(); 
        else {
          printf("Missing semi-colon after continue.\n");
          closeASA(x);
          return NULL;
        }
      }
    }
   else if (sym == BREAK_SYM) /* break [ <id> ] ; */
    {
      x = new_node(BREAK_NOID);
      if (x != NULL) {
      
        int r = setjmp(env);
        if (r != 0) {
          closeASA(x);
          return NULL;
        }
        next_sym();

        if (sym == ID){
          x->kind = BREAK_ID;
          
          x->o1 = term();
          if (x->o2 == NULL) {
            closeASA(x);
            return NULL;
          }
          
          if (x->o1->kind != VAR) {
            printf("Invalid label in break <Label>;.\n");
            closeASA(x);
            return NULL;
          }

        }
	      if (sym == SEMI) next_sym(); 
        else {
          printf("Missing semi-colon after break.\n");
          closeASA(x);
          return NULL;
        }
      }
    }
  else                     /* <expr> ";" */
    {
      if (sym == ID && ch == ':') {
        x = new_node(ETQ);
        if (x != NULL) {
          x->o1 = term();
	  
          if (x->o1 == NULL) {
            closeASA(x);
            return NULL;
          }
	 
          if (etqFlag[x->o1->val]) {
            printf("Etq already used;.\n");
            closeASA(x);
            return NULL;
          }
          etqFlag[x->o1->val]++;

          int r = setjmp(env);
          if (r != 0) {
            closeASA(x);
            return NULL;
          }
          next_sym();
          
          x->o2 = statement();
          if (x->o2 == NULL) {
            closeASA(x);
            return NULL;
          }
        }
      }
      else {
        x = new_node(EXPR);
        if (x != NULL) {
          x->o1 = expr();
          if (x->o1 == NULL) {
            closeASA(x);
            return NULL;
          }
          
          if (sym == SEMI) { 
            int r = setjmp(env);
            if (r != 0) {
              closeASA(x);
              return NULL;
            }
            next_sym(); 
          }
          
          else {
            printf("Missing semi-colon after experssion.\n");
            closeASA(x);
            return NULL;
          }
        }
      }
    }

  return x;
}

node *program()  /* <program> ::= <stat> */
{
  node *x = new_node(PROG);
  
  int r = setjmp(env);
  if (r != 0) {
    closeASA(x);
    exit(1);
  }
  
  next_sym();
  x->o1 = statement();
  if (sym != EOI || x->o1 == NULL)  {
    if (x->o1 != NULL) {
      printf("Too many statement without brakets.\n");
    }
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


code *etqno[26];


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
  code temp = *src;
  *src = dst-src;
  if ((*src <= 0 && temp < 0 && *dst > 0) ||
      (*src >= 0 && temp > 0 && *dst < 0)) {
    printf("Invalid jump in code : the jump is too long.\n");
    longjmp(env, 1);
  }
}

/* fix les jump de continue entre topcontinueno et start vers to*/
void fixContinueNoId(code **start, code *to) {
  while(start - (code**)&continueno <
        topcontinueno - (code**)&continueno) {
      fix(*--topcontinueno, to);
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
  if (x->kind == PROG) {
    int r = setjmp(env);
    if (r != 0) {
      closeASA(x);
      exit(1);
    }
  }
  
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

    case PRINT : c(x->o1);
                gi(IPRINT); break;
    
    case GOTOID  : gi(GOTO);
      if(etqFlag[x->o1->val]){
	fix(here++, etqno[x->o1->val]);
      }else{
	/* faire dequoi dautre si l'etq existe pas deja*/
      }
                   free(x->o1); break;
    
    case CONTINUE_NOID  : gi(GOTO); 
                          *topcontinueno++ = here++; break;
    case BREAK_NOID  : gi(GOTO); 
                          *topbreakno++ = here++; break;

      case ASSIGN: c(x->o2);
                   gi(DUP);
                   gi(ISTORE); g(x->o1->val); 
                   free(x->o1); break;

      case IF1   : { code *p1;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2); fix(p1,here); break;
                   }

      case IF2   : { code *p1, *p2;
                     c(x->o1);
                     gi(IFEQ); p1 = here++;
                     c(x->o2);
                     gi(GOTO); p2 = here++; fix(p1,here);
                     c(x->o3); fix(p2,here); break;
                   }

    case ETQ   :
      if(pendingGoto[x->o1->val][0] !=NULL){
	int i=0;
	while(pendingGoto[x->o1->val][i] != NULL){
	  
          fix(pendingGoto[x->o1->val][i], here);
	}
      }else{
      etqno[x->o1->val] = here;
		   c(x->o2);
      }
       		   free(x->o1); break;
                   
      case WHILE : { code **begin = topcontinueno;
                     code **beginBreak = topbreakno;
                     code *p1 = here, *p2;
                     c(x->o1);
                     gi(IFEQ); p2 = here++;
                     c(x->o2);
                     fixContinueNoId(begin, p1);
                     gi(GOTO); fix(here++,p1); fix(p2,here);
                     fixBreakNoId(beginBreak, here); break;
                   }

      case DO    : { code **begin = topcontinueno;
                     code **beginBreak = topbreakno; 
                     code *p1 = here; c(x->o1);
                     fixContinueNoId(begin, here);
                     c(x->o2);
                     gi(IFNE); fix(here++,p1); 
                     fixBreakNoId(beginBreak, here); break;
                   }

      case EMPTY : break;

      case SEQ   : c(x->o1);
                   c(x->o2); break;

      case EXPR  : c(x->o1);
                   gi(POP); break;

      case PROG  : c(x->o1);
                   gi(RETURN); break;
    }
    if (x != NULL) {
      free(x);
    }
    
}

/*---------------------------------------------------------------------------*/

/* Machine virtuelle. */

int globals[26];

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
        case IADD  : sp[-2] = sp[-2] + sp[-1]; --sp;     break;
        case ISUB  : sp[-2] = sp[-2] - sp[-1]; --sp;     break;
        case IMULT : sp[-2] = sp[-2] * sp[-1]; --sp;     break;
        case IDIV : sp[-2] = sp[-2] / sp[-1]; --sp;      break;
        case IMOD : sp[-2] = sp[-2] % sp[-1]; --sp;      break;
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

int main()
{
  int i;
  
  for(i=0;i<26;i++) etqFlag[i]=0;

  c(program());
  
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

  for (i=0; i<26; i++)
    if (globals[i] != 0)
      printf("%c = %d\n", 'a'+i, globals[i]);

  return 0;
}
/*---------------------------------------------------------------------------*/
