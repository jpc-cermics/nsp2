#ifndef NSP_TOKENIZER 
#define NSP_TOKENIZER 

#include "nsp/object.h" 

#define LINEMAXSIZE 4096 

typedef struct _curline Curline; 

struct _curline {
    integer lpt1; /* position of the begining of the before last recognised token */
    integer lpt2; /* position of the begining of the last recognised token */
    integer lpt3; /* current char position in buf */
    char buf[LINEMAXSIZE];
} ;


/*
 * structure for current parsed token 
 */ 

#define TBUF 1024

typedef struct _token token; 

struct _token 
{
  char buf[TBUF];
  char syn[NAME_MAXL];
  double syv;
  char NextC;
  int id;
  int FlagEqu;
  int Line;
};

/*
 * tokenizer 
 */

typedef struct _tokenizer Tokenizer; 

typedef int token_NextToken(Tokenizer *);
typedef int token_ParseCommandArg(Tokenizer *);
typedef int token_ParseString(Tokenizer *);
typedef int token_GetChar(Tokenizer *);
typedef int token_ForceNextChar(Tokenizer *);
typedef int token_FuncForceNextChar(Tokenizer *);
typedef int token_IgnoreWSpaces(Tokenizer *);
typedef int token_viewch(Tokenizer *);
typedef int token_backch(Tokenizer *);
typedef int token_IsDotDotDot(Tokenizer *);
typedef int token_IsDotDot(Tokenizer *);
typedef int token_IsDotStarStar(Tokenizer *);
typedef int token_IsDotAlphaOld(Tokenizer *);
typedef int token_IsDotAlpha(Tokenizer *);
typedef int token_IsTranspose(Tokenizer *);
typedef int token_ParseShowLine(Tokenizer *);
typedef int token_Getlin(Tokenizer *,char *prompt);
typedef int token_TokenLineSet(Tokenizer *,int l);
typedef void token_ParseOperators(Tokenizer *);
typedef int token_ParseNumber(Tokenizer *);
typedef int token_ParseSymb(Tokenizer *,char *str, int *l);
typedef int token_ParseComment(Tokenizer *);
typedef int token_ParseError(Tokenizer *,char *fmt,...);
typedef char *token_code2name(Tokenizer *T,int key);

typedef struct _smat_tokenizer smat_tokenizer ;

struct _smat_tokenizer { 
  NspSMatrix *smat;
  int ind;
  int pos;
  char *line;
} ;

typedef struct _string_tokenizer string_tokenizer ;

struct _string_tokenizer { 
  char *str;
  int pos;
} ;

struct _tokenizer {
  /* data */
  Curline curline; 
  token  token;
  FILE *file; /* used when the tokenizer uses a File as input */
  smat_tokenizer strings; /* used when the tokenizer uses a string matrix as input */
  string_tokenizer string; /* used when the tokenizer uses a string matrix as input */
  /* methods */
  token_NextToken *NextToken;
  token_ParseCommandArg *ParseCommandArg;
  token_ParseString *ParseString;
  token_GetChar *GetChar;
  token_ForceNextChar *ForceNextChar;
  token_FuncForceNextChar *FuncForceNextChar;
  token_IgnoreWSpaces *IgnoreWSpaces;
  token_viewch *viewch;
  token_backch *backch;
  token_IsDotDotDot *IsDotDotDot;
  token_IsDotDot *IsDotDot;
  token_IsDotStarStar *IsDotStarStar;
  token_IsDotAlphaOld *IsDotAlphaOld;
  token_IsDotAlpha *IsDotAlpha;
  token_IsTranspose *IsTranspose;
  token_ParseShowLine *ParseShowLine;
  token_Getlin *Getlin;
  token_TokenLineSet *TokenLineSet;
  token_ParseOperators *ParseOperators;
  token_ParseNumber *ParseNumber;
  token_ParseSymb *ParseSymb;
  token_ParseComment *ParseComment;
  token_ParseError *ParseError;
  token_code2name *code2name;
};

extern void nsp_init_tokenizer(Tokenizer *T);
extern void nsp_tokeniser_file(Tokenizer *T,FILE *f);
extern void nsp_tokeniser_string(Tokenizer *T,char *str);
extern void nsp_tokeniser_smat(Tokenizer *T,NspSMatrix *S);

typedef void (SciReadFunc) (Tokenizer *T, char *prompt,char *buffer, int *buf_size,int *len_line,int *eof);
typedef SciReadFunc *SciReadFunction;
extern SciReadFunction SciReadLine1 ;
extern SciReadFunction SetSciReadFunction(SciReadFunction F);

typedef void (*MoreFun) (Tokenizer *T,int *n);
extern MoreFun scimore ;
extern void scimore_void(Tokenizer *T,int *n);
MoreFun SetScilabMore(MoreFun F);

#endif /*  NSP_TOKENIZER  */

#ifdef Private_Tokenizer 
static token_NextToken next_token;
static token_ParseCommandArg parse_command_arg;
static token_ParseString parse_string;
static token_GetChar get_char;
static token_ForceNextChar force_next_char;
static token_FuncForceNextChar func_force_next_char;
static token_IgnoreWSpaces ignore_white_spaces;
static token_viewch view_char;
static token_backch back_char;
static token_IsDotDotDot is_dot_dot_dot;
static token_IsDotDot is_dot_dot;
static token_IsDotStarStar is_dot_star_star;
static token_IsDotAlphaOld is_dot_alpha_old;
static token_IsDotAlpha is_dot_alpha;
static token_IsTranspose is_transpose;
static token_ParseShowLine parse_show_line;
static token_Getlin get_line;
static token_TokenLineSet token_line_set;
static token_ParseOperators parse_operators;
static token_ParseNumber parse_number;
static token_ParseSymb parse_symb;
static token_ParseComment parse_comment;
static token_ParseError parse_error ;
static token_code2name code2name;
static int cdigit2num(char c);
static void nsp_string_readline_internal(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof, 
					 char **nsp_input_string,  int *nsp_input_pos );
#endif 
