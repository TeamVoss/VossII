%define parse.error verbose
%locations
//%define api.pure full

%code requires {
#include <vector>
#include <string>
#include <cstring>
using namespace std;

#include <isomatch.h>  // build it against isomatch
#include "parseTools.h"
}

%code provides {
CircuitGroup* doParse(FILE* in);
}

%code {
#include <cstdlib>
using namespace parseTools;

vector<WireManager*> wireManagers;

WireId* nextWire() {
    static int curId = 0;
    return wireManagers.back()->fresh(
        std::string(" _wire_") + std::to_string(curId++));
}

void yyerror(const char *error)
{
    fprintf(stderr, "error:%d:%d %s\n",
        yylloc.first_line, yylloc.first_column, error);
}

typedef ListElem<CircuitTree*> CircList;

extern "C" {
    int yywrap() {
        return 1;
    }
}

extern int yylex(void);
extern int yylex_destroy(void);
extern FILE* yyin;

CircuitGroup* outcome = NULL;

CircuitGroup* doParse(FILE* in) {
    yyin = in;
    int rc = yyparse();
    if(rc != 0)
        return NULL;
    yylex_destroy();
    return outcome;
}
}

%union {
    YYSTYPE() { memset(this, 0, sizeof(YYSTYPE)); }
    char* sval;
    int ival;
    CircuitTree* circtree_val;
    parseTools::ListElem<string>* strlist_val;
    parseTools::ListElem<CircuitTree*>* circtreelist_val;
    parseTools::ExprConstruction exprconstruction_val;
    expr::ExpressionBinOperator binop_val;
    expr::ExpressionUnOperator unop_val;
    expr::ExpressionUnOperatorCst unopcst_val;
}

%token OP_AND OP_OR OP_XOR OP_ADD OP_SUB OP_MUL OP_DIV OP_MOD
%token OP_LSR OP_LSL OP_ASR OP_NOT OP_SLICE OP_MERGE
%token OP_CLSR OP_CLSL OP_CASR
%token DELAY TRISTATE ASSERT
%token LET
%token ARROW
%token <sval> IDENT
%token <ival> NUMBER

%start entry
//%type <circgroup_val> entry
%type <circtree_val> group
%type <strlist_val> identCommaList
%type <circtreelist_val> stmtList
%type <circtreelist_val> stmt
%type <exprconstruction_val> expr
%type <binop_val> binop
%type <unop_val> unop
%type <unopcst_val> unopcst

%%

entry:
     group              { outcome = static_cast<CircuitGroup*>($1); }

group:
     let IDENT
        '(' identCommaList ')' ARROW
        '(' identCommaList ')' '{'
        stmtList
        '}'
                        {
                            CircuitGroup* stub = new CircuitGroup(
                                $2, wireManagers.back());
                            makeGroup(stub,
                                      $4->yield(),
                                      $8->yield(),
                                      $11->yield());
                            wireManagers.pop_back();
                            $$ = stub;
                            free($2);
                        }

let:
    LET                 {
                            wireManagers.push_back(new WireManager());
                        }

identCommaList:
    IDENT               {
                            $$ = new ListElem<string>($1);
                            free($1);
                        }
  | IDENT ',' identCommaList
                        {
                            $$ = new ListElem<string>($1, $3);
                            free($1);
                        }

stmtList:
    stmt                { $$ = $1; }
  | stmt stmtList       {
                            $$ = CircList::concat($1, $2);
                        }

stmt:
    IDENT '=' expr      {
                            wireManagers.back()->rename(
                                $3.outWire->name(), $1);
                            $$ = $3.gates;
                            free($1);
                        }
  | group               { $$ = new CircList($1); }

expr:
    '(' expr ')'        { $$ = $2; }
  | binop expr expr     {
                            WireId* outWire = nextWire();
                            WireId* left  = $2.outWire;
                            WireId* right = $3.outWire;
                            CircuitComb* comb = new CircuitComb();
                            comb->addInput(left);
                            comb->addInput(right);
                            comb->addOutput(
                                new ExpressionBinOp(
                                    new ExpressionVar(0),
                                    new ExpressionVar(1),
                                    $1),
                                outWire);

                            CircList* nexts = CircList::concat(
                                $2.gates, $3.gates);
                            $$ = ExprConstruction(
                                outWire,
                                new CircList(comb, nexts));
                        }
  | unop expr           {
                            WireId* outWire = nextWire();
                            WireId* from = $2.outWire;
                            CircuitComb* comb = new CircuitComb();
                            comb->addInput(from);
                            comb->addOutput(
                                new ExpressionUnOp(
                                    new ExpressionVar(0),
                                    $1),
                                outWire);

                            $$ = ExprConstruction(
                                outWire,
                                new CircList(comb, $2.gates));
                        }
  | unopcst expr NUMBER {
                            WireId* outWire = nextWire();
                            WireId* from = $2.outWire;
                            CircuitComb* comb = new CircuitComb();
                            comb->addInput(from);
                            comb->addOutput(
                                new ExpressionUnOpCst(
                                    new ExpressionVar(0),
                                    $3,
                                    $1),
                                outWire);

                            $$ = ExprConstruction(
                                outWire,
                                new CircList(comb, $2.gates));
                        }
  | OP_MERGE expr expr  {
                            WireId* outWire = nextWire();
                            WireId* left  = $2.outWire;
                            WireId* right = $3.outWire;
                            CircuitComb* merger = new CircuitComb();
                            merger->addInput(left);
                            merger->addInput(right);
                            merger->addOutput(
                                new ExpressionMerge(
                                    new ExpressionVar(0),
                                    new ExpressionVar(1)),
                                outWire);
                            $$ = ExprConstruction(
                                outWire,
                                new CircList(merger, $2.gates));
                        }
  | OP_SLICE expr NUMBER NUMBER {
                            WireId* outWire = nextWire();
                            WireId* from = $2.outWire;
                            CircuitComb* slicer = new CircuitComb();
                            slicer->addInput(from);
                            slicer->addOutput(
                                new ExpressionSlice(
                                    new ExpressionVar(0), $3, $4),
                                outWire);
                            $$ = ExprConstruction(
                                outWire,
                                new CircList(slicer, $2.gates));
                        }
  | DELAY expr          {
                            WireId* outWire = nextWire();
                            WireId* from = $2.outWire;
                            $$ = ExprConstruction(
                                outWire,
                                new CircList(
                                    new CircuitDelay(from, outWire),
                                    $2.gates));
                        }
  | TRISTATE expr expr  {
                            WireId* outWire = nextWire();
                            WireId* from = $2.outWire;
                            WireId* enable = $3.outWire;
                            CircuitTristate* out = new CircuitTristate(
                                from, outWire, enable);

                            CircList* nexts = CircList::concat(
                                    $2.gates, $3.gates);
                            CircList* outList = new CircList(out, nexts);
                            $$ = ExprConstruction(outWire, outList);
                        }
  | IDENT               {
                            $$ = ExprConstruction(
                                wireManagers.back()->wire($1),
                                (ListElem<CircuitTree*>*)NULL);
                            free((char*)$1);
                        }
  | NUMBER              {
                            WireId* outWire = nextWire();
                            CircuitComb* out = new CircuitComb();
                            out->addOutput(new ExpressionConst($1), outWire);
                            $$ = ExprConstruction(outWire, out);
                        }


binop:
     OP_AND             { $$ = expr::BAnd; }
   | OP_OR              { $$ = expr::BOr; }
   | OP_XOR             { $$ = expr::BXor; }
   | OP_ADD             { $$ = expr::BAdd; }
   | OP_SUB             { $$ = expr::BSub; }
   | OP_MUL				{ $$ = expr::BMul; }
   | OP_DIV				{ $$ = expr::BDiv; }
   | OP_MOD				{ $$ = expr::BMod; }
   | OP_LSR				{ $$ = expr::BLsr; }
   | OP_LSL				{ $$ = expr::BLsl; }
   | OP_ASR				{ $$ = expr::BAsr; }

unop:
    OP_NOT              { $$ = expr::UNot; }

unopcst:
    OP_CLSR             { $$ = expr::UCLsr; }
  | OP_CLSL             { $$ = expr::UCLsl; }
  | OP_CASR             { $$ = expr::UCAsr; }
