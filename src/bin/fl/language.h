/********************************************************************
*                                                                   *
*     Copyright (C) 1993 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* language.h -- header for language.y */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct tvar_list_rec    *tvar_list_ptr;

/* -------- Function prototypes for exported functions -------- */
void            Parse_Init();
void            Install_BuiltIns();
void            Remove_Infix(string s);
string		Get_Fixity(string name);
string		Get_Unary(string s);
void		Insert_infix(string s, int precedence);
void		Insert_infix_unary(string s, int precedence,
				   string unary_function);
void		Insert_infixr(string s, int precedence);
void		Insert_prefix(string s, int precedence);
void		Insert_postfix(string s);
void		Insert_binder(string s);
void		Insert_free_binder(string s);
void		Insert_if_then_else(string then_fun, string else_fun);
void		Insert_then_binder(string fun);
void		Insert_else_binder(string fun);
void		Insert_binder_with_acc(string name, string acc_name);
void		Insert_size_binder_with_acc(string name, string acc_name);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef LANGUAGE_H
#define LANGUAGE_H
#include "fl.h"  /* Global data types and include files               */

typedef struct tvar_list_rec {
        string          var;
        tvar_list_ptr   next;
} tvar_list_rec;

#endif /* LANGUAGE_H */
#endif /* EXPORT_FORWARD_DECL */
