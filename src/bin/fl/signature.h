/********************************************************************
*                                                                   *
*     Copyright (C) 2017 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* signature.h -- header for signature.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Signature_Init();
void	    Signature_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef SIGNATURE_H
#define SIGNATURE_H

#include "../../external/circuit-isomatch/src/c_api/isomatch.h"

#endif /* SIGNATURE_H */
#endif /* EXPORT_FORWARD_DECL */
