#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rinternals.h>

SEXP R_ifelse_ifelse1(SEXP, SEXP, SEXP);
#ifndef NA_COMPLEX
Rcomplex R_ifelse_NaComplex;
#endif

static R_CallMethodDef CallEntries[] =
{
	{"R_ifelse_ifelse1", (DL_FUNC) &R_ifelse_ifelse1, 4},
	{NULL, NULL, 0}
};

void attribute_visible R_init_ifelse(DllInfo *info)
{
	R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
#ifndef NA_COMPLEX
	R_ifelse_NaComplex.r = R_ifelse_NaComplex.i = NA_REAL;
#endif
	return;
}
