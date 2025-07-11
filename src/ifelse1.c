#include <Rinternals.h>

#ifndef NA_COMPLEX
extern Rcomplex R_ifelse_NaComplex;
# define NA_COMPLEX R_ifelse_NaComplex
#endif

SEXP R_ifelse_ifelse1(SEXP test, SEXP yes, SEXP no)
{
	if (!Rf_isLogical(test))
		Rf_error("'%s' [type \"%s\"] is not logical",
		         "test", Rf_type2char(TYPEOF(test)));
	if (!Rf_isVector(yes))
		Rf_error("'%s' [type \"%s\"] is not a vector",
		         "yes", Rf_type2char(TYPEOF(yes)));
	if (!Rf_isVector(no))
		Rf_error("'%s' [type \"%s\"] is not a vector",
		         "no", Rf_type2char(TYPEOF(no)));
	R_xlen_t nyes = XLENGTH(yes), nno = XLENGTH(no),
		nans = XLENGTH(test);
	SEXPTYPE tyes = TYPEOF(yes), tno = TYPEOF(no),
		tans = (tyes <= tno) ? tno : tyes;
	PROTECT(yes = Rf_coerceVector(yes, tans));
	PROTECT( no = Rf_coerceVector( no, tans));
	SEXP ans = PROTECT(Rf_allocVector(tans, nans));
	const int *ptest = LOGICAL_RO(test);

#define mapyes(i) \
	((nyes == 1) ? 0 : (nyes == nans) ? i : i % nyes)
#define mapno(i) \
	((nno == 1) ? 0 : (nno == nans) ? i : i % nno)

#define ifelseAtomic(type, ptr, na) \
	do { \
		type *pyes = ptr(yes), *pno = ptr(no), *pans = ptr(ans); \
		for (R_xlen_t i = 0; i < nans; ++i) \
			pans[i] = (ptest[i] == NA_LOGICAL) ? na : ptest[i] ? pyes[mapyes(i)] : pno[mapno(i)]; \
	} while (0)
#define ifelseRecursive(get, set, na) \
	do { \
		for (R_xlen_t i = 0; i < nans; ++i) \
			set(ans, i, (ptest[i] == NA_LOGICAL) ? na : ptest[i] ? get(yes, mapyes(i)) : get(no, mapno(i))); \
	} while (0)

	switch (tans) {
	case  RAWSXP: ifelseAtomic(   Rbyte,     RAW,          0); break;
	case  LGLSXP: ifelseAtomic(     int, LOGICAL, NA_LOGICAL); break;
	case  INTSXP: ifelseAtomic(     int, INTEGER, NA_INTEGER); break;
	case REALSXP: ifelseAtomic(  double,    REAL,    NA_REAL); break;
	case CPLXSXP: ifelseAtomic(Rcomplex, COMPLEX, NA_COMPLEX); break;
	case  STRSXP: ifelseRecursive(STRING_ELT, SET_STRING_ELT,  NA_STRING); break;
	case  VECSXP:
	case EXPRSXP: ifelseRecursive(VECTOR_ELT, SET_VECTOR_ELT, R_NilValue); break;
	}
	UNPROTECT(3);
	return ans;
}
