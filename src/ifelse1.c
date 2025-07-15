#include <Rinternals.h>

#ifndef NA_COMPLEX
extern Rcomplex R_ifelse_NaComplex;
# define NA_COMPLEX R_ifelse_NaComplex
#endif

#define _(String) (String)
#define imax2(a, b)    ((a < b) ?       b     :       a    )
#define imax3(a, b, c) ((a < b) ? imax2(b, c) : imax2(a, c))

SEXP R_ifelse_ifelse1(SEXP test, SEXP yes, SEXP no, SEXP na)
{
	int withna = na != R_NilValue;
	if (!Rf_isLogical(test))
		Rf_error(_("'%s' [type \"%s\"] is not logical"),
		         "test", Rf_type2char(TYPEOF(test)));
	if (!Rf_isVector(yes))
		Rf_error(_("'%s' [type \"%s\"] is not a vector"),
		         "yes", Rf_type2char(TYPEOF(yes)));
	if (!Rf_isVector(no))
		Rf_error(_("'%s' [type \"%s\"] is not a vector"),
		         "no", Rf_type2char(TYPEOF(no)));
	if (withna && !Rf_isVector(na))
		Rf_error(_("'%s' [type \"%s\"] is not a vector or NULL"),
		         "na", Rf_type2char(TYPEOF(na)));
	R_xlen_t nyes = XLENGTH(yes), nno = XLENGTH(no), \
		nna = (withna) ? XLENGTH(na) : 0, nans = XLENGTH(test);
	SEXPTYPE tyes = TYPEOF(yes), tno = TYPEOF(no), \
		tna = TYPEOF(na), tans = imax3(tyes, tno, tna);
	SEXP ans = PROTECT(Rf_allocVector(tans, nans)),
		dft = PROTECT(Rf_allocVector(tans, 1));
	PROTECT(yes = (nyes) ? Rf_coerceVector(yes, tans) : dft);
	PROTECT(no  = (nno ) ? Rf_coerceVector(no , tans) : dft);
	PROTECT(na  = (nna ) ? Rf_coerceVector(na , tans) : dft);
	const int *ptest = LOGICAL_RO(test);

#define IFELSE(get, set, navalue) \
	do { \
		set(dft, 0, navalue); \
		for (R_xlen_t j = 0; j < nans; ++j) \
			set(ans, j, \
			    (ptest[j] == NA_LOGICAL) \
			    ? get(na , (nna  <= 1) ? 0 : j % nna ) : \
			    (ptest[j]) \
			    ? get(yes, (nyes <= 1) ? 0 : j % nyes) \
			    : get(no , (nno  <= 1) ? 0 : j % nno )); \
	} while (0)
#define IFELSE_ATOMIC(type, ptr, navalue) \
	do { \
		type *pyes = ptr(yes), *pno = ptr(no), \
			*pna = ptr(na), *pans = ptr(ans), *pdft = ptr(dft); \
		IFELSE(ATOMIC_ELT, SET_ATOMIC_ELT, navalue); \
	} while (0)
#define     ATOMIC_ELT(x, i       ) p##x[i]
#define SET_ATOMIC_ELT(x, i, value) p##x[i] = value

	switch (tans) {
	case  RAWSXP: IFELSE_ATOMIC(   Rbyte,     RAW,          0); break;
	case  LGLSXP: IFELSE_ATOMIC(     int, LOGICAL, NA_LOGICAL); break;
	case  INTSXP: IFELSE_ATOMIC(     int, INTEGER, NA_INTEGER); break;
	case REALSXP: IFELSE_ATOMIC(  double,    REAL,    NA_REAL); break;
	case CPLXSXP: IFELSE_ATOMIC(Rcomplex, COMPLEX, NA_COMPLEX); break;
	case  STRSXP: IFELSE(STRING_ELT, SET_STRING_ELT,  NA_STRING); break;
	case  VECSXP:
	case EXPRSXP: IFELSE(VECTOR_ELT, SET_VECTOR_ELT, R_NilValue); break;
	}
	UNPROTECT(5);
	return ans;
}
