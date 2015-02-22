#include <Rinternals.h>

SEXP combine_strings(SEXP a, SEXP b){
  SEXP out = PROTECT(allocVector(STRSXP, length(a) + length(b)));
  for(int i = 0; i < length(a); i++){
    SET_STRING_ELT(out, i, STRING_ELT(a, i));
  }
  for(int i = 0; i < length(b); i++){
    SET_STRING_ELT(out, length(a) + i, STRING_ELT(b, i));
  }
  UNPROTECT(1);
  return out;
}

SEXP signal_exception(const char* message, SEXP conditions){
  SEXP vec = PROTECT(allocVector(VECSXP, 1));
  SET_VECTOR_ELT(vec, 0, mkString(message));
  setAttrib(vec, R_NamesSymbol, mkString("message"));

  //class vector
  SEXP cls = PROTECT(allocVector(STRSXP, 3));
  SET_STRING_ELT(cls, 0, mkChar("curl_error"));
  SET_STRING_ELT(cls, 1, mkChar("error"));
  SET_STRING_ELT(cls, 2, mkChar("condition"));
  SEXP classes = combine_strings(conditions, cls);
  setAttrib(vec, R_ClassSymbol, classes);

  //signal the condition by calling base::stop
  SEXP stop_sym  = PROTECT(Rf_install("stop"));
  SEXP call = PROTECT(Rf_lang2(stop_sym, vec));
  UNPROTECT(4);
  return Rf_eval(call, R_GlobalEnv);
}

SEXP signal_http_exception(int status){
  char err[16] = "http_status_";
  char msg[10] = "HTTP ";
  snprintf(err+12, 4, "%d", status);
  snprintf(msg+5, 4, "%d", status);
  err[15] = '\0';
  msg[9] = '\0';
  return signal_exception(msg, combine_strings(mkString(err), mkString("curl_status_error")));
}
