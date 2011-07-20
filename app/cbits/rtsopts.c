/*
 * At least under GHC 7.0.x, this application performs best if the
 * parallel garbage collector is disabled.
 */

char *ghc_rts_opts = "-N -qg";
