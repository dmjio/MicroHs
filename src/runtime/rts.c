/* Copyright 2025 Lennart Augustsson
 * See LICENSE file for full license.
 */

/*
 * Standalone runtime that runs a combinator program supplied as a byte array.
 *
 * When compiled with emscripten, main() reads the combinator file named on
 * the command line (in JavaScript, with node's fs module), and passes the
 * bytes to mhs_run_comb():
 *   node rts.js prog.comb arg ...
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#if defined(__EMSCRIPTEN__)
#include "emscripten.h"
#endif

unsigned char *combexpr = 0;
int combexprlen = 0;
struct ffi_entry *xffi_table = 0;
struct ffe_entry *xffe_table = 0;

int mhs_main(int argc, char **argv);

/*
 * Run the combinator program in comb[0..len-1].
 * argv[0] is the program name, the remaining arguments are for the program
 * (with +RTS ... -RTS handled as usual).
 * Does not return; exits with the program's exit code.
 */
#if defined(__EMSCRIPTEN__)
EMSCRIPTEN_KEEPALIVE
#endif
int
mhs_run_comb(const unsigned char *comb, int len, int argc, char **argv)
{
  combexpr = (unsigned char *)comb;
  combexprlen = len;
  return mhs_main(argc, argv);
}

#if defined(__EMSCRIPTEN__)
/* Read a file (with node's fs) into a malloc()ed buffer, return 0 on failure. */
EM_JS(unsigned char *, mhs_read_file, (const char *name, int *lenp), {
  var bytes;
  try {
    bytes = require('fs').readFileSync(UTF8ToString(name));
  } catch (e) {
    return 0;
  }
  var p = _malloc(bytes.length);
  HEAPU8.set(bytes, p);
  setValue(lenp, bytes.length, 'i32');
  return p;
});

int
main(int argc, char **argv)
{
  if (argc < 2) {
    fprintf(stderr, "Usage: node rts.js FILE.comb [+RTS ... -RTS] arg ...\n");
    exit(1);
  }
  int len;
  unsigned char *comb = mhs_read_file(argv[1], &len);
  if (!comb) {
    fprintf(stderr, "cannot read %s\n", argv[1]);
    exit(1);
  }
  /* Drop argv[0] (rts.js) so the program name becomes the combinator file. */
  return mhs_run_comb(comb, len, argc - 1, argv + 1);
}
#endif  /* __EMSCRIPTEN__ */
