/* Operations on strings */

#include <string.h>
#include "alloc.h"
#include "fail.h"
#include "mlvalues.h"
#include "misc.h"

mlsize_t string_length(s)
     value s;
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return temp - Byte (s, temp);
}

value ml_string_length(s)     /* ML */
     value s;
{
  mlsize_t temp;
  temp = Bosize_val(s) - 1;
  Assert (Byte (s, temp - Byte (s, temp)) == 0);
  return Val_long(temp - Byte (s, temp));
}

value create_string(len)        /* ML */
     value len;
{
  mlsize_t size = Long_val(len);
  if (size > Max_wosize * sizeof(value) - 2) invalid_argument("String.create");
  return alloc_string(size);
}

value blit_string(argv, argc)   /* ML */
     value * argv;
     int argc;
{
  bcopy(&Byte(argv[0], Long_val(argv[1])),
        &Byte(argv[2], Long_val(argv[3])),
        Int_val(argv[4]));
  return Atom(0);
}

value fill_string(s, offset, len, init) /* ML */
     value s, offset, len, init;
{
  register char * p;
  register mlsize_t n;
  register char c;

  c = Long_val(init);
  for(p = &Byte(s, Long_val(offset)), n = Long_val(len);
      n > 0; n--, p++)
    *p = c;
  return Atom(0);
}

static unsigned char printable_chars_ascii[] = /* 0x20-0x7E */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000";
static unsigned char printable_chars_iso[] = /* 0x20-0x7E 0xA1-0xFF */
  "\000\000\000\000\377\377\377\377\377\377\377\377\377\377\377\177\000\000\000\000\376\377\377\377\377\377\377\377\377\377\377\377";

value is_printable(chr) /* ML */
     value chr;
{
  int c;
  static int iso_charset = -1;
  unsigned char * printable_chars;

  if (iso_charset == -1) {
    char * lc_ctype = (char *) getenv("LC_CTYPE");
    if (lc_ctype != 0 && strcmp(lc_ctype, "iso_8859_1") == 0)
      iso_charset = 1;
    else
      iso_charset = 0;
  }
  printable_chars = iso_charset ? printable_chars_iso : printable_chars_ascii;
  c = Int_val(chr);
  return Val_bool(printable_chars[c >> 3] & (1 << (c & 7)));
}
