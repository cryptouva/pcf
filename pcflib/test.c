#include "pcflib.h"

#include <stdio.h>
#include <malloc.h>

void read_instr(const char * line);

int main(int argc, char**argv)
{
  char line[128];
  fgets(line, 127, stdin);

  read_instr(line);

  return 0;
}
