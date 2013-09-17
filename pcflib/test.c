#include "pcflib.h"

#include <stdio.h>
#include <malloc.h>
#include <limits.h>

void read_instr(const char * line);

int main(int argc, char**argv)
{
  char line[LINE_MAX];
  fgets(line, LINE_MAX-1, stdin);

  read_instr(line);

  return 0;
}
