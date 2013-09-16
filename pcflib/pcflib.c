#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include <search.h>
#include <assert.h>

#include "pcflib.h"
#include "opdefs.h"

void check_alloc(void * ptr)
{
 if(ptr == 0)
    {
      fprintf(stderr, "Failed to allocate memory: %s\n", strerror(errno));
      exit(-1);
    }
}

struct label {
  char * str;
};

const char * skip_to_colon(const char * line)
{
  while(line[0] != ':')
    {
      assert(line[0] != '\0');
      line++;
    }
  return line;
}

const char * assert_token(const char * line, char * buf, char * bitr, const char * token)
{
  while(line[0] != ' ')
    {
      assert(line[0] != '\0');
      bitr[0] = line[0];
      line++;
      bitr++;
    }
  bitr[0] = '\0';

  assert(strcmp(buf, token) == 0);

  return line;
}

const char * read_token(const char * line, char * bitr)
{
  while(line[0] == ' ')
    {
      assert(line[0] != '\0');
      line++;
    }
  while(line[0] != ' ')
    {
      assert(line[0] != '\0');
      bitr[0] = line[0];
      bitr++;
      line++;
    }
  bitr[0] = '\0';
  return line;
}

PCFOP * read_label(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(struct PCFOP));
  check_alloc(ret);

  ret->op = nop;

  bitr = buf;
  
  line = skip_to_colon(line);

  // Skip over the ':'
  line++;

  line = assert_token(line, buf, bitr, "STR");

  while((line[0] == ' ') || (line[0] == '"'))
    {
      assert(line[0] != '\0');
      line++;
    }

  bitr = buf;

  while((line[0] != ' ') && (line[0] != '"') && (line[0] != ')'))
    {
      assert(line[0] != '\0');
      bitr[0] = line[0];
      line++;
      bitr++;
    }
  bitr[0] = '\0';
  printf("%s\n", buf);

  return ret;
}

PCFOP * read_initbase(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(struct PCFOP));
  check_alloc(ret);

  ret->op = initbase_op;

  uint32_t base;
  bitr = buf;
  bitr[0] = '\0';
  line = skip_to_colon(line);
  line++;

  line = assert_token(line, buf, bitr, "BASE");

  sscanf(line, "%d\n", &base);
  assert(base >= 0);
  printf("%d\n", base);

  ret->data = malloc(sizeof(uint32_t));
  check_alloc(ret->data);

  *((uint32_t*)ret->data) = base;

  return ret;
}

PCFOP * read_const(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  struct const_op_data * data = malloc(sizeof(struct const_op_data));

  check_alloc(ret);
  check_alloc(data);


  bitr = buf;

  ret->op = const_op;
  ret->data = data;

  bitr[0] = '\0';
  line = skip_to_colon(line);
  line++;

  bitr = buf;
  line = read_token(line, bitr);
  if(strcmp(buf, "DEST") == 0)
    {
      bitr = buf;
      line = read_token(line, bitr);
      assert(sscanf(buf, "%d", &data->dest) == 1);
    }
  else if(strcmp(buf, "OP1") == 0)
    {
      bitr = buf;
      line = read_token(line, bitr);
      assert(sscanf(buf, "%d", &data->dest) == 1);
    }
  else assert(0);

  line = skip_to_colon(line);
  line++;

  bitr = buf;
  line = read_token(line, bitr);
  if(strcmp(buf, "DEST") == 0)
    {
      bitr = buf;
      line = read_token(line, bitr);
      assert(sscanf(buf, "%d", &data->dest) == 1);
    }
  else if(strcmp(buf, "OP1") == 0)
    {
      bitr = buf;
      line = read_token(line, bitr);
      assert(sscanf(buf, "%d", &data->dest) == 1);
    }
  else assert(0);

  return ret;
}

PCFOP * read_instr(const char * line)
{
  char buf[LINE_MAX], *bitr;
  buf[0] = '\0';
  bitr = buf;

  assert(line[0] == '(');
  line++;

  while((line[0] != ' ') && (line[0] != ')'))
    {
      bitr[0] = line[0];
      line++;
      bitr++;
    }
  bitr[0] = '\0';
  printf("%s\n", buf);

  if(strcmp(buf, "LABEL") == 0)
    return read_label(line);
  else if(strcmp(buf, "INITBASE") == 0)
    return read_initbase(line);
  else if(strcmp(buf, "CONST") == 0)
    return read_const(line);
  assert(0);
}

PCFState * load_pcf_file(const char * fname, void * key0, void * key1, void *(*copy_key)(void*))
{
  FILE * input;
  PCFState * ret;
  //char line[LINE_MAX];
  //uint32_t icount = 0;
  //uint32_t i = 0;

  ret = (PCFState*)malloc(sizeof(struct PCFState));
  ret->alice_outputs = 0;
  ret->bob_outputs = 0;
  ret->inp_i = 0;
  check_alloc(ret);

  input = fopen(fname, "r");

  while(!feof(input))
    {
      
    }
  return ret;
}
