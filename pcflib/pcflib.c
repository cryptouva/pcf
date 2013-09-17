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

PCFOP * read_gate(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  struct PCFGate * data = malloc(sizeof(struct PCFGate));
  uint32_t i = 0;
  check_alloc(ret);
  check_alloc(data);
  bitr = buf;
  ret->op = gate_op;
  ret->data = data;

  bitr[0] = '\0';
  for(i = 0; i < 4; i++)
    {
      line = skip_to_colon(line);
      line++;

      bitr = buf;
      line = read_token(line, bitr);
      if(strcmp(buf, "DEST") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->reswire) == 1);
        }
      else if(strcmp(buf, "OP1") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->wire1) == 1);
        }

      else if(strcmp(buf, "OP2") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->wire2) == 1);
        }

      else if(strcmp(buf, "TRUTH-TABLE") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(buf[0] == '#');
          assert(buf[1] == '*');
          data->truth_table = 
            (buf[2] == '1' ? 1 : 0) |
            (buf[3] == '1' ? 2 : 0) |
            (buf[4] == '1' ? 4 : 0) |
            (buf[5] == '1' ? 8 : 0);
        }
      else assert(0);
    }
  return ret;
}

PCFOP * read_copy(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  struct copy_op_data * data = malloc(sizeof(struct copy_op_data));
  uint32_t i = 0;
  check_alloc(ret);
  check_alloc(data);
  bitr = buf;
  ret->op = copy_op;
  ret->data = data;

  bitr[0] = '\0';
  for(i = 0; i < 3; i++)
    {
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
          assert(sscanf(buf, "%d", &data->source) == 1);
        }

      else if(strcmp(buf, "OP2") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->width) == 1);
        }
      else assert(0);
    }
  return ret;
}

PCFOP * read_copy_indir(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  struct copy_op_data * data = malloc(sizeof(struct copy_op_data));
  uint32_t i = 0;
  check_alloc(ret);
  check_alloc(data);
  bitr = buf;
  ret->op = copy_indir_op;
  ret->data = data;

  bitr[0] = '\0';
  for(i = 0; i < 3; i++)
    {
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
          assert(sscanf(buf, "%d", &data->source) == 1);
        }

      else if(strcmp(buf, "OP2") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->width) == 1);
        }
      else assert(0);
    }
  return ret;
}


PCFOP * read_indir_copy(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  struct copy_op_data * data = malloc(sizeof(struct copy_op_data));
  uint32_t i = 0;
  check_alloc(ret);
  check_alloc(data);
  bitr = buf;
  ret->op = indir_copy_op;
  ret->data = data;

  bitr[0] = '\0';
  for(i = 0; i < 3; i++)
    {
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
          assert(sscanf(buf, "%d", &data->source) == 1);
        }

      else if(strcmp(buf, "OP2") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->width) == 1);
        }
      else assert(0);
    }
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

int count_tokens_to_close_paren(const char * line)
{
  uint32_t cnt = 0;
  while(line[0] == ' ') line++;
  assert(line[0] == '(');
  line++;
  while(line[0] != ')')
    {
      assert(line[0] != '\0');
      cnt++;
      while((line[0] != ' ') && (line[0] != ')')) line++;
      while((line[0] == ' ') && (line[0] != ')')) line++;
    }
  return cnt;
}

PCFOP * read_bits(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = (PCFOP*)malloc(sizeof(PCFOP));
  uint32_t i = 0;

  struct bits_op_data * data = (struct bits_op_data*)malloc(sizeof(struct bits_op_data));

  check_alloc(ret);
  check_alloc(data);

  ret->op = bits_op;
  ret->data = data;
  bitr = buf;

  for(i = 0; i < 2; i++)
    {
      line = skip_to_colon(line);
      line++;

      bitr = buf;
      line = read_token(line, bitr);

      if(strcmp(buf, "DEST") == 0)
        {
          int cnt = count_tokens_to_close_paren(line);
          data->ndests = cnt;
          char buf2[10];
          int i, j, k;
          fprintf(stderr, "%d", cnt);

          data->dests = (uint32_t*)malloc(cnt * sizeof(uint32_t));
          check_alloc(data->dests);
          i = 0;
          j = 0;
          k = 0;
          while(line[i] == '(') i++;
          while(line[i] == ' ') i++;
          while(line[i] == '(') i++;
          while(line[i] != ')')
            {
              assert(line[i] != '\0');
              while((line[i] != ' ') && (line[i] != ')'))
                {
                  assert(line[i] != '\0');
                  buf2[j] = line[i];
                  i++;
                  j++;
                }
              buf2[j] = '\0';
              assert(sscanf(buf2, "%d", &data->dests[k]) == 1);
              k++;
              while(line[i] == ' ') 
                {
                  assert(line[i] != '\0');
                  i++;
                }
              j = 0;
            }
          assert(k == cnt);
          line += i;
        }
      else if(strcmp(buf, "OP1") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->source) == 1);
        }
      else
        {
          assert(0);
        }
    }
  return ret;
}

PCFOP * read_mkptr(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  uint32_t * data = malloc(sizeof(uint32_t));

  check_alloc(ret);
  check_alloc(data);

  ret->op = mkptr_op;
  ret->data = data;

  bitr = buf;

  line = skip_to_colon(line);
  line++;

  bitr = buf;
  line = read_token(line, bitr);

  if(strcmp(buf, "DEST") == 0)
    {
      bitr = buf;
      line = read_token(line, bitr);
      assert(sscanf(buf, "%d", data) == 1);
    }
  else
    assert(0);

  return ret;
}

PCFOP * read_call(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = (PCFOP*)malloc(sizeof(PCFOP));
  uint32_t i = 0;

  struct call_op_data * data = (struct call_op_data*)malloc(sizeof(struct call_op_data));

  check_alloc(ret);
  check_alloc(data);

  ret->op = call_op;
  ret->data = data;
  bitr = buf;
  
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
  else if(strcmp(buf, "GATE") == 0)
    return read_gate(line);
  else if(strcmp(buf, "BITS") == 0)
    return read_bits(line);
  else if(strcmp(buf, "MKPTR") == 0)
    return read_mkptr(line);
  else if(strcmp(buf, "COPY") == 0)
    return read_copy(line);
  else if(strcmp(buf, "COPY-INDIR") == 0)
    return read_copy_indir(line);
  else if(strcmp(buf, "INDIR-COPY") == 0)
    return read_indir_copy(line);
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
