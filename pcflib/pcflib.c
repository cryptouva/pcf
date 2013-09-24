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

PCFOP * read_label(const char * line, struct PCFState * st, uint32_t iptr)
{
  ENTRY * newent, * r;
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

  newent = (ENTRY*)malloc(sizeof(ENTRY));
  check_alloc(newent);

  newent->key = malloc(strlen(buf)+1);
  check_alloc(newent->key);
  strcpy(newent->key, buf);

  newent->data = malloc(sizeof(uint32_t));
  check_alloc(newent->data);
  *((uint32_t*)newent->data) = iptr;

  //hsearch_r(*newent, ENTER, &r, st->labels);

  if(hsearch_r(*newent, ENTER, &r, st->labels) == 0)
    {
      fprintf(stderr, "Problem inserting hash table for %s %d: %s\n", newent->key, *((uint32_t*)newent->data), strerror(errno));
      abort();
    }


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

  data->tag = TAG_INTERNAL;

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

PCFOP * read_arith(const char * line, void (*op)(struct PCFState *, struct PCFOP *))
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = malloc(sizeof(PCFOP));
  struct arith_op_data * data = malloc(sizeof(struct arith_op_data));
  uint32_t i = 0;
  check_alloc(ret);
  check_alloc(data);
  bitr = buf;
  ret->op = op;
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
          assert(sscanf(buf, "%d", &data->op1) == 1);
        }

      else if(strcmp(buf, "OP2") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->op2) == 1);
        }
      else assert(0);
    }

  return ret;
}

PCFOP * read_add(const char * line)
{
  return read_arith(line, add_op);
}

PCFOP * read_mul(const char * line)
{
  return read_arith(line, mul_op);
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
      assert(sscanf(buf, "%d", &data->value) == 1);
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
      assert(sscanf(buf, "%d", &data->value) == 1);

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

PCFOP * read_join(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = (PCFOP*)malloc(sizeof(PCFOP));
  uint32_t i = 0;

  struct join_op_data * data = (struct join_op_data*)malloc(sizeof(struct join_op_data));

  check_alloc(ret);
  check_alloc(data);

  ret->op = join_op;
  ret->data = data;
  bitr = buf;

  for(i = 0; i < 2; i++)
    {
      line = skip_to_colon(line);
      line++;

      bitr = buf;
      line = read_token(line, bitr);

      if(strcmp(buf, "OP1") == 0)
        {
          int cnt = count_tokens_to_close_paren(line);
          data->nsources = cnt;
          char buf2[10];
          int i, j, k;

          data->sources = (uint32_t*)malloc(cnt * sizeof(uint32_t));
          check_alloc(data->sources);
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
              assert(sscanf(buf2, "%d", &data->sources[k]) == 1);
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
      else if(strcmp(buf, "DEST") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->dest) == 1);
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

  for(i = 0; i < 2; i++)
    {
      line = skip_to_colon(line);
      line++;

      bitr = buf;
      line = read_token(line, bitr);

      if(strcmp(buf, "NEWBASE") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->newbase) == 1);
        }
      else if(strcmp(buf, "FNAME") == 0)
        {
          ENTRY * ent = (ENTRY*)malloc(sizeof(ENTRY));
          check_alloc(ent);

          while((line[0] == ' ') || (line[0] == '"'))
            {
              assert(line[0] != '\0');
              line++;
            }

          bitr = buf;
          line = read_token(line, bitr);

          assert(buf[strlen(buf)-1] == '"');
          buf[strlen(buf)-1] = '\0';

          ent->key = malloc(strlen(buf)+1);
          strcpy(ent->key, buf);
          data->target = ent;
        }
      else
        {
          assert(0);
        }
    }
  
  return ret;
}

PCFOP * read_branch(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = (PCFOP*)malloc(sizeof(PCFOP));
  uint32_t i = 0;

  struct branch_op_data * data = (struct branch_op_data*)malloc(sizeof(struct call_op_data));

  check_alloc(ret);
  check_alloc(data);

  ret->op = branch_op;
  ret->data = data;
  bitr = buf;

  for(i = 0; i < 2; i++)
    {
      line = skip_to_colon(line);
      line++;

      bitr = buf;
      line = read_token(line, bitr);

      if(strcmp(buf, "CND") == 0)
        {
          bitr = buf;
          line = read_token(line, bitr);
          assert(sscanf(buf, "%d", &data->cnd_wire) == 1);
        }
      else if(strcmp(buf, "TARG") == 0)
        {
          ENTRY * ent = (ENTRY*)malloc(sizeof(ENTRY));
          check_alloc(ent);

          while((line[0] == ' ') || (line[0] == '"'))
            {
              assert(line[0] != '\0');
              line++;
            }

          bitr = buf;
          line = read_token(line, bitr);

          assert(buf[strlen(buf)-1] == '"');
          buf[strlen(buf)-1] = '\0';

          ent->key = malloc(strlen(buf)+1);
          strcpy(ent->key, buf);
          data->target = ent;
        }
      else
        {
          assert(0);
        }
    }
  
  return ret;
}

PCFOP * read_ret(const char * line)
{
  PCFOP * ret = (PCFOP*)malloc(sizeof(PCFOP));
  ret->op = ret_op;
  return ret;
}

PCFOP * read_clear(const char * line)
{
  char buf[LINE_MAX], *bitr;
  PCFOP * ret = (PCFOP*)malloc(sizeof(PCFOP));
  struct clear_op_data * data = (struct clear_op_data*)malloc(sizeof(struct clear_op_data));

  check_alloc(ret);
  check_alloc(data);

  ret->op = clear_op;

  bitr = buf;
  bitr[0] = '\0';
  line = skip_to_colon(line);
  line++;

  line = assert_token(line, buf, bitr, "LOCALSIZE");

  sscanf(line, "%d\n", &data->localsize);
  assert(data->localsize >= 0);

  ret->data = data;
  return ret;
}

PCFOP * read_instr(struct PCFState * st, const char * line, uint32_t iptr)
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

  if(strcmp(buf, "LABEL") == 0)
    return read_label(line, st, iptr);
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
  else if(strcmp(buf, "CALL") == 0)
    return read_call(line);
  else if(strcmp(buf, "RET") == 0)
    return read_ret(line);
  else if(strcmp(buf, "BRANCH") == 0)
    return read_branch(line);
  else if(strcmp(buf, "CLEAR") == 0)
    return read_clear(line);
  else if(strcmp(buf, "JOIN") == 0)
    return read_join(line);
  else if(strcmp(buf, "ADD") == 0)
    return read_add(line);
  else if(strcmp(buf, "MUL") == 0)
    return read_mul(line);
  assert(0);
}

PCFState * load_pcf_file(const char * fname, void * key0, void * key1, void *(*copy_key)(void*))
{
  FILE * input;
  PCFState * ret;
  char line[LINE_MAX];
  uint32_t icount = 0;
  uint32_t i = 0;

  ret = (PCFState*)malloc(sizeof(struct PCFState));
  check_alloc(ret);

  ret->alice_outputs = 0;
  ret->bob_outputs = 0;
  ret->inp_i = 0;
  ret->constant_keys[0] = copy_key(key0);
  ret->constant_keys[1] = copy_key(key1);
  ret->copy_key = copy_key;
  ret->call_stack = 0;
  ret->done = 0;
  ret->labels = (struct hsearch_data *)malloc(sizeof(struct hsearch_data));
  check_alloc(ret->labels);

  ret->wires = (struct wire *)malloc(1000000 * sizeof(struct wire));
  check_alloc(ret->wires);

  for(i = 0; i < 200000; i++)
    {
      ret->wires[i].flags = KNOWN_WIRE;
      ret->wires[i].value = 0;
      ret->wires[i].keydata = copy_key(key0);
    }

  memset(ret->labels, 0, sizeof(struct hsearch_data));

  ret->done = 0;
  ret->base = 1;
  ret->PC = 0;

  fprintf(stderr, "%s\n", fname);
  input = fopen(fname, "r");
  if(input == 0)
    {
      fprintf(stderr, "%s: %s\n", fname, strerror(errno));
      assert(0);
    }

  while(!feof(input))
    {
      fgets(line, LINE_MAX-1, input);
      icount++;
    }

  if(hcreate_r(icount, ret->labels) == 0)
    {
      fprintf(stderr, "Unable to allocate hash table: %s\n", strerror(errno));
      abort();
      //      exit(-1);
    }

  ret->icount = icount;
  ret->ops = (PCFOP*)malloc(icount * sizeof(PCFOP));
  check_alloc(ret->ops);

  assert(fseek(input, 0, SEEK_SET) == 0);

  icount = 0;

  while(!feof(input))
    {
      PCFOP * op;
      fgets(line, LINE_MAX-1, input);
      op = read_instr(ret, line, icount);
      ret->ops[icount] = *op;
      free(op);
      icount++;
    }

  fclose(input);

  ret->wires[0].value = 1;
  ret->wires[0].keydata = ret->copy_key(ret->constant_keys[1]);
  ret->wires[0].flags = KNOWN_WIRE;

  return ret;
}

void finalize(PCFState * st)
{
  uint32_t i = 0;
  for(i = 0; i < 200000; i++)
    {
      if(st->wires[i].keydata != 0)
        st->delete_key(st->wires[i].keydata);
    }
  free(st->wires);
  //  free(st);
}

struct PCFGate * get_next_gate(struct PCFState * st)
{
  st->curgate = 0;
  while((st->curgate == 0) && (st->done == 0))
    {
      st->ops[st->PC].op(st, &st->ops[st->PC]);
      st->PC++;
      assert((st->PC < st->icount));
    }
  if((st->curgate == 0) || (st->done != 0))
    {
      finalize(st);
      return 0;
    }
  else
    return st->curgate;
}

void * get_wire_key(struct PCFState * st, uint32_t idx)
{
  return st->wires[idx].keydata;
}

void set_wire_key(struct PCFState * st, uint32_t idx, void * kd)
{
  setWireKey(&st->wires[idx], kd);
}

wire * getWire(struct PCFState * st, uint32_t idx)
{
  return &st->wires[idx];
}

void set_external_state(struct PCFState * st, void * et)
{
  st->external_state = et;
}

void * get_external_state(struct PCFState * st)
{
  return st->external_state;
}

void * getWireKey(struct wire * w)
{
  return w->keydata;
}

void setWireKey(struct wire * w, void * k)
{
  w->keydata = k;
}

void set_callback(struct PCFState * st, void* (*callback)(struct PCFState *,struct PCFGate*))
{
  st->callback = callback;
}

void set_key_copy_function(struct PCFState * st, void *(*f)(void*))
{
  st->copy_key = f;
}

void set_key_delete_function(struct PCFState * st, void (*f)(void*))
{
  st->delete_key = f;
}

uint32_t read_alice_length(const char * fname)
{
  uint32_t alice_in = -1;
  char line[LINE_MAX];
  FILE * cnf;
  cnf = fopen(fname, "r");
  // Assume that the inputs are all on one line -- probably a bad assumption, but whatever
  fgets(line, LINE_MAX-1, cnf);
  fclose(cnf);
  alice_in = (strlen(line) - 1) * 4;
  return alice_in;
}

uint32_t read_bob_length(const char * fname)
{
  uint32_t bob_in = 32;
  FILE * cnf;
  char line[LINE_MAX];
  cnf = fopen(fname, "r");
  // Bob's input is on the second line
  //
  // The input file format should be like this:
  //
  // 0xALICEINPUTSINHEX
  // 0x0000000000000000
  //
  // or
  //
  // 0x0000000000000000
  // 0xBOBINPUTSINHEX00
  fgets(line, LINE_MAX-1, cnf);
  fgets(line, LINE_MAX-1, cnf);
  fclose(cnf);
  bob_in = (strlen(line) - 1) * 4;
  return bob_in;
}
