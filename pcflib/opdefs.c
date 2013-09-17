#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <search.h>
#include <errno.h>
#include <string.h>
#include "opdefs.h"

void nop(struct PCFState * st, struct PCFOP * op)
{}

void initbase_op(struct PCFState * st, struct PCFOP * op)
{
  st->base = *((uint32_t*)op->data);
}

void mkptr_op(struct PCFState * st, struct PCFOP * op)
{
  uint32_t idx = *((uint32_t*)op->data);
  assert(st->wires[idx].flags == KNOWN_WIRE);

  st->wires[idx + st->base].value += st->base;
}

void const_op(struct PCFState * st, struct PCFOP * op)
{
  struct const_op_data * data = op->data;
  uint32_t idx = data->dest + st->base;
  st->wires[idx].value = data->value;
  if(st->wires[idx].keydata)
    st->delete_key(st->wires[idx].keydata);
  st->wires[idx].keydata = 0;
  st->wires[idx].flags = KNOWN_WIRE;
}

void bits_op(struct PCFState * st, struct PCFOP * op)
{
  struct bits_op_data * data = op->data;
  uint32_t s_idx = data->source + st->base;
  uint32_t i = 0, cval;

  assert(st->wires[s_idx].flags == KNOWN_WIRE);

  cval = st->wires[s_idx].value;

  for(i = 0; i < data->ndests; i++)
    {
      st->wires[data->dests[i] + st->base].value = (cval & 0x01);
      st->wires[data->dests[i] + st->base].flags = KNOWN_WIRE;

      if(st->wires[data->dests[i] + st->base].keydata != 0)
        st->delete_key(st->wires[data->dests[i] + st->base].keydata);

      st->wires[data->dests[i] + st->base].keydata = st->copy_key(st->constant_keys[cval & 0x01]);

      cval = cval >> 1;
    }
}

void call_op (struct PCFState * st, struct PCFOP * op)
{
  struct call_op_data * data = (struct call_op_data*)op->data;

  ENTRY * ent = data->target;
  ENTRY * r = 0;

  if(strcmp(data->target->key, "alice") == 0)
    {
      uint32_t i = 0;
      // Get the argument to this function
      for(i = 0; i < 32; i++)
        {
          st->wires[st->base + data->newbase + i].value = st->alice_inputs[i].value;
        }
    }
  else if(strcmp(data->target->key, "bob") == 0)
    {

      uint32_t i = 0;
      // Get the argument to this function
      for(i = 0; i < 32; i++)
        {
          st->wires[st->base + data->newbase + i].value = st->bob_inputs[i].value;
        }
    }
  else
    {
      struct activation_record * newtop = malloc(sizeof(struct activation_record));
      check_alloc(newtop);

      newtop->rest = st->call_stack;
      newtop->ret_pc = st->PC;
      st->call_stack = newtop;

      if(hsearch_r(*ent, FIND, &r, st->labels) == 0)
        {
          fprintf(stderr, "Problem searching hash table: %s\n", strerror(errno));
          abort();
        }
 
      long * target = r->data;
      st->PC = *target;
      st->base += data->newbase;
    }
}

void gate_op(struct PCFState * st, struct PCFOP * op)
{
  struct PCFGate * data = (struct PCFGate*)op->data;
  uint32_t op1idx = data->wire1 + st->base;
  uint32_t op2idx = data->wire2 + st->base;
  uint32_t destidx = data->reswire + st->base;
  uint8_t bits[4];
  int8_t i = 0;
  uint8_t tab = data->truth_table;
  for(i = 3; i >= 0; i--)
    {
      bits[i] = tab & 0x01;
      tab = tab >> 1;
    }

  assert(st->curgate == 0);


  st->delete_key(st->wires[destidx].keydata);

  if((st->wires[op1idx].flags != KNOWN_WIRE) || (st->wires[op2idx].flags != KNOWN_WIRE))
    {
      // Time for the callback
      assert((st->wires[op1idx].keydata != 0) && (st->wires[op2idx].keydata != 0));

      st->curgate->wire1 = op1idx;
      st->curgate->wire2 = op2idx;
      st->curgate->reswire = destidx;
      st->curgate->truth_table = data->truth_table;
      
      st->wires[destidx].keydata = st->copy_key(st->callback(st, st->curgate));
      st->wires[destidx].flags = UNKNOWN_WIRE;
    }
  else
    {
      // Check that we are dealing only with bits
      assert((st->wires[op1idx].value < 2) && (st->wires[op2idx].value < 2));
      st->wires[destidx].keydata = st->copy_key(st->constant_keys[bits[(st->wires[op1idx].value) + (2*(st->wires[op2idx].value))]]);
      st->wires[destidx].value = bits[(st->wires[op1idx].value) + (2*(st->wires[op2idx].value))];
      st->wires[destidx].flags = KNOWN_WIRE;
    }
}

void copy_op(struct PCFState * st, struct PCFOP * op)
{
  struct copy_op_data * data = (struct copy_op_data*)op->data;
  uint32_t i;
  uint32_t dest = data->dest + st->base;
  uint32_t source = data->source + st->base;
  for(i = 0; i < data->width; i++)
    {
      if(st->wires[source+i].keydata != 0)
        st->wires[dest+i].keydata = st->copy_key(st->wires[source+i].keydata);

      st->wires[dest+i].value = st->wires[source+i].value;
      st->wires[dest+i].flags = st->wires[source+i].flags;
    }
}

void indir_copy_op(struct PCFState * st, struct PCFOP * op)
{
  struct copy_op_data * data = (struct copy_op_data*)op->data;
  uint32_t dest = st->wires[data->dest + st->base].value;
  uint32_t source = data->source + st->base;
  uint32_t i;
  for(i = 0; i < data->width; i++)
    {
      if(st->wires[source+i].keydata != 0)
        st->wires[dest+i].keydata = st->copy_key(st->wires[source+i].keydata);

      st->wires[dest+i].value = st->wires[source+i].value;
      st->wires[dest+i].flags = st->wires[source+i].flags;
    }
}

void copy_indir_op(struct PCFState * st, struct PCFOP * op)
{
  struct copy_op_data * data = (struct copy_op_data*)op->data;
  uint32_t dest = data->dest + st->base;
  uint32_t source = st->wires[data->source + st->base].value;
  uint32_t i;
  for(i = 0; i < data->width; i++)
    {
      if(st->wires[source+i].keydata != 0)
        st->wires[dest+i].keydata = st->copy_key(st->wires[source+i].keydata);

      st->wires[dest+i].value = st->wires[source+i].value;
      st->wires[dest+i].flags = st->wires[source+i].flags;
    }

}
