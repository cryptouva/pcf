#ifndef __OPDEFS_H
#define __OPDEFS_H

#include <search.h>
#include "pcflib.h"

struct clear_op_data
{
  uint32_t localsize;
};

void clear_op(struct PCFState * st, struct PCFOP * op);

void nop(struct PCFState * st, struct PCFOP * op);
void initbase_op(struct PCFState * st, struct PCFOP * op);

struct const_op_data
{
  uint32_t dest, value;
};

void const_op(struct PCFState * st, struct PCFOP * op);

struct bits_op_data
{
  uint32_t * dests;
  uint32_t ndests;
  uint32_t source;
};

void bits_op(struct PCFState*, struct PCFOP *);

struct join_op_data
{
  uint32_t * sources;
  uint32_t nsources;
  uint32_t dest;
};

void join_op(struct PCFState *, struct PCFOP*);

void mkptr_op(struct PCFState*, struct PCFOP*);

struct copy_op_data
{
  uint32_t dest;
  uint32_t source;
  uint32_t width;
};

void copy_op(struct PCFState *, struct PCFOP*);
void indir_copy_op(struct PCFState *, struct PCFOP*);
void copy_indir_op(struct PCFState *, struct PCFOP*);

struct gate_op_data
{
  uint8_t truth_table;
  uint32_t dest;
  uint32_t op1;
  uint32_t op2;
};

void gate_op(struct PCFState *, struct PCFOP*);

void ret_op(struct PCFState *, struct PCFOP*);

struct call_op_data
{
  uint32_t newbase;
  ENTRY * target;
};

void call_op(struct PCFState *, struct PCFOP*);
void ret_op(struct PCFState *, struct PCFOP*);

struct branch_op_data
{
  uint32_t cnd_wire;
  ENTRY * target;
};

void branch_op(struct PCFState *, struct PCFOP*);

struct arith_op_data
{
  uint32_t dest;
  uint32_t op1;
  uint32_t op2;
};

void add_op(struct PCFState *, struct PCFOP*);
void sub_op(struct PCFState *, struct PCFOP*);
void mul_op(struct PCFState *, struct PCFOP*);

#endif
