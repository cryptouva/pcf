#ifndef __PCFLIB_H
#define __PCFLIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <pthread.h>
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <search.h>

  struct PCFState;

  /* This should be immutable -- we should be able to safely make a shallow copy. */
typedef struct PCFOP {
  void * data;
  void (* op)(struct PCFState*, struct PCFOP*);
}PCFOP;

typedef struct PCFGate {
  uint32_t wire1;
  uint32_t wire2;
  uint32_t reswire;


  /* The truth table is laid out like this:
     reswire = truth_table{wire1 + 2 * wire2}

     In other words, the truth table bits b0-b3 will be:

     wire1 | wire2 | reswire
      0    |  0    |  b0
      1    |  0    |  b1
      0    |  1    |  b2
      1    |  1    |  b3
   */
  uint8_t truth_table;

  // The tag
  uint32_t tag;
}PCFGate;

  enum {TAG_INTERNAL = 0, TAG_INPUT_A = 2, TAG_INPUT_B = 4, TAG_OUTPUT_A = 6, TAG_OUTPUT_B = 8};

typedef struct wire {
  uint32_t value;
  uint8_t flags;

  /* The key(s) associated with this wire, which will be set by some
     external function. */
  void * keydata;
}wire;

  void * getWireKey(struct wire *);
  void setWireKey(struct wire *, void *);

  enum {KNOWN_WIRE = 0, UNKNOWN_WIRE = 1};

struct activation_record {
  uint32_t ret_pc;
  uint32_t base;
  struct activation_record * rest;
};

  struct activation_record * copy_activation_record(struct activation_record *);
  void check_alloc(void * ptr);

typedef struct PCFState {
  uint32_t PC;
  wire * wires;
  uint32_t wire_table_size;

  uint32_t base;

  PCFOP * ops;

  uint32_t icount;

  struct hsearch_data * labels;
  uint32_t alice_in_size;
  uint32_t bob_in_size;
  PCFGate * curgate;
  wire * alice_inputs;
  wire * bob_inputs;
  wire * alice_outputs;
  wire * bob_outputs;

  void * constant_keys[2];

  PCFGate input_g;

  int32_t inp_i;
  uint32_t inp_idx;

  struct activation_record * call_stack;

  uint8_t done;

  PCFGate g_copy;

  /* This is the state of the program that is utilizing this library,
     which may be needed by the callback function to generate the keys
     for the wires in the circuit. */
  void * external_state;

  /* This function is called when a gate should be emitted.  It should
     create the appropriate keys for the output wire of the gate, and
     return them. */
  void * (*callback)(struct PCFState *, struct PCFGate*);

  /* The function that will be used to make copies of the keys
     associated with a wire. */
  void * (*copy_key)(void *);

  /* The function that will be used to delete keys when a wire is
     destroyed. */
  void (*delete_key)(void*);
}PCFState;

  enum {ALICE = 0, BOB = 1};

  void set_external_state(struct PCFState *, void *);
  void * get_external_state(struct PCFState *);
  void set_key_delete_function(struct PCFState *, void (*)(void*));
  void set_key_copy_function(struct PCFState *, void *(*)(void*));
  void set_callback(struct PCFState *, void* (*)(struct PCFState *, struct PCFGate *));
  PCFGate * get_next_gate(PCFState *);
  void reinitialize(PCFState *);
  PCFState * load_pcf_file(const char *, void *, void *, void *(*)(void*));

  void set_constant_keys(PCFState *, void *, void*);

  uint32_t get_input_size(PCFState *, uint32_t);

  PCFState * copy_pcf_state(struct PCFState *);

  wire * getWire(struct PCFState *, uint32_t);
  void * get_wire_key(struct PCFState *, uint32_t);
  void set_wire_key(struct PCFState *, uint32_t, void *);

  uint32_t read_alice_length(const char *);
  uint32_t read_bob_length(const char *); 
  void make_internal_thread(PCFState * st);

#ifdef __cplusplus
}
#endif
#endif //__PCFLIB_H
