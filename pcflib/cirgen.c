// Like test.c but prints an explicit list of gates for this circuit

#include "pcflib.h"

#include <stdio.h>
#include <malloc.h>
#include <limits.h>
#include <assert.h>
#include <string.h>

void read_instr(const char * line);

uint32_t key0 = 0;
uint32_t key1 = 1;

void * copy_key(void * k)
{
  uint32_t * ret = (uint32_t*)malloc(sizeof(uint32_t));
  *ret = *((uint32_t*)k);
  return ret;
}

void delete_key(void* k)
{
  assert(k != 0);
  if(k != 0)
    free(k);
}

uint32_t wcnt = 0;

void * m_callback(struct PCFState * st, struct PCFGate * gate)
{
  // We just want to associate a unique ID with each wire
  if(gate->tag == TAG_INTERNAL)
    {
      wcnt++;
      printf("Gate: %u %u -> %u %hhu\n", *((uint32_t*)(get_wire_key(st,gate->wire1))), 
             *((uint32_t*)(get_wire_key(st,gate->wire2))), wcnt, gate->truth_table);

      // Checks to make sure XOR gates don't have the same input wires. If
      // they do this will leak information in Free XOR garbled circuit
      // systems.
      // assert( !( (is xor gate) && (wire 1 id == wire 2 id) ) )
      assert(!((gate->truth_table == 6) && ((*((uint32_t*)(get_wire_key(st,gate->wire1)))) == (*((uint32_t*)(get_wire_key(st,gate->wire2)))))));
    }
  else if(gate->tag == TAG_INPUT_A)
    {
      wcnt++;
      printf("Input -> %u\n", wcnt);
    }

  else if(gate->tag == TAG_INPUT_B)
    {
      wcnt++;
      printf("Input -> %u\n", wcnt);
    }

  else 
    {
      printf("%u -> Output\n", *((uint32_t*)(get_wire_key(st,gate->wire1))));
    }
  return &wcnt;
}

void setup_alice_inputs_from_string(struct PCFState * st, const char * inputs)
{
  uint32_t i,j;
  st->alice_inputs = (struct wire*)malloc(strlen(inputs) * 8 * sizeof(struct wire));
  for(i = 0; i < strlen(inputs); i++)
    {
      uint8_t q = inputs[i];
      for(j = 0; j < 8; j++)
        {
          st->alice_inputs[8*i + j].flags = UNKNOWN_WIRE;
          st->alice_inputs[8*i + j].keydata = copy_key((q & 0x01) == 1 
                                                       ? (st->constant_keys[1]) 
                                                       : (st->constant_keys[0]));
          q = q >> 1;
        }
    }
  st->alice_in_size = 8 * strlen(inputs);
}

void setup_bob_inputs_from_string(struct PCFState * st, const char * inputs)
{
  uint32_t i,j;
  st->bob_inputs = (struct wire*)malloc(strlen(inputs) * 8 * sizeof(struct wire));
  for(i = 0; i < strlen(inputs); i++)
    {
      uint8_t q = inputs[i];
      for(j = 0; j < 8; j++)
        {
          st->bob_inputs[8*i + j].flags = UNKNOWN_WIRE;
          st->bob_inputs[8*i + j].keydata = copy_key((q & 0x01) == 1 
                                                     ? (st->constant_keys[1]) 
                                                     : (st->constant_keys[0]));
          q = q >> 1;
        }
    }
  st->bob_in_size = 8 * strlen(inputs);
}

void setup_alice_inputs_from_hex_string(struct PCFState * st, const char * inputs)
{
  uint32_t i, j;
  st->alice_inputs = (struct wire *)malloc(strlen(inputs)*4*sizeof(struct wire));
  for(i = 0; i < strlen(inputs); i++)
    {
      uint8_t q;
      if((inputs[i] >= '0') && (inputs[i] <= '9'))
        q = inputs[i] - '0';
      else if((inputs[i] >= 'A') && (inputs[i] <= 'F'))
        q = inputs[i] - 'A' + 0xA;
      else if((inputs[i] >= 'a') && (inputs[i] <= 'f'))
        q = inputs[i] - 'a' + 0xA;
      else
        assert(0);

      for(j = 0; j < 4; j++)
        {
          st->alice_inputs[4*i + j].flags = UNKNOWN_WIRE;
          st->alice_inputs[4*i + j].keydata = copy_key((q & 0x01) == 1 
                                                     ? (st->constant_keys[1])
                                                     : (st->constant_keys[0]));
          q = q >> 1;
        }
    }
  st->alice_in_size = 4 * strlen(inputs);
}

void setup_bob_inputs_from_hex_string(struct PCFState * st, const char * inputs)
{
  uint32_t i, j;
  st->bob_inputs = (struct wire *)malloc(strlen(inputs)*4*sizeof(struct wire));
  for(i = 0; i < strlen(inputs); i++)
    {
      uint8_t q;
      if((inputs[i] >= '0') && (inputs[i] <= '9'))
        q = inputs[i] - '0';
      else if((inputs[i] >= 'A') && (inputs[i] <= 'F'))
        q = inputs[i] - 'A' + 0xA;
      else if((inputs[i] >= 'a') && (inputs[i] <= 'f'))
        q = inputs[i] - 'a' + 0xA;
      else
        assert(0);

      for(j = 0; j < 4; j++)
        {
          st->bob_inputs[4*i + j].flags = UNKNOWN_WIRE;
          st->bob_inputs[4*i + j].keydata = copy_key((q & 0x01) == 1 
                                                     ? (st->constant_keys[1])
                                                     : (st->constant_keys[0]));
          q = q >> 1;
        }
    }
  st->bob_in_size = 4 * strlen(inputs);
}

int main(int argc, char**argv)
{
  struct PCFState * st;
  struct PCFGate * g;

  st = load_pcf_file(argv[1], &key0, &key1, copy_key);
  st->delete_key = delete_key;
  st->callback = m_callback;
  setup_alice_inputs_from_string(st, "AC000000");
  setup_bob_inputs_from_string(st, "xy(S//NF)000000");

  g = get_next_gate(st);
  while(g != 0)
    g = get_next_gate(st);

  return 0;
}
