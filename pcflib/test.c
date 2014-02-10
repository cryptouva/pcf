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

uint32_t i_key = 0;

void * m_callback(struct PCFState * st, struct PCFGate * gate)
{
  if(gate->tag == TAG_INTERNAL)
    {
      uint8_t bits[4];
      int8_t i = 0;
      uint8_t tab = gate->truth_table;
      uint32_t wire1val, wire2val;
      for(i = 0; i < 4; i++)
        {
          bits[i] = tab & 0x01;
          tab = tab >> 1;
        }

      /* if(st->wires[gate->wire1].flags == KNOWN_WIRE) */
      /*   wire1val = st->wires[gate->wire1].value; */
      /* else */
        wire1val = *((uint32_t*)st->wires[gate->wire1].keydata);

      /* if(st->wires[gate->wire2].flags == KNOWN_WIRE) */
      /*   wire2val = st->wires[gate->wire2].value; */
      /* else */
        wire2val = *((uint32_t*)st->wires[gate->wire2].keydata);

      if(wire1val + 2 * wire2val >= 4)
        fprintf(stderr, "Problem!\n");

      assert(wire1val < 2);
      assert(wire2val < 2);

      i_key = bits[wire1val + 2*wire2val];

      return &i_key;
    }
  else if(gate->tag == TAG_INPUT_A)
    {
      printf("Alice input (%d) %x\n", gate->wire1, *((uint32_t*)st->alice_inputs[gate->wire1].keydata));
      return st->alice_inputs[gate->wire1].keydata;
    }
  else if(gate->tag == TAG_INPUT_B)
    {

      printf("Bob input (%d) %x\n", gate->wire1, *((uint32_t*)st->bob_inputs[gate->wire1].keydata));
      return st->bob_inputs[gate->wire1].keydata;
    }
  else
    {
      i_key = 0;
      printf("Output bit (wire = %d): %x [%x]\n", gate->wire1, st->wires[gate->wire1].value, *((uint32_t*)st->wires[gate->wire1].keydata));
      return &i_key;
    }
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
  //  char line[LINE_MAX];
  struct PCFState * st;
  struct PCFGate * g;

  //  fgets(line, LINE_MAX-1, stdin);

  //  assert(strlen(line) > 0);
  st = load_pcf_file(argv[1], &key0, &key1, copy_key);
  st->delete_key = delete_key;
  st->callback = m_callback;
  setup_alice_inputs_from_hex_string(st, "00000000000000000000000000");//"5FC83262147D140DE3DE952304"); //"AC000000");
  setup_bob_inputs_from_string(st, "TS--ROCKYBEACH");  //"DDFDC090F019861BC5E8D37562"); //"(S//NF)abcdef");

  g = get_next_gate(st);
  while(g != 0)
    g = get_next_gate(st);

  return 0;
}
