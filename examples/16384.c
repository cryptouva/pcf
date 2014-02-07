unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);

void main(void)
{
  unsigned int x=0;
  unsigned int i;
  unsigned int borrow=0;

  for(i = 0; i < 32;)
    {
      unsigned int a1 = alice(i);
      unsigned int b1 = bob(i);
      unsigned int b = borrow;
      borrow = 0;

      if(a1 < (b1 + b))
        borrow = 1;

      i += 32;
    }
  if(borrow == 0) x= 0x1;
  else x = 0xffffffff;

  output_alice(x);
}
