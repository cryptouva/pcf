unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);
void output_bob(unsigned int);

void main(void)
{
  unsigned int x = 0;
  if(alice(0) >= bob(0))
    x = 5;
  x += 7;
  output_alice(x);
}
