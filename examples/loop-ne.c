unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);

void main(void)
{
  unsigned int x = alice(0);
  unsigned int i;
  for(i = 5; i != 1; i--)
    output_alice(x);
}
