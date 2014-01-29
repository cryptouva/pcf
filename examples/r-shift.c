unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);

void main(void)
{
  unsigned int x = alice(0);
  output_alice(x >> 1);
}
