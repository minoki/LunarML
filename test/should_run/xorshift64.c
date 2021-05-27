#include <stdio.h>
#include <inttypes.h>

uint64_t seed = 0xBADCAFE;
uint64_t xorshift64(void)
{
    seed ^= seed << 13;
    seed ^= seed >> 7;
    return seed ^= seed << 17;
}

int main(void)
{
    for (int i = 0; i < 100; ++i) {
        printf("0x%" PRIX64 "\n", xorshift64());
    }
}
